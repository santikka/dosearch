#' Call the `csisearch` Algorithm from R for LDAGs
#'
#' @inheritParams dosearch
#' @noRd
get_derivation_ldag <- function(data, query, graph, control = list()) {
  control <- control_defaults(control)
  cl <- list(
    data = data,
    query = query,
    graph = graph,
    transportability = NULL,
    selection_bias = NULL,
    missing_data = NULL,
    control = control
  )
  if (control$empty) {
    return(empty_output(cl))
  }
  args <- list(
    dir_lhs = integer(0L),
    dir_rhs = integer(0L),
    vars = character(0L),
    nums = integer(0L),
    n = 0L,
    con_vars = character(0L),
    intv_vars = character(0L),
    parents = list(),
    contexts = c(),
    label_map = NULL,
    local_csi = list()
  )
  args <- transform_graph_ldag(args, graph)
  args <- parse_labels(args)
  args <- parse_query_ldag(args, query)
  args <- parse_input_distributions_ldag(args, data)
  args <- validate_input_distributions_ldag(args)
  args <- validate_query_ldag(args)
  res <- initialize_csisearch(
    as.numeric(args$nums[args$dir_lhs]),
    as.numeric(args$nums[args$dir_rhs]),
    as.character(args$vars),
    args$p_list,
    args$q_vec,
    args$label_map,
    args$local_csi,
    to_dec(args$nums[args$con_vars], args$n),
    to_dec(args$nums[args$intv_vars], args$n),
    args$n,
    control$time_limit,
    control$rules,
    control$benchmark,
    control$benchmark_rules,
    control$draw_derivation,
    control$draw_all,
    control$formula,
    control$improve,
    control$heuristic,
    control$cache,
    control$verbose
  )
  res$call <- cl
  structure(
    res[
      c(
        TRUE, # always include identifiability result
        control$formula,
        control$draw_derivation,
        control$benchmark,
        control$benchmark_rules,
        TRUE # always include the call
      )
    ],
    class = "dosearch"
  )
}

#' Transform the Input LDAG
#'
#' @inheritParams dosearch
#' @param args A `list` of arguments for `initialize_csisearch`
#' @param graph The graph as a `character` string.
#' @noRd
transform_graph_ldag <- function(args, graph) {
  graph_lines <- trimws(strsplit(graph, "\r|\n")[[1L]])
  graph_lines <- graph_lines[nzchar(graph_lines)]
  row_pattern <- "^[^\\:]+(\\:[^\\:]+){0,1}$"
  malformed_lines <- !grepl(row_pattern, graph_lines, perl = TRUE)
  if (any(malformed_lines)) {
    stop_(
      "Invalid graph, malformed lines found: ",
      cs(graph_lines[malformed_lines])
    )
  }
  graph_split <- strsplit(graph_lines, ":")
  graph_lines <- vapply(graph_split, "[[", character(1L), 1L)
  args <- transform_graph_dag(
    args,
    paste0(graph_lines, collapse = "\n"),
    NULL
  )
  if (!is.null(args$bi_lhs)) {
    stop_(
      "Invalid graph, bidirected edges are not supported for LDAGs."
    )
  }
  labeled <- which(lengths(graph_split) > 1L)
  labels_split <- list()
  if (length(labeled) > 0L) {
    labels <- vapply(graph_split[labeled], "[[", character(1L), 2L)
    labels <- gsub("\\s+", "", labels)
    labels_split <- strsplit(labels, "[;]")
    labels_split <- lapply(labels_split, strsplit, "[,]")
    args$targets <- lapply(seq_along(labels_split), function(i) {
      c("from" = args$dir_lhs[labeled[i]], "to" = args$dir_rhs[labeled[i]])
    })
  }
  args$labels <- labels_split
  intv <- args$dir_lhs[substr(args$dir_lhs, 1L, 2L) == "I_"]
  ivar <- which(args$vars %in% intv)
  args$vars <- args$vars[c(setdiff(seq_len(args$n), ivar), ivar)]
  args$intv_vars <- args$vars[which(args$vars %in% intv)]
  args$nums <- seq_len(args$n)
  names(args$vars) <- args$nums
  names(args$nums) <- args$vars
  for (v in args$vars) {
    args$parents[[v]] <- character(0L)
  }
  for (i in seq_along(args$dir_rhs)) {
    args$parents[[args$dir_rhs[i]]] <- union(
      args$parents[[args$dir_rhs[i]]],
      args$dir_lhs[i]
    )
  }
  args
}

#' Parse Edge Labels of an LDAG
#'
#' @param args A `list` of arguments for `initialize_csisearch`
#' @noRd
parse_labels <- function(args) {
  input_labels <- matrix(0L, sum(lengths(args$labels)), 5L)
  index <- 0L
  args$null_context <- c()
  args$inferred <- 0L
  args$inferred_labels <- matrix(0L, 0L, 5L)
  vanishing <- matrix(0L, 0L, 2L)
  # Loop labels
  for (i in seq_along(args$labels)) {
    from <- args$targets[[i]]["from"]
    to <- args$targets[[i]]["to"]
    pa <- setdiff(args$parents[[to]], from)
    n_pa <- length(pa)
    vals <- expand.grid(rep(list(c(0L, 1L)), n_pa))
    names(vals) <- pa
    vals$present <- rep(FALSE, nrow(vals))
    # Loop individual assignments within label
    for (j in seq_along(args$labels[[i]])) {
      index <- index + 1L
      label_split <- strsplit(args$labels[[i]][[j]], "[=]")
      label_lhs <- vapply(label_split, "[[", character(1L), 1L)
      label_rhs <- vapply(label_split, "[[", character(1L), 2L)
      msg <- ""
      validate_label(from, to, pa, label_lhs)
      intv <- substr(label_lhs, 1L, 2L) == "I_"
      args$con_vars <- c(args$con_vars, label_lhs[!intv])
      zero <- which(label_rhs == 0L)
      one <- which(label_rhs == 1L)
      input_labels[index, 1L] <- to_dec(args$nums[label_lhs[zero]], args$n)
      input_labels[index, 2L] <- to_dec(args$nums[label_lhs[one]], args$n)
      input_labels[index, 3L] <- args$nums[from]
      input_labels[index, 4L] <- args$nums[to]
      input_labels[index, 5L] <- to_dec(args$nums[pa], args$n)
      vals <- label_values(vals, zero, one, pa, label_lhs)
    }
    if (all(vals$present)) {
      vanishing <- rbind(vanishing, c(args$nums[from], args$nums[to]))
    }
    args <- infer_labels(args, vals, from, to, pa)
  }
  input_labels <- rbind(input_labels, args$inferred_labels)
  input_labels <- input_labels[!duplicated(input_labels), , drop = FALSE]
  args$con_vars <- unique(args$con_vars)
  args$label_map <- list()
  args <- parse_contexts(args, input_labels, vanishing)
  args <- parse_interventions(args, input_labels, vanishing)
  args$label_map[args$null_context] <- NULL
  if (nrow(vanishing) > 0L) {
    edge_mat <- cbind(args$nums[args$dir_lhs], args$nums[args$dir_rhs])
    present <- !duplicated(
      rbind(edge_mat, vanishing),
      fromLast = TRUE
    )[seq_len(nrow(edge_mat))]
    args$dir_lhs <- args$dir_lhs[present]
    args$dir_rhs <- args$dir_rhs[present]
  }
  args
}

#' Validate an Edge Label of an LDAG
#'
#' @param from A `character` string, vertex name of the edge's origin.
#' @param to A `character` string, vertex, name of the edge's endpoint.
#' @param pa A `character` vector, vertex names of the parents of `to`.
#' @param label_lhs A `character` vector, variable names bound by the edge
#'   label value assignments.
#' @noRd
validate_label <- function(from, to, pa, label_lhs) {
  if (from %in% label_lhs) {
    stop_(
      "Invalid label for edge ", from, " -> ", to, ": ",
      from, " cannot appear in the label."
    )
  }
  if (to %in% label_lhs) {
    stop_(
      "Invalid label for edge ", from, " -> ", to, ": ",
      to, " cannot appear in the label."
    )
  }
  if (any(duplicated(label_lhs))) {
    stop_(
      "Invalid label for edge ", from, " -> ", to, ": ",
      "duplicate assignment."
    )
  }
  if (any(!label_lhs %in% pa)) {
    stop_(
      "Invalid label for edge ", from, " -> ", to, ": ",
      "only other parents of ", to, " may be assigned."
    )
  }
}

#' Get Value Combinations that Appear in an Edge Label
#'
#' @inheritParams validate_label
#' @param vals A `data.frame` of possible value assignments of the parents.
#' @param zero An `integer` vector denoting the indices of assignment to 0.
#' @param one An `integer` vector denoting the indices of assignment to 1.
#' @noRd
label_values <- function(vals, zero, one, pa, label_lhs) {
  zl <- length(zero)
  ol <- length(one)
  if (zl == 0L) {
    ones <- vals[, which(pa %in% label_lhs[one]), drop = FALSE]
    if (nrow(ones) > 0L) {
      vals[
        which(apply(ones, 1L, function(x) all(x == 1))),
        "present"
      ] <- TRUE
    }
  } else if (ol == 0L) {
    zeros <- vals[, which(pa %in% label_lhs[zero]), drop = FALSE]
    if (nrow(zeros) > 0L) {
      vals[
        which(apply(zeros, 1, function(x) all(x == 0L))),
        "present"
      ] <- TRUE
    }
  } else {
    zeros <- vals[, which(pa %in% label_lhs[zero]), drop = FALSE]
    ones <- vals[, which(pa %in% label_lhs[one]), drop = FALSE]
    ind_z <- which(apply(zeros, 1L, function(x) all(x == 0L)))
    ind_o <- which(apply(ones, 1L, function(x) all(x == 1L)))
    ind_zo <- intersect(ind_z, ind_o)
    if (length(ind_zo) > 0L) {
      vals[ind_zo, "present"] <- TRUE
    }
  }
  vals
}

#' Infer Edge Labels That Were Not Explicitly Provided
#'
#' @inheritParams validate_label
#' @inheritParams label_values
#' @param args A `list` of arguments for `initialize_csisearch`
#' @noRd
infer_labels <- function(args, vals, from, to, pa) {
  n_pa <- length(pa)
  n_sets <- nrow(vals) - 1L
  for (j in seq_min2(n_sets)) {
    sub_pa <- pa[which(vals[j, seq_len(n_pa)] == 1L)]
    sub_ind <- which(pa %in% sub_pa)
    sub_vals <- vals[, c(sub_ind, n_pa + 1L)]
    assignments <- expand.grid(rep(list(c(0L, 1L)), length(sub_pa)))
    names(assignments) <- sub_pa
    for (k in seq_len(nrow(assignments))) {
      zero <- sub_pa[which(assignments[k, ] == 0L)]
      one <- sub_pa[which(assignments[k, ] == 1L)]
      assign_ind <- apply(
        sub_vals[, -ncol(sub_vals), drop = FALSE],
        1L,
        function(x) {
          identical(
            as.integer(x),
            as.integer(assignments[k, ])
          )
        }
      )
      if (any(assign_ind) && all(sub_vals[assign_ind, "present"])) {
        args$inferred <- args$inferred + 1L
        args$inferred_labels <- rbind(
          args$inferred_labels,
          c(
            to_dec(args$nums[zero], args$n),
            to_dec(args$nums[one], args$n),
            args$nums[from],
            args$nums[to],
            to_dec(args$nums[pa], args$n)
          )
        )
      }
    }
  }
  args
}

#' Parse Context Implied by the Edge Labels
#'
#' @param args A `list` of arguments for `initialize_csisearch`
#' @param input_labels A `matrix` with 5 columns that holds information on
#'   the explicit and implied labels of the graph (one label per row).
#'   The columns are:
#'   1. Set of variables assigned to 0.
#'   2. Set of variables assigned to 1.
#'   3. The origin of the edge.
#'   4. The endpoint of the edge.
#'   5. The (other) parents of the edge endpoint.
#' @param vanishing A `matrix` denoting edges that do not exist in any context
#' @noRd
parse_contexts <- function(args, input_labels, vanishing) {
  all_contexts <- expand.grid(rep(list(c(0L, 1L)), length(args$con_vars)))
  args$n_con <- nrow(all_contexts)
  args$index_csi <- 0L
  for (i in seq_min2(args$n_con)) {
    sub_vars <- args$con_vars[which(all_contexts[i, ] == 1)]
    con_vals <- expand.grid(rep(list(c(0L, 1L)), length(sub_vars)))
    args$label_map[[i - 1L]] <- list(
      vars = to_dec(args$nums[sub_vars], args$n),
      contexts = vector(mode = "list", length = nrow(con_vals))
    )
    equiv_ind <- 0L
    unique_context <- list()
    for (j in seq_len(nrow(con_vals))) {
      zero <- sub_vars[which(con_vals[j, ] == 0L)]
      one <- sub_vars[which(con_vals[j, ] == 1L)]
      z_set <- to_dec(args$nums[zero], args$n)
      o_set <- to_dec(args$nums[one], args$n)
      args$label_map[[i - 1L]][["contexts"]][[j]]$zero <- z_set
      args$label_map[[i - 1L]][["contexts"]][[j]]$one <- o_set
      args <- parse_local_csi(args, z_set, o_set, input_labels, vanishing)
      args$label_map[[i - 1L]][["contexts"]][[j]]$from <- args$endpoints[, 1L]
      args$label_map[[i - 1L]][["contexts"]][[j]]$to <- args$endpoints[, 2L]
      pos <- Position(
        function(x) {
          identical(args$endpoints[, 1L], x$from) &&
            identical(args$endpoints[, 2L], x$to)
        },
        unique_context
      )
      if (is.na(pos)) {
        equiv_ind <- equiv_ind + 1L
        args$label_map[[i - 1L]][["contexts"]][[j]]$equivalence <- equiv_ind
        unique_context[[equiv_ind]] <- list(
          from = args$endpoints[, 1L],
          to = args$endpoints[, 2L]
        )
      } else {
        args$label_map[[i - 1L]][["contexts"]][[j]]$equivalence <- pos
      }
    }
    from_lengths <- vapply(
      args$label_map[[i - 1L]][["contexts"]],
      function(x) length(x[["from"]]),
      integer(1L)
    )
    if (all(from_lengths == 0)) {
      args$null_context <- c(args$null_context, i - 1L)
    }
  }
  args
}

#' Parse a Local Context-Specific (Conditional) Independence
#'
#' @inheritParams parse_contexts
#' @param z_set An `integer` representing the set of variables assigned to 0
#'   in the current context
#' @param o_set An `integer` representing the set of variables assigned to 1
#'   in the current context.
#' @noRd
parse_local_csi <- function(args, z_set, o_set, input_labels, vanishing) {
  args$endpoints <- matrix(0L, 0L, 2L)
  for (k in seq_len(nrow(input_labels))) {
    z_inp <- input_labels[k, 1L]
    o_inp <- input_labels[k, 2L]
    if ((bitwAnd(z_set, z_inp) == z_inp && bitwAnd(o_set, o_inp) == o_inp)) {
      csi_v <- apply(vanishing, 1L, function(x) {
        isTRUE(all.equal(x, input_labels[k, 3L:4L]))
      })
      if (!any(csi_v)) {
        args$endpoints <- rbind(args$endpoints, input_labels[k, 3L:4L])
        pa <- input_labels[k, 5L]
        lab <- bitwOr(z_set, o_set)
        if (pa == lab) {
          args$index_csi <- args$index_csi + 1L
          args$local_csi[[args$index_csi]] <- list(
            x = to_dec(input_labels[k, 3L], args$n),
            y = to_dec(input_labels[k, 4L], args$n),
            z = pa,
            zero = z_set,
            one = o_set
          )
        }
      }
    }
  }
  args$endpoints <- unique(args$endpoints)
  args
}

#' Parse Labels Implied by Intervention Nodes
#'
#' @inheritParams parse_contexts
#' @noRd
parse_interventions <- function(args, input_labels, vanishing) {
  all_interventions <- expand.grid(
    rep(list(c(0L, 1L)), length(args$intv_vars))
  )
  n_intv <- nrow(all_interventions)
  for (i in seq_min2(n_intv)) {
    index <- max(args$n_con - 1L, 0L) + i - 1L
    sub_vars <- args$intv_vars[which(all_interventions[i, ] == 1L)]
    o <- to_dec(args$nums[sub_vars], args$n)
    args$label_map[[index]] <- list(
      vars = o,
      contexts = list(list(zero = 0L, one = o))
    )
    endpoints <- matrix(0L, 0L, 2L)
    for (k in seq_len(nrow(input_labels))) {
      z_inp <- input_labels[k, 1L]
      o_inp <- input_labels[k, 2L]
      if (z_inp == 0 && bitwAnd(o, o_inp) == o_inp) {
        csi_v <- apply(vanishing, 1L, function(x) {
          isTRUE(all.equal(x, input_labels[k, 3L:4L]))
        })
        if (!any(csi_v)) {
          endpoints <- rbind(endpoints, input_labels[k, 3L:4L])
        }
      }
    }
    endpoints <- unique(endpoints)
    args$label_map[[index]][["contexts"]][[1L]]$from <- endpoints[, 1L]
    args$label_map[[index]][["contexts"]][[1L]]$to <- endpoints[, 2L]
    args$label_map[[index]][["contexts"]][[1L]]$equivalence <- 1L
  }
  args
}

#' Parse a Distribution in the Internal Character Format for LDAGs
#'
#' @param args A `list` of arguments for `initialize_dosearch`.
#' @param d A `character` string representing the distribution.
#' @param type A `character` string indicating the distribution type.
#' @param out A `character` string indicating the a name of `args` to
#'   in which to assign the results.
#' @param i An `integer` indicating the iteration.
#' @noRd
parse_distribution_ldag <- function(args, d, type, out, i) {
  zero <- character(0L)
  one <- character(0L)
  d_parsed <- gsub("\\s+", "", d)
  d_parsed <- gsub("do", "$", d_parsed)
  d_split <- match_distribution_ldag(d_parsed)
  if (any(is.na(d_split[[1L]]))) {
    stop_(
      stop_("Unable to parse ", type, ": ", d)
    )
  }
  d_null <- vapply(d_split, is.null, logical(1L))
  for (j in which(!d_null)) {
    dup <- duplicated(d_split[[j]])
    if (any(dup)) {
      stop_(
        "Invalid ", type, ": ", d, ", ",
        "duplicated variables ", d_split[[j]][dup], "."
      )
    }
    equals <- grep("=", d_split[[j]], value = FALSE)
    if (length(equals) > 0L) {
      eq_split <- strsplit(d_split[[j]][equals], "[=]")
      eq_lhs <- vapply(eq_split, "[[", character(1L), 1L)
      eq_lhs <- gsub("\\s+", "", eq_lhs)
      eq_rhs <- vapply(eq_split, "[[", character(1L), 2L)
      eq_rhs <- gsub("\\s+", "", eq_rhs)
      uniq_rhs <- unique(eq_rhs)
      if (!(uniq_rhs[1L] %in% 0L:1L)) {
        stop_(
          "Invalid value assignment in ", type, ": ", d, "."
        )
      }
      d_split[[j]][equals] <- eq_lhs
      z <- which(eq_rhs == 1L)
      o <- which(eq_rhs == 0L)
      zero <- c(zero, eq_lhs[eq_rhs == 0L])
      one <- c(one, eq_lhs[eq_rhs == 1L])
    }
  }
  d1_new <- d_split[[1L]][which(!(d_split[[1L]] %in% args$vars))]
  d2_new <- d_split[[2L]][which(!(d_split[[2L]] %in% args$vars))]
  new_vars <- unique(c(d1_new, d2_new))
  args <- add_new_vars(args, new_vars)
  d_process <- list(
    args$nums[d_split[[1L]]],
    args$nums[d_split[[2L]]],
    args$nums[zero],
    args$nums[one],
    d
  )
  if (i > 0L) {
    args[[out]][[i]] <- d_process
  } else {
    args[[out]] <- d_process
  }
  args
}

#' Parse a Target Distribution
#'
#' @inheritParams dosearch
#' @param args A `list` of arguments for `initialize_csisearch`.
#' @noRd
parse_query_ldag <- function(args, query) {
  parse_distribution_ldag(args, query, "target distribution", "q_process", 0L)
}

#' Parse Input Distributions
#'
#' @inheritParams dosearch
#' @param args A `list` of arguments for `initialize_csisearch`.
#' @noRd
parse_input_distributions_ldag <- function(args, data) {
  data_split <- strsplit(data, "\r|\n")[[1]]
  data_split <- gsub("\\s+", "", data_split)
  data_split <- data_split[which(nzchar(data_split))]
  args$p_process <- vector(mode = "list", length = length(data_split))
  for (i in seq_along(data_split)) {
    args <- parse_distribution_ldag(
      args,
      data_split[[i]],
      "input distribution",
      "p_process",
      i
    )
  }
  args
}

#' Check the Validity of a Distribution
#'
#' @param args A `list` of arguments for `initialize_dosearch`.
#' @param d An `integer` vector of length 4 denoting the distribution.
#' @noRd
validate_distribution_ldag <- function(args, msg, d, d_str) {
  if (bitwAnd(d[1L], d[2L]) > 0L) {
    stop_(
      "Invalid input distribution ", d_str, ": ",
      "same variable on the left and right-hand side."
    )
  }
}

#' Check the Validity of Input Distributions
#'
#' @param args A `list` of arguments for `initialize_csisearch`.
#' @noRd
validate_input_distributions_ldag <- function(args) {
  for (i in seq_along(args$p_process)) {
    p <- args$p_process[[i]]
    args$p_list[[i]] <- c(
      to_dec(p[[1L]], args$n),
      to_dec(p[[2L]], args$n),
      to_dec(p[[3L]], args$n),
      to_dec(p[[4L]], args$n)
    )
    validate_distribution_ldag(
      args,
      "Invalid input distribution ",
      args$p_list[[i]],
      p[[5L]]
    )
  }
  args
}

#' Check the Validity of a Target Distribution
#'
#' @param args A `list` of arguments for `initialize_csisearch`.
#' @noRd
validate_query_ldag <- function(args) {
  q <- args$q_process
  args$q_vec <- c(
    to_dec(q[[1L]], args$n),
    to_dec(q[[2L]], args$n),
    to_dec(q[[3L]], args$n),
    to_dec(q[[4L]], args$n)
  )
  validate_distribution_ldag(
    args,
    "Invalid query ",
    args$q_vec,
    q[[5L]]
  )
  args
}

#' Determine the Type of a Distribution
#'
#' Checks whether a distribution is of the form p(y) or p(y|z).
#'
#' @param d A `character` string representing the distribution.
#' @noRd
match_distribution_ldag <- function(d) {
  dist_pattern <- character(2L)
  # Pattern for p(y)
  dist_pattern[1L] <- "^[Pp]\\(([^|\\$\\),]++(?>,[^|\\$\\),]+)*)\\)$"
  # Pattern for p(y|z)
  dist_pattern[2L] <- paste0(
    "^[Pp]\\(([^|\\$\\),]++(?>,[^|\\$\\),]+)*)",
    "[|]",
    "([^|\\$\\),]++(?>,[^|\\$\\),]+)*)\\)$"
  )
  matches <- lapply(dist_pattern, function(p) {
    regexec(p, d, perl = TRUE)
  })
  match_lens <- vapply(
    matches,
    function(x) {
      length(attr(x[[1L]], "match.length"))
    },
    integer(1L)
  )
  best_match <- which.max(match_lens)[1L]
  parts <- regmatches(d, matches[[best_match]])[[1L]]
  d_split <- vector(mode = "list", length = 2L)
  d_split[[1L]] <- strsplit(parts[2L], "[,]")[[1L]]
  if (best_match == 2) {
    d_split[[2L]] <- strsplit(parts[3L], "[,]")[[1L]]
  }
  d_split
}
