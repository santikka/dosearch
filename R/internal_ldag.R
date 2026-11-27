#' Call the `csisearch` Algorithm from R for LDAGs
#'
#' @inheritParams dosearch
#' @noRd
get_derivation_ldag <- function(data, query, graph, control = list()) {
  control <- control_defaults(control)
  control$rules <- validate_rules(
    control$rules,
    c(-8L, -7L, -5L, -4L, -3L, -2L, 0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L)
  )
  cl <- list(
    data = data,
    query = query,
    graph = graph,
    transportability = NULL,
    selection_bias = NULL,
    missing_data = NULL,
    control = control,
    case_reasoning = FALSE
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
  args$graph_vars <- args$vars
  args <- parse_query_ldag(args, query)
  args <- parse_data_ldag(args, data)
  args <- validate_data_ldag(args)
  args <- validate_query_ldag(args)
  unknown_con <- setdiff(control$con_vars, args$vars)
  if (length(unknown_con) > 0L) {
    stop_(
      "Unknown context variables in argument `control`: ", cs(unknown_con)
    )
  }
  # Contexts are constructed only once all context variables are known,
  # because the search relies on the settings of every combination of them
  args$con_vars <- union(args$con_vars, control$con_vars)
  args <- parse_contexts(args, args$input_labels)
  args <- parse_interventions(args, args$input_labels)
  args$label_map[args$null_context] <- NULL
  if (control$warn) {
    warn_unknown_query_vars(args)
    warn_unknown_input_vars(args)
  }
  res <- initialize_csisearch(
    as.integer(args$nums[args$dir_lhs]),
    as.integer(args$nums[args$dir_rhs]),
    as.character(args$vars),
    args$p_list,
    args$q_list,
    args$label_map,
    args$local_csi,
    as_set(args$nums[args$con_vars]),
    as_set(args$nums[args$intv_vars]),
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
        "identifiable", # always include identifiability result
        if (control$formula) "formula",
        # the variants are only included if case-by-case reasoning was used
        if (control$formula && length(res$variants) > 0L) "variants",
        if (control$draw_derivation) "derivation",
        if (control$benchmark) "time",
        if (control$benchmark_rules) "rule_times",
        "call" # always include the call
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
    # Edges are read from the labeled lines themselves so that duplicated
    # lines elsewhere in the graph cannot shift labels to other edges
    edge_split <- strsplit(trimws(graph_lines[labeled]), "\\s+")
    edge_keys <- vapply(
      edge_split,
      function(x) paste(x[1L], x[3L]),
      character(1L)
    )
    unique_keys <- unique(edge_keys)
    # Labels given on separate lines for the same edge are combined
    labels_split <- lapply(unique_keys, function(k) {
      do.call(c, labels_split[edge_keys == k])
    })
    args$targets <- lapply(
      edge_split[match(unique_keys, edge_keys)],
      function(x) {
        c("from" = x[1L], "to" = x[3L])
      }
    )
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
#' Each explicit or inferred label is stored as a `list` with the elements
#' `zero` and `one` (the sets of vertices assigned to 0 and 1), `from` and `to`
#' (the endpoints of the edge), `pa` (the set of other parents of `to`) and
#' `vanishing` (whether the edge is absent in every context).
#'
#' @param args A `list` of arguments for `initialize_csisearch`
#' @noRd
parse_labels <- function(args) {
  input_labels <- vector(mode = "list", length = sum(lengths(args$labels)))
  index <- 0L
  args$null_context <- c()
  args$inferred_labels <- list()
  vanishing <- matrix(0L, 0L, 2L)
  # Loop labels
  for (i in seq_along(args$labels)) {
    from <- args$targets[[i]]["from"]
    to <- args$targets[[i]]["to"]
    pa <- setdiff(args$parents[[to]], from)
    n_pa <- length(pa)
    vals <- as.matrix(expand.grid(rep(list(c(0L, 1L)), n_pa)))
    present <- logical(nrow(vals))
    # Loop individual assignments within label
    for (j in seq_along(args$labels[[i]])) {
      index <- index + 1L
      label_split <- strsplit(args$labels[[i]][[j]], "[=]")
      if (any(malformed_pairs(label_split))) {
        stop_(
          "Invalid label for edge ", from, " -> ", to, ": ",
          "malformed value assignment."
        )
      }
      label_lhs <- vapply(label_split, "[[", character(1L), 1L)
      label_rhs <- vapply(label_split, "[[", character(1L), 2L)
      validate_label(from, to, pa, label_lhs, label_rhs)
      intv <- substr(label_lhs, 1L, 2L) == "I_"
      args$con_vars <- c(args$con_vars, label_lhs[!intv])
      zero <- which(label_rhs == 0L)
      one <- which(label_rhs == 1L)
      input_labels[[index]] <- new_label(
        args, label_lhs[zero], label_lhs[one], from, to, pa
      )
      present <- present | label_values(vals, zero, one, pa, label_lhs)
    }
    if (all(present)) {
      vanishing <- rbind(vanishing, c(args$nums[from], args$nums[to]))
    }
    args <- infer_labels(args, vals, present, from, to, pa)
  }
  input_labels <- c(input_labels, args$inferred_labels)
  input_labels <- input_labels[!duplicated(input_labels)]
  # Whether an edge vanishes depends only on the edge, not on the context
  vanishing_keys <- paste(vanishing[, 1L], vanishing[, 2L])
  for (k in seq_along(input_labels)) {
    input_labels[[k]]$vanishing <- paste(
      input_labels[[k]]$from,
      input_labels[[k]]$to
    ) %in% vanishing_keys
  }
  args$con_vars <- unique(args$con_vars)
  args$input_labels <- input_labels
  args$label_map <- list()
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

#' Create an Edge Label
#'
#' @param args A `list` of arguments for `initialize_csisearch`
#' @param zero A `character` vector of variables assigned to 0.
#' @param one A `character` vector of variables assigned to 1.
#' @param from A `character` string, vertex name of the edge's origin.
#' @param to A `character` string, vertex name of the edge's endpoint.
#' @param pa A `character` vector, vertex names of the other parents of `to`.
#' @noRd
new_label <- function(args, zero, one, from, to, pa) {
  list(
    zero = as_set(args$nums[zero]),
    one = as_set(args$nums[one]),
    from = as.integer(args$nums[from]),
    to = as.integer(args$nums[to]),
    pa = as_set(args$nums[pa])
  )
}

#' Validate an Edge Label of an LDAG
#'
#' @param from A `character` string, vertex name of the edge's origin.
#' @param to A `character` string, vertex, name of the edge's endpoint.
#' @param pa A `character` vector, vertex names of the parents of `to`.
#' @param label_lhs A `character` vector, variable names bound by the edge
#'   label value assignments.
#' @param label_rhs A `character` vector, the values assigned to the
#'   variables in `label_lhs`.
#' @noRd
validate_label <- function(from, to, pa, label_lhs, label_rhs) {
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
  if (any(!label_rhs %in% c("0", "1"))) {
    stop_(
      "Invalid label for edge ", from, " -> ", to, ": ",
      "assigned values must be either 0 or 1."
    )
  }
}

#' Get Value Combinations that Appear in an Edge Label
#'
#' @inheritParams validate_label
#' @param vals An `integer` matrix of all possible value assignments of the
#'   parents (one assignment per row).
#' @param zero An `integer` vector denoting the indices of assignment to 0.
#' @param one An `integer` vector denoting the indices of assignment to 1.
#' @return A `logical` vector indicating the rows of `vals` that are
#'   consistent with the label.
#' @noRd
label_values <- function(vals, zero, one, pa, label_lhs) {
  zeros <- vals[, which(pa %in% label_lhs[zero]), drop = FALSE]
  ones <- vals[, which(pa %in% label_lhs[one]), drop = FALSE]
  rowSums(zeros != 0L) == 0L & rowSums(ones != 1L) == 0L
}

#' Infer Edge Labels That Were Not Explicitly Provided
#'
#' A label is inferred for an assignment to a proper subset of the parents
#' if every full assignment of the parents consistent with it is present.
#'
#' @inheritParams validate_label
#' @inheritParams label_values
#' @param args A `list` of arguments for `initialize_csisearch`
#' @param present A `logical` vector indicating the rows of `vals` that are
#'   present in the labels of the edge.
#' @noRd
infer_labels <- function(args, vals, present, from, to, pa) {
  n_sets <- nrow(vals) - 1L
  for (j in seq_min2(n_sets)) {
    sub_ind <- which(vals[j, ] == 1L)
    sub_pa <- pa[sub_ind]
    n_sub <- length(sub_ind)
    pow <- 2L^(seq_len(n_sub) - 1L)
    # Encode the assignment of each row to the subset as an integer
    # in the same order as expand.grid
    codes <- drop(vals[, sub_ind, drop = FALSE] %*% pow)
    complete <- tapply(
      present,
      factor(codes, levels = seq.int(0L, 2L^n_sub - 1L)),
      all
    )
    for (k in which(complete)) {
      bits <- ((k - 1L) %/% pow) %% 2L
      args$inferred_labels[[length(args$inferred_labels) + 1L]] <- new_label(
        args, sub_pa[bits == 0L], sub_pa[bits == 1L], from, to, pa
      )
    }
  }
  args
}

#' Find the Edges of the Labels That Hold in a Context
#'
#' @param input_labels A `list` of explicit and inferred labels, see
#'   `parse_labels()`.
#' @param zero An `integer` vector of vertices assigned to 0 in the context.
#' @param one An `integer` vector of vertices assigned to 1 in the context.
#' @return A `logical` vector indicating the labels whose assignments are
#'   implied by the context and whose edge does not vanish.
#' @noRd
labels_in_context <- function(input_labels, zero, one) {
  vapply(
    input_labels,
    function(x) {
      !x$vanishing && all(x$zero %in% zero) && all(x$one %in% one)
    },
    logical(1L)
  )
}

#' Collect the Unique Endpoints of the Given Labels
#'
#' @inheritParams labels_in_context
#' @param active A `logical` vector indicating the labels to include.
#' @noRd
label_endpoints <- function(input_labels, active) {
  endpoints <- cbind(
    vapply(input_labels[active], "[[", integer(1L), "from"),
    vapply(input_labels[active], "[[", integer(1L), "to")
  )
  unique(endpoints)
}

#' Parse Context Implied by the Edge Labels
#'
#' @param args A `list` of arguments for `initialize_csisearch`
#' @param input_labels A `list` of explicit and inferred labels, see
#'   `parse_labels()`.
#' @noRd
parse_contexts <- function(args, input_labels) {
  all_contexts <- expand.grid(rep(list(c(0L, 1L)), length(args$con_vars)))
  args$n_con <- nrow(all_contexts)
  args$index_csi <- 0L
  for (i in seq_min2(args$n_con)) {
    sub_vars <- args$con_vars[which(all_contexts[i, ] == 1)]
    con_vals <- expand.grid(rep(list(c(0L, 1L)), length(sub_vars)))
    args$label_map[[i - 1L]] <- list(
      vars = as_set(args$nums[sub_vars]),
      contexts = vector(mode = "list", length = nrow(con_vals))
    )
    equiv_ind <- 0L
    unique_context <- list()
    for (j in seq_len(nrow(con_vals))) {
      zero <- sub_vars[which(con_vals[j, ] == 0L)]
      one <- sub_vars[which(con_vals[j, ] == 1L)]
      z_set <- as_set(args$nums[zero])
      o_set <- as_set(args$nums[one])
      args$label_map[[i - 1L]][["contexts"]][[j]]$zero <- z_set
      args$label_map[[i - 1L]][["contexts"]][[j]]$one <- o_set
      args <- parse_local_csi(args, z_set, o_set, input_labels)
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
#' @param z_set An `integer` vector of the vertices assigned to 0
#'   in the current context
#' @param o_set An `integer` vector of the vertices assigned to 1
#'   in the current context.
#' @noRd
parse_local_csi <- function(args, z_set, o_set, input_labels) {
  active <- labels_in_context(input_labels, z_set, o_set)
  lab <- union(z_set, o_set)
  for (k in which(active)) {
    pa <- input_labels[[k]]$pa
    if (length(pa) == length(lab) && all(pa %in% lab)) {
      args$index_csi <- args$index_csi + 1L
      args$local_csi[[args$index_csi]] <- list(
        x = input_labels[[k]]$from,
        y = input_labels[[k]]$to,
        z = pa,
        zero = z_set,
        one = o_set
      )
    }
  }
  args$endpoints <- label_endpoints(input_labels, active)
  args
}

#' Parse Labels Implied by Intervention Nodes
#'
#' @inheritParams parse_contexts
#' @noRd
parse_interventions <- function(args, input_labels) {
  all_interventions <- expand.grid(
    rep(list(c(0L, 1L)), length(args$intv_vars))
  )
  n_intv <- nrow(all_interventions)
  for (i in seq_min2(n_intv)) {
    index <- max(args$n_con - 1L, 0L) + i - 1L
    sub_vars <- args$intv_vars[which(all_interventions[i, ] == 1L)]
    o <- as_set(args$nums[sub_vars])
    args$label_map[[index]] <- list(
      vars = o,
      contexts = list(list(zero = integer(0L), one = o))
    )
    # Only labels without assignments to zero hold under interventions
    active <- labels_in_context(input_labels, integer(0L), o)
    endpoints <- label_endpoints(input_labels, active)
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
  d_parsed <- gsub("([|,])do\\(", "\\1$(", d_parsed)
  d_split <- match_distribution_ldag(d_parsed)
  if (any(is.na(d_split[[1L]]))) {
    stop_("Unable to parse ", type, ": ", d)
  }
  d_null <- vapply(d_split, is.null, logical(1L))
  for (j in which(!d_null)) {
    equals <- grep("=", d_split[[j]], value = FALSE)
    if (length(equals) > 0L) {
      eq_split <- strsplit(d_split[[j]][equals], "[=]")
      if (any(malformed_pairs(eq_split))) {
        stop_(
          "Invalid value assignment in ", type, ": ", d, "."
        )
      }
      eq_lhs <- vapply(eq_split, "[[", character(1L), 1L)
      eq_lhs <- gsub("\\s+", "", eq_lhs)
      eq_rhs <- vapply(eq_split, "[[", character(1L), 2L)
      eq_rhs <- gsub("\\s+", "", eq_rhs)
      if (any(!eq_rhs %in% c("0", "1"))) {
        stop_(
          "Invalid value assignment in ", type, ": ", d, "."
        )
      }
      d_split[[j]][equals] <- eq_lhs
      zero <- c(zero, eq_lhs[eq_rhs == "0"])
      one <- c(one, eq_lhs[eq_rhs == "1"])
    }
    dup <- duplicated(d_split[[j]])
    if (any(dup)) {
      stop_(
        "Invalid ", type, ": ", d, ", ",
        "duplicated variables ", cs(unique(d_split[[j]][dup])), "."
      )
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
    args$var_pool <- union(args$var_pool, unlist(d_split))
    args$input_vars[[i]] <- unique(unlist(d_split))
    args[[out]][[i]] <- d_process
  } else {
    args$query_vars <- unique(unlist(d_split))
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
parse_data_ldag <- function(args, data) {
  data_split <- strsplit(data, "\r|\n")[[1]]
  data_split <- gsub("\\s+", "", data_split)
  data_split <- data_split[which(nzchar(data_split))]
  args$p_process <- vector(mode = "list", length = length(data_split))
  args$input_vars <- vector(mode = "list", length = length(data_split))
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
#' @param args A `list` of arguments for `initialize_csisearch`.
#' @param msg A `character` string to prefix error messages with.
#' @param d A `list` of four sets of vertex indices denoting the distribution.
#' @param d_str A `character` string representing the distribution.
#' @noRd
validate_distribution_ldag <- function(args, msg, d, d_str) {
  if (any(d[[1L]] %in% d[[2L]])) {
    stop_(
      msg, d_str, ": ",
      "same variable on the left and right-hand side."
    )
  }
}

#' Convert a Parsed Distribution into Sets of Vertex Indices
#'
#' @param p A `list` of parsed distribution components.
#' @noRd
distribution_sets_ldag <- function(p) {
  list(
    as_set(p[[1L]]),
    as_set(p[[2L]]),
    as_set(p[[3L]]),
    as_set(p[[4L]])
  )
}

#' Check the Validity of Input Distributions
#'
#' @param args A `list` of arguments for `initialize_csisearch`.
#' @noRd
validate_data_ldag <- function(args) {
  args$p_list <- vector(mode = "list", length = length(args$p_process))
  for (i in seq_along(args$p_process)) {
    p <- args$p_process[[i]]
    args$p_list[[i]] <- distribution_sets_ldag(p)
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
  args$q_list <- distribution_sets_ldag(q)
  validate_distribution_ldag(
    args,
    "Invalid query ",
    args$q_list,
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
