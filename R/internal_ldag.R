#' Call the dosearch Algorithm from R for LDAGs
#'
#' @inheritParams dosearch
#' @noRd
get_derivation_ldag <- function(data, query, graph, control = list()) {
  control <- control_defaults(control)
  args <- list(
    dir_lhs = integer(0L),
    dir_rhs = integer(0L),
    bi_lhs = integer(0L),
    bi_rhs = integer(0L),
    vars = character(0L),
    nums = integer(0L),
    n = 0L,
    con_vars = character(0L),
    intv_vars = character(0L),
    parents = list(),
    contexts = c(),
    label_map = NULL,
    local_csi = NULL
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
  res$call <- list(
    data = data, 
    query = query, 
    graph = graph, 
    transportability = NULL, 
    selection_bias = NULL, 
    missing_data = NULL, 
    control = control
  )
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

transform_graph_ldag <- function(args, graph) {
  if (!nzchar(graph)){
    stop("Invalid graph: the graph is empty.")
  } else {
    row_pattern <- "^(.+(?>->|--|<->)[^\\:]+)(?>\\:(.+))?$"
    graph_split <- strsplit(graph, "\r|\n")[[1L]]
    graph_split <- gsub("\\s", "", graph_split)
    valid_rows <- grep(row_pattern, graph_split, perl = TRUE)
    graph_split <- graph_split[valid_rows]
    graph_match <- regexec(row_pattern, graph_split, perl = TRUE)
    split_rows <- regmatches(graph_split, graph_match)
    edges <- sapply(split_rows, "[[", 2L)
    directed <- strsplit(grep("(.+)->(.+)", edges, value = TRUE), "->")
    if (length(directed) > 0L) {
      args$dir_lhs <- vapply(directed, "[[", character(1L), 1L)
      args$dir_rhs <- vapply(directed, "[[", character(1L), 2L)
      if (any(args$dir_lhs == args$dir_rhs)) {
        stop("Invalid graph: no self loops are allowed.")
      }
    }
    contexts_split <- list()
    contextuals <- which(nzchar(vapply(split_rows, "[[", character(1L), 3L)))
    labels_split <- list()
    if (length(contextuals) > 0L) {
      edges_split <- strsplit(edges, "(->)")
      labels <- vapply(split_rows[contextuals], "[[", character(1L), 3L)
      labels_split <- strsplit(labels, "[;]")
      labels_split <- lapply(labels_split, strsplit, "[,]")
      args$targets <- lapply(seq_len(length(labels_split)), function(i) {
        c(
          "from" = edges_split[[contextuals[i]]][1L],
          "to" = edges_split[[contextuals[i]]][2L]
        )
      })
    }
    args$labels <- labels_split
    args$vars <- unique(c(args$dir_rhs, args$dir_lhs))
    args$n <- length(args$vars)
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
  }
  args
}

parse_labels <- function(args) {
  if (length(args$labels) > 0L) {
    input_labels <- matrix(0L, sum(lengths(args$labels)), 5L)
    index <- 0L
    index_csi <- 0L
    inferred <- 0L
    inferred_labels <- matrix(0L, 0L, 5L)
    args$local_csi <- list()
    vanishing <- matrix(0L, 0L, 2L)
    for (i in seq_along(args$labels)) {
      from <- args$targets[[i]]["from"]
      to <- args$targets[[i]]["to"]
      pa <- setdiff(args$parents[[to]], from)
      n_pa <- length(pa)
      if (n_pa == 0) {
        stop(
          "Invalid label for edge", from, " -> ", to, ": no parents to assign."
        )
      }
      vals <- expand.grid(rep(list(c(0L, 1L)), n_pa))
      names(vals) <- pa
      vals$present <- FALSE
      # Loop individual assignments within label
      for (j in seq_along(args$labels[[i]])) {
        index <- index + 1L
        label_split <- strsplit(args$labels[[i]][[j]], "[=]")
        label_lhs <- vapply(label_split, "[[", character(1L), 1L)
        label_rhs <- vapply(label_split, "[[", character(1L), 2L)
        msg <- ""
        if (any(duplicated(label_lhs))) {
          stop(
            "Invalid label for edge ", 
            from, " -> ", to, 
            ": duplicate assignment."
          )
        }
        if (from %in% label_lhs) {
          stop(
            "Invalid label for edge ", 
            from, " -> ", to, ": ", from, 
            " cannot appear in the label"
          )
        }
        if (to %in% label_lhs) {
          stop(
            "Invalid label for edge ", 
            from, " -> ", to, ": ", to, 
            " cannot appear in the label."
          )
        }
        if (any(!(label_lhs %in% pa))) {
          stop(
            "Invalid label for edge ", 
            from, " -> ", to, 
            ": only other parents of ", to, " may be assigned."
          )
        }
        intv <- substr(label_lhs, 1L, 2L) == "I_"
        args$con_vars <- c(args$con_vars, label_lhs[!intv])
        zero <- which(label_rhs == 0L)
        one <- which(label_rhs == 1L)
        input_labels[index, 1L] <- to_dec(args$nums[label_lhs[zero]], args$n)
        input_labels[index, 2L] <- to_dec(args$nums[label_lhs[one]], args$n)
        input_labels[index, 3L] <- args$nums[from]
        input_labels[index, 4L] <- args$nums[to]
        input_labels[index, 5L] <- to_dec(args$nums[pa], args$n)
        # Infer non-explicit labels from input
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
      }
      if (all(vals$present)) {
        vanishing <- rbind(vanishing, c(args$nums[from], args$nums[to]))
      }
      # Cannot infer from empty set
      n_sets <- nrow(vals) - 1L
      if (n_sets > 1L) {
        for (j in seq.int(2L, n_sets)) {
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
              inferred <- inferred + 1L
              inferred_labels <- rbind(
                inferred_labels, 
                c(
                  to_dec(args$nums[zero], args$n), 
                  to_dec(args$nums[one], args$n), 
                  args$nums[from], 
                  args$nums[to], 
                  to_dec(args$nums[pa], args$n))
                )
            }
          }
        }
      }
    }
    if (inferred > 0L) {
      input_labels <- rbind(input_labels, inferred_labels)
      input_labels <- input_labels[!duplicated(input_labels), ]
    }
    args$con_vars <- unique(args$con_vars)
    all_contexts <- expand.grid(rep(list(c(0L, 1L)), length(args$con_vars)))
    args$label_map <- list()
    null_context <- c()
    n_con <- nrow(all_contexts)
    if (n_con > 0L) {
      for (i in seq.int(2L, n_con)) {
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
          z <- to_dec(args$nums[zero], args$n)
          o <- to_dec(args$nums[one], args$n)
          args$label_map[[i - 1L]][["contexts"]][[j]]$zero <- z
          args$label_map[[i - 1L]][["contexts"]][[j]]$one <- o
          endpoints <- matrix(0L, 0L, 2L)
          for (k in seq_len(nrow(input_labels))) {
            z_inp <- input_labels[k, 1L]
            o_inp <- input_labels[k, 2L]
            if ((bitwAnd(z, z_inp) == z_inp && bitwAnd(o, o_inp) == o_inp)) {
              csi_v <- apply(vanishing, 1L, function(x) {
                isTRUE(all.equal(x, input_labels[k, 3L:4L]))
              })
              if (!any(csi_v)) {
                endpoints <- rbind(endpoints, input_labels[k, 3L:4L])
                pa <- input_labels[k, 5L]
                lab <- bitwOr(z, o)
                if (pa == lab) {
                  index_csi <- index_csi + 1L
                  args$local_csi[[index_csi]] <- list(
                    x = to_dec(input_labels[k, 3], args$n),
                    y = to_dec(input_labels[k, 4], args$n),
                    z = pa,
                    zero = z,
                    one = o)
                }
              }
            }
          }
          endpoints <- unique(endpoints)
          args$label_map[[i - 1L]][["contexts"]][[j]]$from <- endpoints[, 1L]
          args$label_map[[i - 1L]][["contexts"]][[j]]$to <- endpoints[, 2L]
          pos <- Position(
            function(x) {
              identical(endpoints[, 1L], x$from) && 
                identical(endpoints[, 2L], x$to)
            }, 
            unique_context
          )
          if (is.na(pos)) {
            equiv_ind <- equiv_ind + 1L
            args$label_map[[i - 1L]][["contexts"]][[j]]$equivalence <- equiv_ind
            unique_context[[equiv_ind]] <- list(
              from = endpoints[, 1L], 
              to = endpoints[, 2L]
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
          null_context <- c(null_context, i - 1L)
        }
      }
    }
    all_interventions <- expand.grid(
      rep(list(c(0L, 1L)), length(args$intv_vars))
    )
    n_intv <- nrow(all_interventions)
    if (n_intv > 0L) {
      for (i in seq.int(2L, n_intv)) {
        index <- max(n_con - 1L, 0L) + i - 1L
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
    }
    args$label_map[null_context] <- NULL
    if (nrow(vanishing) > 0L) {
      edge_mat <- cbind(args$nums[args$dir_lhs], args$nums[args$dir_rhs])
      present <- !duplicated(
        rbind(edge_mat, vanishing), 
        fromLast = TRUE
      )[seq_len(nrow(edge_mat))]
      args$dir_lhs <- args$dir_lhs[present]
      args$dir_rhs <- args$dir_rhs[present]
    }
  }
  args
}

parse_query_ldag <- function(args, query) {
  parts <- NULL
  q_split <- list(NULL, NULL, NULL)
  zero <- c()
  one <- c()
  query_parsed <- gsub("\\s+", "", query)
  query_parsed <- gsub("do", "$", query_parsed)
  q_split <- match_distribution_ldag(query_parsed)
  for (i in seq_len(2L)) {
    if (!is.null(q_split[[i]])) {
      dup <- duplicated(q_split[[i]])
      if (any(dup)) {
        stop(
          "Invalid query: cannot contain duplicated variables: ", 
          q_split[[i]][dup],
          "."
        )
      }
      equals <- grep("=", q_split[[i]], value = FALSE)
      eq_split <- strsplit(q_split[[i]][equals], "[=]")
      eq_lhs <- eq_rhs <- c()
      if (length(equals) > 0L) {
        eq_lhs <- vapply(eq_split, "[[", character(1L), 1L)
        eq_lhs <- gsub("\\s+", "", eq_lhs)
        eq_rhs <- vapply(eq_split, "[[", character(1L), 2L)
        eq_rhs <- gsub("\\s+", "", eq_rhs)
        uniq_rhs <- unique(eq_rhs)
        if (!(uniq_rhs[1L] %in% 0L:1L)) {
          stop(paste0("Invalid value assignment in query."))
        }
        q_split[[i]][equals] <- eq_lhs
        z <- which(eq_rhs == 1L)
        o <- which(eq_rhs == 0L)
        zero <- c(zero, eq_lhs[eq_rhs == 0L])
        one <- c(one, eq_lhs[eq_rhs == 1L])
      }
    }
  }
  q1_new <- q_split[[1L]][which(!(q_split[[1L]] %in% args$vars))]
  q2_new <- q_split[[2L]][which(!(q_split[[2L]] %in% args$vars))]
  new_vars <- unique(c(q1_new, q2_new))
  args <- add_new_vars(args, new_vars)
  args$q_process <- list(
    args$nums[q_split[[1L]]], 
    args$nums[q_split[[2L]]], 
    args$nums[zero], 
    args$nums[one], 
    parts[1]
  )
  args
}

parse_input_distributions_ldag <- function(args, data) {
  data_split <- strsplit(data, "\r|\n")[[1]]
  data_split <- gsub("\\s+", "", data_split)
  data_split <- data_split[which(nzchar(data_split))]
  p_list <- list()
  args$p_process <- vector(mode = "list", length = length(data_split))
  for (i in seq_along(data_split)) {
    parts <- NULL
    p_split <- list(NULL, NULL, NULL)
    zero <- c()
    one <- c()
    p_parsed <- gsub("\\s+", "", data_split[[i]])
    p_parsed <- gsub("do", "$", p_parsed)
    p_split <- match_distribution_ldag(p_parsed)
    if (any(is.na(p_split[[1]]))) {
      stop(
        "Invalid input distribution on data line ", 
        i , ": ", data_split[[i]], "."
      )
    }
    for (j in seq_len(2L))  {
      if (!is.null(p_split[[j]])) {
        dup <- duplicated(p_split[[j]])
        if (any(dup)) {
          stop(
            "Invalid input distribution: ", data_split[[i]],
            "cannot contain duplicated variables ", p_split[[j]][dup], "."
          )
        }
        equals <- grep("=", p_split[[j]], value = FALSE)
        eq_split <- strsplit(p_split[[j]][equals], "[=]")
        eq_lhs <- eq_rhs <- c()
        if (length(equals) > 0L) {
          eq_lhs <- vapply(eq_split, "[[", character(1L), 1L)
          eq_lhs <- gsub("\\s+", "", eq_lhs)
          eq_rhs <- vapply(eq_split, "[[", character(1L), 1L)
          eq_rhs <- gsub("\\s+", "", eq_rhs)
          uniq_rhs <- unique(eq_rhs)
          if (!(uniq_rhs[1L] %in% 0L:1L)) {
            stop(
              "Invalid value assignment on data line ", 
              i ,": ", data_split[[i]], "."
            )
          }
          p_split[[j]][equals] <- eq_lhs
          z <- which(eq_rhs == 1L)
          o <- which(eq_rhs == 0L)
          zero <- c(zero, eq_lhs[eq_rhs == 0L])
          one <- c(one, eq_lhs[eq_rhs == 1L])
        }
      }
    }
    p1_new <- p_split[[1L]][which(!(p_split[[1L]] %in% args$vars))]
    p2_new <- p_split[[2L]][which(!(p_split[[2L]] %in% args$vars))]
    new_vars <- unique(c(p1_new, p2_new))
    args <- add_new_vars(args, new_vars)
    args$p_process[[i]] <- list(
      args$nums[p_split[[1L]]],
      args$nums[p_split[[2L]]], 
      args$nums[zero], 
      args$nums[one], 
      data_split[[i]]
    )
  }
  args
}

validate_input_distributions_ldag <- function(args) {
  for (i in seq_along(args$p_process)) {
    p <- args$p_process[[i]]
    args$p_list[[i]] <- c(
      to_dec(p[[1L]], args$n), 
      to_dec(p[[2L]], args$n), 
      to_dec(p[[3L]], args$n), 
      to_dec(p[[4L]], args$n)
    )
    if (bitwAnd(args$p_list[[i]][1L], args$p_list[[i]][2L]) > 0L) {
      stop(
        "Invalid input distribution on data line ", 
        i, ": ", p[[4L]], ", ",
        "same variable on the left and right-hand side."
      )
    }
  }
  args
}

validate_query_ldag <- function(args) {
  args$q_vec <- c(
    to_dec(args$q_process[[1L]], args$n), 
    to_dec(args$q_process[[2L]], args$n), 
    to_dec(args$q_process[[3L]], args$n), 
    to_dec(args$q_process[[4L]], args$n)
  )
  if (bitwAnd(args$q_vec[1L], args$q_vec[2L]) > 0L) {
    stop(
      "Invalid query: ", 
      args$q_process[[4L]], ", ",
      "same variable on the left and right-hand side."
    )
  }
  args
}


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
  match_lens <- sapply(matches, function(x) {
    length(attr(x[[1L]], "match.length"))
  })
  best_match <- which.max(match_lens)[1L]
  parts <- regmatches(d, matches[[best_match]])[[1L]]
  d_split <- vector(mode = "list", length = 2L)
  d_split[[1L]] <- strsplit(parts[2L], "[,]")[[1L]]
  if (best_match == 2) {
    d_split[[2L]] <- strsplit(parts[3L], "[,]")[[1L]]
  }
  d_split
}
