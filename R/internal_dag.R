#' Call the `dosearch` Algorithm from R for DAGs
#'
#' @inheritParams dosearch
#' @noRd
get_derivation_dag <- function(data, query, graph, transportability = NULL,
                               selection_bias = NULL, missing_data = NULL,
                               control = list()) {
  control <- control_defaults(control)
  args <- list(
    dir_lhs = integer(0L),
    dir_rhs = integer(0L),
    bi_lhs = integer(0L),
    bi_rhs = integer(0L),
    vars = character(0L),
    nums = integer(0L),
    tr_nums = integer(0L),
    sb_nums = integer(0L),
    n = 0L,
    n_tr = 0L,
    n_sb = 0L,
    tr = 0L,
    sb = 0L,
    md_s = 0L,
    md_p = 0L,
    md_t = 0L,
    md_sym = control$md_sym
  )
  args <- transform_graph_dag(args, graph)
  args <- parse_missing_data(args, missing_data)
  args <- parse_transportability(args, transportability)
  args <- parse_selection_bias(args, selection_bias)
  args <- reorder_variables(args)
  args <- parse_query_dag(args, query, missing_data)
  args <- parse_input_distributions_dag(args, data, missing_data)
  if (control$warn) {
    var_dec <- to_dec(args$nums[args$var_pool], args$n)
    if (!is.null(missing_data)) {
      inc_md <- bitwAnd(args$md_s, var_dec)
      if (inc_md != args$md_s) {
        no_ind <- args$vars[
          which(to_vec(bitwAnd(args$md_s, bitwNot(inc_md)), args$n) == 1L)
        ]
        warning(
          "There are response indicators ",
          "that are not present in any input distribution: ",
          paste(no_ind, collapse = ", ")
        )
      }
    }
  }
  args <- validate_input_distributions_dag(args)
  args <- validate_query_dag(args)
  res <- initialize_dosearch(
    as.numeric(args$nums[args$dir_lhs]),
    as.numeric(args$nums[args$dir_rhs]),
    as.numeric(args$nums[args$bi_lhs]),
    as.numeric(args$nums[args$bi_rhs]),
    as.character(args$vars),
    args$p_list,
    args$q_vec,
    args$n,
    args$tr,
    args$sb,
    args$md_s,
    args$md_p,
    control$time_limit,
    control$rules,
    control$benchmark,
    control$benchmark_rules,
    control$draw_derivation,
    control$draw_all,
    control$formula,
    control$improve,
    control$heuristic,
    control$md_sym,
    control$verbose
  )
  res$call <- list(
    data = data,
    query = query,
    graph = graph,
    transportability = transportability,
    selection_bias = selection_bias,
    missing_data = missing_data,
    control = control
  )
  structure(
    res[
      c(
        TRUE, # always include identifiability
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

#' Transform the Input DAG
#'
#' @inheritParams dosearch
#' @param args A `list` of arguments for `initialize_dosearch`
#' @param graph The graph as a `character` string.
#' @noRd
transform_graph_dag <- function(args, graph, missing_data) {
  if (!nzchar(graph)) {
    if (is.null(missing_data)) {
      stop("Invalid graph: the graph is empty.")
    }
  } else {
    graph <- gsub("<->", "--", graph)
    graph_split <- strsplit(strsplit(graph, "\r|\n")[[1L]], "\\s+")
    line_lengths <- lengths(graph_split)
    graph_split <- graph_split[line_lengths > 2L]
    arrow_indices <- lapply(graph_split, grep, pattern = "(->)|(--)")
    graph_split <- lapply(seq_len(length(graph_split)), function(x) {
      graph_split[[x]][-1:1 + arrow_indices[[x]]]
    })
    graph_split <- vapply(graph_split, paste, character(1L), collapse = "")
    directed <- strsplit(graph_split[grep("(.+)?->(.+)?", graph_split)], "->")
    bidirected <- strsplit(graph_split[grep("(.+)?--(.+)?", graph_split)], "--")
    if (length(directed) > 0L) {
      args$dir_lhs <- sapply(directed, "[[", 1L)
      args$dir_rhs <- sapply(directed, "[[", 2L)
      if (any(args$dir_lhs == args$dir_rhs)) {
        stop("Invalid graph: no self loops are allowed.")
      }
    }
    if (length(bidirected) > 0L) {
      args$bi_lhs <- vapply(bidirected, "[[", character(1L), 1L)
      args$bi_rhs <- vapply(bidirected, "[[", character(1L), 2L)
      if (any(args$bi_lhs == args$bi_rhs)) {
        stop("Invalid graph: no self loops are allowed.")
      }
    }
    args$vars <- unique(
      c(args$dir_rhs, args$dir_lhs, args$bi_rhs, args$bi_lhs)
    )
  }
  args
}

#' Parse Missing Data Mechanisms
#'
#' @inheritParams dosearch
#' @param args A `list` of arguments for `initialize_dosearch`.
#' @noRd
parse_missing_data <- function(args, missing_data) {
  if (!is.null(missing_data)) {
    md_pairs <- gsub("\\s+", "", strsplit(missing_data, ",")[[1L]])
    if (length(md_pairs) == 0L) {
      stop("Invalid missing data mechanisms.")
    }
    md_mechanisms <- strsplit(md_pairs, ":")
    md_true <- sapply(md_mechanisms, "[[", 2L)
    md_switch <- sapply(md_mechanisms, "[[", 1L)
    md_proxy <- paste0(md_true, "*")
    if (any(md_switch %in% dir_lhs[dir_rhs %in% md_true])) {
      stop("A missing data mechanism cannot be a parent of a true variable.")
    }
    dir_lhs <- c(dir_lhs, md_true, md_switch)
    dir_rhs <- c(dir_rhs, md_proxy, md_proxy)
    vars_md <- as.vector(rbind(md_true, md_switch, md_proxy))
    args$vars <- c(vars_md, args$vars[!(args$vars %in% vars_md)])
    args$n <- length(args$vars)
    args$nums <- seq_len(args$n)
    names(args$vars) <- args$nums
    names(args$nums) <- args$vars
    md_switch_nums <- args$nums[md_switch]
    md_proxy_nums <- args$nums[md_proxy]
    if (any(is.na(md_switch_nums)) || any(is.na(md_proxy_nums))) {
      stop("Invalid missing data mechanisms.")
    }
    args$md_s <- to_dec(md_switch_nums, args$n)
    args$md_p <- to_dec(md_proxy_nums, args$n)
    args$md_t <- bitwShiftR(args$md_p, 2L)
  } else {
    args$n <- length(args$vars)
    args$nums <- seq_len(args$n)
    names(args$vars) <- args$nums
    names(args$nums) <- args$vars
  }
  args
}

#' Parse Transportability Nodes
#'
#' @inheritParams dosearch
#' @param args A `list` of arguments for `initialize_dosearch`.
#' @noRd
parse_transportability <- function(args, transportability) {
  if (!is.null(transportability)) {
    args$tr_nums <- args$nums[
      gsub("\\s+", "", strsplit(transportability, ",")[[1L]])
    ]
    args$n_tr <- length(args$tr_nums)
    if (args$n_tr == 0L) {
      stop("Invalid transportability nodes.")
    }
    pa <- args$nums[c(args$dir_rhs, args$bi_rhs, args$bi_lhs)]
    if (any(args$tr_nums %in% pa)) {
      stop(
        "Invalid graph: ",
        "a transportability node cannot be a child of another node."
      )
    }
  }
  args
}

#' Parse Selection Bias Nodes
#'
#' @inheritParams dosearch
#' @param args A list of arguments for `initialize_dosearch`.
#' @noRd
parse_selection_bias <- function(args, selection_bias) {
  if (!is.null(selection_bias)) {
    args$sb_nums <- args$nums[
      gsub("\\s+", "", strsplit(selection_bias, ",")[[1]])
    ]
    args$n_sb <- length(args$sb_nums)
    if (args$n_sb == 0L) {
      stop("Invalid selection bias nodes.\n")
    }
    if (any(args$sb_nums %in% args$nums[args$dir_lhs])) {
      stop(
        "Invalid graph: ",
        "a selection bias node cannot be a parent of another node."
      )
    }
  }
  args
}

#' Place Transportability and Selection Bias Nodes Last
#'
#' @param args A `list` of arguments for `initialize_dosearch`.
#' @noRd
reorder_variables <- function(args) {
  if (args$n_tr > 0L || args$n_sb > 0L) {
    args$vars <- args$vars[
      c(
        setdiff(args$nums, union(args$tr_nums, args$sb_nums)),
        args$tr_nums,
        args$sb_nums
      )
    ]
    args$nums <- seq_len(args$n)
    names(args$vars) <- args$nums
    names(args$nums) <- args$vars
    if (args$n_tr > 0L) {
      args$tr_nums <- seq.int(
        args$n - args$n_tr - args$n_sb + 1L,
        args$n - args$n_sb
      )
      args$tr <- to_dec(args$tr_nums, args$n)
    }
    if (args$n_sb > 0L) {
      args$sb_nums <- seq.int(args$n - args$n_sb + 1L, args$n)
      args$sb <- to_dec(args$sb_nums, args$n)
    }
  }
  args
}

#' Parse a Distribution in the Internal Character Format for DAGs
#'
#' @inheritParams dosearch
#' @param args A `list` of arguments for `initialize_dosearch`.
#' @param d A `character` string representing the distribution.
#' @param type A `character` string indicating the distribution type.
#' @param out A `character` string indicating the a name of `args` to
#'   in which to assign the results.
#' @param i An `integer` indicating the iteration.
#' @noRd
parse_distribution_dag <- function(args, d, type, out, i, missing_data) {
  enabled <- character(0L)
  d_parsed <- gsub("\\s+", "", d)
  d_parsed <- gsub("do", "$", d_parsed)
  d_split <- match_distribution_dag(d_parsed)
  if (any(is.na(d_split[[1L]]))) {
    stop("Invalid ", type, ".")
  }
  d_null <- vapply(d_split, is.null, logical(1L))
  for (j in which(!d_null)) {
    dup <- duplicated(d_split[[j]])
    if (any(dup)) {
      stop(
        "Invalid ", type, ": ", d, ", ",
        "cannot contain duplicated variables ", d_split[[j]][dup], "."
      )
    }
    if (!is.null(missing_data)) {
      equals <- grep("=", d_split[[j]], value = FALSE)
      if (length(equals) > 0L) {
        eq_split <- strsplit(d_split[[j]][equals], "[=]")
        eq_lhs <- sapply(eq_split, "[[", 1L)
        eq_lhs <- gsub("\\s+", "", eq_lhs)
        eq_rhs <- sapply(eq_split, "[[", 2L)
        eq_rhs <- gsub("\\s+", "", eq_rhs)
        uniq_rhs <- unique(eq_rhs)
        if (length(uniq_rhs) > 1L) {
          stop(
            "Cannot use multiple symbols ",
            "to denote active missing data mechanisms."
          )
        }
        if (uniq_rhs[1L] != args$md_sym) {
          stop(
            "Invalid symbol for missing data mechanism on data line ",
            i,
            ": ",
            uniq_rhs[1],
            "."
          )
        }
        d_split[[i]][equals] <- eq_lhs
        enabled <- c(enabled, eq_lhs)
      }
    }
  }
  d1_new <- d_split[[1L]][which(!(d_split[[1L]] %in% args$vars))]
  d2_new <- d_split[[2L]][which(!(d_split[[2L]] %in% args$vars))]
  d3_new <- d_split[[3L]][which(!(d_split[[3L]] %in% args$vars))]
  new_vars <- unique(c(d1_new, d2_new, d3_new))
  args <- add_new_vars(args, new_vars)
  d_process <- list(
    args$nums[d_split[[1L]]],
    args$nums[d_split[[2L]]],
    args$nums[d_split[[3L]]],
    args$nums[enabled],
    d
  )
  if (i > 0L) {
    args$var_pool <- union(args$var_pool, d_split[[1]])
    args[[out]][[i]] <- d_process
  } else {
    args[[out]] <- d_process
  }
  args
}

#' Parse a Target Distribution
#'
#' @inheritParams dosearch
#' @param args A `list` of arguments for `initialize_dosearch`.
#' @noRd
parse_query_dag <- function(args, query, missing_data) {
  parse_distribution_dag(
    args,
    query,
    "target distribution",
    "q_process",
    0L,
    missing_data
  )
}

#' Parse Input Distributions
#'
#' @inheritParams dosearch
#' @param args A `list` of arguments for `initialize_dosearch`.
#' @noRd
parse_input_distributions_dag <- function(args, data, missing_data) {
  data_split <- strsplit(data, "\r|\n")[[1]]
  data_split <- gsub("\\s+", "", data_split)
  data_split <- data_split[which(nzchar(data_split))]
  args$p_process <- vector(mode = "list", length = length(data_split))
  for (i in seq_along(data_split)) {
    args <- parse_distribution_dag(
      args,
      data_split[[i]],
      "input distribution",
      "p_process",
      i,
      missing_data
    )
  }
  args
}

#' Check the Validity of a Distribution
#'
#' @param args A `list` of arguments for `initialize_dosearch`.
#' @param d An `integer` vector of length 4 denoting the distribution.
#' @noRd
validate_distribution_dag <- function(args, d) {
  msg <- ""
  left_proxy <- bitwAnd(
    bitwShiftR(bitwAnd(d[1L], args$md_p), 2L),
    bitwAnd(d[2L], args$md_t)
  )
  left_true <- bitwAnd(
    bitwShiftL(bitwAnd(d[1L], args$md_t), 2L),
    bitwAnd(d[2L], args$md_p)
  )
  both_left <- bitwAnd(
    bitwShiftR(bitwAnd(d[1L], args$md_p), 2L),
    bitwAnd(d[1L], args$md_t)
  )
  both_right <- bitwAnd(
    bitwShiftR(bitwAnd(d[2L], args$md_p), 2L),
    bitwAnd(d[2L], args$md_t)
  )
  if (left_proxy > 0L) {
    msg <- "proxy variable of a true variable present on the left-hand side."
  } else if (left_true > 0L) {
    msg <- "true variable of a proxy variable present on the left-hand side."
  } else if (both_left > 0L) {
    msg <- paste0(
      "true and proxy versions of the same variable ",
      "present on the left-hand side."
    )
  } else if (both_right > 0L) {
    msg <- paste0(
      "true and proxy versions of the same variable ",
      "present on the right-hand side."
    )
  } else if (bitwAnd(d[1L], d[2L]) > 0L) {
    msg <- "same variable on the left and right-hand side."
  } else if (bitwAnd(d[1L], args$tr) > 0L) {
    msg <- "cannot contain a transportability node on the left-hand side."
  } else if (bitwAnd(d[1L], args$sb) > 0L) {
    msg <- "cannot contain a a selection bias node on the left-hand side."
  } else if (bitwAnd(d[3L], args$tr) > 0L) {
    msg <- "cannot intervene on a transportability node."
  } else if (bitwAnd(d[3L], args$sb) > 0L) {
    msg <- "cannot intervene on a selection bias node."
  } else if (bitwAnd(d[4L], args$md_s) != d[4L] ) {
    msg <- "cannot set value of non-missing data mechanism.\n"
  }
  msg
}

#' Check the Validity of Input Distributions
#'
#' @param args A `list` of arguments for `initialize_dosearch`.
#' @noRd
validate_input_distributions_dag <- function(args) {
  args$p_list <- vector(mode = "list", length = length(args$p_process))
  for (i in seq_along(args$p_process)) {
    p <- args$p_process[[i]]
    args$p_list[[i]] <- c(
      to_dec(p[[1]], args$n),
      to_dec(c(p[[2]], p[[3]]), args$n),
      to_dec(p[[3]], args$n),
      to_dec(p[[4]], args$n)
    )
    msg <- validate_distribution_dag(args, args$p_list[[i]])
    if (nzchar(msg)) {
      stop(
        "Invalid input distribution on data line ",
        i,
        ": ",
        p[[4L]],
        ", ",
        msg
      )
    }
  }
  args
}

#' Check the Validity of a Target Distribution
#'
#' @param args A `list` of arguments for `initialize_dosearch`.
#' @noRd
validate_query_dag <- function(args) {
  args$q_vec <- c(
    to_dec(args$q_process[[1L]], args$n),
    to_dec(c(args$q_process[[2L]], args$q_process[[3L]]), args$n),
    to_dec(args$q_process[[3L]], args$n),
    to_dec(args$q_process[[4L]], args$n)
  )
  msg <- validate_distribution_dag(args, args$q_vec)
  if (nzchar(msg)) {
    stop(
      "Invalid query: ",
      msg
    )
  }
  args
}

#' Determine the Type of a Distribution
#'
#' Checks whether a distribution is of the form p(y), p(y|z), p(y|do(x)),
#' p(y|z,do(x)) or p(y|do(x),z).
#'
#' @param d A `character` string representing the distribution.
#' @noRd
match_distribution_dag <- function(d) {
  dist_pattern <- character(5L)
  # Pattern for p(y)
  dist_pattern[1L] <- "^[Pp]\\(([^|\\$\\),]++(?>,[^|\\$\\),]+)*)\\)$"
  # Pattern for p(y|z)
  dist_pattern[2L] <- paste0(
    "^[Pp]\\(([^|\\$\\),]++(?>,[^|\\$\\),]+)*)",
    "[|]",
    "([^|\\$\\),]++(?>,[^|\\$\\),]+)*)\\)$"
  )
  # Pattern for p(y|do(x))
  dist_pattern[3L] <- paste0(
    "^[Pp]\\(([^|\\$\\),]++(?>,[^|\\$\\),]+)*)",
    "[|]",
    "(?:[\\$]\\(([^|\\$\\),]++(?>,[^|\\$\\),]+)*)\\))\\)$"
  )
  # Pattern for p(y|z,do(x))
  dist_pattern[4L] <- paste0(
    "^[Pp]\\(([^|\\$\\),]++(?>,[^|\\$\\),]+)*)",
    "[|]",
    "([^|\\$\\),]++(?>,[^|\\$\\),]+)*)",
    "[,]",
    "(?:[\\$]\\(([^|\\$\\),]++(?>,[^|\\$\\),]+)*)\\))\\)$"
  )
  # Pattern for p(y|do(x),z)
  dist_pattern[5L] <- paste0(
    "^[Pp]\\(([^|\\$\\),]++(?>,[^|\\$\\),]+)*)",
    "[|]",
    "(?:[\\$]\\(([^|\\$\\),]++(?>,[^|\\$\\),]+)*)\\))",
    "[,]",
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
  d_split <- vector(mode = "list", length = 3L)
  d_split[[1L]] <- strsplit(parts[2L], "[,]")[[1L]]
  if (best_match == 2L) {
    d_split[[2L]] <- strsplit(parts[3L], "[,]")[[1L]]
  } else if (best_match == 3L) {
    d_split[[3L]] <- strsplit(parts[3L], "[,]")[[1L]]
  } else if (best_match == 4L) {
    d_split[[2L]] <- strsplit(parts[3L], "[,]")[[1L]]
    d_split[[3L]] <- strsplit(parts[4L], "[,]")[[1L]]
  } else if (best_match == 5L) {
    d_split[[2L]] <- strsplit(parts[4L], "[,]")[[1L]]
    d_split[[3L]] <- strsplit(parts[3L], "[,]")[[1L]]
  }
  d_split
}
