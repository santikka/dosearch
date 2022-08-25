#' Call the `dosearch` Algorithm from R for DAGs
#'
#' @inheritParams dosearch
#' @noRd
get_derivation_dag <- function(data, query, graph, transportability,
                               selection_bias, missing_data, control) {
  control <- control_defaults(control)
  cl <- list(
    data = data,
    query = query,
    graph = graph,
    transportability = transportability,
    selection_bias = selection_bias,
    missing_data = missing_data,
    control = control
  )
  if (control$empty) {
    return(empty_output(cl))
  }
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
  args <- transform_graph_dag(args, graph, missing_data)
  args <- parse_missing_data(args, missing_data)
  args <- parse_transportability(args, transportability)
  args <- parse_selection_bias(args, selection_bias)
  args <- reorder_variables(args)
  args <- parse_query_dag(args, query, missing_data)
  args <- parse_input_distributions_dag(args, data, missing_data)
  args <- validate_input_distributions_dag(args)
  args <- validate_query_dag(args)
  check_valid_input(args, control, missing_data)
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
  res$call <- cl
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
  if (!nzchar(graph) && is.null(missing_data)) {
    stop_("Invalid graph, the graph is empty.")
  }
  graph <- gsub("<->", "--", graph)
  graph_lines <- trimws(unique(strsplit(graph, "\r|\n")[[1L]]))
  graph_split <- strsplit(graph_lines, "\\s+")
  line_lengths <- lengths(graph_split)
  malformed_lines <- line_lengths > 0 & line_lengths != 3L
  if (any(malformed_lines)) {
    stop_(
      "Invalid graph, malformed lines found: ",
      cs(graph_lines[malformed_lines])
    )
  }
  graph_split <- graph_split[line_lengths == 3L]
  graph_lines <- graph_lines[line_lengths == 3L]
  edges <- vapply(graph_split, "[[", character(1L), 2L)
  valid_edges <- grepl("(->)|(--)", edges)
  if (any(!valid_edges)) {
    stop_(
      "Invalid graph, unknown edge types found: ",
      cs(edges[!valid_edges])
    )
  }
  directed <- grepl("->", edges)
  bidirected <- grepl("--", edges)
  if (any(directed)) {
    di <- graph_split[directed]
    args$dir_lhs <- vapply(di, "[[", character(1L), 1L)
    args$dir_rhs <- vapply(di, "[[", character(1L), 3L)
    loops <- args$dir_lhs == args$dir_rhs
    if (any(loops)) {
      stop_(
        "Invalid graph, the graph contains self-loops: ",
        cs(graph_lines[directed][loops])
      )
    }
    if (!is_acyclic(args$dir_lhs, args$dir_rhs)) {
      stop_(
        "Invalid graph, the graph contains cycles."
      )
    }
  }
  if (any(bidirected)) {
    bi <- unique(
      lapply(graph_split[bidirected], function(x) {
        sort(x[-2L])
      })
    )
    args$bi_lhs <- vapply(bi, "[[", character(1L), 1L)
    args$bi_rhs <- vapply(bi, "[[", character(1L), 2L)
    loops <- args$bi_lhs == args$bi_rhs
    if (any(loops)) {
      loop_edges <- gsub("--", "<->", cs(graph_lines[bidirected][loops]))
      stop_(
        "Invalid graph, the graph contains self-loops: ",
        loop_edges
      )
    }
  }
  args$vars <- unique(
    c(args$dir_rhs, args$dir_lhs, args$bi_rhs, args$bi_lhs)
  )
  args$n <- length(args$vars)
  args$nums <- seq_len(args$n)
  names(args$vars) <- args$nums
  names(args$nums) <- args$vars
  args
}

#' Parse Missing Data Mechanisms
#'
#' @inheritParams dosearch
#' @param args A `list` of arguments for `initialize_dosearch`.
#' @noRd
parse_missing_data <- function(args, missing_data) {
  if (is.null(missing_data)) {
    return(args)
  }
  md_pairs <- gsub("\\s+", "", strsplit(missing_data, ",")[[1L]])
  if (length(md_pairs) == 0L) {
    stop_("Malformed missing data mechanisms.")
  }
  md_mechanisms <- strsplit(md_pairs, ":")
  md_true <- vapply(md_mechanisms, "[[", character(1L), 2L)
  md_switch <- vapply(md_mechanisms, "[[", character(1L), 1L)
  md_proxy <- paste0(md_true, "*")
  if (any(md_switch %in% args$dir_lhs[args$dir_rhs %in% md_true])) {
    stop_("A missing data mechanism cannot be a parent of a true variable.")
  }
  args$dir_lhs <- c(args$dir_lhs, md_true, md_switch)
  args$dir_rhs <- c(args$dir_rhs, md_proxy, md_proxy)
  vars_md <- as.vector(rbind(md_true, md_switch, md_proxy))
  args$vars <- c(vars_md, args$vars[!(args$vars %in% vars_md)])
  args$n <- length(args$vars)
  args$nums <- seq_len(args$n)
  names(args$vars) <- args$nums
  names(args$nums) <- args$vars
  md_switch_nums <- args$nums[md_switch]
  md_proxy_nums <- args$nums[md_proxy]
  args$md_s <- to_dec(md_switch_nums, args$n)
  args$md_p <- to_dec(md_proxy_nums, args$n)
  args$md_t <- bitwShiftR(args$md_p, 2L)
  args
}

#' Parse Transportability Nodes
#'
#' @inheritParams dosearch
#' @param args A `list` of arguments for `initialize_dosearch`.
#' @noRd
parse_transportability <- function(args, transportability) {
  if (is.null(transportability)) {
    return(args)
  }
  tr_vars <- gsub("\\s+", "", strsplit(transportability, ",")[[1L]])
  if (!all(tr_vars %in% args$vars)) {
    tr_mis <- tr_vars[!tr_vars %in% args$vars]
    stop_(
      "Transportability nodes ", cs(tr_mis), " are not present in the graph."
    )
  }
  args$tr_nums <- args$nums[tr_vars]
  args$n_tr <- length(args$tr_nums)
  pa <- args$nums[c(args$dir_rhs, args$bi_rhs, args$bi_lhs)]
  if (any(args$tr_nums %in% pa)) {
    stop_(
      "Invalid graph: ",
      "a transportability node cannot be a child of another node."
    )
  }
  args
}

#' Parse Selection Bias Nodes
#'
#' @inheritParams dosearch
#' @param args A list of arguments for `initialize_dosearch`.
#' @noRd
parse_selection_bias <- function(args, selection_bias) {
  if (is.null(selection_bias)) {
    return(args)
  }
  sb_vars <- gsub("\\s+", "", strsplit(selection_bias, ",")[[1]])
  if (!all(sb_vars %in% args$vars)) {
    sb_mis <- sb_vars[!sb_vars %in% args$vars]
    stop_(
      "Selection bias nodes ", cs(sb_mis), " are not present in the graph."
    )
  }
  args$sb_nums <- args$nums[sb_vars]
  args$n_sb <- length(args$sb_nums)
  if (any(args$sb_nums %in% args$nums[args$dir_lhs])) {
    stop_(
      "Invalid graph: ",
      "a selection bias node cannot be a parent of another node."
    )
  }
  args
}

#' Place Transportability and Selection Bias Nodes Last
#'
#' @param args A `list` of arguments for `initialize_dosearch`.
#' @noRd
reorder_variables <- function(args) {
  if (args$n_tr == 0 && args$n_sb == 0) {
    return(args)
  }
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
    stop_("Unable to parse ", type, ": ", d)
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
    if (!is.null(missing_data)) {
      equals <- grep("=", d_split[[j]], value = FALSE)
      if (length(equals) > 0L) {
        eq_split <- strsplit(d_split[[j]][equals], "[=]")
        eq_lhs <- vapply(eq_split, "[[", character(1L), 1L)
        eq_lhs <- gsub("\\s+", "", eq_lhs)
        eq_rhs <- vapply(eq_split, "[[", character(1L), 2L)
        eq_rhs <- gsub("\\s+", "", eq_rhs)
        uniq_rhs <- unique(eq_rhs)
        if (length(uniq_rhs) > 1L) {
          stop_(
            "Invalid ", type, " ", d, ": ",
            "multiple symbols used for missing data mechanisms."
          )
        }
        if (uniq_rhs[1L] != args$md_sym) {
          stop_(
            "Invalid ", type, " ", d, ": ",
            "invalid symbol used for a missing data mechanism."
          )
        }
        d_split[[j]][equals] <- eq_lhs
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
validate_distribution_dag <- function(args, msg, d, d_str) {
  left_proxy <- bitwAnd(
    bitwShiftR(bitwAnd(d[1L], args$md_p), 2L),
    bitwAnd(d[2L], args$md_t)
  )
  if (left_proxy > 0L) {
    stop_(
      msg, d_str, ": ",
      "proxy variable of a true variable on the left-hand side."
    )
  }
  left_true <- bitwAnd(
    bitwShiftL(bitwAnd(d[1L], args$md_t), 2L),
    bitwAnd(d[2L], args$md_p)
  )
  if (left_true > 0L) {
    stop_(
      msg, d_str, ": ",
      "true variable of a proxy variable on the left-hand side."
    )
  }
  both_left <- bitwAnd(
    bitwShiftR(bitwAnd(d[1L], args$md_p), 2L),
    bitwAnd(d[1L], args$md_t)
  )
  if (both_left > 0L) {
    stop_(
      msg, d_str, ": ",
      "true and proxy versions of the same variable on the left-hand side."
    )
  }
  both_right <- bitwAnd(
    bitwShiftR(bitwAnd(d[2L], args$md_p), 2L),
    bitwAnd(d[2L], args$md_t)
  )
  if (both_right > 0L) {
    stop_(
      msg, d_str, ": ",
      "true and proxy versions of the same variable on the right-hand side."
    )
  }
  if (bitwAnd(d[1L], d[2L]) > 0L) {
    stop_(
      msg, d_str, ": ",
      "same variable on the left and right-hand side."
    )
  }
  if (bitwAnd(d[1L], args$tr) > 0L) {
    stop_(
      msg, d_str, ": ",
      "transportability node on the left-hand side."
    )
  }
  if (bitwAnd(d[1L], args$sb) > 0L) {
    stop_(
      msg, d_str, ": ",
      "selection bias node on the left-hand side."
    )
  }
  if (bitwAnd(d[3L], args$tr) > 0L) {
    stop_(
      msg, d_str, ":\n",
      "intervention on a transportability node."
    )
  }
  if (bitwAnd(d[3L], args$sb) > 0L) {
    stop_(
      msg, d_str, ": ",
      "intervention on a selection bias node."
    )
  }
  if (bitwAnd(d[4L], args$md_s) != d[4L]) {
    stop_(
      msg, d_str, ": ",
      "value assignment of a non-missing data mechanism."
    )
  }
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
    validate_distribution_dag(
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
#' @param args A `list` of arguments for `initialize_dosearch`.
#' @noRd
validate_query_dag <- function(args) {
  q <- args$q_process
  args$q_vec <- c(
    to_dec(q[[1L]], args$n),
    to_dec(c(q[[2L]], q[[3]]), args$n),
    to_dec(q[[3L]], args$n),
    to_dec(q[[4L]], args$n)
  )
  validate_distribution_dag(
    args,
    "Invalid query ",
    args$q_vec,
    q[[5L]]
  )
  args
}

#' Check Otherwise Valid Inputs For Potential Mistakes
#'
#' @param args A `list` of arguments for `initialize_dosearch`.
#' @inheritParams dosearch
#' @noRd
check_valid_input <- function(args, control, missing_data) {
  if (!control$warn) {
    return()
  }
  var_dec <- to_dec(args$nums[args$var_pool], args$n)
  if (!is.null(missing_data)) {
    inc_md <- bitwAnd(args$md_s, var_dec)
    if (inc_md != args$md_s) {
      no_ind <- args$vars[
        which(to_vec(bitwAnd(args$md_s, bitwNot(inc_md)), args$n) == 1L)
      ]
      warning_(
        "There are response indicators ",
        "that are not present in any input distribution: ",
        paste(no_ind, collapse = ", ")
      )
    }
  }
  has_lower <- grepl("^[[:lower:]]+$", args$vars)
  has_upper <- grepl("^[[:upper:]]+$", args$vars)
  if (any(has_lower) && any(has_upper)) {
    warning_(
      "Both lower case and upper case inputs detected."
    )
  }
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
  match_lens <- vapply(
    matches,
    function(x) {
      length(attr(x[[1L]], "match.length"))
    },
    integer(1L)
  )
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
