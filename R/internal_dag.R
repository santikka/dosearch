#' Call the `dosearch` Algorithm from R for DAGs
#'
#' @inheritParams dosearch
#' @noRd
get_derivation_dag <- function(data, query, graph, transportability,
                               selection_bias, missing_data, control,
                               case_reasoning = FALSE) {
  control <- control_defaults(control)
  control$rules <- validate_rules(
    control$rules,
    c(
      -13L, -11L, -9L, -8L, -7L, -6L, -3L, -2L, -1L,
      1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 13L
    )
  )
  cl <- list(
    data = data,
    query = query,
    graph = graph,
    transportability = transportability,
    selection_bias = selection_bias,
    missing_data = missing_data,
    control = control,
    case_reasoning = case_reasoning
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
    tr = integer(0L),
    sb = integer(0L),
    md_s = integer(0L),
    md_p = integer(0L),
    md_t = integer(0L)
  )
  args <- transform_graph_dag(args, graph, missing_data)
  args <- parse_missing_data(args, missing_data)
  args <- parse_transportability(args, transportability)
  args <- parse_selection_bias(args, selection_bias)
  args <- reorder_variables(args)
  args$graph_vars <- args$vars
  args <- parse_query_dag(args, query, missing_data)
  args <- parse_data_dag(args, data, missing_data)
  args <- validate_data_dag(args)
  args <- validate_query_dag(args)
  check_valid_input(args, control, missing_data)
  res <- initialize_dosearch(
    as.integer(args$nums[args$dir_lhs]),
    as.integer(args$nums[args$dir_rhs]),
    as.integer(args$nums[args$bi_lhs]),
    as.integer(args$nums[args$bi_rhs]),
    as.character(args$vars),
    args$p_list,
    args$q_list,
    args$n,
    args$tr,
    args$sb,
    args$md_s,
    args$md_p,
    control$time_limit,
    control$rules,
    case_reasoning,
    control$benchmark,
    control$benchmark_rules,
    control$draw_derivation,
    control$draw_all,
    control$formula,
    control$improve,
    control$heuristic,
    control$verbose
  )
  res$call <- cl
  structure(
    res[
      c(
        "identifiable", # always include identifiability
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
  unique_edges <- !duplicated(graph_split)
  graph_split <- graph_split[unique_edges]
  graph_lines <- graph_lines[unique_edges]
  edges <- vapply(graph_split, "[[", character(1L), 2L)
  valid_edges <- edges %in% c("->", "--")
  if (any(!valid_edges)) {
    stop_(
      "Invalid graph, unknown edge types found: ",
      cs(edges[!valid_edges])
    )
  }
  directed <- edges == "->"
  bidirected <- edges == "--"
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
  malformed <- malformed_pairs(md_mechanisms)
  if (any(malformed)) {
    stop_(
      "Malformed missing data mechanisms: ",
      cs(md_pairs[malformed]),
      ". Each mechanism must be of the form `M_X : X`."
    )
  }
  md_true <- vapply(md_mechanisms, "[[", character(1L), 2L)
  md_switch <- vapply(md_mechanisms, "[[", character(1L), 1L)
  md_proxy <- paste0(md_true, "*")
  if (anyDuplicated(md_true) > 0L) {
    stop_(
      "Each variable can have only one missing data mechanism: ",
      cs(unique(md_true[duplicated(md_true)]))
    )
  }
  if (anyDuplicated(md_switch) > 0L) {
    stop_(
      "A missing data mechanism cannot be shared by multiple variables: ",
      cs(unique(md_switch[duplicated(md_switch)]))
    )
  }
  if (any(md_switch %in% md_true)) {
    stop_(
      "A variable cannot be both a missing data mechanism ",
      "and a variable with missing data: ",
      cs(md_switch[md_switch %in% md_true])
    )
  }
  if (any(md_switch %in% args$dir_lhs[args$dir_rhs %in% md_true])) {
    stop_("A missing data mechanism cannot be a parent of a true variable.")
  }
  md_vars <- as.vector(rbind(md_true, md_switch))
  args$md_absent <- md_vars[!md_vars %in% args$vars]
  args$dir_lhs <- c(args$dir_lhs, md_true, md_switch)
  args$dir_rhs <- c(args$dir_rhs, md_proxy, md_proxy)
  vars_md <- as.vector(rbind(md_true, md_switch, md_proxy))
  args$vars <- c(vars_md, args$vars[!(args$vars %in% vars_md)])
  args$n <- length(args$vars)
  args$nums <- seq_len(args$n)
  names(args$vars) <- args$nums
  names(args$nums) <- args$vars
  # The true variable, the switch and the proxy of each mechanism have
  # consecutive indices
  args$md_s <- as_set(args$nums[md_switch])
  args$md_p <- as_set(args$nums[md_proxy])
  args$md_t <- args$md_p - 2L
  args$md_vars <- vars_md
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
  tr_vars <- unique(tr_vars[nzchar(tr_vars)])
  if (!all(tr_vars %in% args$vars)) {
    tr_mis <- tr_vars[!tr_vars %in% args$vars]
    stop_(
      "Transportability nodes ", cs(tr_mis), " are not present in the graph."
    )
  }
  md_tr <- intersect(tr_vars, args$md_vars)
  if (length(md_tr) > 0L) {
    stop_(
      "Transportability nodes ", cs(md_tr), " cannot be ",
      "variables with missing data, missing data mechanisms or proxies."
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
  sb_vars <- unique(sb_vars[nzchar(sb_vars)])
  if (!all(sb_vars %in% args$vars)) {
    sb_mis <- sb_vars[!sb_vars %in% args$vars]
    stop_(
      "Selection bias nodes ", cs(sb_mis), " are not present in the graph."
    )
  }
  md_sb <- intersect(sb_vars, args$md_vars)
  if (length(md_sb) > 0L) {
    stop_(
      "Selection bias nodes ", cs(md_sb), " cannot be ",
      "variables with missing data, missing data mechanisms or proxies."
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
    args$tr <- as_set(args$tr_nums)
  }
  if (args$n_sb > 0L) {
    args$sb_nums <- seq.int(args$n - args$n_sb + 1L, args$n)
    args$sb <- as_set(args$sb_nums)
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
  disabled <- character(0L)
  d_parsed <- gsub("\\s+", "", d)
  d_parsed <- gsub("([|,])do\\(", "\\1$(", d_parsed)
  d_split <- match_distribution_dag(d_parsed)
  if (any(is.na(d_split[[1L]]))) {
    stop_("Unable to parse ", type, ": ", d)
  }
  d_null <- vapply(d_split, is.null, logical(1L))
  for (j in which(!d_null)) {
    equals <- grep("=", d_split[[j]], value = FALSE)
    if (length(equals) > 0L && is.null(missing_data)) {
      stop_(
        "Invalid ", type, " ", d, ": ",
        "value assignments are only supported for missing data mechanisms."
      )
    }
    if (!is.null(missing_data)) {
      if (length(equals) > 0L) {
        eq_split <- strsplit(d_split[[j]][equals], "[=]")
        if (any(malformed_pairs(eq_split))) {
          stop_(
            "Invalid ", type, " ", d, ": ",
            "malformed value assignment."
          )
        }
        eq_lhs <- vapply(eq_split, "[[", character(1L), 1L)
        eq_lhs <- gsub("\\s+", "", eq_lhs)
        eq_rhs <- vapply(eq_split, "[[", character(1L), 2L)
        eq_rhs <- gsub("\\s+", "", eq_rhs)
        # 1 denotes an enabled and 0 a disabled missing data mechanism
        if (any(!eq_rhs %in% c("0", "1"))) {
          stop_(
            "Invalid ", type, " ", d, ": ",
            "the value of a missing data mechanism must be either 0 or 1."
          )
        }
        d_split[[j]][equals] <- eq_lhs
        enabled <- c(enabled, eq_lhs[eq_rhs == "1"])
        disabled <- c(disabled, eq_lhs[eq_rhs == "0"])
      }
    }
    dup <- duplicated(d_split[[j]])
    if (any(dup)) {
      stop_(
        "Invalid ", type, ": ", d, ", ",
        "duplicated variables ", cs(unique(d_split[[j]][dup])), "."
      )
    }
  }
  both <- intersect(d_split[[2L]], d_split[[3L]])
  if (length(both) > 0L) {
    stop_(
      "Invalid ", type, ": ", d, ", ",
      "variables cannot be both conditioned on and intervened on: ",
      cs(both), "."
    )
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
    args$nums[disabled],
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
parse_data_dag <- function(args, data, missing_data) {
  data_split <- strsplit(data, "\r|\n")[[1]]
  data_split <- gsub("\\s+", "", data_split)
  data_split <- data_split[which(nzchar(data_split))]
  args$p_process <- vector(mode = "list", length = length(data_split))
  args$input_vars <- vector(mode = "list", length = length(data_split))
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
#' @param msg A `character` string to prefix error messages with.
#' @param d A `list` of five sets of vertex indices denoting the distribution.
#' @param d_str A `character` string representing the distribution.
#' @noRd
validate_distribution_dag <- function(args, msg, d, d_str) {
  a <- d[[1L]]
  b <- d[[2L]]
  a_proxy_true <- a[a %in% args$md_p] - 2L
  if (any(a_proxy_true %in% b)) {
    stop_(
      msg, d_str, ": ",
      "proxy variable of a true variable on the left-hand side."
    )
  }
  if (any((a[a %in% args$md_t] + 2L) %in% b)) {
    stop_(
      msg, d_str, ": ",
      "true variable of a proxy variable on the left-hand side."
    )
  }
  if (any(a_proxy_true %in% a)) {
    stop_(
      msg, d_str, ": ",
      "true and proxy versions of the same variable on the left-hand side."
    )
  }
  if (any((b[b %in% args$md_p] - 2L) %in% b)) {
    stop_(
      msg, d_str, ": ",
      "true and proxy versions of the same variable on the right-hand side."
    )
  }
  if (any(a %in% b)) {
    stop_(
      msg, d_str, ": ",
      "same variable on the left and right-hand side."
    )
  }
  if (any(a %in% args$tr)) {
    stop_(
      msg, d_str, ": ",
      "transportability node on the left-hand side."
    )
  }
  if (any(a %in% args$sb)) {
    stop_(
      msg, d_str, ": ",
      "selection bias node on the left-hand side."
    )
  }
  if (any(d[[3L]] %in% args$tr)) {
    stop_(
      msg, d_str, ":\n",
      "intervention on a transportability node."
    )
  }
  if (any(d[[3L]] %in% args$sb)) {
    stop_(
      msg, d_str, ": ",
      "intervention on a selection bias node."
    )
  }
  if (!all(c(d[[4L]], d[[5L]]) %in% args$md_s)) {
    stop_(
      msg, d_str, ": ",
      "value assignment of a non-missing data mechanism."
    )
  }
}

#' Convert a Parsed Distribution into Sets of Vertex Indices
#'
#' The sets are the left-hand side variables, the right-hand side variables,
#' the intervened variables, the enabled missing data mechanisms and the
#' disabled missing data mechanisms.
#'
#' @param p A `list` of parsed distribution components.
#' @noRd
distribution_sets_dag <- function(p) {
  list(
    as_set(p[[1L]]),
    as_set(c(p[[2L]], p[[3L]])),
    as_set(p[[3L]]),
    as_set(p[[4L]]),
    as_set(p[[5L]])
  )
}

#' Check the Validity of Input Distributions
#'
#' @param args A `list` of arguments for `initialize_dosearch`.
#' @noRd
validate_data_dag <- function(args) {
  args$p_list <- vector(mode = "list", length = length(args$p_process))
  for (i in seq_along(args$p_process)) {
    p <- args$p_process[[i]]
    args$p_list[[i]] <- distribution_sets_dag(p)
    validate_distribution_dag(
      args,
      "Invalid input distribution ",
      args$p_list[[i]],
      p[[6L]]
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
  args$q_list <- distribution_sets_dag(q)
  validate_distribution_dag(
    args,
    "Invalid query ",
    args$q_list,
    q[[6L]]
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
  if (!is.null(missing_data)) {
    if (length(args$md_absent) > 0L) {
      warning_(
        "There are missing data mechanisms or variables with missing data ",
        "that are not present in the graph: ",
        cs(args$md_absent)
      )
    }
    no_ind <- args$vars[setdiff(args$md_s, args$nums[args$var_pool])]
    if (length(no_ind) > 0L) {
      warning_(
        "There are response indicators ",
        "that are not present in any input distribution: ",
        paste(no_ind, collapse = ", ")
      )
    }
  }
  warn_unknown_query_vars(args)
  warn_unknown_input_vars(args)
  # Names are classified by their letters only, e.g., X_1 is upper case
  var_letters <- gsub("[^[:alpha:]]", "", args$vars)
  has_letters <- nzchar(var_letters)
  has_lower <- has_letters & var_letters == tolower(var_letters)
  has_upper <- has_letters & var_letters == toupper(var_letters)
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
