#' Stop Function Execution Without Displaying the Call
#'
#' @inheritParams base::stop
#' @noRd
stop_ <- function(...) {
  stop(..., call. = FALSE)
}

#' Display a Warning Message Displaying the Call
#'
#' @inheritParams base::warning
#' @noRd
warning_ <- function(...) {
  warning(..., call. = FALSE)
}

#' Create a Sequence from 2 to `n` If `n` Is At Least 2
#'
#' @param n An `integer` specifying the last value of the sequence.
#' @noRd
seq_min2 <- function(n) {
  if (n < 2L) {
    integer(0L)
  } else {
    seq.int(2L, n)
  }
}

#' Convert a Vector of Vertex Indices to a Set
#'
#' Sets of vertices are passed to the C++ side as sorted vectors of unique
#' vertex indices, which places no limit on the number of vertices.
#'
#' @param vec An `integer` vector of vertex indices.
#' @noRd
as_set <- function(vec) {
  sort(unique(as.integer(vec)))
}

#' Create a Comma-separated Character String
#'
#' @param x A `character` vector.
#' @noRd
cs <- function(x) {
  paste0(x, collapse = ", ")
}

#' Check that a Directed Graph is Acyclic via Topological Sort
#'
#' @param lhs A `character` vector giving the left-hand side
#'   variables of the edges.
#' @param rhs A `character` vector giving the right-hand side
#'   variables of the edges.
#' @noRd
is_acyclic <- function(lhs, rhs) {
  s <- setdiff(lhs, rhs)
  while (length(s) > 0L) {
    v <- s[1L]
    s <- s[-1L]
    e <- which(lhs == v)
    e_rhs <- rhs[e]
    for (i in seq_along(e)) {
      w <- e_rhs[i]
      lhs[e[i]] <- ""
      rhs[e[i]] <- ""
      if (!w %in% rhs) {
        s <- c(s, w)
      }
    }
  }
  lhs <- lhs[nzchar(lhs)]
  if (identical(length(lhs), 0L)) {
    TRUE
  } else {
    FALSE
  }
}

#' Wrapper for `requireNamespace` for Mocking
#'
#' @inheritParams requireNamespace
#' @noRd
require_namespace <- function(package, ..., quietly = FALSE) {
  requireNamespace(package, ..., quietly = quietly)
}

#' Add New Variables to a `dosearch` Call
#'
#' @param args A `list` of arguments to `initialize_dosearch`.
#' @param new_vars A `character` vector of variable names to add.
#' @noRd
add_new_vars <- function(args, new_vars) {
  if (length(new_vars) > 0L) {
    args$n <- args$n + length(new_vars)
    args$vars <- c(args$vars, new_vars)
    args$nums <- seq_len(args$n)
    names(args$vars) <- args$nums
    names(args$nums) <- args$vars
  }
  args
}

#' Check Validity of Data Generating Mechanisms
#'
#' @param spec_name A `character` vector of length one naming the mechanism.
#' @param spec `transportability`, `selection_bias` or `missing_data` argument
#'   of `dosearch`.
#' @noRd
validate_special <- function(spec_name, spec) {
  if (!is.null(spec) && (!is.character(spec) || length(spec) > 1L)) {
    stop_("Argument `", spec_name, "` must be a character vector of length 1.")
  }
}

#' Parse Input Distributions for Internal Processing
#'
#' @inheritParams dosearch
#' @noRd
parse_data <- function(data) {
  if (is.character(data)) {
    if (length(data) > 1L) {
      stop_("Argument `data` must be of length 1 when of type `character`.")
    }
    data
  } else if (is.list(data)) {
    paste0(vapply(data, parse_distribution, character(1L)), collapse = "\n")
  } else if (is.numeric(data)) {
    parse_distribution(data)
  } else {
    stop_("Argument `data` is of an unsupported type.")
  }
}

#' Parse the Target Distribution for Internal Processing
#'
#' @inheritParams dosearch
#' @noRd
parse_query <- function(query) {
  if (is.character(query)) {
    if (length(query) > 1L) {
      stop_("Argument `query` must be of length 1 when of type `character`.")
    }
    query
  } else if (is.numeric(query)) {
    parse_distribution(query)
  } else {
    stop_("Argument `query` is of an unsupported type.")
  }
}

#' Parse A Single Distribution for Internal Processing
#'
#' @param d A `numeric` or a `character` vector.
#' @noRd
parse_distribution <- function(d) {
  if (is.character(d)) {
    d
  } else if (is.numeric(d)) {
    v <- names(d)
    if (is.null(v)) {
      stop_(
        "Invalid distribution format ", deparse1(d), ": ",
        "role values must be given as a named vector."
      )
    }
    if (any(is.na(d) | !is.finite(d))) {
      stop_(
        "Invalid distribution format ", deparse1(d), ": ",
        "all role values must be non-missing and finite."
      )
    }
    if (any(!d %in% 0:2)) {
      stop_(
        "Invalid variable roles in distribution format ", deparse1(d), ": ",
        "all role values must be either 0, 1 or 2."
      )
    }
    if (all(d > 0)) {
      stop_(
        "Invalid variable roles in distribution format ", deparse1(d), ": ",
        "at least one variable must have role value 0."
      )
    }
    d <- as.integer(d)
    a_set <- v[which(d == 0L)]
    b_set <- v[which(d == 1L)]
    c_set <- v[which(d == 2L)]
    a_val_set <- rep("", length(a_set))
    b_val_set <- rep("", length(b_set))
    c_val_set <- rep("", length(c_set))
    a_str <- paste(a_set, a_val_set, sep = "", collapse = ",")
    b_str <- paste(b_set, b_val_set, sep = "", collapse = ",")
    c_str <- paste(c_set, c_val_set, sep = "", collapse = ",")
    n_b <- nchar(b_str)
    n_c <- nchar(c_str)
    paste(
      "p(",
      a_str,
      ifelse(n_b > 0 | n_c > 0, "|", ""),
      ifelse(n_b > 0, "do(", ""),
      b_str,
      ifelse(n_b > 0, ")", ""),
      ifelse(n_b > 0 & n_c > 0, ",", ""),
      c_str,
      ")",
      sep = ""
    )
  } else {
    stop_("Unable to parse distribution format ", deparse1(d), ".")
  }
}

#' Parse the Graph for Internal Processing
#'
#' @inheritParams dosearch
#' @noRd
parse_graph <- function(graph) {
  if (inherits(graph, "igraph")) {
    if (require_namespace("igraph", quietly = TRUE)) {
      e <- igraph::E(graph)
      v <- igraph::vertex_attr(graph, "name")
      if (is.null(v)) {
        n <- length(igraph::V(graph))
        pow <- max(1L, ceiling(log(n) / log(26)))
        d <- apply(
          expand.grid(rep(list(letters), pow)),
          1,
          paste0,
          collapse = ""
        )
        v <- d[seq_len(n)]
        message(
          "Argument `graph` is not named, node names have been assigned: ",
          cs(v)
        )
      }
      g_obs <- ""
      g_unobs <- ""
      e_attr <- igraph::edge_attr(graph, "description", index = e)
      obs_edges <- e
      unobs_edges <- NULL
      if (!is.null(e_attr)) {
        obs_edges <- e[is.na(e_attr) | e_attr != "U"]
        unobs_edges <- e[!is.na(e_attr) & e_attr == "U"]
      }
      if (length(obs_edges) > 0L) {
        obs_ind <- igraph::ends(graph, obs_edges, names = FALSE)
        g_obs <- paste(
          v[obs_ind[, 1L]],
          "->",
          v[obs_ind[, 2L]],
          collapse = "\n"
        )
      }
      if (length(unobs_edges) > 0L) {
        unobs_ind <- igraph::ends(graph, unobs_edges, names = FALSE)
        # A bidirected edge may be given in either or both directions
        unobs_ind <- unique(
          cbind(
            pmin(unobs_ind[, 1L], unobs_ind[, 2L]),
            pmax(unobs_ind[, 1L], unobs_ind[, 2L])
          )
        )
        g_unobs <- paste(
          v[unobs_ind[, 1L]],
          "<->",
          v[unobs_ind[, 2L]],
          collapse = "\n"
        )
      }
      paste0(c(g_obs, g_unobs), collapse = "\n")
    } else {
      stop_("The `igraph` package is not available.")
    }
  } else if (inherits(graph, "dagitty")) {
    if (require_namespace("dagitty", quietly = TRUE)) {
      if (!identical(dagitty::graphType(graph), "dag")) {
        stop_("Attempting to use `dagitty`, but the graph type is not `dag`.")
      }
      e <- dagitty::edges(graph)
      paste(e[, 1L], e[, 3L], e[, 2L], collapse = "\n")
    } else {
      stop_("The `dagitty` package is not available.")
    }
  } else if (is.character(graph)) {
    if (length(graph) > 1L) {
      stop_("Argument `graph` must be of length 1 when of `character` type.")
    }
    graph
  } else {
    stop_("Argument `graph` is of an unsupported type.")
  }
}

#' Set Default Values for Control Arguments
#'
#' @inheritParams dosearch
#' @noRd
control_defaults <- function(control) {
  if ("md_sym" %in% names(control)) {
    warning_(
      "Argument `md_sym` of `control` is deprecated and will be ignored. ",
      "The value 1 always denotes an enabled missing data mechanism ",
      "and the value 0 a disabled one."
    )
    control$md_sym <- NULL
  }
  rules <- as.integer(control$rules)
  con_vars <- as.character(control$con_vars)
  control$rules <- integer(0L)
  control$con_vars <- character(0L)
  control_lengths <- lengths(control)
  if (any(control_lengths > 1L)) {
    stop_(
      "All elements of argument `control` ",
      "must be of length 1 (except `rules` and `con_vars`).\n",
      "The following elements have length > 1: ",
      cs(names(control)[control_lengths > 1L])
    )
  }
  control$rules <- rules
  control$con_vars <- con_vars
  default <- list(
    benchmark = FALSE,
    benchmark_rules = FALSE,
    cache = TRUE,
    draw_all = FALSE,
    draw_derivation = FALSE,
    empty = FALSE,
    formula = TRUE,
    heuristic = FALSE,
    improve = TRUE,
    rules = integer(0L),
    time_limit = -1.0,
    verbose = FALSE,
    warn = TRUE,
    con_vars = character(0L)
  )
  default_names <- names(default)
  control_names <- names(control)
  if (any(!control_names %in% default_names)) {
    stop_(
      "Unknown control arguments: ",
      cs(control_names[!control_names %in% default_names])
    )
  }
  given_args <- match(control_names, default_names)
  control_full <- default
  control_full[given_args] <- control
  control <- control_full
  default_types <- vapply(default, typeof, character(1L))
  control_types <- vapply(control, typeof, character(1L))
  invalid_types <- default_types != control_types
  if (any(invalid_types)) {
    stop_(
      "Some elements of argument `control` have an invalid type.\n",
      "Invalid arguments: ",
      paste0(names(control)[invalid_types], collapse = ", "),
      "\nProvided types: ",
      paste0(control_types[invalid_types], collapse = ", "),
      "\nExpected types: ",
      paste0(default_types[invalid_types], collapse = ", ")
    )
  }
  if (control$benchmark_rules) {
    control$benchmark <- TRUE
  }
  control
}

#' Find Elements That Are Not Pairs of Non-empty Strings
#'
#' @param x A `list` of `character` vectors, e.g., value assignments of the
#'   form `X = v` split by `=`.
#' @noRd
malformed_pairs <- function(x) {
  vapply(
    x,
    function(y) length(y) != 2L || !all(nzchar(y)),
    logical(1L)
  )
}

#' Warn About Target Variables Missing From the Graph and the Inputs
#'
#' @param args A `list` of arguments for `initialize_dosearch` or
#'   `initialize_csisearch`.
#' @noRd
warn_unknown_query_vars <- function(args) {
  unknown <- setdiff(args$query_vars, union(args$graph_vars, args$var_pool))
  if (length(unknown) > 0L) {
    warning_(
      "The target distribution contains variables that are not present ",
      "in the graph or in any input distribution: ",
      cs(unknown)
    )
  }
}

#' Warn About Input Variables Missing From the Graph and Other Distributions
#'
#' A variable that is not present in the graph, in any other input
#' distribution or in the target distribution is likely a typo.
#'
#' @param args A `list` of arguments for `initialize_dosearch` or
#'   `initialize_csisearch`.
#' @noRd
warn_unknown_input_vars <- function(args) {
  v <- unlist(args$input_vars)
  once <- v[!v %in% v[duplicated(v)]]
  unknown <- setdiff(once, union(args$graph_vars, args$query_vars))
  if (length(unknown) > 0L) {
    warning_(
      "The input distributions contain variables that are not present ",
      "in the graph, in any other input distribution ",
      "or in the target distribution: ",
      cs(unknown)
    )
  }
}

#' Check the Validity of Inference Rules Given via `control`
#'
#' @param rules An `integer` vector of rule identifiers.
#' @param valid An `integer` vector of valid rule identifiers.
#' @noRd
validate_rules <- function(rules, valid) {
  invalid <- rules[!rules %in% valid]
  if (length(invalid) > 0L) {
    stop_(
      "Invalid rules in argument `control`: ", cs(unique(invalid)), ".\n",
      "Valid rules are: ", cs(valid)
    )
  }
  unique(rules)
}

#' Construct an Empty `dosearch` Object
#'
#' @param cl A `list` containing the original `dosearch` call arguments.
#' @noRd
empty_output <- function(cl) {
  structure(
    list(
      identifiable = FALSE,
      formula = "",
      call = cl
    ),
    class = "dosearch"
  )
}

#' Is the Argument a `dosearch` Object?
#'
#' @param x An \R object.
#' @noRd
is_dosearch <- function(x) {
  inherits(x, "dosearch")
}

.onUnload <- function(libpath) {
  library.dynam.unload("dosearch", libpath)
}
