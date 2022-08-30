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

#' Convert a Vector of Unique Integers to a Set
#'
#' @param vec An `integer` vector.
#' @param n An `integer`, the number of unique elements.
#' @noRd
to_dec <- function(vec, n) {
  sum(2L^seq.int(0L, n - 1L)[vec])
}

#' Convert a Set of Integers to a Vector of Unique Integers
#'
#' @param dec An `integer`, representing a set of integers.
#' @param n An `integer`, the number of unique elements.
#' @noRd
to_vec <- function(dec, n) {
  b <- integer(n)
  for (i in seq_len(n)) {
    b[n - i + 1L] <- (dec %% 2L) * 1L
    dec <- (dec %/% 2L)
  }
  rev(b)
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
#' @srrstats {NW2.7} Checks for NAs and non-finite values.
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
#' @srrstats {NW2.3} Checks existence of vertex names attributes for `igraph`
#'   and graphs and assigns defaults if the graph is not named with a message.
#' @noRd
parse_graph <- function(graph) {
  if (inherits(graph, "igraph")) {
    if (require_namespace("igraph", quietly = TRUE)) {
      e <- igraph::E(graph)
      v <- igraph::vertex_attr(graph, "name")
      if (is.null(v)) {
        n <- length(igraph::V(graph))
        pow <- ceiling(log(n) / log(26))
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
      description <- NULL
      obs_edges <- e[(is.na(description) | description != "U")]
      unobs_edges <- e[description == "U" & !is.na(description)]
      if (length(obs_edges) > 0L) {
        obs_ind <- igraph::get.edges(graph, obs_edges)
        g_obs <- paste(
          v[obs_ind[, 1L]],
          "->",
          v[obs_ind[, 2L]],
          collapse = "\n"
        )
      }
      if (length(unobs_edges) > 0L) {
        unobs_ind <- igraph::get.edges(graph, unobs_edges)
        unobs_ind <- unobs_ind[
          unobs_ind[, 1L] < unobs_ind[, 2L], ,
          drop = FALSE
        ]
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

#' Check True Graph Size
#'
#' @param args n The number of nodes.
#' @noRd
check_graph_size <- function(n) {
  if (n > 30) {
    stop_("The inputs imply a graph with more than 30 nodes.")
  }
}

#' Set Default Values for Control Arguments
#'
#' @inheritParams dosearch
#' @noRd
control_defaults <- function(control) {
  rules <- as.integer(control$rules)
  control$rules <- integer(0L)
  control_lengths <- lengths(control)
  if (any(control_lengths > 1L)) {
    stop(
      "All elements of argument `control` ",
      "must be of length 1 (except `rules`).\n",
      "The following elements have length > 1: ",
      cs(names(control)[control_lengths > 1L])
    )
  }
  control$rules <- rules
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
    md_sym = "1",
    rules = integer(0L),
    time_limit = -1.0,
    verbose = FALSE,
    warn = TRUE
  )
  default_names <- names(default)
  control_names <- names(control)
  if (any(!control_names %in% default_names)) {
    stop_(
      "Unknown control arguments: ",
      control_names[!control_names %in% default_names]
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
  control
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
is.dosearch <- function(x) {
  inherits(x, "dosearch")
}

.onUnload <- function(libpath) {
  library.dynam.unload("dosearch", libpath)
}
