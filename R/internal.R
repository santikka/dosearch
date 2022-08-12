#' Convert a Vector of Unique Intergers to a Set
#'
#' @param vec An `integer` vector.
#' @param n An `integer`, the number of unique elements.
#' @noRd
to_dec <- function(vec, n) {
  if (is.null(vec)) {
    0L
  } else {
    sum(2L^seq.int(0L, n - 1L)[vec])
  }
}

#' Convert a Set of Intergers to a Vector of Unique Integers
#'
#' @param dec An `integer`, representing a set of integers.
#' @param n An `integer`, the number of unique elements.
#' @noRd
to_vec <- function(dec, n) {
  if (n == 0L) {
    integer(0L)
  } else {
    b <- integer(n)
    for (i in seq_len(n)) {
      b[n - i + 1L] <- (dec %/% 2)
      dec <- (dec %/% 2)
    }
    rev(b)
  }
}

#' Create a Comma-separated Character String
cs <- function(x) {
  paste0(x, collapse = ", ")
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
    stop("Argument `", spec_name, "` must be a character vector of length 1.")
  }
}

#' Parse Input Distributions for Internal Processing
#'
#' @inheritParams dosearch
#' @noRd
parse_data <- function(data) {
  if (is.character(data)) {
    if (length(data) > 1L) {
      stop("Argument `data` must be of length 1 when of `character` type.")
    }
    data
  } else if (is.list(data)) {
    paste0(vapply(data, parse_distribution, character(1L)), collapse = "\n")
  } else if (is.numeric(data)) {
    parse_distribution(data)
  } else {
    stop("Argument `data` is of an unsupported type.")
  }
}

#' Parse the Target Distribution for Internal Processing
#'
#' @inheritParams dosearch
#' @noRd
parse_query <- function(query) {
  if (is.character(query)) {
    if (length(query) > 1L) {
      stop("Argument `query` must be a character vector of length 1.")
    }
    query
  } else if (is.numeric(query)) {
    parse_distribution(query)
  } else {
    stop("Argument `query` is of an unsupported type.")
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
    d <- as.integer(d)
    if (any(is.na(d) | !is.finite(d))) {
      stop(
        "Invalid distribution format: ", d,
        "All values must be non-missing and finite."
      )
    }
    if (any(!p %in% 0L:2L)) {
      stop(
        "Invalid variable roles in distribution format: ", d,
        "Role value must be either 0, 1 or 2."
      )
    }
    if (all(pre > 0, na.rm = TRUE)) {
      stop(
        "Invalid variable roles in distribution format: ", d,
        "At least one variable must have role value 0."
      )
    }
    if (is.null(v)) {
      v <- seq_len(3L)
    }
    A_set <- v[which(d == 0L)]
    B_set <- v[which(d == 1L)]
    C_set <- v[which(d == 2L)]
    A_val_set <- rep("", length(A_set))
    B_val_set <- rep("", length(B_set))
    C_val_set <- rep("", length(C_set))
    A <- paste(A_set, A_val_set, sep = "", collapse = ",")
    B <- paste(B_set, B_val_set, sep = "", collapse = ",")
    C <- paste(C_set, C_val_set, sep = "", collapse = ",")
    nb <- nchar(B)
    nc <- nchar(C)
    paste(
      "p(", A, ifelse(nb > 0 | nc > 0, "|", ""),
      ifelse(nb > 0, "do(", ""), B, ifelse(nb > 0, ")", ""),
      ifelse(nb > 0 & nc > 0, ",", ""),
      C, ")", sep = ""
    )
  } else {
    stop("Unsupported distribution format: ", d)
  }
}

#' Parse the Graph for Internal Processing
#'
#' @inheritParams dosearch
#' @noRd
parse_graph <- function(graph) {
  if (inherits(graph, "igraph")) {
    if (requireNamespace("igraph", quietly = TRUE)) {
      e <- igraph::E(graph)
      v <- igraph::vertex_attr(graph, "name")
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
          unobs_ind[, 1L] < unobs_ind[, 2L], , drop = FALSE
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
      stop("The `igraph` package is not available.")
    }
  } else if (inherits(graph, "dagitty")) {
    if (requireNamespace("dagitty", quietly = TRUE)) {
      if (!identical(dagitty::graphType(graph), "dag")) {
        stop("Attempting to use `dagitty`, but argument `graph` is not a DAG.")
      }
      e <- dagitty::edges(graph)
      paste(e[, 1L], e[, 3L], e[, 2L], collapse = "\n")
    } else {
      stop("The `dagitty` package is not available.")
    }
  } else if (is.character(graph)) {
    if (length(graph) > 1L) {
      stop("Argument `graph` must be of length 1 when of `character` type.")
    }
    graph
  } else {
    stop("Argument `graph` is of an unsupported type.")
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
      "Elements ",
      paste0(names(control)[control_lengths > 1L], collapse = ", "),
      " have length > 1."
    )
  }
  control$rules <- rules
  default <- list(
    benchmark = FALSE,
    benchmark_rules = FALSE,
    cache = TRUE,
    draw_all = FALSE,
    draw_derivation = FALSE,
    formula = TRUE,
    improve = TRUE,
    heuristic = FALSE,
    md_sym = "1",
    rules = integer(0L),
    time_limit = -1.0,
    verbose = FALSE,
    warn = TRUE
  )
  default_names <- names(default)
  control_names <- names(control)
  if (any(!control_names %in% default_names)) {
    stop(
      "Unrecognized control arguments: ",
      control_names[!control_names %in% default_names]
    )
  }
  given_args <- which(default_names %in% control_names)
  control_full <- default
  control_full[given_args] <- control
  control <- control_full
  default_types <- vapply(default, typeof, character(1L))
  control_types <- vapply(control, typeof, character(1L))
  invalid_types <- default_types != control_types
  if (any(invalid_types)) {
    stop(
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
  # Default value for heuristic is set later
  # after checking for missing data mechanisms
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
