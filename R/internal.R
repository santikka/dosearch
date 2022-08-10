to_dec <- function(set, n) {
  if (is.null(set)) {
    0L
  } else {
    sum(2L^seq.int(0L, n - 1L)[set])
  }
}

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

#' Check Validity of Data Generating Mechanisms
#' 
#' @inheritParams dosearch
#' @noRd
parse_specials <- function(tr, sb, md) {
  if (!is.null(tr) && (!is.character(tr) || length(tr) > 1L)) {
    stop("Argument `transportability` must be a character vector of length 1.")
  }
  if (!is.null(sb) && (!is.character(sb) || length(sb) > 1L)) {
    stop("Argument `selection_bias` must be a character vector of length 1.")
  }
  if (!is.null(md) && (!is.character(md) || length(md) > 1L)) {
    stop("Argument `missing_data` must be a character vector of length 1.")
  }
}

#' Check Validity of Input Distributions
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
    dv <- vapply(data, parse_distribution, character(1L))
    paste0(dv, collapse = "\n")
  } else if (is.numeric(data)) {
    parse_distribution(data)
  } else {
    stop("Argument `data` is of an unsupported type.")
  }
}

parse_distribution <- function(p) {
  p_len <- length(p)
  if (p_len > 1L) {
    stop("Argument `query` must be a character vector of length 1.")
  }
  if (is.character(p)) { 
    p
  } else if (is.list(p) || is.numeric(p)) {
    val <- NULL
    if (is.list(p)) {
      if (p_len > 2L) {
        stop("Unsupported distribution format: ", p)
      }
      pre <- p[[1L]]
      if (!is.numeric(pre)) {
        stop("Unsupported distribution format: ", p)
      }
      if (p_len == 2L) {
        val <- p[[2L]]
        if (!is.numeric(val)) {
          stop("Unsupported value assignments: ", val)
        }
        if (length(val) != length(pre)) {
          stop("Length mismatch between variables and value assignments ", p)
        }
      }
    } else {
      pre <- p
    }
    if (any(pre < 0 | pre > 2, na.rm = TRUE)) {
      stop("Invalid variable roles in distribution format: ", p)
    }
    if (all(pre > 0, na.rm = TRUE)) {
      stop("Invalid variable roles in distribution format: ", p)
    }
    v <- names(pre)
    if (is.null(v)) {
      v <- seq_len(length(pre))
    }
    A_set <- v[which(pre == 0)]
    B_set <- v[which(pre == 1)]
    C_set <- v[which(pre == 2)]
    A_val_set <- rep("", length(A_set))
    B_val_set <- rep("", length(B_set))
    C_val_set <- rep("", length(C_set))
    if (!is.null(val)) {
      names(val) <- v
      A_val_set <- as.character(val[v %in% A_set])
      B_val_set <- as.character(val[v %in% B_set])
      C_val_set <- as.character(val[v %in% C_set])
      A_val_set <- gsub("(.*)", " = \\1", A_val_set)
      B_val_set <- gsub("(.*)", " = \\1", B_val_set)
      C_val_set <- gsub("(.*)", " = \\1", C_val_set)
      A_val_set[is.na(A_val_set)] <- ""
      B_val_set[is.na(B_val_set)] <- ""
      C_val_set[is.na(C_val_set)] <- ""
    }
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
    stop("Unsupported distribution format: ", p)
  }
}

parse_graph <- function(graph) {
  if (length(graph) > 1L) {
    stop("Argument `graph` must be of length 1.")
  }
  if (is.character(graph)) {
    graph
  } else if (inherits(graph, "igraph")) {
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
      e <- dagitty::edges(g)
      paste(e[, 1L], e[, 3L], e[, 2L], collapse = "\n")
    } else {
      stop("The `dagitty` package is not available.")
    }
  } else {
    stop("Argument `graph` is of an unsupported type.")
  }
}

set_control_defaults <- function(control) {
  control_lengths <- lengths(control)
  if (any(control_lengths > 1L)) {
    stop(
      "All elements of argument `control` must be of length 1.\n",
      "Elements ",
      paste0(names(control)[control_lengths > 1L], collapse = ", "),
      " have length > 1."
    )
  }
  default <- list(
    benchmark = FALSE,
    benchmark_rules = FALSE,
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
  given_args <- names(control) %in% default_names
  control_full <- default
  control_full[given_args] <- control[given_args]
  control <- control_full
  default_types <- vapply(default, typeof, character(1L))
  control_types <- vapply(control, typeof, character(1L))
  invalid_types <- default_types != control_types
  if (any(invalid_types)) {
    stop(
      "Some elements of argument `control` have an invalid type.\n",
      "Elements ",
      paste0(names(control)[invalid_types], collapse = ", "),
      " have types ",
      paste0(control_types[invalid_types], collapse = ", "),
      "\nExpected types: ", 
      paste0(default_types[invalid_types], collapse = ", ")
    )
  }
  control
  # Default value for heuristic is set later 
  # after checking for missing data mechanisms
}

match_distribution <- function(d) {
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

#' Is the Argument a `dosearch` Object?
#' 
#' @param x An \R object.
#' @noRd
is.dosearch <- function(x) {
  inherits(x, "dosearch")
}
