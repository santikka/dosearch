# Wrapper function to call different variations of the search

dosearch <- function(
    data, query, graph, 
    transportability = NULL, selection_bias = NULL, missing_data = NULL,
    control = list()) {

    data <- parse_data(data)
    query <- parse_distribution(query)
    graph <- parse_graph(graph)

    if (!is.null(transportability) || !is.null(selection_bias) || !is.null(missing_data)) {
        return(get_derivation_dag(data, query, graph, transportability, selection_bias, missing_data, control))
    }
    if (grepl(":", graph[1])) {
        return(get_derivation_ldag(data, query, graph, control))
    }
    return(get_derivation_dag(data, query, graph, transportability, selection_bias, missing_data, control))
}

summary.dosearch <- function(object, ...) {
    took <- NA
    units <- NA
    if (!is.null(object$time)) {
        took <- object$time / 1000.0
        units <- "seconds."
        if (object$time >= 60 & object$time < 3600) {
            took <- took / 60.0
            units <- "minutes."
        } else if (object$time >= 3600) {
            took <- took / 3600.0
            units <- "hours."
        }
    }
    d <- gsub("\n", "\n\t", trimws(object$call$data, which = "both"))
    d <- gsub(" ", "", d)
    g <- gsub("\n", "\n\t", trimws(object$call$graph, which = "both"))
    g <- gsub(" ", "", g)
    g <- gsub("->", " -> ", g)
    g <- gsub("<->", " <-> ", g)
    g <- gsub("--", " -- ", g)
    f <- ifelse(is.null(object$formula), "", object$formula)
    ans <- list(result = list(identifiable = object$identifiable, formula = f),
                call = object$call,
                time = took,
                units = units,
                data = d,
                graph = g
    )
    class(ans) <- "summary.dosearch"
    return(ans)
}

print.summary.dosearch <- function(x, ..., maxchar = 72) {
    res <- x$result
    y <- x$call
    cat("The query", y$query, "is", ifelse(res$identifiable, "identifiable.", "non-identifiable."), "\n")
    if (identical(res$formula, "")) cat("Formula: NA\n")
    else {
        f <- res$formula
        if (nchar(f) > maxchar) f <- paste0(substr(res$formula, 0, maxchar), "...")
        cat("Formula:\n\t", f, "\n", sep = "")
    }
    if (!is.na(x$time)) cat("Derivation took", x$time, x$units, "\n")
    cat("Input data:\n")
    cat("\t", x$data, "\n", sep = "")
    cat("Input graph:\n")
    cat("\t", x$graph, "\n", sep = "")
    invisible(x)
}

print.dosearch <- function(x, ...) {
    if (is.null(x$formula) | identical(x$formula, "")) { 
        cat("The query", x$call$query, "is", ifelse(x$identifiable, "identifiable", "non-identifiable."), "\n")
    } else cat(format(x$formula, ...), "\n")
}

is_identifiable <- function(x) {
    if (is_dosearch(x)) return(x$identifiable)
    else stop("Object is not of class 'dosearch': ", x)
}

get_formula <- function(x, run_again = FALSE) {
    if (is_dosearch(x)) {
        if (!is.null(x$formula)) return(x$formula)
        else {
            if (run_again) {
                y <- x$call
                z <- dosearch(y$data, y$query, y$graph, y$transportability, y$selection_bias, y$missing_data, y$control)
                return(z$formula)
            }
            cat("No formula is available.\n")
        }
    } else stop("Object is not of class 'dosearch': ", x)
}

get_derivation <- function(x, run_again = FALSE, draw_all = FALSE) {
    if (is_dosearch(x)) {
        if (!is.null(x$derivation)) return(x$derivation)
        else {
            if (run_again) {
                y <- x$call
                control <- y$control
                control$draw_derivation <- TRUE
                control$draw_all <- draw_all
                z <- dosearch(y$data, y$query, y$graph, y$transportability, y$selection_bias, y$missing_data, control)
                return(z$derivation)
            }
            cat("No derivation is available.\n")
        }
    } else stop("Object is not of class 'dosearch': ", x)
}

get_benchmark <- function(x, run_again = FALSE) {
    if (is_dosearch(x)) {
        if (!is.null(x$time)) return(list(x$time, x$rule_times))
        else {
            if (run_again) {
                y <- x$call
                control <- y$control
                control$benchmark <- TRUE
                z <- dosearch(y$data, y$query, y$graph, y$transportability, y$selection_bias, y$missing_data, control)
                return(list(z$time, z$rule_times))
            }
            cat("No benchmark is available.\n")
        }
    } else stop("Object is not of class 'dosearch': ", x)
}