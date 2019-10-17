# Function to call the search from R
#
# data             : A string describing the known distributions.
# query            : A string describing the target distribution.
# graph            : A string describing the graph.
# transportability : A string that lists the transportability nodes.
# selection_bias   : A string that lists the selection bias nodes.
# missing_data     : A string that lists the missing data mechanisms.
#
# control is a list that accepts the following components
#
# benchmark       : A logical value. If TRUE, record time it took for the search (in milliseconds).
# draw_all        : A logical value. If TRUE, all steps of the search are drawn. If FALSE, only steps resulting in the identifying formula are drawn.
# draw_derivation : A logical value. If TRUE, a string representing the derivation steps as a dot graph is also provided.
# formula         : A logical value. If TRUE, a formula for an identifiable effect is provided. If false, the output is a boolean instead.
# heuristic       : A logical value. If TRUE, a search heuristic is applied.
# md_sym          : A single character describing the value that a missing data mechanism attains when it is enabled (defaults to "1")
# rules           : A numeric vector of do-calculus/probability rules used in the search.
# time_limit      : A numeric value for maximum search time (in hours). Will only be in effect if benchmark = TRUE.
# verbose         : A logical value. If TRUE, various diagnostic information is printed to the console during the search.
# warn            : A logical value. If TRUE, gives warnings on possibly mistyped/unwanted input data

get_derivation_dag <- function(
    data, query, graph, 
    transportability = NULL, selection_bias = NULL, missing_data = NULL,
    control = list()) {

    if (is.null(control$benchmark)        || typeof(control$benchmark) != "logical"        || length(control$benchmark) > 1)        control$benchmark <- FALSE
    if (is.null(control$draw_all)         || typeof(control$draw_all) != "logical"         || length(control$draw_all) > 1)         control$draw_all <- FALSE
    if (is.null(control$draw_derivation)  || typeof(control$draw_derivation) != "logical"  || length(control$draw_derivation) > 1)  control$draw_derivation <- FALSE
    if (is.null(control$formula)          || typeof(control$formula) != "logical"          || length(control$formula) > 1)          control$formula <- TRUE
    if (is.null(control$md_sym)           || typeof(control$md_sym) != "character"         || length(control$verbose) > 1)          control$md_sym <- "1"
    if (is.null(control$rules)            || class(control$rules) != "numeric"             || length(control$rules) == 0)           control$rules <- numeric(0)
    if (is.null(control$time_limit)       || class(control$time_limit) != "numeric"        || length(control$time_limit) == 0)      control$time_limit <- 0.5
    if (is.null(control$verbose)          || typeof(control$verbose) != "logical"          || length(control$verbose) > 1)          control$verbose <- FALSE
    if (is.null(control$warn)             || typeof(control$warn) != "logical"             || length(control$warn) > 1)             control$warn <- TRUE
    # Default value for heuristic is set later after checking for missing data mechanisms

    dir_lhs <- c()
    dir_rhs <- c()
    bi_lhs <- c()
    bi_rhs <- c()
    vars <- c()
    nums <- c()
    tr_nums <- c()
    sb_nums <- c()
    n <- 0
    tr <- 0
    sb <- 0
    md_s <- 0
    md_p <- 0
    md_t <- 0
    ntr <- 0
    nsb <- 0
    dist_pattern <- character(5)
    dist_pattern[1] <- "^[Pp]\\(([^|\\$\\),]++(?>,[^|\\$\\),]+)*)\\)$" # Pattern for p(y)
    dist_pattern[2] <- "^[Pp]\\(([^|\\$\\),]++(?>,[^|\\$\\),]+)*)[|]([^|\\$\\),]++(?>,[^|\\$\\),]+)*)\\)$" # Pattern for p(y|z)
    dist_pattern[3] <- "^[Pp]\\(([^|\\$\\),]++(?>,[^|\\$\\),]+)*)[|](?:[\\$]\\(([^|\\$\\),]++(?>,[^|\\$\\),]+)*)\\))\\)$" # Pattern for p(y|do(x))
    dist_pattern[4] <- "^[Pp]\\(([^|\\$\\),]++(?>,[^|\\$\\),]+)*)[|]([^|\\$\\),]++(?>,[^|\\$\\),]+)*)[,](?:[\\$]\\(([^|\\$\\),]++(?>,[^|\\$\\),]+)*)\\))\\)$" # Pattern for p(y|z,do(x))
    dist_pattern[5] <- "^[Pp]\\(([^|\\$\\),]++(?>,[^|\\$\\),]+)*)[|](?:[\\$]\\(([^|\\$\\),]++(?>,[^|\\$\\),]+)*)\\))[,]([^|\\$\\),]++(?>,[^|\\$\\),]+)*)\\)$" # Pattern for p(y|do(x),z)

    # transform the graph
    if ( nchar(graph) == 0 ) {
        if ( is.null(missing_data) ) stop("Invalid graph: the graph is empty.\n")
    } else {
        graph <- gsub("<->", "--", graph)
        graph_split <- strsplit(strsplit(graph, "\r|\n")[[1]], "\\s+")
        line_lengths <- sapply(graph_split, length)
        graph_split <- graph_split[line_lengths > 2]
        arrow_indices <- sapply(graph_split, grep, pattern = "(->)|(--)")
        graph_split <- lapply(1:length(graph_split), function(x) {
            graph_split[[x]][-1:1 + arrow_indices[x]]
        })
        graph_split <- sapply(graph_split, paste, collapse = "")
        directed <- strsplit(graph_split[grep("(.+)?->(.+)?", graph_split)], "->")
        bidirected <- strsplit(graph_split[grep("(.+)?--(.+)?", graph_split)], "--")
        if ( length(directed) > 0 ) {
            dir_lhs <- sapply(directed, "[[", 1)
            dir_rhs <- sapply(directed, "[[", 2)
            if ( any(dir_lhs == dir_rhs) ) stop("Invalid graph: no self loops are allowed.\n")
        }
        if ( length(bidirected) > 0 ) {
            bi_lhs <- sapply(bidirected, "[[", 1)
            bi_rhs <- sapply(bidirected, "[[", 2)
            if ( any(bi_lhs == bi_rhs) ) stop("Invalid graph: no self loops are allowed.\n")
        }
        vars <- unique(c(dir_rhs, dir_lhs, bi_rhs, bi_lhs))
    }

    # parse missing data mechanisms and add proxies
    if ( !is.null(missing_data) ) {
        md_pairs <- gsub("\\s+", "", strsplit(missing_data, ",")[[1]])
        if ( length(md_pairs) == 0 ) stop("Invalid missing data mechanisms.\n")
        md_mechanisms <- strsplit(md_pairs, ":")
        md_true <- sapply(md_mechanisms, "[[", 2)
        md_switch <- sapply(md_mechanisms, "[[", 1)
        md_proxy <- paste0(md_true, "*")
        if ( any(md_switch %in% dir_lhs[dir_rhs %in% md_true]) ) stop("Missing data mechanism cannot be a parent of a true variable.\n")
        dir_lhs <- c(dir_lhs, md_true, md_switch)
        dir_rhs <- c(dir_rhs, md_proxy, md_proxy)
        vars_md <- as.vector(rbind(md_true, md_switch, md_proxy))
        vars <- c(vars_md, vars[!(vars %in% vars_md)])
        n <- length(vars)
        nums <- 1:n
        names(vars) <- nums
        names(nums) <- vars
        md_switch_nums <- nums[md_switch]
        md_proxy_nums <- nums[md_proxy]
        if ( any(is.na(md_switch_nums)) ) stop("Invalid missing data mechanisms.\n")
        if ( any(is.na(md_proxy_nums)) ) stop("Invalid missing data mechanisms.\n")
        md_s <- to_dec(md_switch_nums, n)
        md_p <- to_dec(md_proxy_nums, n)
        md_t <- bitwShiftR(md_p, 2)
        if ( is.null(control$heuristic) || typeof(control$heuristic) != "logical" || length(control$heuristic) > 1 ) control$heuristic <- FALSE
    } else {
        n <- length(vars)
        nums <- 1:n
        names(vars) <- nums
        names(nums) <- vars
        if ( is.null(control$heuristic) || typeof(control$heuristic) != "logical" || length(control$heuristic) > 1 ) control$heuristic <- TRUE
    }

    # parse transportability nodes
    if ( !is.null(transportability) ) {
        tr_nums <- nums[gsub("\\s+", "", strsplit(transportability, ",")[[1]])]
        ntr <- length(tr_nums)
        if ( ntr == 0 ) stop("Invalid transportability nodes.\n")
        if ( any(tr_nums %in% nums[c(dir_rhs, bi_rhs, bi_lhs)]) ) stop("Invalid graph: a transportability node cannot be a child of another node.\n")
    }

    # parse selection bias nodes
    if ( !is.null(selection_bias) ) {
        sb_nums <- nums[gsub("\\s+", "", strsplit(selection_bias, ",")[[1]])]
        nsb <- length(sb_nums)
        if ( nsb == 0 ) stop("Invalid selection bias nodes.\n")
        if ( any(sb_nums %in% nums[dir_lhs]) ) stop("Invalid graph: a selection bias node cannot be a parent of another node.\n")
    }

    if ( ntr > 0 || nsb > 0 ) {
        vars <- vars[c(setdiff(nums, union(tr_nums, sb_nums)), tr_nums, sb_nums)]
        nums <- 1:n
        names(vars) <- nums
        names(nums) <- vars
        if ( ntr > 0 ) {
            tr_nums <- (n - ntr - nsb + 1):(n - nsb)
            tr <- to_dec(tr_nums, n)
        }
        if ( nsb > 0 ) {
            sb_nums <- (n - nsb + 1):n
            sb <- to_dec(sb_nums, n)
        }
    }

    # transform the query
    parts <- NULL
    q_split <- list(NULL, NULL, NULL)
    enabled <- c()
    query_parsed <- gsub("\\s+", "", query)
    query_parsed <- gsub("do", "$", query_parsed)
    matches <- lapply(dist_pattern, function(p) regexec(p, query_parsed, perl = TRUE))
    match_lens <- sapply(matches, function(x) length(attr(x[[1]], "match.length")))
    best_match <- which.max(match_lens)[1]
    parts <- regmatches(query_parsed, matches[[best_match]])[[1]]
    q_split[[1]] <- strsplit(parts[2], "[,]")[[1]]
    if ( best_match == 2 ) {
        q_split[[2]] <- strsplit(parts[3], "[,]")[[1]]
    } else if ( best_match == 3 ) {
        q_split[[3]] <- strsplit(parts[3], "[,]")[[1]]
    } else if ( best_match == 4 ) {
        q_split[[2]] <- strsplit(parts[3], "[,]")[[1]]
        q_split[[3]] <- strsplit(parts[4], "[,]")[[1]]
    } else if ( best_match == 5 ) {
        q_split[[2]] <- strsplit(parts[4], "[,]")[[1]]
        q_split[[3]] <- strsplit(parts[3], "[,]")[[1]]
    }
    if ( any(is.na(q_split[[1]])) ) stop("Invalid query.\n")
    err <- FALSE
    for ( i in 1:3 ) {
        if ( !is.null(q_split[[i]]) ) {
            if ( any(dup <- duplicated(q_split[[i]])) ) {
                msg <- paste0(c("cannot contain duplicated variables ", q_split[[i]][dup], ".\n"))
                err <- TRUE
            }
            if ( err ) stop(paste0(c("Invalid query: ", msg)))
            if ( !is.null(missing_data) ) {
                equals <- grep("=", q_split[[i]], value = FALSE)
                eq_split <- strsplit(q_split[[i]][equals], "[=]")
                eq_lhs <- eq_rhs <- c()
                if ( length(equals) > 0 ) {
                    eq_lhs <- sapply(eq_split, "[[", 1)
                    eq_lhs <- gsub("\\s+", "", eq_lhs)
                    eq_rhs <- sapply(eq_split, "[[", 2)
                    eq_rhs <- gsub("\\s+", "", eq_rhs)
                    uniq_rhs <- unique(eq_rhs)
                    if ( length(uniq_rhs) > 1 ) stop("Cannot use multiple symbols to denote active missing data mechanisms.\n")
                    if ( uniq_rhs[1] != control$md_sym ) stop(paste0("Invalid symbol for missing data mechanism on data line ", i, ": ", uniq_rhs[1], ".\n"))
                    q_split[[i]][equals] <- eq_lhs
                    enabled <- c(enabled, eq_lhs)
                }
            }
        }
    }
    q1_new <- q_split[[1]][which(!(q_split[[1]] %in% vars))]
    q2_new <- q_split[[2]][which(!(q_split[[2]] %in% vars))]
    q3_new <- q_split[[3]][which(!(q_split[[3]] %in% vars))]
    new_vars <- unique(c(q1_new, q2_new, q3_new))
    if ( length(new_vars) > 0 ) {
        n <- n + length(new_vars)
        vars <- c(vars, new_vars)
        nums <- 1:n
        names(vars) <- nums
        names(nums) <- vars
    }
    q_process <- list(nums[q_split[[1]]], nums[q_split[[2]]], nums[q_split[[3]]], nums[enabled], parts[1])

    # transform the data
    data_split <- strsplit(data, "\r|\n")[[1]]
    data_split <- gsub("\\s+", "", data_split)
    data_split <- data_split[which(nchar(data_split) > 0)]
    p_list <- list()
    p_process <- list()
    var_pool <- c()
    for ( i in 1:length(data_split) ) {
        parts <- NULL
        p_split <- list(NULL, NULL, NULL)
        enabled <- c()
        p_parsed <- gsub("\\s+", "", data_split[[i]])
        p_parsed <- gsub("do", "$", p_parsed)
        matches <- lapply(dist_pattern, function(p) regexec(p, p_parsed, perl = TRUE))
        match_lens <- sapply(matches, function(x) length(attr(x[[1]], "match.length")))
        best_match <- which.max(match_lens)[1]
        parts <- regmatches(p_parsed, matches[[best_match]])[[1]]
        p_split[[1]] <- strsplit(parts[2], "[,]")[[1]]
        if ( best_match == 2 ) {
            p_split[[2]] <- strsplit(parts[3], "[,]")[[1]]
        } else if ( best_match == 3 ) {
            p_split[[3]] <- strsplit(parts[3], "[,]")[[1]]
        } else if ( best_match == 4 ) {
            p_split[[2]] <- strsplit(parts[3], "[,]")[[1]]
            p_split[[3]] <- strsplit(parts[4], "[,]")[[1]]
        } else if ( best_match == 5 ) {
            p_split[[2]] <- strsplit(parts[4], "[,]")[[1]]
            p_split[[3]] <- strsplit(parts[3], "[,]")[[1]]
        }
        if ( any(is.na(p_split[[1]])) ) {
            stop(paste0("Invalid input distribution on data line ", i ,": ", data_split[[i]], ".\n")) 
        }
        err <- FALSE
        for ( j in 1:3 ) {
            if ( !is.null(p_split[[j]]) ) {
                if ( any(dup <- duplicated(p_split[[j]])) ) {
                    msg <- paste0(c("cannot contain duplicated variables ", p_split[[j]][dup], ".\n"))
                    err <- TRUE
                }
                if ( err ) stop(paste0(c("Invalid input distribution: ", data_split[[i]], ", ", msg)))
                if ( !is.null(missing_data) ) {
                    equals <- grep("=", p_split[[j]], value = FALSE)
                    eq_split <- strsplit(p_split[[j]][equals], "[=]")
                    eq_lhs <- eq_rhs <- c()
                    if ( length(equals) > 0 ) {
                        eq_lhs <- sapply(eq_split, "[[", 1)
                        eq_lhs <- gsub("\\s+", "", eq_lhs)
                        eq_rhs <- sapply(eq_split, "[[", 2)
                        eq_rhs <- gsub("\\s+", "", eq_rhs)
                        uniq_rhs <- unique(eq_rhs)
                        if ( length(uniq_rhs) > 1 ) stop("Cannot use multiple symbols to denote active missing data mechanisms.\n")
                        if ( uniq_rhs[1] != control$md_sym ) stop(paste0("Invalid symbol for missing data mechanism on data line ", i, ": ", uniq_rhs[1], ".\n"))
                        p_split[[j]][equals] <- eq_lhs
                        enabled <- c(enabled, eq_lhs)
                    }
                }
            }
        }
        p1_new <- p_split[[1]][which(!(p_split[[1]] %in% vars))]
        p2_new <- p_split[[2]][which(!(p_split[[2]] %in% vars))]
        p3_new <- p_split[[3]][which(!(p_split[[3]] %in% vars))]
        new_vars <- unique(c(p1_new, p2_new, p3_new))
        if ( length(new_vars) > 0 ) {
            n <- n + length(new_vars)
            vars <- c(vars, new_vars)
            nums <- 1:n
            names(vars) <- nums
            names(nums) <- vars
        }
        p_process[[i]] <- list(nums[p_split[[1]]], nums[p_split[[2]]], nums[p_split[[3]]], nums[enabled], data_split[[i]])
        var_pool <- union(var_pool, p_split[[1]])
    }

    if ( control$warn ) {
        var_dec <- to_dec(nums[var_pool], n)
        if ( !is.null(missing_data) ) {
            if ( (inc_md <- bitwAnd(md_s, var_dec)) != md_s ) {
                no_ind <- vars[which(to_vec(bitwAnd(md_s, bitwNot(inc_md)), n) == 1)]
                warning(paste0(c("There are response indicators that are not present in any input distribution: ", paste(no_ind, collapse = ", "))))
            }
        }
    }

    for ( i in 1:length(p_process) ) {
        p <- p_process[[i]]
        p_list[[i]] <- c(to_dec(p[[1]], n), to_dec(c(p[[2]], p[[3]]), n), to_dec(p[[3]], n), to_dec(p[[4]], n))
        err <- FALSE
        msg <- ""
        if ( bitwAnd(bitwShiftR(bitwAnd(p_list[[i]][1], md_p), 2), bitwAnd(p_list[[i]][2], md_t)) > 0 ) {
            msg <- "proxy variable of a true variable present on the left-hand side.\n"
            err <- TRUE
        } else if ( bitwAnd(bitwShiftL(bitwAnd(p_list[[i]][1], md_t), 2), bitwAnd(p_list[[i]][2], md_p)) > 0 ) {
            msg <- "true variable of a proxy variable present on the left-hand side.\n"
            err <- TRUE
        } else if ( bitwAnd(bitwShiftR(bitwAnd(p_list[[i]][1], md_p), 2), bitwAnd(p_list[[i]][1], md_t)) > 0 ) {
            msg <- "true and proxy versions of the same variable present on the left-hand side.\n"
            err <- TRUE
        } else if ( bitwAnd(bitwShiftR(bitwAnd(p_list[[i]][2], md_p), 2), bitwAnd(p_list[[i]][2], md_t)) > 0 ) {
            msg <- "true and proxy versions of the same variable present on the right-hand side.\n"
            err <- TRUE
        } else if ( bitwAnd(p_list[[i]][1], p_list[[i]][2]) > 0 ) {
            msg <- "same variable on the left and right-hand side.\n"
            err <- TRUE
        } else if ( bitwAnd(p_list[[i]][1], tr) > 0 ) {
            msg <- "cannot contain a transportability node on the left-hand side.\n"
            err <- TRUE
        } else if ( bitwAnd(p_list[[i]][1], sb) > 0 ) {
            msg <- "cannot contain a a selection bias node on the left-hand side.\n"
            err <- TRUE
        } else if ( bitwAnd(p_list[[i]][3], tr) > 0 ) {
            msg <- "cannot intervene on a transportability node.\n"
            err <- TRUE
        } else if ( bitwAnd(p_list[[i]][3], sb) > 0 ) {
            msg <- "cannot intervene on a selection bias node.\n"
            err <- TRUE
        } else if (bitwAnd(p_list[[i]][4], md_s) != p_list[[i]][4] ) {
            msg <- "cannot set value of non-missing data mechanism.\n"
            err <- TRUE
        }
        if ( err ) stop(paste0(c("Invalid input distribution on data line ", i, ": ", p[[4]], ", ", msg)))
    }

    q_vec <- c(to_dec(q_process[[1]], n), to_dec(c(q_process[[2]], q_process[[3]]), n), to_dec(q_process[[3]], n), to_dec(q_process[[4]], n))
    err <- FALSE
    msg <- ""
    if ( bitwAnd(bitwShiftR(bitwAnd(q_vec[1], md_p), 2), bitwAnd(q_vec[2], md_t)) > 0 ) {
        msg <- "proxy variable of a true variable present on the left-hand side.\n"
        err <- TRUE
    } else if ( bitwAnd(bitwShiftL(bitwAnd(q_vec[1], md_t), 2), bitwAnd(q_vec[2], md_p)) > 0 ) {
        msg <- "true variable of a proxy variable present on the left-hand side.\n"
        err <- TRUE
    } else if ( bitwAnd(bitwShiftR(bitwAnd(q_vec[1], md_p), 2), bitwAnd(q_vec[1], md_t)) > 0 ) {
        msg <- "true and proxy versions of the same variable present on the left-hand side.\n"
        err <- TRUE
    } else if ( bitwAnd(bitwShiftR(bitwAnd(q_vec[2], md_p), 2), bitwAnd(q_vec[2], md_t)) > 0 ) {
        msg <- "true and proxy versions of the same variable present on the right-hand side.\n"
        err <- TRUE
    } else if ( bitwAnd(q_vec[1], q_vec[2]) > 0 ) {
        msg <- "same variable on the left and right-hand side.\n"
        err <- TRUE
    } else if ( bitwAnd(q_vec[1], tr) > 0 ) {
        msg <- "cannot contain a transportability node on the left-hand side.\n"
        err <- TRUE
    } else if ( bitwAnd(q_vec[1], sb) > 0 ) {
        msg <- "cannot contain a a selection bias node on the left-hand side.\n"
        err <- TRUE
    } else if ( bitwAnd(q_vec[3], tr) > 0 ) {
        msg <- "cannot intervene on a transportability node.\n"
        err <- TRUE
    } else if ( bitwAnd(q_vec[3], sb) > 0 ) {
        msg <- "cannot intervene on a selection bias node.\n"
        err <- TRUE
    } else if ( bitwAnd(q_vec[3], md_s) > 0 ) {
        msg <- "cannot intervene on a missing data mechanism.\n"
        err <- TRUE
    } else if ( bitwAnd(q_vec[4], md_s) != q_vec[4] ) {
        msg <- "cannot set value of non-missing data mechanism.\n"
        err <- TRUE
    }
    if ( err ) {
        stop(paste0(c("Invalid query: ", msg)))
    }

    res <- initialize_dosearch(
        as.numeric(nums[dir_lhs]),
        as.numeric(nums[dir_rhs]),
        as.numeric(nums[bi_lhs]),
        as.numeric(nums[bi_rhs]),
        as.character(vars),
        p_list,
        q_vec,
        n,
        tr,
        sb,
        md_s,
        md_p,
        control$time_limit,
        control$rules,
        control$benchmark,
        control$draw_derivation,
        control$draw_all,
        control$formula,
        control$heuristic,
        control$md_sym,
        control$verbose
    )

    return(res[c(
        TRUE,
        control$formula,
        control$draw_derivation,
        control$benchmark,
        control$benchmark
    )])

}
