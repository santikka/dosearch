# Function to call the search from R
#
# data             : A string describing the known distributions.
# query            : A string describing the target distribution.
# graph            : A string describing the graph.
#
# control is a list that accepts the following components
#
# benchmark       : A logical value. If TRUE, record time it took for the search (in milliseconds).
# draw_derivation : A logical value. If TRUE, a string representing the derivation steps as a dot graph is also provided.
# draw_all        : A logical value. If TRUE, all steps of the search are drawn. If FALSE, only steps resulting in the identifying formula are drawn.
# cache           : A logical value. If TRUE, derived separation criteria are stored and not evaluated again
# formula         : A logical value. If TRUE, a formula for an identifiable effect is provided. If false, the output is a boolean instead.
# heuristic       : A logical value. If TRUE, a search heuristic is applied.
# rules           : A numeric vector of do-calculus/probability rules used in the search.
# time_limit      : A numeric value for maximum search time (in hours). Will only be in effect if benchmark = TRUE.
# verbose         : A logical value. If TRUE, various diagnostic information is printed to the console during the search.

get_derivation_ldag <- function(
    data, query, graph, control = list()) {

    if (is.null(control$benchmark)        || typeof(control$benchmark) != "logical"        || length(control$benchmark) > 1)        control$benchmark <- FALSE
    if (is.null(control$draw_derivation)  || typeof(control$draw_derivation) != "logical"  || length(control$draw_derivation) > 1)  control$draw_derivation <- FALSE
    if (is.null(control$draw_all)         || typeof(control$draw_all) != "logical"         || length(control$draw_all) > 1)         control$draw_all <- FALSE
    if (is.null(control$cache)            || typeof(control$cache) != "logical"            || length(control$cache) > 1)            control$cache <- TRUE
    if (is.null(control$formula)          || typeof(control$formula) != "logical"          || length(control$formula) > 1)          control$formula <- TRUE
    if (is.null(control$heuristic)        || typeof(control$heuristic) != "logical"        || length(control$heuristic) > 1)        control$heuristic <- TRUE
    if (is.null(control$rules)            || class(control$rules) != "numeric"             || length(control$rules) == 0)           control$rules <- numeric(0)
    if (is.null(control$time_limit)       || class(control$time_limit) != "numeric"        || length(control$time_limit) == 0)      control$time_limit <- -1.0
    if (is.null(control$verbose)          || typeof(control$verbose) != "logical"          || length(control$verbose) > 1)          control$verbose <- FALSE

    dir_lhs <- c()
    dir_rhs <- c()
    bi_lhs <- c()
    bi_rhs <- c()
    vars <- c()
    nums <- c()
    n <- 0
    con_vars <- c()
    intv_vars <- c()
    parents <- list()
    contexts <- c()
    target <- NULL
    label_map <- NULL
    local_csi <- NULL
    dist_pattern <- character(2)
    dist_pattern[1] <- "^[Pp]\\(([^|\\$\\),]++(?>,[^|\\$\\),]+)*)\\)$" # Pattern for p(y)
    dist_pattern[2] <- "^[Pp]\\(([^|\\$\\),]++(?>,[^|\\$\\),]+)*)[|]([^|\\$\\),]++(?>,[^|\\$\\),]+)*)\\)$" # Pattern for p(y|z)

    # transform the graph
    if (nchar(graph) == 0) stop("Invalid graph: the graph is empty. \n")
    else {
        row_pattern <- "^(.+(?>->|--)[^\\:]+)(?>\\:(.+))?$"
        graph_split <- strsplit(graph, "\r|\n")[[1]]
        graph_split <- gsub("\\s", "", graph_split)
        valid_rows <- grep(row_pattern, graph_split, perl = TRUE)
        graph_split <- graph_split[valid_rows]
        graph_match <- regexec(row_pattern, graph_split, perl = TRUE)
        split_rows <- regmatches(graph_split, graph_match)
        edges <- sapply(split_rows, "[[", 2)
        directed <- strsplit(grep("(.+)->(.+)", edges, value = TRUE), "->")
        if (length(directed) > 0) {
            dir_lhs <- sapply(directed, "[[", 1)
            dir_rhs <- sapply(directed, "[[", 2)
            if ( any(dir_lhs == dir_rhs) ) stop("Invalid graph: no self loops are allowed.\n")
        }
        contexts_split <- list()
        contextuals <- which(nchar(sapply(split_rows, "[[", 3)) > 0)
        labels_split <- list()
        if (length(contextuals) > 0) {
            edges_split <- strsplit(edges, "(->)")
            labels <- sapply(split_rows[contextuals], "[[", 3)
            labels_split <- strsplit(labels, "[;]")
            labels_split <- lapply(labels_split, strsplit, "[,]")
            targets <- lapply(1:length(labels_split), function(i) {
              c("from" = edges_split[[contextuals[i]]][1],
                "to" = edges_split[[contextuals[i]]][2])
            })
        }
        labels <- labels_split
        vars <- unique(c(dir_rhs, dir_lhs))
        n <- length(vars)
        intv <- dir_lhs[substr(dir_lhs, 1, 2) == "I_"]
        ivar <- which(vars %in% intv)
        vars <- vars[c(setdiff(1:n, ivar), ivar)]
        intv_vars <- vars[which(vars %in% intv)]
        nums <- 1:n
        names(vars) <- nums
        names(nums) <- vars
        for (v in vars) {
            parents[[v]] <- character(0)
        }
        for (i in seq_along(dir_rhs)) {
            parents[[dir_rhs[i]]] <- union(parents[[dir_rhs[i]]], dir_lhs[i])
        }
    }

    # parse labels
    if (length(labels) > 0) {
        input_labels <- matrix(0, sum(sapply(labels, length)), 5)
        index <- 0
        index_csi <- 0
        inferred <- 0
        inferred_labels <- matrix(0, 0, 5)
        local_csi <- list()
        vanishing <- matrix(0, 0, 2)
        for (i in seq_along(labels)) { # Labels
            from <- targets[[i]]["from"]
            to <- targets[[i]]["to"]
            pa <- setdiff(parents[[to]], from)
            npa <- length(pa)
            if (npa == 0) stop(paste0("Invalid label for edge", from, " -> ", to, ": no parents to assign.\n"))
            vals <- expand.grid(rep(list(c(0, 1)), npa))
            names(vals) <- pa
            vals$present <- FALSE
            for (j in seq_along(labels[[i]])) { # Individual assignments within label
                index <- index + 1
                label_split <- strsplit(labels[[i]][[j]], "[=]")
                label_lhs <- sapply(label_split, "[[", 1)
                label_rhs <- sapply(label_split, "[[", 2)
                if (any(duplicated(label_lhs))) stop(paste0("Invalid label for edge", from, " -> ", to, ": duplicate assignment.\n"))
                if (from %in% label_lhs) stop(paste0("Invalid label for edge", from, " -> ", to, ": ", from, " cannot appear in the label.\n"))
                if (to %in% label_lhs) stop(paste0("Invalid label for edge", from, " -> ", to, ": ", to, " cannot appear in the label.\n"))
                if (any(!(label_lhs %in% pa))) stop(paste0("Invalid label for edge", from, " -> ", to, ": only other parents of ", to, " may be assigned.\n"))
                intv <- substr(label_lhs, 1, 2) == "I_"
                con_vars <- c(con_vars, label_lhs[!intv])
                zero <- which(label_rhs == 0)
                one <- which(label_rhs == 1)
                input_labels[index,1] <- to_dec(nums[label_lhs[zero]], n)
                input_labels[index,2] <- to_dec(nums[label_lhs[one]], n)
                input_labels[index,3] <- nums[from]
                input_labels[index,4] <- nums[to]
                input_labels[index,5] <- to_dec(nums[pa], n)
                # Infer non-explicit labels from input
                zl <- length(zero)
                ol <- length(one)
                if (zl == 0) {
                    ones <- vals[ ,which(pa %in% label_lhs[one]), drop = FALSE]
                    if (nrow(ones) > 0) {
                        vals[which(apply(ones, 1, function(x) all(x == 1))),"present"] <- TRUE
                    }
                } else if (ol == 0) {
                    zeros <- vals[ ,which(pa %in% label_lhs[zero]), drop = FALSE]
                    if (nrow(zeros) > 0) {
                        vals[which(apply(zeros, 1, function(x) all(x == 0))),"present"] <- TRUE
                    }
                } else {
                    zeros <- vals[ ,which(pa %in% label_lhs[zero]), drop = FALSE]
                    ones <- vals[ ,which(pa %in% label_lhs[one]), drop = FALSE]
                    ind_z <- which(apply(zeros, 1, function(x) all(x == 0)))
                    ind_o <- which(apply(ones, 1, function(x) all(x == 1)))
                    ind_zo <- intersect(ind_z, ind_o)
                    if (length(ind_zo) > 0) {
                        vals[ind_zo,"present"] <- TRUE
                    }
                }
            }
            if (all(vals$present)) {
                vanishing <- rbind(vanishing, c(nums[from], nums[to]))
            }
            #stop(paste0("Invalid label for edge: ", from, " -> ", to, ": label is satisfied in every context.\n"))
            # Cannot infer from empty set
            if ((nsets <- nrow(vals) - 1) > 1) {
                for (j in 2:nsets) {
                    sub_pa <- pa[which(vals[j,1:npa] == 1)]
                    sub_ind <- which(pa %in% sub_pa)
                    sub_vals <- vals[ ,c(sub_ind, npa + 1)]
                    assignments <- expand.grid(rep(list(c(0, 1)), length(sub_pa)))
                    names(assignments) <- sub_pa
                    for (k in 1:nrow(assignments)) {
                        zero <- sub_pa[which(assignments[k, ] == 0)]
                        one <- sub_pa[which(assignments[k, ] == 1)]
                        assign_ind <- apply(sub_vals[ ,-ncol(sub_vals), drop = FALSE], 1, function(x) identical(as.numeric(x), as.numeric(assignments[k, ])))
                        if (all(sub_vals[assign_ind,"present"])) {
                            inferred <- inferred + 1
                            inferred_labels <- rbind(inferred_labels, c(to_dec(nums[zero], n), to_dec(nums[one], n), nums[from], nums[to], to_dec(nums[pa], n)))
                        }
                    }
                }
            }
        }
        if (inferred > 0) {
            input_labels <- rbind(input_labels, inferred_labels)
            input_labels <- input_labels[!duplicated(input_labels), ]
        }
        con_vars <- unique(con_vars)
        all_contexts <- expand.grid(rep(list(c(0, 1)), length(con_vars)))
        label_map <- list()
        null_context <- c()
        if ((ncon <- nrow(all_contexts)) > 0) {
            for (i in 2:ncon) {
                sub_vars <- con_vars[which(all_contexts[i, ] == 1)]
                con_vals <- expand.grid(rep(list(c(0, 1)), length(sub_vars)))
                label_map[[i-1]] <- list(vars = to_dec(nums[sub_vars], n), contexts = vector(mode = "list", length = nrow(con_vals)))
                equiv_ind <- 0
                unique_context <- list()
                for (j in 1:nrow(con_vals)) {
                    zero <- sub_vars[which(con_vals[j, ] == 0)]
                    one <- sub_vars[which(con_vals[j, ] == 1)]
                    z <- to_dec(nums[zero], n)
                    o <- to_dec(nums[one], n)
                    label_map[[i-1]][["contexts"]][[j]]$zero <- z
                    label_map[[i-1]][["contexts"]][[j]]$one <- o
                    endpoints <- matrix(0, 0, 2)
                    for (k in 1:nrow(input_labels)) {
                        z_inp <- input_labels[k,1]
                        o_inp <- input_labels[k,2]
                        if ((bitwAnd(z, z_inp) == z_inp && bitwAnd(o, o_inp) == o_inp)) {
                            if (!any(apply(vanishing, 1, function(x) isTRUE(all.equal(x, input_labels[k,3:4]))))) {
                                endpoints <- rbind(endpoints, input_labels[k,3:4])
                                pa <- input_labels[k,5]
                                lab <- bitwOr(z, o)
                                if (pa == lab) {
                                    index_csi <- index_csi + 1
                                    local_csi[[index_csi]] <- list(
                                        x = to_dec(input_labels[k,3], n),
                                        y = to_dec(input_labels[k,4], n),
                                        z = pa,
                                        zero = z,
                                        one = o)
                                }
                            }
                        }
                    }
                    endpoints <- unique(endpoints)
                    label_map[[i-1]][["contexts"]][[j]]$from <- endpoints[ ,1]
                    label_map[[i-1]][["contexts"]][[j]]$to <- endpoints[ ,2]
                    pos <- Position(function(x) identical(endpoints[ ,1], x$from) && identical(endpoints[ ,2], x$to), unique_context)
                    if ( is.na(pos) ) {
                        equiv_ind <- equiv_ind + 1
                        label_map[[i-1]][["contexts"]][[j]]$equivalence <- equiv_ind
                        unique_context[[equiv_ind]] <- list(from = endpoints[ ,1], to = endpoints[ ,2])
                    } else {
                        label_map[[i-1]][["contexts"]][[j]]$equivalence <- pos
                    }
                }
                if (all(sapply(label_map[[i-1]][["contexts"]], function(x) length(x[["from"]])) == 0)) null_context <- c(null_context, i - 1)
            }
        }
        all_interventions <- expand.grid(rep(list(c(0, 1)), length(intv_vars)))
        if ((nintv <- nrow(all_interventions)) > 0) {
            for (i in 2:nintv) {
                index <- max(ncon - 1, 0) + i - 1
                sub_vars <- intv_vars[which(all_interventions[i, ] == 1)]
                o <- to_dec(nums[sub_vars], n)
                label_map[[index]] <- list(vars = o, contexts = list(list(zero = 0, one = o)))
                endpoints <- matrix(0, 0, 2)
                for (k in 1:nrow(input_labels)) {
                    z_inp <- input_labels[k,1]
                    o_inp <- input_labels[k,2]
                    if ( z_inp == 0 && bitwAnd(o, o_inp) == o_inp) {
                        if (!any(apply(vanishing, 1, function(x) isTRUE(all.equal(x, input_labels[k,3:4]))))) {
                            endpoints <- rbind(endpoints, input_labels[k,3:4])
                        }
                    }
                }
                endpoints <- unique(endpoints)
                label_map[[index]][["contexts"]][[1]]$from <- endpoints[ ,1]
                label_map[[index]][["contexts"]][[1]]$to <- endpoints[ ,2]
                label_map[[index]][["contexts"]][[1]]$equivalence <- 1
            }
        }
        label_map[null_context] <- NULL
        if (nrow(vanishing) > 0) {
            edge_mat <- cbind(nums[dir_lhs], nums[dir_rhs])
            present <- !duplicated(rbind(edge_mat, vanishing), fromLast = TRUE)[1:nrow(edge_mat)]
            dir_lhs <- dir_lhs[present]
            dir_rhs <- dir_rhs[present]
        }
    }

    # transform the query
    parts <- NULL
    q_split <- list(NULL, NULL, NULL)
    zero <- c()
    one <- c()
    query_parsed <- gsub("\\s+", "", query)
    query_parsed <- gsub("do", "$", query_parsed)
    matches <- lapply(dist_pattern, function(p) regexec(p, query_parsed, perl = TRUE))
    match_lens <- sapply(matches, function(x) length(attr(x[[1]], "match.length")))
    best_match <- which.max(match_lens)[1]
    parts <- regmatches(query_parsed, matches[[best_match]])[[1]]
    q_split[[1]] <- strsplit(parts[2], "[,]")[[1]]
    if (best_match == 2) {
        q_split[[2]] <- strsplit(parts[3], "[,]")[[1]]
    }
    if (any(is.na(q_split[[1]]))) stop("Invalid query.\n")
    err <- FALSE
    for (i in 1:2) {
        if (!is.null(q_split[[i]])) {
            if (any(dup <- duplicated(q_split[[i]]))) {
                msg <- paste0(c("cannot contain duplicated variables ", q_split[[i]][dup], ".\n"))
                err <- TRUE
            }
            if (err) stop(paste0(c("Invalid query: ", msg)))
            equals <- grep("=", q_split[[i]], value = FALSE)
            eq_split <- strsplit(q_split[[i]][equals], "[=]")
            eq_lhs <- eq_rhs <- c()
            if (length(equals) > 0) {
                eq_lhs <- sapply(eq_split, "[[", 1)
                eq_lhs <- gsub("\\s+", "", eq_lhs)
                eq_rhs <- sapply(eq_split, "[[", 2)
                eq_rhs <- gsub("\\s+", "", eq_rhs)
                uniq_rhs <- unique(eq_rhs)
                if (!(uniq_rhs[1] %in% 0:1)) stop(paste0("Invalid value assignment in query. \n"))
                q_split[[i]][equals] <- eq_lhs
                z <- which(eq_rhs == 1)
                o <- which(eq_rhs == 0)
                zero <- c(zero, eq_lhs[eq_rhs == 0])
                one <- c(one, eq_lhs[eq_rhs == 1])
            }
        }
    }
    q1_new <- q_split[[1]][which(!(q_split[[1]] %in% vars))]
    q2_new <- q_split[[2]][which(!(q_split[[2]] %in% vars))]
    new_vars <- unique(c(q1_new, q2_new))
    if (length(new_vars) > 0) {
        n <- n + length(new_vars)
        vars <- c(vars, new_vars)
        nums <- 1:n
        names(vars) <- nums
        names(nums) <- vars
    }
    q_process <- list(nums[q_split[[1]]], nums[q_split[[2]]], nums[zero], nums[one], parts[1])

    # transform the data
    data_split <- strsplit(data, "\r|\n")[[1]]
    data_split <- gsub("\\s+", "", data_split)
    data_split <- data_split[which(nchar(data_split) > 0)]
    p_list <- list()
    p_process <- list()
    for (i in 1:length(data_split)) {
        parts <- NULL
        p_split <- list(NULL, NULL, NULL)
        zero <- c()
        one <- c()
        p_parsed <- gsub("\\s+", "", data_split[[i]])
        p_parsed <- gsub("do", "$", p_parsed)
        matches <- lapply(dist_pattern, function(p) regexec(p, p_parsed, perl = TRUE))
        match_lens <- sapply(matches, function(x) length(attr(x[[1]], "match.length")))
        best_match <- which.max(match_lens)[1]
        parts <- regmatches(p_parsed, matches[[best_match]])[[1]]
        p_split[[1]] <- strsplit(parts[2], "[,]")[[1]]
        if (best_match == 2) {
            p_split[[2]] <- strsplit(parts[3], "[,]")[[1]]
        }
        if (any(is.na(p_split[[1]]))) {
            stop(paste0("Invalid input distribution on data line ", i ,": ", data_split[[i]], ".\n")) 
        }
        err <- FALSE
        for (j in 1:2)  {
            if (!is.null(p_split[[j]])) {
                if (any(dup <- duplicated(p_split[[j]]))) {
                    msg <- paste0(c("cannot contain duplicated variables ", p_split[[j]][dup], ".\n"))
                    err <- TRUE
                }
                if (err) stop(paste0(c("Invalid input distribution: ", data_split[[i]], ", ", msg)))
                equals <- grep("=", p_split[[j]], value = FALSE)
                eq_split <- strsplit(p_split[[j]][equals], "[=]")
                eq_lhs <- eq_rhs <- c()
                if (length(equals) > 0) {
                    eq_lhs <- sapply(eq_split, "[[", 1)
                    eq_lhs <- gsub("\\s+", "", eq_lhs)
                    eq_rhs <- sapply(eq_split, "[[", 2)
                    eq_rhs <- gsub("\\s+", "", eq_rhs)
                    uniq_rhs <- unique(eq_rhs)
                    if (!(uniq_rhs[1] %in% 0:1)) stop(paste0("Invalid value assignment on data line ", i ,": ", data_split[[i]], ".\n")) 
                    p_split[[j]][equals] <- eq_lhs
                    z <- which(eq_rhs == 1)
                    o <- which(eq_rhs == 0)
                    zero <- c(zero, eq_lhs[eq_rhs == 0])
                    one <- c(one, eq_lhs[eq_rhs == 1])
                }
            }
        }
        p1_new <- p_split[[1]][which(!(p_split[[1]] %in% vars))]
        p2_new <- p_split[[2]][which(!(p_split[[2]] %in% vars))]
        new_vars <- unique(c(p1_new, p2_new))
        if ( length(new_vars) > 0 ) {
            n <- n + length(new_vars)
            vars <- c(vars, new_vars)
            nums <- 1:n
            names(vars) <- nums
            names(nums) <- vars
        }
        p_process[[i]] <- list(nums[p_split[[1]]], nums[p_split[[2]]], nums[zero], nums[one], data_split[[i]])
    }

    for (i in 1:length(p_process)) {
        p <- p_process[[i]]
        p_list[[i]] <- c(to_dec(p[[1]], n), to_dec(p[[2]], n), to_dec(p[[3]], n), to_dec(p[[4]], n))
        err <- FALSE
        msg <- ""
        if (bitwAnd(p_list[[i]][1], p_list[[i]][2]) > 0) {
            msg <- "same variable on the left-hand and right-hand side.\n"
            err <- TRUE
        }
        if (err) stop(paste0(c("Invalid input distribution on data line ", i, ": ", p[[4]], ", ", msg)))
    }

    q_vec <- c(to_dec(q_process[[1]], n), to_dec(q_process[[2]], n), to_dec(q_process[[3]], n), to_dec(q_process[[4]], n))
    err <- FALSE
    msg <- ""
    if (bitwAnd(q_vec[1], q_vec[2]) > 0) {
        msg <- "same variable on the left and right-hand side.\n"
        err <- TRUE
    }
    if (err) {
        stop(paste0(c("Invalid query: ", msg)))
    }

    res <- initialize_csisearch(
        as.numeric(nums[dir_lhs]),
        as.numeric(nums[dir_rhs]),
        as.character(vars),
        p_list,
        q_vec,
        label_map,
        local_csi,
        to_dec(nums[con_vars], n),
        to_dec(nums[intv_vars], n),
        n,
        control$time_limit,
        control$rules,
        control$benchmark,
        control$draw_derivation,
        control$draw_all,
        control$formula,
        control$heuristic,
        control$cache,
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
