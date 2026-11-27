# Helpers for checking formulas numerically in random models and for
# generating random identifiability problems (see test-formulas.R).
#
# A model is a random discrete model compatible with the graph: binary
# variables, a binary latent common cause for each bidirected edge, and proxy
# variables x* = x when r_x = 1 and missing (coded as 2) when r_x = 0. In an
# LDAG, the edge from a parent vanishes in the contexts of its label.
# A formula is read with the conventional scope of summations, i.e., a
# summation applies to every term that follows it in the same product, up to
# a minus sign or a plus sign of a sum that is written out in the variants of
# a formula. Proxy variables may be given as x^{\ast} or as x*. A
# label that is not bound by a summation is free. A proxy variable that is
# not bound by a summation over the proxy variable itself stands for its true
# variable with the same primes when the true variable is bound or a variable
# of the target. Otherwise, it is a proxy variable that is removed from a
# distribution, which is missing when its mechanism has the value 0 in the
# same distribution and observed otherwise. A formula is correct if it has the
# value of the target for every combination of values of the variables of the
# target and the other free variables of the formula.

# The code of a missing value of a proxy variable
na_code <- 2L

# The name of the variable of a label, without primes and superscripts
label_name <- function(label) {
  gsub("'|\\^\\{\\([0-9]+\\)\\}", "", label)
}

is_proxy_label <- function(label) {
  endsWith(label_name(label), "*")
}

# The label of the true variable of a proxy variable, with the same primes
true_label <- function(label) {
  sub("*", "", label, fixed = TRUE)
}

# Rewrite the superscripts of the proxy variables of a formula, e.g.,
# x^{\ast}, x^{\ast\prime} and x^{\ast(4)}, as x*, x*' and x*^{(4)}
normalize_formula <- function(formula) {
  formula <- gsub("\\^\\{\\\\ast\\(([0-9]+)\\)\\}", "*^{(\\1)}", formula)
  repeat {
    prev <- formula
    formula <- gsub(
      "\\^\\{\\\\ast((\\\\prime)*)\\\\prime\\}",
      "^{\\\\ast\\1}'",
      formula
    )
    if (identical(prev, formula)) break
  }
  gsub("^{\\ast}", "*", formula, fixed = TRUE)
}

# The query with the values of a variant of a formula assigned to its
# variables, where the variant is named by the values, e.g.,
# "r_x = 0, r_y = 1" (see the variants of dosearch)
assigned_query <- function(query, variant) {
  if (is.null(variant) || !nzchar(variant)) return(query)
  for (a in strsplit(variant, ", ", fixed = TRUE)[[1L]]) {
    kv <- trimws(strsplit(a, "=", fixed = TRUE)[[1L]])
    query <- gsub(
      paste0("(?<=[(,|])", kv[1L], "(?=[,|)])"),
      paste0(kv[1L], " = ", kv[2L]),
      query,
      perl = TRUE
    )
  }
  query
}

# Split text at a separator that is not enclosed in parentheses or braces
split_test_text <- function(text, sep) {
  chars <- strsplit(text, "", fixed = TRUE)[[1L]]
  depth <- cumsum(chars %in% c("(", "{")) - cumsum(chars %in% c(")", "}"))
  cuts <- which(chars == sep & depth == 0L)
  starts <- c(1L, cuts + 1L)
  ends <- c(cuts - 1L, length(chars))
  substring(text, starts, ends)
}

# Parse the content of a distribution p(...) into its items, i.e., the labels,
# the values assigned to them (NA if none) and the sides they appear on
# ("a" for the left-hand side, "do" for actions and "b" for observations)
parse_items <- function(content) {
  content <- gsub(" ", "", content, fixed = TRUE)
  parts <- split_test_text(content, "|")
  out <- list(label = character(0L), value = integer(0L), side = character(0L))
  add <- function(out, text, side) {
    for (item in split_test_text(text, ",")) {
      if (!nzchar(item)) next
      kv <- strsplit(item, "=", fixed = TRUE)[[1L]]
      out$label <- c(out$label, kv[1L])
      out$value <- c(out$value, if (length(kv) > 1L) as.integer(kv[2L]) else NA)
      out$side <- c(out$side, side)
    }
    out
  }
  out <- add(out, parts[1L], "a")
  if (length(parts) > 1L) {
    for (item in split_test_text(parts[2L], ",")) {
      if (startsWith(item, "do(") && endsWith(item, ")")) {
        out <- add(out, substr(item, 4L, nchar(item) - 1L), "do")
      } else {
        out <- add(out, item, "b")
      }
    }
  }
  out
}

# Parse a distribution given as "p(...)"
parse_test_distribution <- function(text) {
  text <- trimws(text)
  parse_items(substr(text, 3L, nchar(text) - 1L))
}

# Parse a formula into a tree of nodes of types "term", "prod", "sum",
# "frac", "diff", "add" and "one"
parse_formula <- function(formula) {
  formula <- normalize_formula(formula)
  pos <- 1L
  len <- nchar(formula)
  at <- function(token) {
    substr(formula, pos, pos + nchar(token) - 1L) == token
  }
  expect <- function(token) {
    if (!at(token)) {
      stop("Expected '", token, "' at position ", pos, " of ", formula)
    }
    pos <<- pos + nchar(token)
  }
  # The content up to the closing bracket that matches the opening bracket
  # just before the current position
  group <- function(open, close) {
    depth <- 1L
    start <- pos
    while (depth > 0L) {
      if (pos > len) stop("Unbalanced brackets in ", formula)
      ch <- substr(formula, pos, pos)
      if (ch == open) depth <- depth + 1L
      if (ch == close) depth <- depth - 1L
      pos <<- pos + 1L
    }
    substr(formula, start, pos - 2L)
  }
  expr <- function() {
    node <- product()
    while (at(" - ") || at(" + ")) {
      type <- if (at(" - ")) "diff" else "add"
      pos <<- pos + 3L
      node <- list(type = type, a = node, b = product())
    }
    node
  }
  product <- function() {
    factors <- list()
    repeat {
      if (pos > len || at("}") || at(" - ") || at(" + ") || at("\\right)")) {
        break
      }
      if (at("\\left[")) stop("Case-by-case notation is not supported")
      if (at("\\sum_{")) {
        pos <<- pos + 6L
        vars <- split_test_text(group("{", "}"), ",")
        factors[[length(factors) + 1L]] <- list(
          type = "sum",
          vars = vars,
          body = product()
        )
        break
      }
      if (at("\\frac{")) {
        pos <<- pos + 6L
        num <- expr()
        expect("}{")
        den <- expr()
        expect("}")
        factors[[length(factors) + 1L]] <- list(
          type = "frac",
          num = num,
          den = den
        )
      } else if (at("\\left(")) {
        pos <<- pos + 6L
        factors[[length(factors) + 1L]] <- expr()
        expect("\\right)")
      } else if (at("p(")) {
        pos <<- pos + 2L
        text <- group("(", ")")
        factors[[length(factors) + 1L]] <- list(
          type = "term",
          text = text,
          items = parse_items(text)
        )
      } else if (at("1")) {
        pos <<- pos + 1L
        factors[[length(factors) + 1L]] <- list(type = "one")
      } else {
        stop("Unexpected input at position ", pos, " of ", formula)
      }
    }
    list(type = "prod", factors = factors)
  }
  tree <- expr()
  if (pos <= len) stop("Unexpected input at position ", pos, " of ", formula)
  tree
}

# All terms of a formula tree
formula_terms <- function(node) {
  switch(node$type,
    term = list(node),
    prod = unlist(lapply(node$factors, formula_terms), recursive = FALSE),
    sum = formula_terms(node$body),
    frac = c(formula_terms(node$num), formula_terms(node$den)),
    diff = ,
    add = c(formula_terms(node$a), formula_terms(node$b)),
    list()
  )
}

# Parse an edge list into a list of directed and bidirected edges and the
# labels of the directed edges of an LDAG, where a label is a list of
# contexts given as named vectors of values
parse_test_graph <- function(graph) {
  lines <- trimws(strsplit(graph, "\n", fixed = TRUE)[[1L]])
  lines <- lines[nzchar(lines)]
  parts <- strsplit(lines, ":", fixed = TRUE)
  edges <- trimws(vapply(parts, "[[", character(1L), 1L))
  edges <- regmatches(
    edges,
    regexec("^(\\S+)\\s*(<->|->|--)\\s*(\\S+)$", edges)
  )
  if (any(lengths(edges) != 4L)) stop("Unable to parse graph ", graph)
  edges <- do.call(rbind, c(list(matrix("", 0L, 4L)), edges))
  labels <- lapply(parts, function(p) {
    if (length(p) < 2L) return(list())
    lapply(strsplit(p[2L], ";", fixed = TRUE)[[1L]], function(context) {
      kv <- strsplit(trimws(strsplit(context, ",", fixed = TRUE)[[1L]]), "=")
      stats::setNames(
        as.integer(vapply(kv, function(x) trimws(x[2L]), character(1L))),
        vapply(kv, function(x) trimws(x[1L]), character(1L))
      )
    })
  })
  dir <- edges[, 3L] == "->"
  bidirected <- edges[!dir, c(2L, 4L), drop = FALSE]
  bidirected <- t(apply(bidirected, 1L, sort))
  list(
    directed = edges[dir, c(2L, 4L), drop = FALSE],
    labels = labels[dir],
    bidirected = unique(matrix(bidirected, ncol = 2L))
  )
}

# Parse missing data mechanisms "r_x : x, r_y : y" into c(x = "r_x", ...)
parse_test_md <- function(md) {
  if (is.null(md) || !nzchar(md)) return(character(0L))
  pairs <- strsplit(trimws(strsplit(md, ",", fixed = TRUE)[[1L]]), ":")
  pairs <- lapply(pairs, trimws)
  stats::setNames(
    vapply(pairs, "[[", character(1L), 1L),
    vapply(pairs, "[[", character(1L), 2L)
  )
}

# A random model compatible with the graph. The variables of extra are added
# as independent variables.
random_model <- function(graph, md = "", extra = character(0L)) {
  g <- parse_test_graph(graph)
  mech <- parse_test_md(md)
  vars <- unique(c(t(g$directed), t(g$bidirected), mech, names(mech), extra))
  vars <- vars[!endsWith(vars, "*")]
  pa <- lapply(stats::setNames(vars, vars), function(v) {
    unique(g$directed[g$directed[, 2L] == v, 1L])
  })
  ord <- character(0L)
  while (length(ord) < length(vars)) {
    ready <- vapply(vars, function(v) {
      !(v %in% ord) && all(pa[[v]] %in% ord)
    }, logical(1L))
    ord <- c(ord, vars[ready])
  }
  lat <- paste0(".U", seq_len(nrow(g$bidirected)))
  for (i in seq_len(nrow(g$bidirected))) {
    for (v in g$bidirected[i, ]) pa[[v]] <- c(pa[[v]], lat[i])
  }
  axes <- c(lat, ord)
  if (length(axes) > 16L) stop("Model too large to enumerate")
  states <- as.matrix(expand.grid(rep(list(0:1), length(axes))))
  colnames(states) <- axes
  base <- rep(1, nrow(states))
  for (u in lat) {
    p1 <- stats::runif(1L, 0.2, 0.8)
    base <- base * ifelse(states[, u] == 1L, p1, 1 - p1)
  }
  factors <- matrix(1, nrow(states), length(ord), dimnames = list(NULL, ord))
  for (v in ord) {
    k <- length(pa[[v]])
    p1 <- stats::runif(2L^k, 0.1, 0.9)
    idx <- 1L
    if (k > 0L) {
      cfg <- states[, pa[[v]], drop = FALSE]
      # The edge from a parent vanishes in the contexts of its label, so the
      # parent is set to 0 in them until no further edge vanishes
      edges <- which(g$directed[, 2L] == v & lengths(g$labels) > 0L)
      repeat {
        prev <- cfg
        for (e in edges) {
          holds <- Reduce(`|`, lapply(g$labels[[e]], function(context) {
            Reduce(`&`, lapply(names(context), function(c) {
              cfg[, c] == context[[c]]
            }))
          }))
          cfg[holds, g$directed[e, 1L]] <- 0L
        }
        if (identical(cfg, prev)) break
      }
      idx <- 1L + as.vector(cfg %*% 2L^(seq_len(k) - 1L))
    }
    factors[, v] <- ifelse(states[, v] == 1L, p1[idx], 1 - p1[idx])
  }
  values <- states[, ord, drop = FALSE]
  for (x in names(mech)) {
    proxy <- ifelse(states[, mech[[x]]] == 1L, states[, x], na_code)
    values <- cbind(values, proxy)
    colnames(values)[ncol(values)] <- paste0(x, "*")
  }
  model <- new.env()
  model$mech <- mech
  model$base <- base
  model$factors <- factors
  model$states <- states
  model$values <- values
  model$joints <- list()
  model
}

# The joint distribution of the model under the intervention do
model_joint <- function(model, do) {
  key <- paste(names(do), do, sep = "=", collapse = ",")
  w <- model$joints[[key]]
  if (is.null(w)) {
    keep <- setdiff(colnames(model$factors), names(do))
    w <- model$base * apply(model$factors[, keep, drop = FALSE], 1L, prod)
    for (v in names(do)) w <- w * (model$states[, v] == do[[v]])
    model$joints[[key]] <- w
  }
  w
}

# The probability of an event (a named vector of values) under do
model_prob <- function(model, event, do) {
  w <- model_joint(model, do)
  mask <- rep(TRUE, length(w))
  for (v in names(event)) {
    if (!(v %in% colnames(model$values))) stop("Unknown variable ", v)
    mask <- mask & model$values[, v] == event[[v]]
  }
  sum(w[mask])
}

# P(A = a | do(C = c), B = b), or NA if the condition has zero probability
model_term <- function(model, a, do, b) {
  den <- if (length(b) > 0L) model_prob(model, b, do) else 1
  if (den == 0) return(NA_real_)
  model_prob(model, c(a, b), do) / den
}

# The value of a formula tree in the model, where env holds the values of the
# bound labels and free the values of the free labels. A term whose condition
# has zero probability is given the value zero, as in a summation where its
# weight is zero, and so is a quotient whose denominator is zero.
formula_value <- function(node, model, env, free, memo) {
  memo$.work <- memo$.work + 1L
  if (memo$.work > memo$.limit) stop("Formula too complex to evaluate")
  switch(node$type,
    one = 1,
    term = term_value(node, model, env, free, memo),
    prod = {
      v <- 1
      for (f in node$factors) {
        v <- v * formula_value(f, model, env, free, memo)
        if (v == 0) break
      }
      v
    },
    frac = {
      den <- formula_value(node$den, model, env, free, memo)
      if (den == 0) 0 else formula_value(node$num, model, env, free, memo) / den
    },
    diff = formula_value(node$a, model, env, free, memo) -
      formula_value(node$b, model, env, free, memo),
    add = formula_value(node$a, model, env, free, memo) +
      formula_value(node$b, model, env, free, memo),
    sum = {
      doms <- lapply(node$vars, function(l) {
        if (is_proxy_label(l)) c(0L, 1L, na_code) else 0:1
      })
      grid <- as.matrix(expand.grid(doms))
      total <- 0
      for (i in seq_len(nrow(grid))) {
        env_i <- env
        env_i[node$vars] <- grid[i, ]
        total <- total + formula_value(node$body, model, env_i, free, memo)
      }
      total
    }
  )
}

# The value of a label of a term (see the description at the top), where
# target holds the labels of the target
label_value <- function(label, value, items, model, env, free, target) {
  if (!is.na(value)) return(value)
  if (label %in% names(env)) return(env[[label]])
  if (label %in% names(free)) return(free[[label]])
  if (is_proxy_label(label)) {
    x <- true_label(label)
    if (!(x %in% names(env)) && !(x %in% target)) {
      r <- model$mech[[sub("*", "", label_name(label), fixed = TRUE)]]
      j <- which(label_name(items$label) == r)
      if (length(j) == 1L) {
        rv <- label_value(
          items$label[j], items$value[j], items, model, env, free, target
        )
        if (rv == 0L) return(na_code)
      }
    }
    return(label_value(x, NA, items, model, env, free, target))
  }
  stop("Unresolved label ", label)
}

term_value <- function(node, model, env, free, memo) {
  items <- node$items
  vals <- vapply(seq_along(items$label), function(j) {
    label_value(
      items$label[j], items$value[j], items, model, env, free, memo$.target
    )
  }, integer(1L))
  names(vals) <- label_name(items$label)
  key <- paste(node$text, paste(vals, collapse = ","))
  v <- memo[[key]]
  if (is.null(v)) {
    side <- items$side
    v <- model_term(
      model,
      vals[side == "a"],
      vals[side == "do"],
      vals[side == "b"]
    )
    if (is.na(v)) v <- 0
    memo[[key]] <- v
  }
  v
}

# The labels that are free in a formula tree but not variables of the target
free_labels <- function(node, bound, target) {
  switch(node$type,
    term = {
      out <- character(0L)
      items <- node$items
      for (j in seq_along(items$label)) {
        l <- items$label[j]
        if (!is.na(items$value[j]) || l %in% c(bound, target)) next
        if (is_proxy_label(l)) {
          l <- true_label(l)
          if (l %in% c(bound, target)) next
        }
        out <- c(out, l)
      }
      out
    },
    prod = unique(unlist(lapply(node$factors, free_labels, bound, target))),
    sum = free_labels(node$body, c(bound, node$vars), target),
    frac = unique(c(
      free_labels(node$num, bound, target),
      free_labels(node$den, bound, target)
    )),
    diff = ,
    add = unique(c(
      free_labels(node$a, bound, target),
      free_labels(node$b, bound, target)
    )),
    character(0L)
  )
}

# Compare a formula with the target in a random model for every combination
# of values of the free variables, or for max_combinations random
# combinations. Returns NULL if the formula is correct and a description of
# the first difference otherwise. An error is raised if the evaluation of the
# formula takes more than limit steps.
formula_difference <- function(formula, query, graph, md = "",
                               max_combinations = 16L, limit = 50000L) {
  tree <- parse_formula(formula)
  target <- parse_test_distribution(query)
  tnames <- target$label[is.na(target$value)]
  extra <- unique(c(
    label_name(target$label),
    label_name(unlist(lapply(formula_terms(tree), function(t) t$items$label)))
  ))
  extra <- unique(true_label(extra))
  model <- random_model(graph, md, extra)
  flabels <- free_labels(tree, character(0L), target$label)
  labels <- c(tnames, flabels)
  doms <- lapply(labels, function(l) {
    if (is_proxy_label(l)) c(0L, 1L, na_code) else 0:1
  })
  grid <- as.matrix(expand.grid(doms))
  if (length(labels) == 0L) grid <- matrix(0L, 1L, 0L)
  if (nrow(grid) > max_combinations) {
    grid <- grid[sample.int(nrow(grid), max_combinations), , drop = FALSE]
  }
  memo <- new.env()
  memo$.work <- 0L
  memo$.limit <- limit
  memo$.target <- target$label
  fixed <- stats::setNames(target$value, target$label)[!is.na(target$value)]
  for (i in seq_len(nrow(grid))) {
    free <- c(stats::setNames(as.vector(grid[i, ]), labels), fixed)
    tv <- target$value
    tv[is.na(tv)] <- free[target$label[is.na(tv)]]
    names(tv) <- target$label
    side <- target$side
    truth <- model_term(
      model,
      tv[side == "a"],
      tv[side == "do"],
      tv[side == "b"]
    )
    if (is.na(truth)) next
    value <- formula_value(tree, model, integer(0L), free, memo)
    if (abs(value - truth) > 1e-9) {
      return(paste0(
        "formula ", formula, " has the value ", value, " instead of ", truth,
        " at ", paste(names(free), free, sep = " = ", collapse = ", ")
      ))
    }
  }
  NULL
}

# Is the term a marginal or conditional distribution of the input
# distribution, possibly evaluated at values assigned to the variables
# assignable (the missing data mechanisms or the context variables of an
# LDAG) and without the proxy variables that are missing because their
# mechanism has the value 0 in the term?
term_of_input <- function(term, input, mech, assignable) {
  tn <- label_name(term$label)
  inn <- label_name(input$label)
  tval <- stats::setNames(term$value, tn)
  ival <- stats::setNames(input$value, inn)
  if (!setequal(tn[term$side == "do"], inn[input$side == "do"])) return(FALSE)
  if (!all(tn[term$side == "a"] %in% inn[input$side == "a"])) return(FALSE)
  if (!all(tn %in% inn)) return(FALSE)
  missing <- character(0L)
  for (v in inn[endsWith(inn, "*")]) {
    r <- mech[[sub("*", "", v, fixed = TRUE)]]
    if (!is.null(r) && r %in% tn && identical(tval[[r]], 0L)) {
      missing <- c(missing, v)
    }
  }
  kept <- setdiff(inn[input$side == "b"], missing)
  if (!all(kept %in% tn[term$side == "b"])) return(FALSE)
  for (v in tn) {
    if (!is.na(ival[[v]])) {
      if (!identical(tval[[v]], ival[[v]])) return(FALSE)
    } else if (!is.na(tval[[v]]) && !(v %in% assignable)) {
      return(FALSE)
    }
  }
  TRUE
}

# The input distributions of which the term is a marginal or conditional
# distribution (see term_of_input)
term_inputs <- function(term, inputs, mech, ldag) {
  assignable <- if (ldag) label_name(term$label) else mech
  which(vapply(inputs, function(q) {
    term_of_input(term, q, mech, assignable)
  }, logical(1L)))
}

parse_inputs <- function(data) {
  inputs <- trimws(strsplit(data, "\n", fixed = TRUE)[[1L]])
  lapply(inputs[nzchar(inputs)], parse_test_distribution)
}

# The terms of a formula that are not a marginal or conditional distribution
# of any input distribution. In LDAGs, any variable can be a context variable.
foreign_terms <- function(formula, data, md = "", ldag = FALSE) {
  inputs <- parse_inputs(data)
  mech <- parse_test_md(md)
  terms <- formula_terms(parse_formula(formula))
  ok <- vapply(terms, function(t) {
    length(term_inputs(t$items, inputs, mech, ldag)) > 0L
  }, logical(1L))
  vapply(terms[!ok], function(t) t$text, character(1L))
}

# Is the quotient of the distributions num and den a marginal or conditional
# distribution of num?
quotient_of_one <- function(num, den) {
  nn <- label_name(num$label)
  dn <- label_name(den$label)
  num_a <- nn[num$side == "a"]
  den_a <- dn[den$side == "a"]
  if (!setequal(nn[num$side == "do"], dn[den$side == "do"])) return(FALSE)
  if (!all(den_a %in% num_a) || setequal(den_a, num_a)) return(FALSE)
  num_b <- nn[num$side == "b"]
  den_b <- dn[den$side == "b"]
  setequal(den_b, num_b) || setequal(den_b, c(num_b, setdiff(num_a, den_a)))
}

# Summations of a single distribution, explicit conditional distributions of
# a single distribution and quotients of two distributions of the same input
# distribution that are a marginal or conditional distribution of it, which
# should be given as marginal and conditional distributions instead. A
# quotient is only considered when both distributions are distributions of
# exactly one input distribution of data.
redundant_operations <- function(formula, data = NULL, md = "", ldag = FALSE) {
  single <- function(node) {
    node$type == "prod" && length(node$factors) == 1L &&
      node$factors[[1L]]$type == "term"
  }
  inputs <- if (is.null(data)) list() else parse_inputs(data)
  mech <- parse_test_md(md)
  out <- character(0L)
  walk <- function(node) {
    if (node$type == "sum") {
      if (single(node$body)) {
        out <<- c(out, "summation of a single distribution")
      }
      walk(node$body)
    } else if (node$type == "prod") {
      for (f in node$factors) walk(f)
    } else if (node$type == "frac") {
      den <- node$den
      if (single(node$num) && den$type == "prod" && length(den$factors) == 1L &&
          den$factors[[1L]]$type == "sum" && single(den$factors[[1L]]$body)) {
        out <<- c(out, "quotient of a single distribution and its marginal")
      } else {
        walk(node$den)
      }
      if (length(inputs) > 0L && single(node$num) && single(den)) {
        num_t <- node$num$factors[[1L]]$items
        den_t <- den$factors[[1L]]$items
        qn <- term_inputs(num_t, inputs, mech, ldag)
        qd <- term_inputs(den_t, inputs, mech, ldag)
        if (length(qn) == 1L && identical(qn, qd) &&
            quotient_of_one(num_t, den_t)) {
          out <<- c(out, "quotient of two distributions of the same input")
        }
      }
      walk(node$num)
    } else if (node$type %in% c("diff", "add")) {
      walk(node$a)
      walk(node$b)
    }
  }
  walk(parse_formula(formula))
  out
}

# The rule labels of the steps of a derivation (draw_derivation = TRUE)
derivation_rules <- function(derivation) {
  lines <- strsplit(derivation, "\n", fixed = TRUE)[[1L]]
  m <- regmatches(lines, regexec("-> n[0-9]+\\[label=\"(.*)\"\\]$", lines))
  vapply(m[lengths(m) == 2L], "[[", character(1L), 2L)
}

# A random integer between lo and hi
rand_int <- function(lo, hi) {
  if (lo >= hi) lo else sample(lo:hi, 1L)
}

# Random subsets
pick <- function(x, size) {
  if (size <= 0L || length(x) == 0L) return(character(0L))
  x[sample.int(length(x), min(size, length(x)))]
}

# A distribution p(a|do(do),b) with the values of the named vector values
distribution_text <- function(a, do = character(0L), b = character(0L),
                              values = integer(0L)) {
  lab <- function(v) {
    ifelse(v %in% names(values), paste0(v, " = ", values[v]), v)
  }
  rhs <- character(0L)
  if (length(do) > 0L) rhs <- paste0("do(", paste(lab(do), collapse = ","), ")")
  rhs <- c(rhs, lab(b))
  paste0(
    "p(", paste(lab(a), collapse = ","),
    if (length(rhs) > 0L) "|", paste(rhs, collapse = ","), ")"
  )
}

# A random graph with directed edges in the order of vars and bidirected
# edges between vars
random_edges <- function(vars, p_dir, p_bi) {
  edges <- character(0L)
  n <- length(vars)
  for (j in seq_len(n)[-1L]) {
    for (i in seq_len(j - 1L)) {
      if (stats::runif(1L) < p_dir) {
        edges <- c(edges, paste(vars[i], "->", vars[j]))
      }
      if (stats::runif(1L) < p_bi) {
        edges <- c(edges, paste(vars[i], "<->", vars[j]))
      }
    }
  }
  edges
}

# A random problem for a DAG with n vertices and no missing data. The inputs
# are joint, conditional and experimental distributions.
random_dag_problem <- function(n) {
  vars <- sample(letters[1:6], n)
  edges <- random_edges(vars, 0.4, 0.15)
  if (length(edges) == 0L) edges <- paste(vars[1L], "->", vars[2L])
  inputs <- character(0L)
  n_inputs <- sample(1:3, 1L, prob = c(0.3, 0.45, 0.25))
  for (k in seq_len(n_inputs)) {
    type <- sample(
      c("joint", "conditional", "experiment"),
      1L,
      prob = c(0.4, 0.3, 0.3)
    )
    if (type == "joint") {
      a <- pick(vars, rand_int(2L, min(n, 4L)))
      inputs <- c(inputs, distribution_text(a))
    } else if (type == "conditional") {
      a <- pick(vars, rand_int(1L, n - 1L))
      b <- pick(setdiff(vars, a), rand_int(1L, 2L))
      inputs <- c(inputs, distribution_text(a, b = b))
    } else {
      do <- pick(vars, rand_int(1L, 2L))
      rest <- setdiff(vars, do)
      a <- pick(rest, rand_int(1L, length(rest)))
      b <- pick(setdiff(rest, a), rand_int(0L, 1L))
      inputs <- c(inputs, distribution_text(a, do = do, b = b))
    }
  }
  type <- sample(
    c("marginal", "conditional", "effect", "conditional effect"),
    1L,
    prob = c(0.2, 0.2, 0.35, 0.25)
  )
  y <- pick(vars, rand_int(1L, 2L))
  rest <- setdiff(vars, y)
  x <- pick(rest, 1L)
  query <- switch(type,
    marginal = distribution_text(y),
    conditional = distribution_text(y, b = pick(rest, rand_int(1L, 2L))),
    effect = distribution_text(y, do = x),
    distribution_text(y, do = x, b = pick(setdiff(rest, x), 1L))
  )
  list(
    data = paste(unique(inputs), collapse = "\n"),
    query = query,
    graph = paste(edges, collapse = "\n"),
    md = ""
  )
}

# A random missing data problem with k partially observed variables and m
# fully observed variables. The inputs are distributions of the proxy
# variables, the mechanisms and the fully observed variables, where the
# mechanisms can be assigned the values 0 and 1, distributions of the true
# variables from other sources, and experiments on the fully observed
# variables.
random_md_problem <- function(k, m) {
  po <- c("x", "y", "z")[seq_len(k)]
  fo <- c("w", "v", "u")[seq_len(m)]
  mech <- stats::setNames(paste0("r_", po), po)
  proxy <- stats::setNames(paste0(po, "*"), po)
  vars <- sample(c(po, fo))
  edges <- random_edges(vars, 0.4, 0.1)
  r_order <- sample(mech)
  for (j in seq_along(r_order)) {
    pa <- c(
      pick(vars, stats::rbinom(1L, length(vars), 0.3)),
      pick(r_order[seq_len(j - 1L)], stats::rbinom(1L, j - 1L, 0.3))
    )
    if (length(pa) > 0L) edges <- c(edges, paste(pa, "->", r_order[j]))
  }
  if (length(edges) == 0L) edges <- paste(vars[1L], "->", vars[2L])
  observed <- function(x) if (x %in% po) c(proxy[[x]], mech[[x]]) else x
  values <- function(rs) {
    v <- sample(
      c(NA, 1L, 0L),
      length(rs),
      replace = TRUE,
      prob = c(0.45, 0.4, 0.15)
    )
    stats::setNames(v[!is.na(v)], rs[!is.na(v)])
  }
  inputs <- character(0L)
  n_inputs <- sample(1:3, 1L, prob = c(0.3, 0.45, 0.25))
  for (i in seq_len(n_inputs)) {
    type <- sample(
      c("observed", "conditional", "external", "experiment"),
      1L,
      prob = c(0.35, 0.3, 0.25, 0.1)
    )
    if (type == "observed") {
      s <- pick(vars, rand_int(1L, length(vars)))
      a <- unlist(lapply(s, observed))
      rs <- intersect(a, mech)
      inputs <- c(inputs, distribution_text(a, values = values(rs)))
    } else if (type == "conditional") {
      s <- pick(vars, rand_int(2L, length(vars)))
      sa <- s[seq_len(rand_int(1L, length(s) - 1L))]
      a <- unlist(lapply(sa, observed))
      b <- unlist(lapply(setdiff(s, sa), observed))
      if (stats::runif(1L) < 0.5) {
        # Mechanisms of the left-hand side are moved to the right-hand side
        moved <- intersect(a, mech)
        a <- setdiff(a, moved)
        b <- c(b, moved)
        if (length(a) == 0L) next
      }
      rs <- intersect(c(a, b), mech)
      inputs <- c(inputs, distribution_text(a, b = b, values = values(rs)))
    } else if (type == "external") {
      s <- pick(vars, rand_int(1L, 2L))
      b <- pick(setdiff(vars, s), rand_int(0L, 1L))
      inputs <- c(inputs, distribution_text(s, b = b))
    } else if (length(fo) > 0L) {
      do <- pick(fo, 1L)
      s <- pick(setdiff(vars, do), rand_int(1L, 2L))
      a <- unlist(lapply(s, observed))
      rs <- intersect(a, mech)
      inputs <- c(inputs, distribution_text(a, do = do, values = values(rs)))
    }
  }
  if (length(inputs) == 0L) {
    inputs <- distribution_text(unlist(lapply(vars, observed)))
  }
  type <- sample(
    c("marginal", "conditional", "effect", "mechanism"),
    1L,
    prob = c(0.35, 0.3, 0.25, 0.1)
  )
  y <- pick(vars, rand_int(1L, 2L))
  rest <- setdiff(vars, y)
  query <- switch(type,
    marginal = distribution_text(y),
    conditional = distribution_text(y, b = pick(rest, 1L)),
    effect = distribution_text(y, do = pick(rest, 1L)),
    {
      x <- pick(po, 1L)
      distribution_text(
        setdiff(c(y, proxy[[x]]), x),
        b = mech[[x]],
        values = stats::setNames(1L, mech[[x]])
      )
    }
  )
  list(
    data = paste(unique(inputs), collapse = "\n"),
    query = query,
    graph = paste(edges, collapse = "\n"),
    md = paste(mech, ":", po, collapse = ", ")
  )
}

# Solve a problem and check the formula. Returns NULL for non-identifiable
# problems and a list of the formula, the rules of the derivation and the
# problems found otherwise.
check_problem <- function(problem, control = list(), case_reasoning = FALSE) {
  control$draw_derivation <- TRUE
  control$warn <- FALSE
  out <- dosearch(
    problem$data,
    problem$query,
    problem$graph,
    missing_data = if (nzchar(problem$md)) problem$md else NULL,
    control = control,
    case_reasoning = case_reasoning
  )
  if (!out$identifiable) return(NULL)
  rules <- derivation_rules(out$derivation)
  issues <- character(0L)
  # A formula derived by case-by-case reasoning is checked by its variants
  formulas <- if (is.null(out$variants)) out$formula else out$variants
  ldag <- grepl(":", problem$graph, fixed = TRUE)
  for (i in seq_along(formulas)) {
    diff <- formula_difference(
      formulas[[i]],
      assigned_query(problem$query, names(formulas)[i]),
      problem$graph,
      problem$md
    )
    if (!is.null(diff)) issues <- c(issues, diff)
    foreign <- foreign_terms(formulas[[i]], problem$data, problem$md, ldag)
    if (length(foreign) > 0L) {
      issues <- c(issues, paste("not a distribution of an input:", foreign))
    }
    issues <- c(
      issues,
      redundant_operations(formulas[[i]], problem$data, problem$md, ldag)
    )
  }
  if (!is.null(out$variants) && !("CbC" %in% rules)) {
    issues <- c(issues, "variants without case-by-case reasoning")
  }
  single <- c("M", "C", "R1", "R2", "R3", "A", "EX")
  one <- "^p\\([^()]*(\\([^()]*\\)[^()]*)?\\)$"
  if (all(rules %in% single) && !grepl(one, normalize_formula(out$formula))) {
    issues <- c(
      issues,
      "a derivation by rules with a single parent is not a single distribution"
    )
  }
  if (length(issues) > 0L) {
    issues <- paste0(
      issues, "\n  data: ", gsub("\n", "; ", problem$data),
      "\n  query: ", problem$query,
      "\n  graph: ", gsub("\n", "; ", problem$graph),
      "\n  md: ", problem$md,
      "\n  control: ",
      paste(names(control), control, sep = " = ", collapse = ", ")
    )
  }
  list(
    formula = out$formula,
    variants = out$variants,
    rules = rules,
    issues = issues
  )
}

# Solve a problem and check that its formula is the expected one, that it is
# correct in a random model, that every distribution in it is a marginal or
# conditional distribution of an input distribution, that it contains no
# summation or quotient of a single distribution, and that the derivation
# uses the rules uses (labels of the derivation steps). If variants is given,
# the formula is derived by case-by-case reasoning, and the checks apply to
# its variants instead, which must be the expected ones.
expect_formula <- function(data, query, graph, expected, md = "",
                           rules = NULL, uses = character(0L),
                           control = list(), case_reasoning = FALSE,
                           variants = NULL) {
  control$draw_derivation <- TRUE
  control$warn <- FALSE
  if (!is.null(rules)) control$rules <- rules
  out <- dosearch(
    data,
    query,
    graph,
    missing_data = if (nzchar(md)) md else NULL,
    control = control,
    case_reasoning = case_reasoning
  )
  expect_true(out$identifiable, info = query)
  expect_identical(out$formula, expected)
  steps <- derivation_rules(out$derivation)
  for (u in uses) {
    expect_true(u %in% steps, info = paste(query, "is derived using", u))
  }
  ldag <- grepl(":", graph, fixed = TRUE)
  expect_identical(out$variants, variants)
  formulas <- if (is.null(variants)) out$formula else out$variants
  for (i in seq_along(formulas)) {
    expect_null(
      formula_difference(
        formulas[[i]],
        assigned_query(query, names(formulas)[i]),
        graph,
        md
      )
    )
    expect_identical(
      foreign_terms(formulas[[i]], data, md, ldag),
      character(0L)
    )
    expect_identical(
      redundant_operations(formulas[[i]], data, md, ldag),
      character(0L)
    )
  }
  invisible(out)
}

# Solve random problems until n of them are identifiable, and collect the
# problems found in their formulas (see check_problem), the rules used in
# their derivations and the formulas. problem and control are functions that
# return a random problem and a random control list.
check_random_problems <- function(n, problem, control,
                                  case_reasoning = FALSE) {
  issues <- character(0L)
  rules <- list()
  formulas <- character(0L)
  variants <- list()
  attempts <- 0L
  while (length(formulas) < n && attempts < 50L * n) {
    attempts <- attempts + 1L
    res <- tryCatch(
      check_problem(problem(), control(), case_reasoning),
      error = function(e) {
        if (!grepl("too complex|too large", conditionMessage(e))) stop(e)
        NULL
      }
    )
    if (is.null(res)) next
    issues <- c(issues, res$issues)
    rules[[length(rules) + 1L]] <- unique(res$rules)
    formulas <- c(formulas, res$formula)
    variants[length(variants) + 1L] <- list(res$variants)
  }
  list(issues = issues, rules = rules, formulas = formulas, variants = variants)
}

# The number of formulas whose derivation uses each rule
rule_counts <- function(result) {
  labels <- c("M", "C", "D", "P", "R1", "R2", "R3", "A", "EX")
  vapply(labels, function(l) {
    sum(vapply(result$rules, function(r) l %in% r, logical(1L)))
  }, integer(1L))
}
