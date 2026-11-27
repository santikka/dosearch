test_that("backdoor formula is identified", {
  data <- "p(x,y,z)"
  query <- "p(y|do(x))"
  graph <- "
    x -> y
    z -> x
    z -> y
  "
  expect_error(
    out <- dosearch(data, query, graph),
    NA
  )
  expect_true(out$identifiable)
  expect_identical(
    out$formula,
    "\\sum_{z}p(z)p(y|x,z)",
  )
})

test_that("frontdoor formula is identified", {
  data <- "p(x,y,z)"
  query <- "p(y|do(x))"
  graph <- "
    x -> z
    z -> y
    x <-> y
  "
  expect_error(
    out <- dosearch(data, query, graph),
    NA
  )
  expect_true(out$identifiable)
  expect_equal(
    out$formula,
    "\\sum_{z}p(z|x)\\sum_{x'}p(x')p(y|z,x')"
  )
})

test_that("bow-arc is non-identifiable", {
  data <- "p(x,y)"
  query <- "p(y|do(x))"
  graph <- "
    x -> y
    x <-> y
  "
  expect_error(
    out <- dosearch(data, query, graph, control = list(heuristic = TRUE)),
    NA
  )
  expect_false(out$identifiable)
})

test_that("all rules are needed", {
  data <- "
    p(w|do(x_2),y,x_1)
    p(y|do(x_2),z_1,z_2,x_1)
    p(x_1|do(x_2),w)
    p(z_2,x_2|do(x_1))
    p(z_1|do(x_1,y),x_2)
  "
  query <- "p(y,x_1|do(x_2),w)"
  graph <- "
    x_1 -> z_2
    x_1 -> z_1
    x_1 -> w
    z_1 -> w
    z_2 -> w
    x_2 -> w
    x_2 -> z_1
    x_2 -> z_2
    z_2 -> y
    z_1 -> y
  "
  expect_error(
    out <- dosearch(
      data,
      query,
      graph,
      control = list(heuristic = TRUE, draw_derivation = TRUE)
    ),
    NA
  )
  expect_true(out$identifiable)
  rules <- c(-2, 2, -3, 3, 4, 5, -6, 6)
  for (r in rules) {
    r_mis <- setdiff(rules, r)
    expect_false(
      dosearch(data, query, graph, control = list(rules = r_mis))$identifiable
    )
  }
})

test_that("transportability and selection bias are checked", {
  data <- "
    p(x,z,y|s)
    p(y,z|t,do(x))
  "
  query <- "p(y|do(x))"
  graph <- "
    x -> z
    z -> y
    x -> s
    t -> z
    x <-> y
  "
  out <- dosearch(
    data,
    query,
    graph,
    transportability = "t",
    selection_bias = "s",
    control = list(heuristic = TRUE, improve = FALSE)
  )
  expect_true(out$identifiable)
  expect_identical(
    out$formula,
    "\\sum_{z}p(z|x,s)p(y|do(x),z,t)"
  )
})

test_that("missing data mechanisms are checked", {
  # simple case-control design scenario
  data <- "p(x*,y*,r_x,r_y)"
  query <- "p(y|do(x))"
  graph <- "
    x -> y
    y -> r_y
    r_y -> r_x
  "
  md <- "r_x : x, r_y : y"
  out <- dosearch(data, query, graph, missing_data = md)
  expect_identical(
    out$identifiable,
    FALSE
  )
  data <- "
    p(x*,y*,r_x,r_y)
    p(y)
  "
  out <- dosearch(
    data,
    query,
    graph,
    missing_data = md,
    control = list(heuristic = TRUE, draw_derivation = TRUE)
  )
  expect_true(out$identifiable)
  out <- dosearch(data, query, graph, missing_data = md)
  expect_identical(
    out$formula,
    paste0(
      "\\frac{p(y)p(x^{\\ast}|r_x = 1,r_y = 1,y^{\\ast})}{\\sum_{y'}p(y')",
      "p(x^{\\ast}|r_x = 1,r_y = 1,y^{\\ast\\prime})}"
    )
  )
  out <- dosearch(
    data,
    query,
    graph,
    missing_data = md,
    control = list(heuristic = TRUE)
  )
})

test_that("trivial non-identifiability is checked", {
  out <- dosearch("p(x)", "p(y)", "x -> y")
  expect_false(out$identifiable)
  expect_identical(out$formula, "")
})

test_that("trivial identifiability is checked", {
  out <- dosearch("p(y)", "p(y)", "x -> y")
  expect_true(out$identifiable)
  expect_identical(out$formula, "p(y)")
  out <- dosearch("p(y)", "p(y)", "x -> y", control = list(heuristic = TRUE))
  expect_true(out$identifiable)
  expect_identical(out$formula, "p(y)")
})

test_that("trivial non-identifiability is reported in diagnostics", {
  out <- capture.output(
    res <- dosearch("p(x)", "p(y)", "x -> y", control = list(verbose = TRUE))
  )
  expect_false(res$identifiable)
  expect_match(
    out,
    "Target is not identifiable: y does not appear on the left-hand side",
    all = FALSE,
    fixed = TRUE
  )
  # The criteria are not evaluated without the search enhancements
  out <- capture.output(
    res <- dosearch(
      "p(x)",
      "p(y)",
      "x -> y",
      control = list(verbose = TRUE, improve = FALSE)
    )
  )
  expect_false(res$identifiable)
  expect_false(any(grepl("Target is not identifiable", out, fixed = TRUE)))
})

test_that("unmentioned variables of the target are detected", {
  # x does not appear in any input distribution, but it is a parent of y
  data <- "p(y,z)"
  query <- "p(y|do(x))"
  graph <- "
    x -> y
    z -> y
  "
  out <- capture.output(
    res <- dosearch(data, query, graph, control = list(verbose = TRUE))
  )
  expect_false(res$identifiable)
  expect_match(
    out,
    "x does not appear in any input distribution, but it is d-connected",
    all = FALSE,
    fixed = TRUE
  )
  expect_identical(length(out), 4L)
  expect_false(
    dosearch(data, query, graph, control = list(improve = FALSE))$identifiable
  )
  # The same applies to conditioning variables
  out <- capture.output(
    res <- dosearch("p(y)", "p(y|w)", "w -> y", control = list(verbose = TRUE))
  )
  expect_false(res$identifiable)
  expect_match(
    out,
    "w does not appear in any input distribution",
    all = FALSE,
    fixed = TRUE
  )
  # Unmentioned variables that are d-separated from the left-hand side
  # of the target do not prevent identification
  res <- dosearch("p(y)", "p(y|do(x))", "y -> x")
  expect_true(res$identifiable)
  expect_identical(res$formula, "p(y)")
  res <- dosearch("p(y,z)", "p(y|z,w)", "w -> z\nz -> y")
  expect_true(res$identifiable)
})

test_that("anchored outcomes are detected", {
  # z is a parent of y that does not appear on the left-hand side of any
  # input distribution, so it cannot be removed from p(y|x,z)
  data <- "
    p(y|x,z)
    p(x)
  "
  query <- "p(y|do(x))"
  graph <- "
    z -> y
    x -> y
  "
  out <- capture.output(
    res <- dosearch(data, query, graph, control = list(verbose = TRUE))
  )
  expect_false(res$identifiable)
  expect_match(
    out,
    "every input distribution with y on the left-hand side is anchored at y",
    all = FALSE,
    fixed = TRUE
  )
  expect_false(
    dosearch(data, query, graph, control = list(improve = FALSE))$identifiable
  )
  # A target that contains the anchor is not affected
  res <- dosearch(data, "p(y|do(x),z)", graph)
  expect_true(res$identifiable)
  expect_identical(res$formula, "p(y|z,x)")
  # An observed sibling is an anchor, but an intervened sibling is not
  graph <- "
    z <-> y
    x -> y
  "
  out <- capture.output(
    res <- dosearch(
      "p(y,x|z) \n p(x)",
      query,
      graph,
      control = list(verbose = TRUE)
    )
  )
  expect_false(res$identifiable)
  expect_match(out, "anchored at y", all = FALSE, fixed = TRUE)
  res <- dosearch("p(y,x|do(z)) \n p(x)", query, graph)
  expect_true(res$identifiable)
  # An input distribution that is not anchored prevents the verdict
  res <- dosearch("p(y,x|z) \n p(y|x)", query, graph)
  expect_true(res$identifiable)
  expect_identical(res$formula, "p(y|x)")
})

test_that("selection bias and transportability nodes are anchors", {
  # Selection depends directly on the outcome
  graph <- "
    x -> y
    y -> s
  "
  out <- capture.output(
    res <- dosearch(
      "p(x,y|s)",
      "p(y|do(x))",
      graph,
      selection_bias = "s",
      control = list(verbose = TRUE)
    )
  )
  expect_false(res$identifiable)
  expect_match(out, "anchored at y", all = FALSE, fixed = TRUE)
  # An unbiased distribution of the outcome makes the effect identifiable
  res <- dosearch(
    "p(x,y|s) \n p(y)",
    "p(y|do(x))",
    graph,
    selection_bias = "s"
  )
  expect_true(res$identifiable)
  # The mechanism of the outcome differs between the domains, and the
  # intervened parent x of the outcome is not an anchor of the target
  graph <- "
    x -> y
    t -> y
  "
  out <- capture.output(
    res <- dosearch(
      "p(y|do(x),t)",
      "p(y|do(x))",
      graph,
      transportability = "t",
      control = list(verbose = TRUE)
    )
  )
  expect_false(res$identifiable)
  expect_match(out, "anchored at y", all = FALSE, fixed = TRUE)
  # Data from the target domain makes the effect identifiable
  res <- dosearch(
    "p(y|do(x),t) \n p(x,y)",
    "p(y|do(x))",
    graph,
    transportability = "t"
  )
  expect_true(res$identifiable)
})

test_that("new criteria are not applied to missing data problems", {
  out <- capture.output(
    res <- dosearch(
      "p(y|z)",
      "p(y)",
      "z -> y\nw -> r_w",
      missing_data = "r_w : w",
      control = list(verbose = TRUE, warn = FALSE)
    )
  )
  expect_false(res$identifiable)
  expect_false(any(grepl("Target is not identifiable", out, fixed = TRUE)))
  expect_match(out, "Derived", all = FALSE, fixed = TRUE)
})

test_that("early non-identifiability verdicts agree with the search", {
  set.seed(123)
  vars <- c("w", "x", "y", "z", "v")
  n <- length(vars)
  # Each variable is on the left-hand side (0), intervened on (1),
  # conditioned on (2) or absent (3)
  random_distribution <- function() {
    roles <- sample(0:3, n, replace = TRUE)
    while (all(roles != 0L)) {
      roles <- sample(0:3, n, replace = TRUE)
    }
    rhs <- vars[roles == 2L]
    if (any(roles == 1L)) {
      actions <- paste(vars[roles == 1L], collapse = ",")
      rhs <- c(paste0("do(", actions, ")"), rhs)
    }
    paste0(
      "p(",
      paste(vars[roles == 0L], collapse = ","),
      if (length(rhs) > 0L) "|",
      paste(rhs, collapse = ","),
      ")"
    )
  }
  for (i in seq_len(100L)) {
    ord <- sample(vars)
    edges <- paste(ord[1L], "->", ord[2L])
    for (j in seq.int(2L, n)) {
      for (k in seq_len(j - 1L)) {
        if (runif(1L) < 0.5) edges <- c(edges, paste(ord[k], "->", ord[j]))
        if (runif(1L) < 0.25) edges <- c(edges, paste(ord[k], "<->", ord[j]))
      }
    }
    graph <- paste(unique(edges), collapse = "\n")
    data <- paste(
      unique(replicate(sample(1:4, 1L), random_distribution())),
      collapse = "\n"
    )
    query <- random_distribution()
    co <- list(warn = FALSE)
    early <- dosearch(data, query, graph, control = co)
    co$improve <- FALSE
    full <- dosearch(data, query, graph, control = co)
    expect_identical(early$identifiable, full$identifiable)
  }
})

test_that("verbose search works", {
  data <- "p(x,y,z)"
  query <- "p(y|do(x))"
  graph <- "
    x -> y
    z -> x
    z -> y
  "
  out <- capture.output(
    dosearch(data, query, graph, control = list(verbose = TRUE))
  )
  out_len <- length(out)
  expect_match(out[1L], "Setting target")
  expect_match(out[2L], "Adding known distribution")
  expect_match(out[3L], "Initializing search")
  for (i in seq.int(4L, out_len - 3L)) {
    expect_match(out[i], "Derived")
  }
  expect_match(out[out_len - 2L], "Target found")
  expect_match(out[out_len - 1L], "Index")
})

test_that("time limit works", {
  data <- "
    p(x*,y*,z*,r_x,r_y,r_z)
    p(y)
  "
  query <- "p(y|do(x))"
  graph <- "
    x -> z
    z -> y
    y -> r_y
    x <-> y
    r_y -> r_x
    r_y -> r_z
    r_y <-> r_x
    r_y <-> r_z
    r_z <-> r_x
  "
  md <- "r_x : x, r_y : y, r_z : z"
  out <- dosearch(
    data,
    query,
    graph,
    missing_data = md,
    control = list(
      heuristic = TRUE,
      improve = FALSE,
      time_limit = 1 / 3600000,
      benchmark = TRUE,
      benchmark_rules = TRUE
    )
  )
  expect_true(out$time > 1.0)
  out <- dosearch(
    data,
    query,
    graph,
    missing_data = md,
    control = list(
      heuristic = TRUE,
      improve = FALSE,
      time_limit = 1 / 3600000,
      benchmark = TRUE
    )
  )
  expect_true(out$time > 1.0)
})

test_that("missing response indicators warns", {
  expect_warning(
    dosearch("p(x*)", "p(x)", "x -> y\nx -> r_x", missing_data = "r_x : x"),
    paste0(
      "There are response indicators that are not present ",
      "in any input distribution"
    )
  )
})

test_that("both lower and upper case warns", {
  expect_warning(
    dosearch("p(x,X)", "p(X)", "x -> X"),
    "Both lower case and upper case inputs detected."
  )
})

test_that("no warnings are given if control$warn = FALSE", {
  co <- list(warn = FALSE)
  expect_silent(
    dosearch("p(x*)", "p(x)", "x -> y", missing_data = "r_x : x", control = co)
  )
  expect_silent(
    dosearch("p(x,X)", "p(X)", "x -> X", control = co)
  )
})

test_that("extra variables do not influence identifiability", {
  query <- "p(y|do(x))"
  graph <- "x -> z \n z -> y \n x <-> y"
  expect_warning(
    out <- dosearch("p(x,y,z,w)", query, graph),
    paste0(
      "The input distributions contain variables that are not present ",
      "in the graph, in any other input distribution ",
      "or in the target distribution: w"
    )
  )
  expect_identical(
    out$identifiable,
    dosearch("p(x,y,z)", query, graph)$identifiable
  )
  # Variables outside the graph that appear in several inputs
  # or in the target distribution do not warn
  expect_silent(dosearch("p(x,y,z,w) \n p(w)", query, graph))
  expect_silent(dosearch("p(x,y,z,w)", "p(y|do(x),w)", graph))
})

test_that("rule 1 works (redundant rule)", {
  graph <- "x -> z \n y -> z"
  out <- dosearch(
    "p(y|x)",
    "p(y)",
    graph,
    control = list(rules = c(-1, 1), draw_derivation = TRUE)
  )
  expect_true(out$identifiable)
  expect_identical(out$formula, "p(y|x)")
  out <- dosearch(
    "p(y)",
    "p(y|x)",
    graph,
    control = list(rules = c(-1, 1), draw_derivation = TRUE)
  )
  expect_true(out$identifiable)
  expect_identical(out$formula, "p(y)")
  data <- "p(x*,y*,r_x,r_y)"
  query <- "p(y|do(x))"
  data <- "
    p(x*,y*,r_x,r_y)
    p(y)
  "
  md <- "r_x : x, r_y : y"
  out <- dosearch(
    data,
    query,
    graph,
    missing_data = md,
    control = list(
      rules = c(-3, -2, -1, 1, 2, 3, 4, 5, -6, 6, -7, 7, -8, 8, 9, 10),
      heuristic = TRUE,
      warn = FALSE
    )
  )
  expect_true(out$identifiable)
  out <- dosearch(
    data,
    query,
    graph,
    missing_data = md,
    control = list(warn = FALSE)
  )
})

test_that("division rules work", {
  graph <- "
    x -> y
    y -> r_x
    r_x -> r_y
  "
  md <- "r_x : x, r_y : y"
  out <- dosearch(
    "p(r_x=1,r_y=1) \n p(r_x=1)",
    "p(r_y=1|r_x=1)",
    graph,
    missing_data = md,
    control = list(draw_derivation = TRUE, rules = 7)
  )
  expect_true(out$identifiable)
  expect_identical(out$formula, "\\frac{p(r_x = 1,r_y = 1)}{p(r_x = 1)}")
  out <- dosearch(
    "p(r_x=1,r_y=1) \n p(r_x=1|r_y=1)",
    "p(r_y=1)",
    graph,
    missing_data = md,
    control = list(draw_derivation = TRUE, rules = -7)
  )
  expect_true(out$identifiable)
  expect_identical(out$formula, paste0(
    "\\frac{p(r_x = 1,r_y = 1)}{p(r_x = 1|r_y = 1)}"
  ))
  out <- dosearch(
    "p(r_x=1) \n p(r_x=1,r_y=1)",
    "p(r_y = 1|r_x = 1)",
    graph,
    missing_data = md,
    control = list(draw_derivation = TRUE, rules = 8)
  )
  expect_true(out$identifiable)
  expect_identical(out$formula, "\\frac{p(r_x = 1,r_y = 1)}{p(r_x = 1)}")
  out <- dosearch(
    "p(r_x=1|r_y=1) \n p(r_x=1,r_y=1)",
    "p(r_y = 1)",
    graph,
    missing_data = md,
    control = list(draw_derivation = TRUE, rules = -8)
  )
  expect_true(out$identifiable)
  expect_identical(out$formula, paste0(
    "\\frac{p(r_x = 1,r_y = 1)}{p(r_x = 1|r_y = 1)}"
  ))
  out <- dosearch(
    "p(r_x=1|z) \n p(r_x=1,r_y=1)",
    "p(r_y = 1|r_x = 1)",
    graph,
    missing_data = md,
    control = list(draw_derivation = TRUE, rules = c(-1, 8), warn = FALSE)
  )
  expect_true(out$identifiable)
  expect_identical(out$formula, "\\frac{p(r_x = 1,r_y = 1)}{p(r_x = 1|z)}")
})

test_that("variable names containing 'do' are parsed correctly", {
  data <- "p(dose,random,endo)"
  query <- "p(endo|do(dose))"
  graph <- "
    dose -> endo
    random -> dose
    random -> endo
  "
  expect_error(
    out <- dosearch(data, query, graph),
    NA
  )
  expect_true(out$identifiable)
  expect_identical(
    out$formula,
    "\\sum_{random}p(random)p(endo|dose,random)"
  )
  expect_error(
    out <- dosearch("p(y,do)", "p(y|do)", "do -> y"),
    NA
  )
  expect_true(out$identifiable)
  expect_identical(out$formula, "p(y|do)")
})

test_that("missing data heuristic agrees with the default search", {
  graph <- "
    x -> y
    y -> r_y
    r_y -> r_x
  "
  md <- "r_x : x, r_y : y"
  cases <- list(
    list(data = "p(x*,y*,r_x,r_y)\np(y)", query = "p(y|do(x))"),
    list(data = "p(x*,y*,r_x,r_y)\np(y)", query = "p(x,y)"),
    list(data = "p(x*,y*,r_x,r_y)", query = "p(y|do(x))")
  )
  for (case in cases) {
    out <- dosearch(case$data, case$query, graph, missing_data = md)
    out_h <- dosearch(
      case$data,
      case$query,
      graph,
      missing_data = md,
      control = list(heuristic = TRUE)
    )
    expect_identical(out_h$identifiable, out$identifiable)
  }
  # The formula obtained with the missing data score
  out_h <- dosearch(
    "p(x*,y*,r_x,r_y)\np(y)",
    "p(x,y)",
    graph,
    missing_data = md,
    control = list(heuristic = TRUE)
  )
  expect_identical(
    out_h$formula,
    "p(y)p(x^{\\ast}|r_x = 1,r_y = 1,y^{\\ast})"
  )
  data <- "p(x*,y*,z*,m_x,m_y,m_z)"
  query <- "p(x,y,z)"
  graph <- "
    z -> x
    x -> y
    x -> m_z
    y -> m_z
    y -> m_x
    z <-> y
  "
  md <- "m_x : x, m_y : y, m_z : z"
  out <- dosearch(
    data,
    query,
    graph,
    missing_data = md,
    control = list(warn = FALSE)
  )
  out_h <- dosearch(
    data,
    query,
    graph,
    missing_data = md,
    control = list(heuristic = TRUE, warn = FALSE)
  )
  expect_identical(out_h$identifiable, out$identifiable)
})

test_that("target variables missing from the graph and the data warn", {
  expect_warning(
    dosearch("p(x,y)", "p(y|do(xx))", "x -> y"),
    paste0(
      "The target distribution contains variables that are not present ",
      "in the graph or in any input distribution: xx"
    )
  )
  expect_silent(
    dosearch(
      "p(x,y)",
      "p(y|do(xx))",
      "x -> y",
      control = list(warn = FALSE)
    )
  )
  expect_warning(
    dosearch("p(x,y,z)", "p(y|w)", "x -> y : z = 1\nz -> y"),
    "in the graph or in any input distribution: w"
  )
})

test_that("response indicators on the right-hand side do not warn", {
  expect_silent(
    dosearch(
      "p(x*|r_x=1)",
      "p(x)",
      "x -> y\nx -> r_x",
      missing_data = "r_x : x"
    )
  )
})

test_that("missing data mechanisms not present in the graph warn", {
  expect_warning(
    dosearch("p(x*,r_x)", "p(x)", "x -> y", missing_data = "r_x : x"),
    paste0(
      "There are missing data mechanisms or variables with missing data ",
      "that are not present in the graph: r_x"
    )
  )
  expect_warning(
    dosearch("p(x*,r_x)", "p(x)", "r_x -> y", missing_data = "r_x : x"),
    paste0(
      "There are missing data mechanisms or variables with missing data ",
      "that are not present in the graph: x"
    )
  )
  expect_silent(
    dosearch(
      "p(x*,r_x)",
      "p(x)",
      "x -> y",
      missing_data = "r_x : x",
      control = list(warn = FALSE)
    )
  )
})

test_that("empty transportability and selection bias nodes are ignored", {
  data <- "
    p(x,z,y|s)
    p(y,z|t,do(x))
  "
  query <- "p(y|do(x))"
  graph <- "
    x -> z
    z -> y
    x -> s
    t -> z
    x <-> y
  "
  out <- dosearch(
    data,
    query,
    graph,
    transportability = ", t",
    selection_bias = "s, ",
    control = list(heuristic = TRUE, improve = FALSE)
  )
  expect_true(out$identifiable)
})

test_that("graphs with more than 64 vertices are supported", {
  # Vertices beyond the first 64 are stored in additional words of a set
  chain <- paste0(
    "v", seq_len(69L), " -> v", seq_len(69L) + 1L,
    collapse = "\n"
  )
  out <- dosearch(
    "p(v69,v70)",
    "p(v70|do(v69))",
    chain,
    control = list(rules = c(5L, 2L))
  )
  expect_true(out$identifiable)
  expect_identical(out$formula, "p(v70|v69)")
  out <- dosearch(
    "p(v69,v70)",
    "p(v70|do(v69))",
    paste0(chain, "\nv69 <-> v70"),
    control = list(rules = c(5L, 2L))
  )
  expect_false(out$identifiable)
  out <- dosearch("p(v70|do(v1))", "p(v70|do(v1))", chain)
  expect_true(out$identifiable)
  out <- dosearch("p(v1)", "p(v70)", chain)
  expect_false(out$identifiable)
})

test_that("missing data mechanisms are supported beyond 64 vertices", {
  # x22, r22 and x22* are vertices 64, 65 and 66
  k <- 22L
  graph <- paste0(
    "x", seq_len(k), " -> y\n", "r", seq_len(k), " -> y",
    collapse = "\n"
  )
  md <- paste0("r", seq_len(k), " : x", seq_len(k), collapse = ", ")
  out <- dosearch(
    "p(x22*,r22)",
    "p(x22)",
    graph,
    missing_data = md,
    control = list(rules = c(4L, 5L, 9L, 10L, -1L), warn = FALSE)
  )
  expect_true(out$identifiable)
  expect_identical(out$formula, "p(x22^{\\ast}|r22 = 1)")
  out <- dosearch(
    "p(x22*,r22)",
    "p(x22)",
    paste0(graph, "\nx22 -> r22"),
    missing_data = md,
    control = list(rules = c(4L, 5L, 9L, 10L, -1L), warn = FALSE)
  )
  expect_false(out$identifiable)
})

test_that("case of variable names is determined by their letters", {
  expect_warning(
    dosearch("p(x,X_1)", "p(X_1)", "x -> X_1"),
    "Both lower case and upper case inputs detected."
  )
  expect_silent(
    dosearch("p(x_1,y)", "p(y)", "x_1 -> y")
  )
})

test_that("bound variables are primed only if the name is in scope", {
  # y is a variable of the target, so the summation over y is primed,
  # whereas the summations over w are not nested
  data <- "p(x,y,z,w)"
  query <- "p(y|do(x))"
  graph <- "
    x -> y
    z -> x
    w -> z
    x <-> w
    w <-> y
  "
  out <- dosearch(data, query, graph)
  expect_true(out$identifiable)
  expect_identical(
    out$formula,
    "\\frac{\\sum_{w}p(w)p(y,x|z,w)}{\\sum_{y'}\\sum_{w}p(w)p(y',x|z,w)}"
  )
})

test_that("variables of the target are primed in nested sums", {
  # y is bound by the summation over y' and again by the summation in the
  # denominator, which is in the scope of the first one
  data <- "
    p(x)
    p(y|x)
    p(z|x,y)
  "
  query <- "p(y|do(x))"
  graph <- "
    x -> y
    z -> x
    z -> y
  "
  out <- dosearch(data, query, graph)
  expect_true(out$identifiable)
  expect_identical(
    out$formula,
    paste0(
      "\\sum_{z}\\sum_{y',x'}p(z|y',x')p(x')p(y'|x')\\frac{p(y|x)p(z|y,x)}",
      "{\\sum_{y''}p(y''|x)p(z|y'',x)}"
    )
  )
})

test_that("up to three primes are used", {
  data <- "p(u,z,w,y,v,x)"
  query <- "p(w|do(x))"
  graph <- "
    x -> z
    x -> y
    z -> y
    x <-> v
    y -> v
    x -> u
    v -> u
    x <-> w
    v -> w
    u -> w
  "
  out <- dosearch(data, query, graph)
  expect_true(out$identifiable)
  expect_identical(
    out$formula,
    paste0(
      "\\sum_{y,v,u}p(u|y,v,x)p(y|x)\\sum_{z,x'}p(z,x')p(v|z,y,x')",
      "\\sum_{z',x''}p(z'|x'')p(w|z',y,v,u,x'')\\frac{p(x'')p(v|y,x'')}{",
      "\\sum_{x'''}p(x''')p(v|y,x''')}"
    )
  )
})

test_that("a superscript is used instead of more than three primes", {
  data <- "p(t,w,u,y,x,v)"
  query <- "p(v|do(y))"
  graph <- "
    w -> y
    y -> x
    w -> t
    y <-> t
    x -> t
    y -> v
    t -> v
    t <-> u
  "
  out <- dosearch(data, query, graph, control = list(heuristic = TRUE))
  expect_true(out$identifiable)
  expect_identical(
    out$formula,
    paste0(
      "\\sum_{x,t,w,u}p(v|y,t)\\sum_{y',v'}p(y',w,u)p(t,v'|y',x,w,u)\\sum_{u'}",
      "\\sum_{v''}\\frac{\\sum_{w'}p(y,x,w')\\frac{p(w',u')p(v''|y,t)}{",
      "\\sum_{v''',u''}p(w',u'')p(v'''|y,t)}}{\\sum_{x',v''',u''}\\sum_{w'}",
      "p(y,x',w')\\frac{p(w',u'')p(v'''|y,t)}{\\sum_{v^{(4)},u'''}",
      "p(w',u''')p(v^{(4)}|y,t)}}"
    )
  )
  expect_false(grepl("''''", out$formula, fixed = TRUE))
})

test_that("proxy variables that stand for true variables share their primes", {
  # The exchanged proxy variables x* and z* stand for the values of x and z
  graph <- "
    x -> y
    y -> r_y
    r_y -> r_x
  "
  out <- dosearch(
    "p(x*,y*,r_x,r_y)\np(y)",
    "p(y|do(x))",
    graph,
    missing_data = "r_x : x, r_y : y",
    control = list(heuristic = TRUE)
  )
  expect_true(out$identifiable)
  expect_identical(
    out$formula,
    paste0(
      "\\frac{p(y)p(x^{\\ast}|r_x = 1,r_y = 1,y^{\\ast})}{\\sum_{y'}p(y')",
      "p(x^{\\ast}|r_x = 1,r_y = 1,y^{\\ast\\prime})}"
    )
  )
  # Case-control design for the front-door criterion (Tikka et al., 2021)
  graph <- "
    x -> z
    z -> y
    y -> r_y
    x <-> y
    r_y -> r_x
    r_y -> r_z
    r_y <-> r_x
    r_y <-> r_z
    r_z <-> r_x
  "
  out <- dosearch(
    "p(x*,y*,z*,r_x,r_y,r_z)\np(y)",
    "p(y|do(x))",
    graph,
    missing_data = "r_x : x, r_y : y, r_z : z",
    control = list(heuristic = TRUE)
  )
  expect_true(out$identifiable)
  expect_identical(
    out$formula,
    paste0(
      "\\sum_{z}\\sum_{y'}\\frac{p(y')p(x^{\\ast},z^{\\ast}|r_x = 1,r_y = 1,",
      "y^{\\ast\\prime},r_z = 1)}{\\sum_{y'',z^{\\ast\\prime}}p(y'')",
      "p(x^{\\ast},z^{\\ast\\prime}|r_x = 1,r_y = 1,y^{\\ast\\prime\\prime},",
      "r_z = 1)}\\sum_{x'}\\sum_{y'',z^{\\ast\\prime}}p(y'')",
      "p(x^{\\ast\\prime},z^{\\ast\\prime}|r_x = 1,r_y = 1,",
      "y^{\\ast\\prime\\prime},r_z = 1)\\frac{p(y)",
      "p(x^{\\ast\\prime},z^{\\ast}|r_x = 1,r_y = 1,y^{\\ast},r_z = 1)}{",
      "\\sum_{y'''}p(y''')p(x^{\\ast\\prime},z^{\\ast}|r_x = 1,r_y = 1,",
      "y^{\\ast\\prime\\prime\\prime},r_z = 1)}"
    )
  )
})

test_that("derivations and diagnostics are not primed", {
  data <- "p(x,y,z)"
  query <- "p(y|do(x))"
  graph <- "
    x -> z
    z -> y
    x <-> y
  "
  out <- dosearch(data, query, graph, control = list(draw_derivation = TRUE))
  expect_match(out$formula, "x'", fixed = TRUE)
  expect_false(grepl("'", out$derivation, fixed = TRUE))
  verbose <- capture.output(
    out <- dosearch(data, query, graph, control = list(verbose = TRUE))
  )
  expect_match(verbose, "Derived", all = FALSE)
  expect_false(any(grepl("'", verbose, fixed = TRUE)))
})

test_that("summations apply to the terms that follow them in a product", {
  # The second summation over w is in the scope of the first one, so the
  # variable that it binds is primed
  data <- "
    p(x)
    p(v,w|x,do(y))
    p(y|v,w,do(x,z))
  "
  graph <- "
    v -> x
    x -> y
    w -> y
    v -> y
    x -> z
    w -> z
    y <-> v
    w <-> v
  "
  out <- dosearch(data, "p(y|do(x))", graph)
  expect_true(out$identifiable)
  expect_identical(
    out$formula,
    paste0(
      "\\sum_{v,w}\\sum_{x',w'}p(x')p(v,w'|do(y),x')p(w|do(y),x,v)",
      "p(y|do(x,z),v,w)"
    )
  )
  expect_false(grepl("\\left(", out$formula, fixed = TRUE))
})

test_that("true variables and proxy variables share their primes", {
  # x is a variable of the target, so the summation over x* is primed
  out <- dosearch(
    "p(x*,y*,r_x,r_y) \n p(y)",
    "p(x)",
    "x -> y \n y -> r_x \n y -> r_y",
    missing_data = "r_x : x, r_y : y",
    control = list(heuristic = TRUE)
  )
  expect_true(out$identifiable)
  expect_identical(
    out$formula,
    paste0(
      "\\sum_{y}p(x^{\\ast}|r_x = 1,r_y = 1,y^{\\ast})",
      "\\sum_{r_x',x^{\\ast\\prime}}p(y)p(r_x',x^{\\ast\\prime}|r_y = 1,",
      "y^{\\ast})"
    )
  )
})

test_that("differences are enclosed in parentheses only when necessary", {
  # A difference that is a term of a product
  out <- dosearch(
    "p(x*,z|r_x = 1) \n p(x*,r_x = 1|z)",
    "p(x,r_x = 0|z)",
    "z -> r_x",
    missing_data = "r_x : x",
    control = list(warn = FALSE),
    case_reasoning = TRUE
  )
  expect_true(out$identifiable)
  expect_identical(
    out$formula,
    "p(x^{\\ast}|r_x = 1,z)\\left(1 - p(r_x = 1|z)\\right)"
  )
  # A difference that is the term of a summation, and differences in a
  # numerator
  out <- dosearch(
    "p(y,r_y = 0|x,r_x = 1) \n p(x,r_x)",
    "p(x|do(y),r_y = 1)",
    "x -> r_y \n x -> r_x",
    missing_data = "r_x : x, r_y : y",
    control = list(warn = FALSE),
    case_reasoning = TRUE
  )
  expect_true(out$identifiable)
  expect_identical(
    out$formula,
    paste0(
      "\\frac{p(x) - p(x)p(r_y = 0|x,r_x = 1)}{\\sum_{x'}",
      "\\left(p(x') - p(x')p(r_y = 0|x',r_x = 1)\\right)}"
    )
  )
  # A difference that is subtracted
  out <- dosearch(
    "p(x*,r_x,y*,r_y,z) \n p(y,r_y,z)",
    "p(y,r_y = 0|do(x),r_x = 0,z)",
    "
      x -> z
      x -> r_x
      z -> r_x
      y -> r_y
      z -> r_y
      z -> y
    ",
    missing_data = "r_x : x, r_y : y",
    control = list(warn = FALSE),
    case_reasoning = TRUE
  )
  expect_true(out$identifiable)
  expect_identical(
    out$formula,
    paste0(
      "\\frac{p(y,r_y = 0|z) - \\left(\\frac{p(r_x = 1,r_y = 1,y^{\\ast}|z)}{",
      "p(r_y = 1|y,z)} - p(r_x = 1,r_y = 1,y^{\\ast}|z)\\right)}{p(r_x = 0|z)}"
    )
  )
})

test_that("marginal and conditional distributions are given as such", {
  # Marginal and conditional distributions of the input distributions
  out <- dosearch("p(x,y)", "p(y|x)", "x -> y")
  expect_identical(out$formula, "p(y|x)")
  out <- dosearch("p(x,y,z)", "p(z)", "x -> y\nz -> y")
  expect_identical(out$formula, "p(z)")
  # Steps of do-calculus do not change the formulas, so conditioning and
  # marginalization after them are not explicit either
  graph <- "z -> y\nx -> w"
  out <- dosearch(
    "p(y,z)",
    "p(y|do(x),z)",
    graph,
    control = list(rules = c(3, 5), warn = FALSE)
  )
  expect_identical(out$formula, "p(y|z)")
  out <- dosearch(
    "p(y,z)",
    "p(y|do(x))",
    graph,
    control = list(rules = c(3, 4), warn = FALSE)
  )
  expect_identical(out$formula, "p(y)")
  # The deleted observation w remains in the formula
  out <- dosearch(
    "p(y,z|w)",
    "p(y|z)",
    graph,
    control = list(rules = c(-1, 5), warn = FALSE)
  )
  expect_identical(out$formula, "p(y|w,z)")
  # Marginalization of a product is explicit
  out <- dosearch(
    "p(z) \n p(y|z)",
    "p(y)",
    "z -> y",
    control = list(rules = c(4, 6), warn = FALSE)
  )
  expect_identical(out$formula, "\\sum_{z}p(z)p(y|z)")
})

test_that("divisions of a single input distribution are given as such", {
  graph <- "x -> y \n x -> r_x"
  co <- list(warn = FALSE)
  run <- function(data, query) {
    dosearch(data, query, graph, missing_data = "r_x : x", control = co)
  }
  # Conditioning by division (rules 7 and 8)
  co$rules <- c(4, 9, 7)
  out <- run("p(x*,r_x,y)", "p(x*,y|r_x = 1)")
  expect_identical(out$formula, "p(x^{\\ast},y|r_x = 1)")
  co$rules <- c(4, 9, 8)
  expect_identical(run("p(x*,r_x,y)", "p(y|r_x = 1)")$formula, "p(y|r_x = 1)")
  # Marginalization by division (rules -7 and -8)
  co$rules <- c(5, 9, -7)
  expect_identical(run("p(r_x,y)", "p(y)")$formula, "p(y)")
  co$rules <- c(9, 5, -8)
  expect_identical(run("p(r_x,y)", "p(y)")$formula, "p(y)")
  # Divisions of different input distributions are explicit
  data <- "p(x*,r_x = 1,y) \n p(r_x = 1)"
  co$rules <- 7
  expect_identical(
    run(data, "p(x*,y|r_x = 1)")$formula,
    "\\frac{p(r_x = 1,x^{\\ast},y)}{p(r_x = 1)}"
  )
  co$rules <- c(4, 8)
  expect_identical(
    run(data, "p(y|r_x = 1)")$formula,
    "\\frac{p(r_x = 1,y)}{p(r_x = 1)}"
  )
})

test_that("values are assigned in the input distributions of a formula", {
  # Case-by-case reasoning over a mechanism that is assigned a value
  # reduces to the case of that value, so the conditional distribution of the
  # input distribution of that case is given as such
  data <- "p(y,r_x = 1) \n p(y,r_x = 0)"
  graph <- "x -> y\nx -> r_x"
  out <- dosearch(
    data,
    "p(y|r_x = 0)",
    graph,
    missing_data = "r_x : x",
    control = list(rules = c(11, 5, -9), warn = FALSE, draw_derivation = TRUE)
  )
  expect_true(out$identifiable)
  expect_match(out$derivation, "label=\"CbC\"", fixed = TRUE)
  expect_identical(out$formula, "p(y|r_x = 0)")
  out <- dosearch(
    data,
    "p(y|r_x = 1)",
    graph,
    missing_data = "r_x : x",
    control = list(rules = c(11, 5, 9), warn = FALSE)
  )
  expect_true(out$identifiable)
  expect_identical(out$formula, "p(y|r_x = 1)")
  # The value is not assigned under a summation over the mechanism, so
  # case-by-case reasoning in the denominator is not reduced
  out <- dosearch(
    "p(y*,r_y) \n p(y)",
    "p(r_y = 1|y)",
    "y -> r_y",
    missing_data = "r_y : y",
    case_reasoning = TRUE
  )
  expect_true(out$identifiable)
  expect_identical(
    out$formula,
    paste0(
      "\\frac{p(r_y = 1,y^{\\ast})}{\\sum_{r_y'}\\left[\\left(p(y) - ",
      "p(r_y' = 1,y^{\\ast})\\right) \\vee p(r_y' = 1,y^{\\ast})\\right]}"
    )
  )
  expect_false(grepl("\\right]_", out$formula, fixed = TRUE))
})

test_that("disabled missing data mechanisms are supported", {
  md <- "r_x : x, r_y : y"
  graph <- "
    x -> y
    y -> r_y
    r_y -> r_x
  "
  out <- dosearch(
    "p(y*,r_y=0)",
    "p(y*,r_y=0)",
    graph,
    missing_data = md,
    control = list(warn = FALSE)
  )
  expect_true(out$identifiable)
  expect_identical(out$formula, "p(r_y = 0,y^{\\ast})")
  # Disabling a mechanism requires case-by-general reasoning
  out <- dosearch(
    "p(x*,y*,r_x,r_y)",
    "p(y*,r_x=0,r_y=1)",
    graph,
    missing_data = md
  )
  expect_false(out$identifiable)
  out <- dosearch(
    "p(x*,y*,r_x,r_y)",
    "p(y*,r_x=0,r_y=1)",
    graph,
    missing_data = md,
    case_reasoning = TRUE
  )
  expect_true(out$identifiable)
  expect_identical(out$formula, "p(r_x = 0,r_y = 1,y^{\\ast})")
  out <- dosearch(
    "p(x*,y*,r_x,r_y)",
    "p(y*,r_x=0,r_y=1)",
    graph,
    missing_data = md,
    control = list(heuristic = TRUE),
    case_reasoning = TRUE
  )
  expect_true(out$identifiable)
  out <- dosearch(
    "p(x*,y*,r_x=1,r_y=1)",
    "p(y*,r_y=0)",
    graph,
    missing_data = md,
    case_reasoning = TRUE
  )
  expect_false(out$identifiable)
  # Disabled mechanisms are kept when exchanged for actions
  out <- dosearch(
    "p(y|r_x=0)",
    "p(y|do(r_x=0))",
    "r_x -> y",
    missing_data = "r_x : x",
    control = list(rules = 2, warn = FALSE)
  )
  expect_true(out$identifiable)
  expect_identical(out$formula, "p(y|r_x = 0)")
})

test_that("case-by-general reasoning is correct for missing data mechanisms", {
  md <- "r_x : x"
  graph <- "x -> y\nx -> r_x"
  out <- dosearch(
    "p(x*,r_x)",
    "p(x*,r_x=0)",
    graph,
    missing_data = md,
    control = list(rules = -9, warn = FALSE)
  )
  expect_true(out$identifiable)
  expect_identical(out$formula, "p(r_x = 0,x^{\\ast})")
  out <- dosearch(
    "p(x*|r_x)",
    "p(x*|r_x=0)",
    graph,
    missing_data = md,
    control = list(rules = -9, warn = FALSE)
  )
  expect_true(out$identifiable)
  expect_identical(out$formula, "p(x^{\\ast}|r_x = 0)")
  out <- dosearch(
    "p(x*,r_x)",
    "p(x*,r_x=0)",
    graph,
    missing_data = md,
    control = list(rules = 9, warn = FALSE)
  )
  expect_false(out$identifiable)
  # Values are also assigned after a step of do-calculus
  graph <- "z -> r_x\nx -> r_x"
  out <- dosearch(
    "p(r_x|z)",
    "p(r_x=0|do(z))",
    graph,
    missing_data = md,
    control = list(rules = c(2, -9), warn = FALSE, draw_derivation = TRUE)
  )
  expect_true(out$identifiable)
  expect_identical(out$formula, "p(r_x = 0|z)")
  expect_match(out$derivation, "label=\"A\"", fixed = TRUE)
  out <- dosearch(
    "p(x*,r_x|z)",
    "p(x*,r_x=1|do(z))",
    graph,
    missing_data = md,
    control = list(rules = c(2, 9), warn = FALSE)
  )
  expect_true(out$identifiable)
  expect_identical(out$formula, "p(r_x = 1,x^{\\ast}|z)")
})

test_that("case-by-case reasoning is correct for missing data mechanisms", {
  md <- "r_x : x"
  graph <- "x -> y\nx -> r_x"
  out <- dosearch(
    "p(y,r_x=1) \n p(y,r_x=0)",
    "p(y,r_x)",
    graph,
    missing_data = md,
    control = list(rules = 11, warn = FALSE, draw_derivation = TRUE)
  )
  expect_true(out$identifiable)
  expect_identical(
    out$formula,
    "\\left[p(r_x = 0,y) \\vee p(r_x = 1,y)\\right]"
  )
  expect_match(out$derivation, "label=\"CbC\"", fixed = TRUE)
  out <- dosearch(
    "p(y,r_x=0) \n p(y,r_x=1)",
    "p(y,r_x)",
    graph,
    missing_data = md,
    control = list(rules = -11, warn = FALSE)
  )
  expect_true(out$identifiable)
  expect_identical(
    out$formula,
    "\\left[p(r_x = 0,y) \\vee p(r_x = 1,y)\\right]"
  )
  out <- dosearch(
    "p(y,r_x=1) \n p(y,r_x=0)",
    "p(y,r_x)",
    graph,
    missing_data = md,
    control = list(rules = -11, warn = FALSE)
  )
  expect_true(out$identifiable)
  # The case of the disabled mechanism is given first in the formula
  graph <- "z -> y\nx -> r_x"
  out <- dosearch(
    "p(y,r_x=1|z) \n p(y,r_x=0|z)",
    "p(y,r_x|do(z))",
    graph,
    missing_data = md,
    control = list(rules = c(2, 11), warn = FALSE)
  )
  expect_true(out$identifiable)
  expect_identical(
    out$formula,
    "\\left[p(r_x = 0,y|z) \\vee p(r_x = 1,y|z)\\right]"
  )
})

test_that("general-by-case reasoning is correct for missing data mechanisms", {
  md <- "r_x : x"
  graph <- "x -> y\nx -> r_x"
  out <- dosearch(
    "p(y,r_x=1) \n p(y)",
    "p(y,r_x=0)",
    graph,
    missing_data = md,
    control = list(rules = 12, warn = FALSE, draw_derivation = TRUE)
  )
  expect_true(out$identifiable)
  expect_identical(out$formula, "p(y) - p(r_x = 1,y)")
  expect_match(out$derivation, "label=\"GbC\"", fixed = TRUE)
  out <- dosearch(
    "p(r_x=1)",
    "p(r_x=0)",
    graph,
    missing_data = md,
    control = list(rules = 12, warn = FALSE)
  )
  expect_true(out$identifiable)
  expect_identical(out$formula, "1 - p(r_x = 1)")
  out <- dosearch(
    "p(y) \n p(y,r_x=0)",
    "p(y,r_x=1)",
    graph,
    missing_data = md,
    control = list(rules = 13, warn = FALSE)
  )
  expect_true(out$identifiable)
  expect_identical(out$formula, "p(y) - p(r_x = 0,y)")
  out <- dosearch(
    "p(y) \n p(y,r_x=1)",
    "p(y,r_x=0)",
    graph,
    missing_data = md,
    control = list(rules = -13, warn = FALSE)
  )
  expect_true(out$identifiable)
  expect_identical(out$formula, "p(y) - p(r_x = 1,y)")
  out <- dosearch(
    "p(y) \n p(y,r_x=1)",
    "p(y,r_x=0)",
    graph,
    missing_data = md,
    control = list(rules = 13, warn = FALSE)
  )
  expect_false(out$identifiable)
  # Formulas after a step of do-calculus
  graph <- "z -> y\nz -> r_x\nx -> r_x"
  out <- dosearch(
    "p(r_x=1|z)",
    "p(r_x=0|do(z))",
    graph,
    missing_data = md,
    control = list(rules = c(2, 12), warn = FALSE)
  )
  expect_true(out$identifiable)
  expect_identical(out$formula, "1 - p(r_x = 1|z)")
  out <- dosearch(
    "p(y,r_x=1|z) \n p(y|z)",
    "p(y,r_x=0|do(z))",
    graph,
    missing_data = md,
    control = list(rules = c(2, 12), warn = FALSE)
  )
  expect_true(out$identifiable)
  expect_identical(out$formula, "p(y|z) - p(r_x = 1,y|z)")
  out <- dosearch(
    "p(y|z) \n p(y,r_x=1|z)",
    "p(y,r_x=0|do(z))",
    graph,
    missing_data = md,
    control = list(rules = c(2, -13), warn = FALSE)
  )
  expect_true(out$identifiable)
  expect_identical(out$formula, "p(y|z) - p(r_x = 1,y|z)")
})

test_that("disabled missing data mechanisms can identify new queries", {
  # Follow-up study of the non-respondents
  md <- "r_y : y"
  out <- dosearch("p(y*,r_y)", "p(y)", "y -> r_y", missing_data = md)
  expect_false(out$identifiable)
  data <- "
    p(y*,r_y)
    p(y|r_y=0)
  "
  out <- dosearch(data, "p(y)", "y -> r_y", missing_data = md)
  expect_false(out$identifiable)
  out <- dosearch(
    data,
    "p(y)",
    "y -> r_y",
    missing_data = md,
    case_reasoning = TRUE
  )
  expect_true(out$identifiable)
  out_h <- dosearch(
    data,
    "p(y)",
    "y -> r_y",
    missing_data = md,
    control = list(heuristic = TRUE),
    case_reasoning = TRUE
  )
  expect_true(out_h$identifiable)
  # Case-control design
  data <- "
    p(x*,y*,r_x,r_y)
    p(y)
  "
  graph <- "
    x -> y
    y -> r_y
    r_y -> r_x
  "
  out <- dosearch(
    data,
    "p(y|r_x=0)",
    graph,
    missing_data = "r_x : x, r_y : y",
    case_reasoning = TRUE
  )
  expect_true(out$identifiable)
  expect_identical(
    out$formula,
    paste0(
      "\\frac{\\sum_{r_y}p(r_x = 0|r_y)\\left[\\left(p(y) - ",
      "p(r_y = 1,y^{\\ast})\\right) \\vee p(r_y = 1,y^{\\ast})\\right]}{",
      "p(r_x = 0)}"
    )
  )
})

test_that("new missing data rules identify queries without disabled inputs", {
  # The missingness mechanism of y is identifiable when the marginal
  # distribution of y is known, which requires general-by-case and
  # case-by-case reasoning
  data <- "
    p(y*,r_y)
    p(y)
  "
  query <- "p(r_y=1|y)"
  graph <- "y -> r_y"
  md <- "r_y : y"
  out <- dosearch(data, query, graph, missing_data = md)
  expect_false(out$identifiable)
  out <- dosearch(data, query, graph, missing_data = md, case_reasoning = TRUE)
  expect_true(out$identifiable)
  expect_identical(
    out$formula,
    paste0(
      "\\frac{p(r_y = 1,y^{\\ast})}{\\sum_{r_y'}\\left[\\left(p(y) - ",
      "p(r_y' = 1,y^{\\ast})\\right) \\vee p(r_y' = 1,y^{\\ast})\\right]}"
    )
  )
})

test_that("case reasoning rules for missing data are enabled on request", {
  data <- "p(x*,y*,r_x,r_y)"
  query <- "p(y|do(x))"
  graph <- "x -> y\ny -> r_y\nr_y -> r_x"
  md <- "r_x : x, r_y : y"
  defaults <- c("4", "5", "9", "10", "-1", "-2", "-3", "2", "3", "6", "-6",
                "7", "-7", "8", "-8")
  case_rules <- c("-9", "11", "-11", "12", "13", "-13")
  out <- dosearch(
    data,
    query,
    graph,
    missing_data = md,
    control = list(benchmark_rules = TRUE)
  )
  expect_named(out$rule_times, defaults)
  expect_false(out$call$case_reasoning)
  out <- dosearch(
    data,
    query,
    graph,
    missing_data = md,
    control = list(benchmark_rules = TRUE),
    case_reasoning = TRUE
  )
  expect_named(out$rule_times, c(defaults, case_rules))
  expect_true(out$call$case_reasoning)
  # The rules are added to the rules given via control
  out <- dosearch(
    data,
    query,
    graph,
    missing_data = md,
    control = list(benchmark_rules = TRUE, rules = c(4, 12)),
    case_reasoning = TRUE
  )
  expect_named(out$rule_times, c("4", "12", "-9", "11", "-11", "13", "-13"))
  # The rules given via control are used as such otherwise
  out <- dosearch(
    data,
    query,
    graph,
    missing_data = md,
    control = list(benchmark_rules = TRUE, rules = c(4, 12))
  )
  expect_named(out$rule_times, c("4", "12"))
  # The rules are not added without missing data mechanisms
  out <- dosearch(
    "p(x,y)",
    "p(y|do(x))",
    "x -> y",
    control = list(benchmark_rules = TRUE),
    case_reasoning = TRUE
  )
  expect_named(out$rule_times, c("4", "5", "-2", "-3", "2", "3", "6", "-6"))
})

test_that("case reasoning is kept when the search is run again", {
  data <- "
    p(y*,r_y)
    p(y|r_y=0)
  "
  out <- dosearch(
    data,
    "p(y)",
    "y -> r_y",
    missing_data = "r_y : y",
    control = list(formula = FALSE, benchmark = TRUE),
    case_reasoning = TRUE
  )
  expect_true(out$identifiable)
  expect_identical(
    get_formula(out, run_again = TRUE),
    paste0(
      "\\sum_{r_y}\\left[p(r_y = 0)p(y|r_y = 0) \\vee p(r_y = 1,y^{\\ast})",
      "\\right]"
    )
  )
  expect_match(get_derivation(out, run_again = TRUE), "CbC", fixed = TRUE)
  expect_false(is.null(get_benchmark(out, run_again = TRUE)$time))
  # Objects without the argument in the call do not use case reasoning
  out$call$case_reasoning <- NULL
  expect_identical(get_formula(out, run_again = TRUE), "")
})

test_that("values of enabled mechanisms are shown after do-calculus", {
  data <- "p(x*,y*,r_x,r_y)"
  md <- "r_x : x, r_y : y"
  out <- dosearch(
    data,
    "p(x)",
    "x -> y\nr_x <-> y",
    missing_data = md,
    control = list(warn = FALSE)
  )
  expect_true(out$identifiable)
  expect_identical(
    out$formula,
    "p(x^{\\ast}|r_x = 1,r_y = 1)"
  )
  out <- dosearch(
    "p(y|r_x)",
    "p(y|do(r_x=1))",
    "r_x -> y",
    missing_data = "r_x : x",
    control = list(rules = 2, warn = FALSE)
  )
  expect_true(out$identifiable)
  expect_identical(out$formula, "p(y|r_x = 1)")
})

test_that("disabled mechanisms are shown in diagnostics", {
  out <- capture.output(
    dosearch(
      "p(x*,r_x)",
      "p(x*,r_x=0)",
      "x -> r_x",
      missing_data = "r_x : x",
      control = list(verbose = TRUE),
      case_reasoning = TRUE
    )
  )
  expect_match(out[1L], "Setting target: p(r_x = 0,x*)", fixed = TRUE)
  expect_match(out, "using rule: -9", all = FALSE, fixed = TRUE)
})

test_that("mechanisms are not removed while their proxy variables remain", {
  # A proxy variable is missing when its mechanism is disabled, so p(y|x*)
  # cannot be derived from p(y|x*,r_x = 0) for observed values of x*
  graph <- "x -> r_x\nz -> y"
  co <- list(rules = -1, warn = FALSE)
  run <- function(data, query) {
    dosearch(data, query, graph, missing_data = "r_x : x", control = co)
  }
  out <- run("p(y|x*,r_x=0)", "p(y|x*)")
  expect_false(out$identifiable)
  co$rules <- c(2, -3)
  out <- run("p(y|x*,r_x=0)", "p(y|x*)")
  expect_false(out$identifiable)
  # The proxy variable itself can be removed
  co$rules <- -1
  out <- run("p(y|x*,r_x=0)", "p(y|r_x=0)")
  expect_true(out$identifiable)
  expect_identical(out$formula, "p(y|r_x = 0)")
  # Neither can the mechanism be divided out when the proxy variable remains
  # on the left-hand side
  data <- "p(r_x=1,x*|z) \n p(r_x=1|x*,z)"
  co$rules <- -7
  expect_false(run(data, "p(x*|z)")$identifiable)
  co$rules <- -8
  expect_false(run(data, "p(x*|z)")$identifiable)
  co$rules <- 7
  out <- run("p(r_x=1,x*|z) \n p(r_x=1|z)", "p(x*|r_x=1,z)")
  expect_true(out$identifiable)
  expect_identical(out$formula, "\\frac{p(r_x = 1,x^{\\ast}|z)}{p(r_x = 1|z)}")
  # Previously, the formulas of these targets were incorrect
  out <- dosearch(
    "p(x*,z|r_x = 1) \n p(x*,r_x = 0,y|z) \n p(y|x*,r_x = 0,z)",
    "p(y|do(x),r_x = 1)",
    "z -> r_y",
    missing_data = "r_x : x, r_y : y",
    control = list(heuristic = TRUE, warn = FALSE)
  )
  expect_true(out$identifiable)
  expect_identical(out$formula, "p(y|r_x = 0,z)")
  out <- dosearch(
    "p(x*,r_x,y*,z|r_y = 1) \n p(z|do(x),r_x = 1) \n p(z)",
    "p(y)",
    "x -> y\nz -> r_y\nx -> r_x\nz -> y",
    missing_data = "r_x : x, r_y : y",
    control = list(heuristic = TRUE, warn = FALSE),
    case_reasoning = TRUE
  )
  expect_true(out$identifiable)
  expect_identical(
    out$formula,
    "\\sum_{r_x,x^{\\ast},z}p(z)p(r_x,x^{\\ast},y^{\\ast}|r_y = 1,z)"
  )
})

test_that("observations of proxy variables can be deleted by default", {
  # The deleted proxy variable x* is missing because r_x = 0, so it is
  # omitted from the formula
  out <- dosearch(
    "p(y*,r_y = 1,z|x*,r_x = 0)",
    "p(z|x,r_x = 1)",
    "y -> r_x\nx -> r_y\nx -> r_x",
    missing_data = "r_x : x, r_y : y",
    control = list(warn = FALSE)
  )
  expect_true(out$identifiable)
  expect_identical(
    out$formula,
    "p(z|r_x = 0,r_y = 1)"
  )
  out <- dosearch(
    "p(y*,r_y = 1,z|x*,r_x = 0)",
    "p(z|x,r_x = 1)",
    "y -> r_x\nx -> r_y\nx -> r_x",
    missing_data = "r_x : x, r_y : y",
    control = list(warn = FALSE, benchmark_rules = TRUE, rules = c(5, 4, -1))
  )
  expect_named(out$rule_times, c("5", "4", "-1"))
})

test_that("removed proxy variables are distinguished from other variables", {
  # y* is deleted from p(x|r_x = 1,r_y = 1,y*), so the formula holds for its
  # observed values, whereas y* of the other term is missing
  out <- dosearch(
    "p(x,z|r_x = 1,y*,r_y = 1) \n p(z|do(x)) \n p(y*,r_y = 0,z|x,r_x = 1)",
    "p(x,z)",
    "r_x -> r_y\nz -> y",
    missing_data = "r_x : x, r_y : y",
    control = list(
      heuristic = TRUE,
      warn = FALSE,
      rules = c(4, 5, 9, 10, -1, -2, -3, 2, 3, 6, -6, 7, -7, 8, -8)
    ),
    case_reasoning = TRUE
  )
  expect_true(out$identifiable)
  expect_identical(
    out$formula,
    paste0(
      "\\frac{\\sum_{y^{\\ast}}p(x|r_x = 1,r_y = 1,y^{\\ast\\prime})",
      "p(r_y = 0,y^{\\ast},z|x,r_x = 1)}{\\sum_{x',y^{\\ast},z'}",
      "p(x'|r_x = 1,r_y = 1,y^{\\ast\\prime})",
      "p(r_y = 0,y^{\\ast},z'|x',r_x = 1)}"
    )
  )
})

test_that("removed proxy variables are omitted when they are missing", {
  graph <- "z -> r_x\ny -> r_y"
  md <- "r_x : x, r_y : y"
  co <- list(warn = FALSE)
  # y* is deleted from p(z|y*,r_y = 0), where it is always missing
  out <- dosearch(
    "p(z|y*,r_y = 0)",
    "p(z|do(y))",
    graph,
    missing_data = md,
    control = co
  )
  expect_true(out$identifiable)
  expect_identical(out$formula, "p(z|r_y = 0)")
  # The mechanism can also be assigned the value 0 after the deletion
  out <- dosearch(
    "p(z,y*|r_y)",
    "p(z|r_y = 0)",
    graph,
    missing_data = md,
    control = list(rules = c(5, -1, -9), warn = FALSE),
    case_reasoning = TRUE
  )
  expect_true(out$identifiable)
  expect_identical(out$formula, "p(z|r_y = 0)")
  # A deleted proxy variable whose mechanism has the value 1 is observed
  out <- dosearch(
    "p(z|y*,r_y = 1)",
    "p(z|r_y = 1)",
    graph,
    missing_data = md,
    control = list(rules = -1, warn = FALSE)
  )
  expect_true(out$identifiable)
  expect_identical(out$formula, "p(z|r_y = 1,y^{\\ast})")
  # The proxy variable is kept when its mechanism is on the left-hand side
  # and the proxy variable is not, because p(r_y = 0|y*) is 1 when y* is
  # missing but p(r_y = 0) is not
  out <- dosearch(
    "p(z,r_y = 0|y*) \n p(r_y = 0|y*)",
    "p(z|r_y = 0)",
    graph,
    missing_data = md,
    control = co
  )
  expect_true(out$identifiable)
  expect_identical(
    out$formula,
    "\\frac{p(r_y = 0,z|y^{\\ast})}{p(r_y = 0|y^{\\ast})}"
  )
})

test_that("omitted proxy variables do not prime other variables", {
  # The summation over x follows the term from which x* is omitted
  out <- dosearch(
    "p(y|x*,r_x = 0) \n p(x) \n p(z|x)",
    "p(y,z)",
    "x -> z\nx -> r_x",
    missing_data = "r_x : x",
    control = list(heuristic = TRUE)
  )
  expect_true(out$identifiable)
  expect_identical(out$formula, "p(y|r_x = 0)\\sum_{x}p(x)p(z|x)")
})
