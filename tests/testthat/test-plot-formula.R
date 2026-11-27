# The plotmath expression of a single formula
plotmath_of <- function(formula) {
  formula_expressions(formula)[[1L]]
}

# The logical or of the symbol font
vee <- rawToChar(as.raw(218L))

test_that("distributions are converted to plotmath", {
  expect_identical(plotmath_of("p(y)"), quote(p("y")))
  expect_identical(plotmath_of("p(y|x)"), quote(p("y" * "|" * "x")))
  expect_identical(
    plotmath_of("p(y,z|x,w)"),
    quote(p(list("y", "z") * "|" * list("x", "w")))
  )
  # Actions
  expect_identical(
    plotmath_of("p(y|do(x,z),w)"),
    quote(p("y" * "|" * list(do(list("x", "z")), "w")))
  )
  expect_identical(plotmath_of("p(y|do(x))"), quote(p("y" * "|" * do("x"))))
  # Values and subscripts
  r_x <- quote("r"["x"] == 1)
  expect_identical(
    plotmath_of("p(r_x = 1,y|z)"),
    bquote(p(list(.(r_x), "y") * "|" * "z"))
  )
  expect_identical(
    plotmath_of("p(Y|X,A = 0)"),
    quote(p("Y" * "|" * list("X", "A" == 0)))
  )
  expect_identical(plotmath_of("p(x_1_2)"), quote(p("x"["1_2"])))
})

test_that("primes and proxy variables are converted to plotmath", {
  expect_identical(plotmath_of("p(x')"), quote(p("x" * minute)))
  expect_identical(plotmath_of("p(x'')"), quote(p("x" * second)))
  marks <- quote(second * minute)
  expect_identical(plotmath_of("p(x''')"), bquote(p("x" * .(marks))))
  expect_identical(plotmath_of("p(x^{(4)})"), quote(p("x"^"(4)")))
  expect_identical(plotmath_of("p(r_y')"), quote(p("r"["y"] * minute)))
  # Proxy variables
  expect_identical(plotmath_of("p(x^{\\ast})"), quote(p("x"^"*")))
  star <- quote("*" * minute)
  expect_identical(plotmath_of("p(x^{\\ast\\prime})"), bquote(p("x"^.(star))))
  star <- bquote("*" * .(marks))
  expect_identical(
    plotmath_of("p(x^{\\ast\\prime\\prime\\prime})"),
    bquote(p("x"^.(star)))
  )
  expect_identical(plotmath_of("p(x^{\\ast(4)})"), quote(p("x"^"*(4)")))
  expect_identical(plotmath_of("p(x_1^{\\ast})"), quote(p("x"["1"]^"*")))
})

test_that("operations are converted to plotmath", {
  # Products and summations, which apply to the factors that follow them
  expect_identical(
    plotmath_of("\\sum_{z}p(z|x)\\sum_{x'}p(x')p(y|z,x')"),
    quote(
      sum(
        p("z" * "|" * "x") *
          sum(
            p("x" * minute) * p("y" * "|" * list("z", "x" * minute)),
            "x" * minute,
            ""
          ),
        "z",
        ""
      )
    )
  )
  expect_identical(
    plotmath_of("\\sum_{y,w}p(y,w)"),
    quote(sum(p(list("y", "w")), list("y", "w"), ""))
  )
  # Fractions
  y <- quote("y" * minute)
  expect_identical(
    plotmath_of("\\frac{p(y)p(x|y)}{\\sum_{y'}p(y')p(x|y')}"),
    bquote(
      frac(
        p("y") * p("x" * "|" * "y"),
        sum(p(.(y)) * p("x" * "|" * .(y)), .(y), "")
      )
    )
  )
  # Differences, sums and the constant 1
  expect_identical(
    plotmath_of("p(y) - p(x) + p(z)"),
    quote(p("y") - p("x") + p("z"))
  )
  r_x <- quote("r"["x"] == 1)
  expect_identical(
    plotmath_of("1 - p(r_x = 1|z)"),
    bquote(1 - p(.(r_x) * "|" * "z"))
  )
  # Parentheses
  expect_identical(
    plotmath_of("p(z|x)\\left(p(y) - p(x)\\right)"),
    quote(p("z" * "|" * "x") * bgroup("(", p("y") - p("x"), ")"))
  )
  # Case-by-case reasoning
  r_x <- quote("r"["x"] == 0)
  expect_identical(
    plotmath_of("\\left[p(r_x = 0,y) \\vee p(y)\\right]"),
    bquote(bgroup("[", p(list(.(r_x), "y")) ~ symbol(.(vee)) ~ p("y"), "]"))
  )
  expect_identical(
    plotmath_of("\\left[\\left(p(y) - p(x)\\right) \\vee p(x)\\right]"),
    bquote(
      bgroup(
        "[",
        bgroup("(", p("y") - p("x"), ")") ~ symbol(.(vee)) ~ p("x"),
        "]"
      )
    )
  )
})

test_that("variants are preceded by their values", {
  exprs <- formula_expressions(
    c("r_x = 0, r_y = 1" = "p(y|r_x = 0)", "r_x = 1, r_y = 1" = "p(y)")
  )
  expect_length(exprs, 2L)
  values <- quote(list("r"["x"] == 0, "r"["y"] == 1))
  r_x <- quote("r"["x"] == 0)
  expect_identical(
    exprs[[1L]],
    bquote(.(values) * ":" ~ ~ p("y" * "|" * .(r_x)))
  )
  values <- quote(list("r"["x"] == 1, "r"["y"] == 1))
  expect_identical(exprs[[2L]], bquote(.(values) * ":" ~ ~ p("y")))
  # A variant without a name is not preceded by values
  expect_identical(formula_expressions(c("p(y)"))[[1L]], quote(p("y")))
  expect_identical(
    formula_expressions(stats::setNames("p(y)", ""))[[1L]],
    quote(p("y"))
  )
})

test_that("malformed formulas fail", {
  malformed <- c(
    "", "\\frac{p(y)", "p(y", "q(y)", "\\left[p(y)\\right]", "p(y)}", "p()",
    "p(y,)", "p(y|)", "p(y|do())", "p(x = 1 = 2)", "p( = 1)", "p(x = )"
  )
  for (f in malformed) {
    expect_error(plotmath_of(f), "Unable to plot the formula")
  }
})

test_that("problematic variable names are cleaned", {
  expect_identical(
    clean_names(c("x", "x.1", "x_1", "a_b_c", "x'", "x-1", "_", "y_", "_w")),
    c("x", "x.1", "x_1", "a_b_c", "x.", "x.1.1", "v", "y", "w")
  )
  cleaned <- clean_names("\u00e4")
  expect_true(cleaned != "\u00e4")
  expect_match(cleaned, "^[A-Za-z0-9._]+$")
  expect_warning(
    exprs <- formula_expressions("p(a-b|x^2,y)"),
    paste0(
      "Variable names were changed for plotting: ",
      "`a-b` -> `a.b`, `x^2` -> `x.2`"
    ),
    fixed = TRUE
  )
  expect_identical(exprs[[1L]], quote(p("a.b" * "|" * list("x.2", "y"))))
  expect_silent(formula_expressions("p(x_1,y.2|z')"))
})

test_that("formulas are plotted", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  out <- dosearch("p(x,y,z)", "p(y|do(x))", "x -> z \n z -> y \n x <-> y")
  expect_invisible(exprs <- plot_formula(out, col = "blue"))
  expect_true(is.expression(exprs))
  expect_identical(exprs, formula_expressions(out$formula))
  # The variants are plotted one below the other
  out <- dosearch(
    "p(y*,r_y = 1) \n p(y,r_y = 0)",
    "p(y|r_y)",
    "y -> r_y",
    missing_data = "r_y : y",
    case_reasoning = TRUE
  )
  expect_length(plot_formula(out), 1L)
  expect_identical(
    plot_formula(out, variants = TRUE),
    formula_expressions(out$variants)
  )
  # Without variants, the formula is plotted
  out <- dosearch("p(x,y)", "p(y|x)", "x -> y")
  expect_identical(
    plot_formula(out, variants = TRUE),
    formula_expressions("p(y|x)")
  )
  # Long formulas are shrunk to fit
  out <- dosearch(
    "p(u,z,w,y,v,x)",
    "p(w|do(x))",
    "
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
  )
  expect_error(plot_formula(out, cex = 3), NA)
  # Changed names are reported
  out <- dosearch(
    "p(a-b,y)",
    "p(y|do(a-b))",
    "a-b -> y",
    control = list(warn = FALSE)
  )
  expect_warning(plot_formula(out), "`a-b` -> `a.b`", fixed = TRUE)
})

test_that("formulas that cannot be plotted are reported", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  out <- dosearch("p(x,y)", "p(y|do(x))", "x -> y \n x <-> y")
  expect_message(
    expect_null(plot_formula(out)),
    "No formula is available to plot."
  )
  out <- dosearch("p(x,y)", "p(y|x)", "x -> y", control = list(formula = FALSE))
  expect_message(plot_formula(out), "No formula is available to plot.")
  out <- dosearch("p(x,y,z)", "p(y|do(x))", "x -> z \n z -> y \n x <-> y")
  expect_error(
    plot_formula(out, max_chars = 10L),
    "The formula has 37 characters, which exceeds `max_chars` \\(10\\)"
  )
})

test_that("invalid arguments of plot_formula fail", {
  out <- dosearch("p(x,y)", "p(y|x)", "x -> y")
  expect_error(
    plot_formula(data.frame()),
    "Argument `x` must be an object of class `dosearch`"
  )
  expect_error(
    plot_formula(out, variants = NA),
    "Argument `variants` must be either `TRUE` or `FALSE`"
  )
  expect_error(
    plot_formula(out, max_chars = 0),
    "Argument `max_chars` must be a positive number"
  )
  expect_error(
    plot_formula(out, cex = "a"),
    "Argument `cex` must be a positive number"
  )
})
