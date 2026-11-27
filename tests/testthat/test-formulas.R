# Tests of the formulas of marginalization, conditioning and division
# (rules 4, 5, 7, -7, 8 and -8). A marginal or conditional distribution is
# given as such when the distribution that is marginalized or conditioned is
# derived from an input distribution by rules that have a single parent
# distribution, and so is a division when both distributions are derived
# from the same input distribution. Otherwise, the summations and quotients
# are explicit. The variants of the formulas derived by case-by-case
# reasoning assign the values of the variables of case-by-case reasoning and
# write out the summations over them. Every formula, or every variant, is
# also compared numerically with the target in a random model (see
# helper-formulas.R).

test_that("the formula checker detects incorrect formulas", {
  set.seed(1)
  graph <- "x -> z\nz -> y\nx <-> y"
  expect_null(
    formula_difference(
      "\\sum_{z}p(z|x)\\sum_{x'}p(x')p(y|z,x')",
      "p(y|do(x))",
      graph
    )
  )
  expect_type(
    formula_difference("\\sum_{z}p(z|x)p(y|z,x)", "p(y|do(x))", graph),
    "character"
  )
  expect_type(formula_difference("p(y|x)", "p(y|do(x))", graph), "character")
  # A summation applies to every term that follows it in the same product
  expect_type(
    formula_difference(
      "\\sum_{z}p(z|x)p(x')\\sum_{x'}p(y|z,x')",
      "p(y|do(x))",
      graph
    ),
    "character"
  )
  # Proxy variables stand for their true variables when the mechanism has
  # the value 1 and are missing when it has the value 0
  graph <- "x -> y\ny -> r_y\nr_y -> r_x"
  md <- "r_x : x, r_y : y"
  casecontrol <- paste0(
    "\\frac{p(y)p(x^{\\ast}|r_x = 1,r_y = 1,y^{\\ast})}{",
    "\\sum_{y'}p(y')p(x^{\\ast}|r_x = 1,r_y = 1,y^{\\ast\\prime})}"
  )
  expect_null(formula_difference(casecontrol, "p(y|do(x))", graph, md))
  expect_type(
    formula_difference(
      sub("r_x = 1", "r_x = 0", casecontrol, fixed = TRUE),
      "p(y|do(x))",
      graph,
      md
    ),
    "character"
  )
  graph <- "z -> r_x\ny -> r_y"
  expect_null(
    formula_difference(
      "\\frac{p(r_y = 0,z|y^{\\ast})}{p(r_y = 0|y^{\\ast})}",
      "p(z|r_y = 0)",
      graph,
      md
    )
  )
  expect_type(
    formula_difference("p(r_y = 0,z)", "p(z|r_y = 0)", graph, md),
    "character"
  )
  # Proxy variables are read in both notations, and sums are supported
  expect_identical(
    normalize_formula("p(x^{\\ast},y^{\\ast\\prime\\prime}|z^{\\ast(4)})"),
    "p(x*,y*''|z*^{(4)})"
  )
  sum <- "p(r_y = 0)p(y|r_y = 0) + p(r_y = 1,y^{\\ast})"
  expect_null(formula_difference(sum, "p(y)", "y -> r_y", "r_y : y"))
  expect_type(
    formula_difference(
      sub("p(r_y = 0)", "p(r_y = 1)", sum, fixed = TRUE),
      "p(y)",
      "y -> r_y",
      "r_y : y"
    ),
    "character"
  )
  # The values of a variant are assigned in the query
  expect_identical(
    assigned_query("p(y,r_x|do(z),r_y)", "r_x = 0, r_y = 1"),
    "p(y,r_x = 0|do(z),r_y = 1)"
  )
  expect_identical(assigned_query("p(y)", ""), "p(y)")
  # Every distribution must be a marginal or conditional distribution of an
  # input distribution, and the operations must not be redundant
  expect_identical(
    foreign_terms("p(y|x)p(x|z)", "p(x,y) \n p(z)"),
    "x|z"
  )
  expect_identical(
    foreign_terms("p(y|r_x = 0)", "p(x*,y|r_x = 0)", "r_x : x"),
    character(0L)
  )
  expect_identical(
    foreign_terms("p(y|r_x = 1)", "p(x*,y|r_x = 0)", "r_x : x"),
    "y|r_x = 1"
  )
  expect_identical(
    redundant_operations("\\sum_{z}p(z,y)"),
    "summation of a single distribution"
  )
  expect_identical(
    redundant_operations("\\frac{p(z,y)}{\\sum_{y'}p(z,y')}"),
    "quotient of a single distribution and its marginal"
  )
  expect_identical(
    redundant_operations("\\sum_{z}p(z)p(y|z)"),
    character(0L)
  )
  expect_identical(
    redundant_operations(
      "\\frac{p(r_y = 1,y*,z)}{p(r_y = 1)}",
      "p(y*,r_y,z)",
      "r_y : y"
    ),
    "quotient of two distributions of the same input"
  )
  expect_identical(
    redundant_operations(
      "\\frac{p(r_y = 1,y*,z)}{p(r_y = 1)}",
      "p(y*,r_y,z) \n p(r_y = 1)",
      "r_y : y"
    ),
    character(0L)
  )
})

test_that("marginal distributions are given as such", {
  set.seed(2)
  graph <- "x -> y\nz -> y\nw -> z"
  expect_formula("p(x,y,z,w)", "p(z)", graph, "p(z)", rules = 4, uses = "M")
  # After conditioning
  expect_formula(
    "p(x,y,z,w)",
    "p(x|z)",
    graph,
    "p(x|z)",
    rules = c(5, 4),
    uses = c("C", "M")
  )
  # A marginal distribution of a conditional input distribution
  expect_formula("p(x,y,z|w)", "p(x|w)", graph, "p(x|w)", rules = 4)
  # After an exchange of an observation for an action
  expect_formula(
    "p(y,z|x)",
    "p(z|do(x))",
    graph,
    "p(z|x)",
    rules = c(2, 4),
    uses = c("R2", "M")
  )
  # After the deletion of an observation, which remains in the formula
  expect_formula(
    "p(y,z|x,w)",
    "p(z|x)",
    "x -> y\nz -> y",
    "p(z|x,w)",
    rules = c(-1, 4),
    uses = c("R1", "M")
  )
  # After the insertion of an action
  expect_formula(
    "p(y,x)",
    "p(y|do(w))",
    "x -> y\nw -> z",
    "p(y)",
    rules = c(3, 4),
    uses = c("R3", "M")
  )
  # A marginal distribution under a summation uses the primes of its scope
  expect_formula(
    "p(x,y,z)",
    "p(y|do(x))",
    "x -> z\nz -> y\nx <-> y",
    "\\sum_{z}p(z|x)\\sum_{x'}p(x')p(y|z,x')"
  )
})

test_that("conditional distributions are given as such", {
  set.seed(3)
  graph <- "x -> y\nz -> y\nw -> z"
  expect_formula("p(x,y,z)", "p(x|y,z)", graph, "p(x|y,z)", rules = 5)
  # After marginalization
  expect_formula(
    "p(x,y,z)",
    "p(x|y)",
    graph,
    "p(x|y)",
    rules = c(4, 5),
    uses = c("M", "C")
  )
  # A conditional distribution of an experiment keeps the action
  expect_formula(
    "p(y,z|do(x))",
    "p(y|do(x),z)",
    graph,
    "p(y|do(x),z)",
    rules = 5
  )
  # After the steps of do-calculus
  expect_formula(
    "p(y,z|x)",
    "p(y|do(x),z)",
    graph,
    "p(y|z,x)",
    rules = c(2, 5),
    uses = c("R2", "C")
  )
  expect_formula(
    "p(y,z|w)",
    "p(y|z)",
    "z -> y\nx -> w",
    "p(y|w,z)",
    rules = c(-1, 5),
    uses = c("R1", "C")
  )
  expect_formula(
    "p(y,z)",
    "p(y|do(x),z)",
    "z -> y\nx -> w",
    "p(y|z)",
    rules = c(3, 5),
    uses = c("R3", "C")
  )
})

test_that("marginalization and conditioning of products are explicit", {
  set.seed(4)
  expect_formula(
    "p(z) \n p(y|z)",
    "p(y)",
    "z -> y",
    "\\sum_{z}p(z)p(y|z)",
    rules = c(-6, 4),
    uses = c("P", "M")
  )
  expect_formula(
    "p(z) \n p(y|z)",
    "p(z|y)",
    "z -> y",
    "\\frac{p(z)p(y|z)}{\\sum_{z'}p(z')p(y|z')}",
    rules = c(-6, 5),
    uses = c("P", "C")
  )
})

test_that("missing data steps keep marginal and conditional distributions", {
  set.seed(5)
  graph <- "x -> y\nx -> r_x"
  md <- "r_x : x"
  # After the exchange of a proxy variable for its true variable, the proxy
  # variable is given
  expect_formula(
    "p(x*,y|r_x = 1)",
    "p(x|r_x = 1)",
    graph,
    "p(x^{\\ast}|r_x = 1)",
    md = md,
    rules = c(10, 4),
    uses = c("EX", "M")
  )
  expect_formula(
    "p(x*,y|r_x = 1)",
    "p(y|x,r_x = 1)",
    graph,
    "p(y|r_x = 1,x^{\\ast})",
    md = md,
    rules = c(10, 5),
    uses = c("EX", "C")
  )
  # After the assignment of a value to a mechanism, the value is given
  expect_formula(
    "p(x*,y,r_x)",
    "p(y,r_x = 1)",
    graph,
    "p(r_x = 1,y)",
    md = md,
    rules = c(9, 4),
    uses = c("A", "M")
  )
  expect_formula(
    "p(x*,y,r_x)",
    "p(y|x*,r_x = 1)",
    graph,
    "p(y|r_x = 1,x^{\\ast})",
    md = md,
    rules = c(5, 9),
    uses = c("C", "A")
  )
  expect_formula(
    "p(x*,r_x,y)",
    "p(y|x,r_x = 1)",
    graph,
    "p(y|r_x = 1,x^{\\ast})",
    md = md,
    rules = c(9, 10, 4, 7, 5),
    uses = c("A", "EX", "C")
  )
})

test_that("divisions of a single input distribution are given as such", {
  set.seed(6)
  graph <- "x -> y\nx -> r_x"
  md <- "r_x : x"
  # Conditioning and marginalization of a division
  expect_formula(
    "p(x*,r_x = 1,y)",
    "p(y|x*,r_x = 1)",
    graph,
    "p(y|r_x = 1,x^{\\ast})",
    md = md,
    rules = c(4, 7, 5),
    uses = c("M", "D", "C")
  )
  expect_formula(
    "p(x*,r_x = 1,y)",
    "p(y|r_x = 1)",
    graph,
    "p(y|r_x = 1)",
    md = md,
    rules = c(7, 4),
    uses = c("D", "M")
  )
  # After an assignment and with rule 8
  expect_formula(
    "p(x*,r_x,y)",
    "p(y|r_x = 1)",
    graph,
    "p(y|r_x = 1)",
    md = md,
    rules = c(9, 4, 8),
    uses = c("A", "M", "D")
  )
  # After the exchange of a proxy variable
  expect_formula(
    "p(x*,r_x,y)",
    "p(x,y|r_x = 1)",
    graph,
    "p(x^{\\ast},y|r_x = 1)",
    md = md,
    rules = c(9, 10, 4, 7),
    uses = c("A", "EX", "D")
  )
  # After a step of do-calculus
  expect_formula(
    "p(x*,r_x,y|w)",
    "p(x*,y|r_x = 1,do(w))",
    "x -> y\nx -> r_x\nw -> y",
    "p(x^{\\ast},y|r_x = 1,w)",
    md = md,
    rules = c(9, 2, 4, 7),
    uses = c("R2", "D")
  )
  # The divisor of rule -8 is derived from a marginal distribution of the
  # input distribution by inserting observations, so the quotient is a
  # conditional distribution of the input distribution
  expect_formula(
    "p(y*,r_y = 1,z)",
    "p(y,z)",
    "y -> z",
    "p(y^{\\ast},z|r_y = 1)",
    md = "r_y : y",
    rules = c(4, 10, 3, -2, -8),
    uses = c("R3", "D")
  )
  # Two mechanisms
  graph <- "x -> y\nx -> r_x\ny -> r_y"
  md <- "r_x : x, r_y : y"
  expect_formula(
    "p(x*,y*,r_x = 1,r_y = 1)",
    "p(x*,y*|r_x = 1,r_y = 1)",
    graph,
    "p(x^{\\ast},y^{\\ast}|r_x = 1,r_y = 1)",
    md = md,
    rules = c(4, 7),
    uses = "D"
  )
  expect_formula(
    "p(x*,y*,r_x = 1,r_y = 1)",
    "p(x*|y*,r_x = 1,r_y = 1)",
    graph,
    "p(x^{\\ast}|r_x = 1,r_y = 1,y^{\\ast})",
    md = md,
    rules = c(4, 7, 5),
    uses = c("D", "C")
  )
  # Marginalization by division (rules -7 and -8)
  graph <- "y -> r_x\nx -> r_x"
  md <- "r_x : x"
  expect_formula(
    "p(r_x,y)",
    "p(y)",
    graph,
    "p(y)",
    md = md,
    rules = c(9, 5, -8),
    uses = c("A", "C", "D")
  )
})

test_that("divisions of different input distributions are explicit", {
  set.seed(7)
  graph <- "x -> y\nx -> r_x"
  md <- "r_x : x"
  data <- "p(x*,r_x = 1,y) \n p(r_x = 1)"
  # Conditioning and marginalization of such a division are explicit
  expect_formula(
    data,
    "p(y|x*,r_x = 1)",
    graph,
    paste0(
      "\\frac{\\frac{p(r_x = 1,x^{\\ast},y)}{p(r_x = 1)}}{",
      "\\sum_{y'}\\frac{p(r_x = 1,x^{\\ast},y')}{p(r_x = 1)}}"
    ),
    md = md,
    rules = c(7, 5),
    uses = c("D", "C")
  )
  expect_formula(
    data,
    "p(y|r_x = 1)",
    graph,
    "\\sum_{x^{\\ast}}\\frac{p(r_x = 1,x^{\\ast},y)}{p(r_x = 1)}",
    md = md,
    rules = c(7, 4),
    uses = c("D", "M")
  )
  # The divisor of rule -8 is evaluated at the value that the quotient
  # assigns to the mechanism of its right-hand side
  expect_formula(
    "p(r_y = 1|x*,r_x) \n p(x*,r_x = 1,r_y = 1)",
    "p(x*,r_x = 1)",
    "x -> r_y\ny -> r_y",
    "\\frac{p(r_x = 1,x^{\\ast},r_y = 1)}{p(r_y = 1|r_x = 1,x^{\\ast})}",
    md = "r_x : x, r_y : y",
    rules = -8,
    uses = "D"
  )
  graph <- "y -> r_x\nx -> r_x"
  data <- "p(r_x = 1|y) \n p(r_x = 1,y)"
  for (rule in c(-7, -8)) {
    expect_formula(
      data,
      "p(y)",
      graph,
      "\\frac{p(r_x = 1,y)}{p(r_x = 1|y)}",
      md = md,
      rules = rule,
      uses = "D"
    )
  }
})

test_that("removed proxy variables are omitted from compact distributions", {
  set.seed(8)
  graph <- "w -> z\ny -> r_y"
  md <- "r_y : y"
  expect_formula(
    "p(z,w|y*,r_y = 0)",
    "p(z|w,r_y = 0)",
    graph,
    "p(z|r_y = 0,w)",
    md = md,
    rules = c(5, -1),
    uses = c("C", "R1")
  )
  expect_formula(
    "p(z,w|y*,r_y = 0)",
    "p(z|r_y = 0)",
    graph,
    "p(z|r_y = 0)",
    md = md,
    rules = c(4, -1),
    uses = c("M", "R1")
  )
})

test_that("marginal and conditional distributions are given as such in LDAGs", {
  set.seed(9)
  # After the deletion of an observation in a context
  expect_formula(
    "p(y,z|x,a = 0)",
    "p(z|a = 0)",
    "x -> y : a = 0\na -> y\nz -> y",
    "p(z|x,a = 0)",
    rules = c(-3, 0),
    uses = c("I-", "M")
  )
  # After case-by-general reasoning
  expect_formula(
    "p(y,a)",
    "p(y|a = 0)",
    "x -> y : a = 0\na -> y",
    "p(y|a = 0)",
    rules = c(8, 1),
    uses = c("CbG", "C")
  )
  # After the insertion of an observation with a value, which does not
  # appear in the formula
  expect_formula(
    "p(y,z)",
    "p(y|z,a = 0)",
    "z -> y\nx -> w : a = 0\na -> w",
    "p(y|z)",
    rules = c(4, 1),
    uses = c("I+0", "C")
  )
  expect_formula(
    "p(y,z,a)",
    "p(z|x,a = 0)",
    "z -> y\nx -> y : a = 0\na -> y\na -> z",
    "p(z|a = 0)",
    rules = c(8, 0, 1, 3),
    uses = c("CbG", "I+")
  )
})

test_that("differences are enclosed in parentheses in products and cases", {
  set.seed(16)
  # The formula checker reads products before differences, so a difference
  # of general-by-case reasoning that is multiplied by another term without
  # parentheses would change the value of the formula
  graph <- "y -> z \n y -> r_x"
  # General-by-case reasoning on the right-hand side (rule 12) and on the
  # left-hand side (rule -13) followed by the product rule
  expect_formula(
    "p(y) \n p(y,r_x = 1) \n p(z|y,r_x = 0)",
    "p(z,y,r_x = 0)",
    graph,
    "p(z|r_x = 0,y)\\left(p(y) - p(r_x = 1,y)\\right)",
    md = "r_x : x",
    rules = c(12, 6, -6),
    uses = c("GbC", "P")
  )
  expect_type(
    formula_difference(
      "p(z|r_x = 0,y)p(y) - p(r_x = 1,y)",
      "p(z,y,r_x = 0)",
      graph,
      "r_x : x"
    ),
    "character"
  )
  expect_formula(
    "p(y) \n p(y,r_x = 1) \n p(z|y,r_x = 0)",
    "p(z,y,r_x = 0)",
    graph,
    "p(z|r_x = 0,y)\\left(p(y) - p(r_x = 1,y)\\right)",
    md = "r_x : x",
    rules = c(-13, 6, -6),
    uses = c("GbC", "P")
  )
  # Under a summation
  expect_formula(
    "p(y) \n p(y,r_x = 1) \n p(z|y,r_x = 0)",
    "p(z,r_x = 0)",
    graph,
    "\\sum_{y}p(z|r_x = 0,y)\\left(p(y) - p(r_x = 1,y)\\right)",
    md = "r_x : x",
    case_reasoning = TRUE,
    uses = c("GbC", "P", "M")
  )
  # In the variant of a case and in a written sum
  expect_formula(
    "p(y) \n p(y,r_x = 1) \n p(z|y,r_x = 0) \n p(z|y,r_x = 1)",
    "p(z,y,r_x)",
    graph,
    paste0(
      "\\left[p(z|r_x = 0,y)\\left(p(y) - p(r_x = 1,y)\\right) \\vee ",
      "p(r_x = 1,y)p(z|r_x = 1,y)\\right]"
    ),
    md = "r_x : x",
    case_reasoning = TRUE,
    uses = c("GbC", "P", "CbC"),
    variants = c(
      "r_x = 0" = "p(z|r_x = 0,y)\\left(p(y) - p(r_x = 1,y)\\right)",
      "r_x = 1" = "p(r_x = 1,y)p(z|r_x = 1,y)"
    )
  )
  expect_formula(
    "p(y) \n p(y,r_x = 1) \n p(z|y,r_x)",
    "p(z)",
    graph,
    paste0(
      "\\sum_{r_x,y}p(z|r_x,y)\\left[\\left(p(y) - p(r_x = 1,y)\\right) \\vee ",
      "p(r_x = 1,y)\\right]"
    ),
    md = "r_x : x",
    case_reasoning = TRUE,
    uses = c("GbC", "P", "CbC"),
    variants = stats::setNames(
      paste0(
        "\\sum_{y}\\left(p(z|r_x = 0,y)\\left(p(y) - p(r_x = 1,y)\\right) + ",
        "p(z|r_x = 1,y)p(r_x = 1,y)\\right)"
      ),
      ""
    )
  )
  # General-by-case reasoning in LDAGs (rules 6 and 7)
  graph <- "y -> x : z = 0 \n z -> x"
  expect_formula(
    "p(y) \n p(y,z=1) \n p(x|y,z=0)",
    "p(x,y,z=0)",
    graph,
    "p(x|y,z = 0)\\left(p(y) - p(y,z = 1)\\right)",
    rules = c(6, 2, -2),
    uses = c("GbC", "P")
  )
  expect_formula(
    "p(y) \n p(y,z=0) \n p(x|y,z=1)",
    "p(x,y,z=1)",
    graph,
    "p(x|y,z = 1)\\left(p(y) - p(y,z = 0)\\right)",
    rules = c(7, -7, 2, -2),
    uses = c("GbC", "P")
  )
  expect_formula(
    "p(y) \n p(y,z=1) \n p(x|y,z=0) \n p(x|y,z=1)",
    "p(x)",
    graph,
    paste0(
      "\\sum_{y,z}\\left[p(y,z = 1)p(x|y,z = 1) \\vee p(x|y,z = 0)\\left(",
      "p(y) - p(y,z = 1)\\right)\\right]"
    ),
    uses = c("GbC", "P", "CbC"),
    variants = stats::setNames(
      paste0(
        "\\sum_{y}\\left(p(x|y,z = 0)\\left(p(y) - p(y,z = 1)\\right) + ",
        "p(y,z = 1)p(x|y,z = 1)\\right)"
      ),
      ""
    )
  )
  # A case of case-by-case reasoning that is a difference is enclosed in
  # parentheses, so that it is not read as a difference of the cases
  expect_formula(
    "p(y) \n p(y,r_x = 1)",
    "p(y,r_x)",
    "y -> z \n y -> r_x",
    "\\left[\\left(p(y) - p(r_x = 1,y)\\right) \\vee p(r_x = 1,y)\\right]",
    md = "r_x : x",
    rules = c(12, 11, -11),
    uses = c("GbC", "CbC"),
    variants = c(
      "r_x = 0" = "p(y) - p(r_x = 1,y)",
      "r_x = 1" = "p(r_x = 1,y)"
    )
  )
  expect_formula(
    "p(y) \n p(y,z=1)",
    "p(y,z)",
    graph,
    "\\left[p(y,z = 1) \\vee \\left(p(y) - p(y,z = 1)\\right)\\right]",
    rules = c(6, 5, -5),
    uses = c("GbC", "CbC"),
    variants = c("z = 0" = "p(y) - p(y,z = 1)", "z = 1" = "p(y,z = 1)")
  )
})

test_that("variants of formulas assign the values of mechanisms", {
  set.seed(13)
  # A summation over the mechanism of case-by-case reasoning is written out as
  # a sum of the cases, and there is a single variant
  expect_formula(
    "p(y*,r_y) \n p(y|r_y = 0)",
    "p(y)",
    "y -> r_y",
    paste0(
      "\\sum_{r_y}\\left[p(r_y = 0)p(y|r_y = 0) \\vee p(r_y = 1,y^{\\ast})",
      "\\right]"
    ),
    md = "r_y : y",
    case_reasoning = TRUE,
    uses = "CbC",
    variants = stats::setNames(
      "p(r_y = 0)p(y|r_y = 0) + p(r_y = 1,y^{\\ast})",
      ""
    )
  )
  # The written sum is the denominator of conditioning
  expect_formula(
    "p(y*,r_y) \n p(y)",
    "p(r_y = 1|y)",
    "y -> r_y",
    paste0(
      "\\frac{p(r_y = 1,y^{\\ast})}{\\sum_{r_y'}\\left[\\left(p(y) - ",
      "p(r_y' = 1,y^{\\ast})\\right) \\vee p(r_y' = 1,y^{\\ast})\\right]}"
    ),
    md = "r_y : y",
    case_reasoning = TRUE,
    uses = "CbC",
    variants = stats::setNames(
      paste0(
        "\\frac{p(r_y = 1,y^{\\ast})}{p(y) - p(r_y = 1,y^{\\ast}) + ",
        "p(r_y = 1,y^{\\ast})}"
      ),
      ""
    )
  )
  # A difference in a written sum is enclosed in parentheses in a product
  expect_formula(
    "p(x*,y*,r_x,r_y) \n p(y)",
    "p(y|r_x = 0)",
    "x -> y \n y -> r_y \n r_y -> r_x",
    paste0(
      "\\frac{\\sum_{r_y}p(r_x = 0|r_y)\\left[\\left(p(y) - ",
      "p(r_y = 1,y^{\\ast})\\right) \\vee p(r_y = 1,y^{\\ast})\\right]}{",
      "p(r_x = 0)}"
    ),
    md = "r_x : x, r_y : y",
    case_reasoning = TRUE,
    uses = "CbC",
    variants = stats::setNames(
      paste0(
        "\\frac{p(r_x = 0|r_y = 0)\\left(p(y) - p(r_y = 1,y^{\\ast})",
        "\\right) + p(r_x = 0|r_y = 1)p(r_y = 1,y^{\\ast})}{p(r_x = 0)}"
      ),
      ""
    )
  )
  # A mechanism that is free in the formula gives a variant for each value
  expect_formula(
    "p(y,r_x = 1|z) \n p(y,r_x = 0|z)",
    "p(y,r_x|do(z))",
    "z -> y \n x -> r_x",
    "\\left[p(r_x = 0,y|z) \\vee p(r_x = 1,y|z)\\right]",
    md = "r_x : x",
    case_reasoning = TRUE,
    uses = "CbC",
    variants = c(
      "r_x = 0" = "p(r_x = 0,y|z)",
      "r_x = 1" = "p(r_x = 1,y|z)"
    )
  )
  # When the case of a value is derived from a single input distribution, its
  # conditional distribution is given as such in the variant
  expect_formula(
    "p(x*,y,r_x = 1) \n p(x,y,r_x = 0)",
    "p(y|x,r_x)",
    "x -> y \n x -> r_x",
    paste0(
      "\\frac{\\left[p(x,r_x = 0,y) \\vee p(r_x = 1,x^{\\ast},y)\\right]}{",
      "\\sum_{y'}\\left[p(x,r_x = 0,y') \\vee p(r_x = 1,x^{\\ast},y')\\right]}"
    ),
    md = "r_x : x",
    case_reasoning = TRUE,
    uses = "CbC",
    variants = c(
      "r_x = 0" = "p(y|x,r_x = 0)",
      "r_x = 1" = "p(y|r_x = 1,x^{\\ast})"
    )
  )
  # Both summations of the general formula are written out
  expect_formula(
    "p(x*,y,r_x = 1) \n p(x,y,r_x = 0)",
    "p(y|x)",
    "x -> y \n x -> r_x",
    paste0(
      "\\frac{\\sum_{r_x}\\left[p(x,r_x = 0,y) \\vee p(r_x = 1,x^{\\ast},y)",
      "\\right]}{\\sum_{y'}\\sum_{r_x}\\left[p(x,r_x = 0,y') \\vee ",
      "p(r_x = 1,x^{\\ast},y')\\right]}"
    ),
    md = "r_x : x",
    case_reasoning = TRUE,
    uses = "CbC",
    variants = stats::setNames(
      paste0(
        "\\frac{p(x,r_x = 0,y) + p(r_x = 1,x^{\\ast},y)}{\\sum_{y'}\\left(",
        "p(x,r_x = 0,y') + p(r_x = 1,x^{\\ast},y')\\right)}"
      ),
      ""
    )
  )
  # Nested case-by-case reasoning over two mechanisms gives a variant for each
  # combination of their values
  expect_formula(
    paste0(
      "p(x*,y*,r_x = 1,r_y = 1) \n p(x,y*,r_x = 0,r_y = 1) \n ",
      "p(x*,y,r_x = 1,r_y = 0) \n p(x,y,r_x = 0,r_y = 0)"
    ),
    "p(y|x,r_x,r_y)",
    "x -> y \n x -> r_x \n y -> r_y",
    paste0(
      "\\frac{\\left[\\left[p(x,r_x = 0,y,r_y = 0) \\vee ",
      "p(r_x = 1,x^{\\ast},y,r_y = 0)\\right] \\vee \\left[",
      "p(x,r_x = 0,r_y = 1,y^{\\ast}) \\vee ",
      "p(r_x = 1,x^{\\ast},r_y = 1,y^{\\ast})\\right]\\right]}{\\sum_{y'}",
      "\\left[\\left[p(x,r_x = 0,y',r_y = 0) \\vee ",
      "p(r_x = 1,x^{\\ast},y',r_y = 0)\\right] \\vee \\left[",
      "p(x,r_x = 0,r_y = 1,y^{\\ast\\prime}) \\vee ",
      "p(r_x = 1,x^{\\ast},r_y = 1,y^{\\ast\\prime})\\right]\\right]}"
    ),
    md = "r_x : x, r_y : y",
    case_reasoning = TRUE,
    uses = "CbC",
    variants = c(
      "r_x = 0, r_y = 0" = "p(y|x,r_x = 0,r_y = 0)",
      "r_x = 0, r_y = 1" = "p(y^{\\ast}|x,r_x = 0,r_y = 1)",
      "r_x = 1, r_y = 0" = "p(y|r_x = 1,x^{\\ast},r_y = 0)",
      "r_x = 1, r_y = 1" = "p(y^{\\ast}|r_x = 1,x^{\\ast},r_y = 1)"
    )
  )
  # A summation over a mechanism and other variables keeps the summation over
  # the other variables, and a mechanism that is assigned a value wherever it
  # appears does not distinguish the variants
  expect_formula(
    paste0(
      "p(x*,r_x = 1,y*,r_y = 1|do(w))\np(w,y*,r_y,x*,r_x = 1)\n",
      "p(y*,r_y = 0,x*,r_x = 1|do(w))"
    ),
    "p(r_x,x,y|do(w))",
    "y -> x\ny -> w\nw -> r_x\nw -> r_y\nr_x -> r_y",
    paste0(
      "\\left[\\left(p(x^{\\ast},y^{\\ast}|do(w),r_x = 1,r_y = 1) - ",
      "p(x^{\\ast},y^{\\ast}|do(w),r_x = 1,r_y = 1)",
      "\\sum_{x^{\\ast\\prime},r_y',y^{\\ast\\prime}}\\left[",
      "p(r_x = 1,x^{\\ast\\prime},r_y' = 0,y^{\\ast\\prime}|do(w)) \\vee ",
      "p(r_x = 1,x^{\\ast\\prime},r_y' = 1,y^{\\ast\\prime}|do(w))\\right]",
      "\\right) \\vee p(x^{\\ast},y^{\\ast}|do(w),r_x = 1,r_y = 1)",
      "\\sum_{x^{\\ast\\prime},r_y',y^{\\ast\\prime}}\\left[",
      "p(r_x = 1,x^{\\ast\\prime},r_y' = 0,y^{\\ast\\prime}|do(w)) \\vee ",
      "p(r_x = 1,x^{\\ast\\prime},r_y' = 1,y^{\\ast\\prime}|do(w))\\right]",
      "\\right]"
    ),
    md = "r_x : x, r_y : y",
    case_reasoning = TRUE,
    uses = "CbC",
    variants = c(
      "r_x = 0" = paste0(
        "p(x^{\\ast},y^{\\ast}|do(w),r_x = 1,r_y = 1) - ",
        "p(x^{\\ast},y^{\\ast}|do(w),r_x = 1,r_y = 1)",
        "\\sum_{x^{\\ast\\prime},y^{\\ast\\prime}}\\left(",
        "p(r_x = 1,x^{\\ast\\prime},r_y = 0,y^{\\ast\\prime}|do(w)) + ",
        "p(r_x = 1,x^{\\ast\\prime},r_y = 1,y^{\\ast\\prime}|do(w))\\right)"
      ),
      "r_x = 1" = paste0(
        "p(x^{\\ast},y^{\\ast}|do(w),r_x = 1,r_y = 1)",
        "\\sum_{x^{\\ast\\prime},y^{\\ast\\prime}}\\left(",
        "p(r_x = 1,x^{\\ast\\prime},r_y = 0,y^{\\ast\\prime}|do(w)) + ",
        "p(r_x = 1,x^{\\ast\\prime},r_y = 1,y^{\\ast\\prime}|do(w))\\right)"
      )
    )
  )
  # Without case-by-case reasoning, there are no variants
  out <- dosearch(
    "p(y,r_x = 1|z) \n p(y|z)",
    "p(y,r_x = 0|do(z))",
    "z -> y \n x -> r_x",
    missing_data = "r_x : x",
    case_reasoning = TRUE
  )
  expect_null(out$variants)
  expect_identical(get_formula(out, variants = TRUE), out$formula)
})

test_that("variants of formulas assign the values of context variables", {
  set.seed(14)
  # The summation over the context variable of fig 1(e) of Tikka et al. (2019)
  # is written out
  expect_formula(
    "p(A,X,Y)",
    "p(Y|X,I_X=1)",
    paste0(
      "I_X -> X\nX -> Y : A = 1\nL -> X : I_X = 1; A = 0\nL -> Y\n",
      "A -> X : I_X = 1\nA -> Y"
    ),
    "\\sum_{A}\\left[p(A = 0)p(Y|X,A = 0) \\vee p(A = 1)p(Y|A = 1)\\right]",
    control = list(heuristic = TRUE),
    uses = "CbC",
    variants = stats::setNames(
      "p(A = 0)p(Y|X,A = 0) + p(A = 1)p(Y|A = 1)",
      ""
    )
  )
  # Summations in the cases remain in the written sum
  expect_formula(
    "p(X,Y,Z,A,W)",
    "p(Y|X,I_X=1)",
    paste0(
      "I_X -> X\nI_Z -> Z\nA -> W\nZ -> Y\nA -> Z\nX -> Z : I_Z = 1; A = 1\n",
      "X -> Y : A = 0\nW -> X : I_X = 1\nW -> Y : A = 0\nA -> Y\n",
      "U -> X : I_X = 1\nU -> Y : A = 1"
    ),
    paste0(
      "\\sum_{A}\\left[p(A = 0)\\sum_{Z}p(Z|X,A = 0)\\sum_{X'}p(X'|A = 0)",
      "p(Y|X',Z,A = 0) \\vee \\sum_{W}p(Y|X,W,A = 1)p(W)p(A = 1|X,W)\\right]"
    ),
    control = list(heuristic = TRUE),
    uses = "CbC",
    variants = stats::setNames(
      paste0(
        "p(A = 0)\\sum_{Z}p(Z|X,A = 0)\\sum_{X'}p(X'|A = 0)p(Y|X',Z,A = 0) + ",
        "\\sum_{W}p(Y|X,W,A = 1)p(W)p(A = 1|X,W)"
      ),
      ""
    )
  )
  # A context variable that is free in the formula gives a variant for each
  # value, also for rule -5
  expect_formula(
    "p(x,z=0) \n p(x,z=1)",
    "p(x,z)",
    "x -> y : w = 0 \n w -> y",
    "\\left[p(x,z = 0) \\vee p(x,z = 1)\\right]",
    rules = 5,
    uses = "CbC",
    variants = c(
      "z = 0" = "p(x,z = 0)",
      "z = 1" = "p(x,z = 1)"
    )
  )
  expect_formula(
    "p(x,z=1) \n p(x,z=0)",
    "p(x,z)",
    "x -> y : w = 0 \n w -> y",
    "\\left[p(x,z = 1) \\vee p(x,z = 0)\\right]",
    rules = -5,
    uses = "CbC",
    variants = c(
      "z = 0" = "p(x,z = 0)",
      "z = 1" = "p(x,z = 1)"
    )
  )
  # The conditional distributions of the cases are given as such
  expect_formula(
    "p(x,z=0) \n p(x,z=1)",
    "p(x|z)",
    "x -> y : w = 0 \n w -> y",
    paste0(
      "\\frac{\\left[p(x,z = 0) \\vee p(x,z = 1)\\right]}{\\sum_{x'}\\left[",
      "p(x',z = 0) \\vee p(x',z = 1)\\right]}"
    ),
    rules = c(5, 1),
    uses = c("CbC", "C"),
    variants = c(
      "z = 0" = "p(x|z = 0)",
      "z = 1" = "p(x|z = 1)"
    )
  )
})

test_that("formulas of random DAG problems are correct and simplified", {
  set.seed(10)
  res <- check_random_problems(
    150L,
    function() random_dag_problem(sample(4:6, 1L)),
    function() {
      list(heuristic = stats::runif(1L) < 0.5, improve = stats::runif(1L) < 0.8)
    }
  )
  expect_length(res$formulas, 150L)
  expect_identical(res$issues, character(0L))
  # The problems cover marginalization and conditioning after each kind of
  # step and explicit summations and quotients
  counts <- rule_counts(res)
  for (r in c("M", "C", "P", "R1", "R2", "R3")) {
    expect_gte(counts[[r]], 5L, label = paste("problems using", r))
  }
  expect_gte(sum(grepl("\\sum_", res$formulas, fixed = TRUE)), 10L)
  expect_gte(sum(grepl("\\frac", res$formulas, fixed = TRUE)), 5L)
})

md_rules <- c(4, 5, 9, 10, -1, -2, -3, 2, 3, 6, -6, 7, -7, 8, -8)

# Random control arguments for missing data problems. With improve = FALSE,
# all rules are used, and without rule 5 or 4, conditioning or
# marginalization over mechanisms requires divisions.
random_md_control <- function() {
  co <- list(
    heuristic = stats::runif(1L) < 0.5,
    benchmark = TRUE,
    time_limit = 1 / 3600
  )
  rules <- sample(
    c("default", "all", "no5", "no4"),
    1L,
    prob = c(0.5, 0.1, 0.25, 0.15)
  )
  if (rules == "all") co$improve <- FALSE
  if (rules == "no5") co$rules <- setdiff(md_rules, 5)
  if (rules == "no4") co$rules <- setdiff(md_rules, 4)
  co
}

# Coverage of the steps and of compact and explicit divisions
expect_md_coverage <- function(res, minimum) {
  counts <- rule_counts(res)
  for (r in c("M", "C", "D", "P", "R1", "A", "EX")) {
    expect_gte(counts[[r]], minimum, label = paste("problems using", r))
  }
  division <- vapply(res$rules, function(r) "D" %in% r, logical(1L))
  fraction <- grepl("\\frac", res$formulas, fixed = TRUE)
  expect_gte(sum(division & !fraction), minimum, label = "compact divisions")
  expect_gte(sum(division & fraction), minimum, label = "explicit divisions")
}

test_that("formulas are correct with two partially observed variables", {
  set.seed(11)
  res <- check_random_problems(
    100L,
    function() random_md_problem(2L, sample(0:3, 1L)),
    random_md_control
  )
  expect_length(res$formulas, 100L)
  expect_identical(res$issues, character(0L))
  expect_md_coverage(res, 3L)
})

test_that("formulas are correct with three partially observed variables", {
  set.seed(12)
  res <- check_random_problems(
    60L,
    function() random_md_problem(3L, 0L),
    random_md_control
  )
  expect_length(res$formulas, 60L)
  expect_identical(res$issues, character(0L))
  expect_md_coverage(res, 2L)
})

# A random missing data problem for case-by-case reasoning. An input
# distribution with a mechanism is replaced by its distributions for both
# values of the mechanism, where the true variable is observed when the
# mechanism has the value 0, as in a follow-up study. A mechanism is also
# added to the left-hand side of the query with probability 0.5, so that it
# is free in the formula.
random_case_problem <- function() {
  problem <- random_md_problem(rand_int(2L, 3L), rand_int(1L, 2L))
  mech <- parse_test_md(problem$md)
  inputs <- strsplit(problem$data, "\n", fixed = TRUE)[[1L]]
  token <- function(v) paste0("(?<=[(,|])", v, "(?=[,|)])")
  for (i in sample(seq_along(inputs))) {
    found <- vapply(mech, function(r) {
      grepl(token(paste0(r, "( = [01])?")), inputs[i], perl = TRUE)
    }, logical(1L))
    if (!any(found)) next
    x <- pick(names(mech)[found], 1L)
    r <- mech[[x]]
    case <- function(value) {
      gsub(
        token(paste0(r, "( = [01])?")),
        paste(r, "=", value),
        inputs[i],
        perl = TRUE
      )
    }
    zero <- gsub(token(paste0(x, "\\*")), x, case(0L), perl = TRUE)
    inputs <- c(inputs[-i], case(1L), zero)
    break
  }
  problem$data <- paste(unique(inputs), collapse = "\n")
  r <- pick(unname(mech), 1L)
  target <- parse_test_distribution(problem$query)
  if (stats::runif(1L) < 0.5 && !(r %in% target$label)) {
    problem$query <- sub("^p\\(", paste0("p(", r, ","), problem$query)
  }
  problem
}

test_that("variants are correct in random case-by-case problems", {
  set.seed(15)
  res <- check_random_problems(
    60L,
    random_case_problem,
    function() list(heuristic = stats::runif(1L) < 0.5, time_limit = 1 / 3600),
    case_reasoning = TRUE
  )
  expect_length(res$formulas, 60L)
  expect_identical(res$issues, character(0L))
  variants <- Filter(Negate(is.null), res$variants)
  named <- vapply(variants, function(v) any(nzchar(names(v))), logical(1L))
  written <- vapply(variants, function(v) {
    any(grepl(" + ", v, fixed = TRUE))
  }, logical(1L))
  expect_gte(length(variants), 10L)
  expect_gte(sum(named), 3L)
  expect_gte(sum(written), 3L)
})
