#' Plot the Formula of a Causal Query
#'
#' `plot_formula` draws the formula of an object of class `dosearch` on the
#' current graphics device with the mathematics rendered, e.g., summations
#' with their limits, fractions, subscripts, superscripts and primes. The
#' formula is converted to a [grDevices::plotmath] expression, so no LaTeX
#' installation is needed.
#'
#' The formula is drawn on a single line without line breaks, and the text is
#' shrunk to fit the plot region, so the function is intended for formulas of
#' moderate length. Formulas longer than `max_chars` characters (as given by
#' [dosearch::get_formula()]) are not plotted.
#'
#' The name of a variable is split at its first underscore into a name and a
#' subscript, e.g., `r_x` is drawn as \eqn{r_x}. Proxy variables are drawn
#' with an asterisk as a superscript, and primes as prime marks, e.g.,
#' `y^{\ast\prime}` is drawn as \eqn{y^{*\prime}}. A variable name that
#' contains other characters than ASCII letters, digits, periods and
#' underscores, or that begins or ends with an underscore, is cleaned for
#' plotting: non-ASCII characters are transliterated, the other characters are
#' replaced by periods, and the underscores at the beginning and the end are
#' removed. A warning lists the names that were changed. The formula itself
#' is not changed.
#'
#' The mathematical symbols are drawn with the symbol font of the graphics
#' device. The [grDevices::pdf()] device does not embed this font in the file,
#' so PDF viewers may substitute another font, which can affect the spacing
#' around the symbols. The [grDevices::cairo_pdf()] device embeds the fonts.
#'
#' @export
#' @param x An object of class `dosearch`.
#' @param variants A `logical` value. If `TRUE`, the variants of the formula
#'   for the values of the variables of case-by-case reasoning (see
#'   `variants` in the value of [dosearch::dosearch()]) are drawn one below
#'   the other, each preceded by the values of the variables. If the formula
#'   has no variants, the formula itself is drawn. The default is `FALSE`.
#' @param max_chars The maximum number of characters of a formula to plot.
#'   The default is 500.
#' @param cex A `numeric` value giving the character expansion of the text.
#'   The text is shrunk further if it does not fit the plot region. The
#'   default is 1.5.
#' @param ... Additional arguments passed to [graphics::text()], e.g., `col`
#'   or `family`. The variants are drawn as a left-aligned block, so `adj`
#'   cannot be given.
#' @return The plotted formulas as an `expression` vector (invisibly), which
#'   can also be drawn by other functions that support
#'   [grDevices::plotmath], or `NULL` (invisibly) if no formula is available.
#' @examples
#' data <- "p(x,y,z)"
#' query <- "p(y|do(x))"
#' graph <- "
#'   x -> z
#'   z -> y
#'   x <-> y
#' "
#' x <- dosearch(data, query, graph)
#' plot_formula(x)
#'
#' # The variants of a formula derived by case-by-case reasoning
#' x <- dosearch(
#'   "p(y*,r_y = 1) \n p(y,r_y = 0)",
#'   "p(y|r_y)",
#'   "y -> r_y",
#'   missing_data = "r_y : y",
#'   case_reasoning = TRUE
#' )
#' plot_formula(x, variants = TRUE)
#'
plot_formula <- function(x, variants = FALSE, max_chars = 500L, cex = 1.5,
                         ...) {
  if (!is_dosearch(x)) {
    stop_("Argument `x` must be an object of class `dosearch`.")
  }
  if (!is.logical(variants) || length(variants) != 1L || is.na(variants)) {
    stop_("Argument `variants` must be either `TRUE` or `FALSE`.")
  }
  if (!is.numeric(max_chars) || length(max_chars) != 1L ||
      is.na(max_chars) || max_chars < 1) {
    stop_("Argument `max_chars` must be a positive number.")
  }
  if (!is.numeric(cex) || length(cex) != 1L || is.na(cex) || cex <= 0) {
    stop_("Argument `cex` must be a positive number.")
  }
  formulas <- get_formula(x, variants = variants)
  if (is.null(formulas) || !any(nzchar(formulas))) {
    message("No formula is available to plot.")
    return(invisible(NULL))
  }
  chars <- nchar(formulas)
  if (any(chars > max_chars)) {
    stop_(
      "The formula has ", max(chars), " characters, ",
      "which exceeds `max_chars` (", max_chars, "). ",
      "Formulas this long may not be legible when plotted."
    )
  }
  exprs <- formula_expressions(formulas)
  op <- graphics::par(mar = c(0, 0, 0, 0))
  on.exit(graphics::par(op))
  graphics::plot.new()
  n <- length(exprs)
  width <- max(graphics::strwidth(exprs, units = "user", cex = cex))
  height <- max(graphics::strheight(exprs, units = "user", cex = cex))
  scale <- min(1, 0.96 / width, 0.8 / (n * height))
  y <- (rev(seq_len(n)) - 0.5) / n
  # Several variants are aligned to the left of a centered block
  left <- 0.5 - scale * width / 2
  graphics::text(
    if (n > 1L) left else 0.5,
    y,
    exprs,
    cex = cex * scale,
    adj = c(if (n > 1L) 0 else 0.5, 0.5),
    ...
  )
  invisible(exprs)
}

#' Convert Formulas to Plotmath Expressions
#'
#' The variable names are cleaned (see `clean_names`), and a warning lists
#' the names that were changed. A formula with a nonempty name, i.e., a
#' variant of a formula, is preceded by the values of its name.
#'
#' @param formulas A `character` vector of formulas given by `dosearch`,
#'   possibly named by the values of the variants (see `get_formula`).
#' @return An `expression` vector.
#' @noRd
formula_expressions <- function(formulas) {
  trees <- lapply(unname(formulas), formula_tree)
  values <- lapply(as.character(names(formulas)), value_items)
  labels <- unique(c(
    unlist(lapply(trees, tree_labels)),
    unlist(lapply(values, function(v) v$label))
  ))
  bases <- unique(vapply(labels, function(l) label_parts(l)$base, ""))
  cleaned <- clean_names(bases)
  changed <- cleaned != bases
  if (any(changed)) {
    warning_(
      "Variable names were changed for plotting: ",
      cs(paste0("`", bases[changed], "` -> `", cleaned[changed], "`"))
    )
  }
  map <- cleaned
  names(map) <- bases
  exprs <- lapply(seq_along(trees), function(i) {
    e <- tree_expression(trees[[i]], map)
    if (length(values) > 0L && length(values[[i]]$label) > 0L) {
      v <- values_expression(values[[i]], map)
      e <- bquote(.(v) * ":" ~ ~ .(e))
    }
    e
  })
  as.expression(exprs)
}

#' Clean Variable Names for Plotting
#'
#' Non-ASCII characters are transliterated, characters other than ASCII
#' letters, digits, periods and underscores are replaced by periods, and
#' underscores at the beginning and the end of a name are removed. Names that
#' become empty are replaced by `v`, and the changed names are made unique
#' among all names.
#'
#' @param x A `character` vector of unique variable names.
#' @return A `character` vector of the cleaned names.
#' @noRd
clean_names <- function(x) {
  y <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT", sub = "?")
  y[is.na(y)] <- gsub("[^ -~]", ".", x[is.na(y)], useBytes = TRUE)
  y <- gsub("[^A-Za-z0-9._]", ".", y)
  y <- gsub("^_+|_+$", "", y)
  y[!nzchar(y)] <- "v"
  changed <- y != x
  if (any(changed)) {
    unique_names <- make.unique(c(y[!changed], y[changed]), sep = ".")
    y[changed] <- unique_names[seq.int(sum(!changed) + 1L, length(y))]
  }
  y
}

#' Split Text at Separators That Are Not Enclosed in Brackets
#'
#' @param text A `character` string.
#' @param sep A single separator character.
#' @noRd
split_top <- function(text, sep) {
  chars <- strsplit(text, "", fixed = TRUE)[[1L]]
  depth <- cumsum(chars %in% c("(", "{")) - cumsum(chars %in% c(")", "}"))
  cuts <- which(chars == sep & depth == 0L)
  starts <- c(1L, cuts + 1L)
  ends <- c(cuts - 1L, length(chars))
  substring(text, starts, ends)
}

#' Parse the Items of a Distribution in a Formula
#'
#' An item is a label with an optional value, e.g., `r_x = 1`, or the
#' actions `do(...)`.
#'
#' @param text A `character` string of comma-separated items.
#' @noRd
parse_formula_items <- function(text) {
  lapply(split_top(text, ","), function(item) {
    if (startsWith(item, "do(") && endsWith(item, ")")) {
      list(
        type = "do",
        items = parse_formula_items(substr(item, 4L, nchar(item) - 1L))
      )
    } else {
      kv <- strsplit(item, " = ", fixed = TRUE)[[1L]]
      if (!(length(kv) %in% 1:2) || !all(nzchar(kv)) ||
          endsWith(item, " = ")) {
        stop_("Unable to plot the formula: invalid item ", item, ".")
      }
      list(type = "label", label = kv[1L], value = kv[2L])
    }
  })
}

#' Parse the Values of a Variant of a Formula
#'
#' @param name The name of a variant, e.g., `"r_x = 0, r_y = 1"`.
#' @noRd
value_items <- function(name) {
  if (!nzchar(name)) {
    return(list(label = character(0L), value = character(0L)))
  }
  kv <- strsplit(strsplit(name, ", ", fixed = TRUE)[[1L]], " = ", fixed = TRUE)
  list(
    label = vapply(kv, "[[", character(1L), 1L),
    value = vapply(kv, "[[", character(1L), 2L)
  )
}

#' Parse a Formula into a Tree
#'
#' A formula is a difference or a sum of products, whose factors are
#' distributions `p(...)`, the constant 1, fractions `\frac{.}{.}`,
#' expressions in parentheses `\left(.\right)`, the cases of case-by-case
#' reasoning `\left[. \vee .\right]` and summations `\sum_{.}`, which apply to
#' the factors that follow them in the same product.
#'
#' @param formula A `character` string of a formula given by `dosearch`.
#' @noRd
formula_tree <- function(formula) {
  pos <- 1L
  len <- nchar(formula)
  at <- function(token) {
    substr(formula, pos, pos + nchar(token) - 1L) == token
  }
  fail <- function() {
    stop_(
      "Unable to plot the formula: unexpected input at position ", pos,
      " of ", formula
    )
  }
  expect <- function(token) {
    if (!at(token)) fail()
    pos <<- pos + nchar(token)
  }
  # The content up to the bracket that closes the bracket before pos
  group <- function(open, close) {
    depth <- 1L
    start <- pos
    while (depth > 0L) {
      if (pos > len) fail()
      ch <- substr(formula, pos, pos)
      if (ch == open) depth <- depth + 1L
      if (ch == close) depth <- depth - 1L
      pos <<- pos + 1L
    }
    substr(formula, start, pos - 2L)
  }
  terms <- function() {
    node <- product()
    while (at(" - ") || at(" + ")) {
      op <- substr(formula, pos + 1L, pos + 1L)
      pos <<- pos + 3L
      node <- list(type = "binary", op = op, a = node, b = product())
    }
    node
  }
  product <- function() {
    factors <- list()
    repeat {
      if (pos > len || at("}") || at(" - ") || at(" + ") ||
          at("\\right") || at(" \\vee ")) {
        break
      }
      if (at("\\sum_{")) {
        pos <<- pos + 6L
        vars <- parse_formula_items(group("{", "}"))
        factors[[length(factors) + 1L]] <- list(
          type = "sum",
          vars = vars,
          body = product()
        )
        break
      }
      if (at("\\frac{")) {
        pos <<- pos + 6L
        num <- terms()
        expect("}{")
        den <- terms()
        expect("}")
        factors[[length(factors) + 1L]] <- list(
          type = "frac",
          num = num,
          den = den
        )
      } else if (at("\\left(")) {
        pos <<- pos + 6L
        inner <- terms()
        expect("\\right)")
        factors[[length(factors) + 1L]] <- list(type = "paren", inner = inner)
      } else if (at("\\left[")) {
        pos <<- pos + 6L
        a <- terms()
        expect(" \\vee ")
        b <- terms()
        expect("\\right]")
        factors[[length(factors) + 1L]] <- list(type = "case", a = a, b = b)
      } else if (at("p(")) {
        pos <<- pos + 2L
        parts <- split_top(group("(", ")"), "|")
        factors[[length(factors) + 1L]] <- list(
          type = "prob",
          lhs = parse_formula_items(parts[1L]),
          rhs = if (length(parts) > 1L) parse_formula_items(parts[2L])
        )
      } else if (at("1")) {
        pos <<- pos + 1L
        factors[[length(factors) + 1L]] <- list(type = "one")
      } else {
        fail()
      }
    }
    if (length(factors) == 0L) fail()
    list(type = "product", factors = factors)
  }
  tree <- terms()
  if (pos <= len) fail()
  tree
}

#' The Labels of the Variables of a Formula Tree
#'
#' @param node A node of a formula tree (see `formula_tree`).
#' @noRd
tree_labels <- function(node) {
  items_labels <- function(items) {
    unlist(lapply(items, function(item) {
      if (item$type == "do") items_labels(item$items) else item$label
    }))
  }
  switch(node$type,
    binary = c(tree_labels(node$a), tree_labels(node$b)),
    product = unlist(lapply(node$factors, tree_labels)),
    sum = c(items_labels(node$vars), tree_labels(node$body)),
    frac = c(tree_labels(node$num), tree_labels(node$den)),
    paren = tree_labels(node$inner),
    case = c(tree_labels(node$a), tree_labels(node$b)),
    prob = c(items_labels(node$lhs), items_labels(node$rhs)),
    character(0L)
  )
}

#' Split the Label of a Variable into Its Name, Primes and Proxy Marker
#'
#' Labels are of the form `x`, `x'`, `x^{(4)}`, `x^{\ast}`,
#' `x^{\ast\prime}` and `x^{\ast(4)}`.
#'
#' @param label A `character` string.
#' @noRd
label_parts <- function(label) {
  proxy <- regmatches(
    label,
    regexec("^(.+)\\^\\{\\\\ast((\\\\prime)*|\\(([0-9]+)\\))\\}$", label)
  )[[1L]]
  if (length(proxy) > 0L) {
    primes <- if (nzchar(proxy[5L])) {
      as.integer(proxy[5L])
    } else {
      nchar(proxy[3L]) %/% nchar("\\prime")
    }
    return(list(base = proxy[2L], primes = primes, proxy = TRUE))
  }
  sup <- regmatches(
    label,
    regexec("^(.+)\\^\\{\\(([0-9]+)\\)\\}$", label)
  )[[1L]]
  if (length(sup) > 0L) {
    return(list(base = sup[2L], primes = as.integer(sup[3L]), proxy = FALSE))
  }
  primed <- regmatches(label, regexec("^(.*[^'])('{1,3})$", label))[[1L]]
  if (length(primed) > 0L) {
    return(list(base = primed[2L], primes = nchar(primed[3L]), proxy = FALSE))
  }
  list(base = label, primes = 0L, proxy = FALSE)
}

#' The Plotmath Expression of the Label of a Variable
#'
#' @param label A `character` string.
#' @param map A named `character` vector of the cleaned names of the
#'   variables (see `clean_names`).
#' @noRd
label_expression <- function(label, map) {
  parts <- label_parts(label)
  name <- map[[parts$base]]
  i <- regexpr("_", name, fixed = TRUE)
  e <- name
  if (i > 0L) {
    e <- call("[", substr(name, 1L, i - 1L), substr(name, i + 1L, nchar(name)))
  }
  marks <- switch(min(parts$primes, 4L) + 1L,
    NULL,
    as.name("minute"),
    as.name("second"),
    call("*", as.name("second"), as.name("minute")),
    paste0("(", parts$primes, ")")
  )
  if (parts$proxy) {
    sup <- "*"
    if (is.character(marks)) {
      sup <- paste0("*", marks)
    } else if (!is.null(marks)) {
      sup <- call("*", "*", marks)
    }
    e <- call("^", e, sup)
  } else if (is.character(marks)) {
    e <- call("^", e, marks)
  } else if (!is.null(marks)) {
    e <- call("*", e, marks)
  }
  e
}

#' The Plotmath Expression of a Comma-separated List
#'
#' @param exprs A `list` of plotmath expressions.
#' @noRd
list_expression <- function(exprs) {
  if (length(exprs) == 1L) {
    exprs[[1L]]
  } else {
    as.call(c(as.name("list"), exprs))
  }
}

#' The Plotmath Expression of the Items of a Distribution
#'
#' @param items A `list` of items (see `parse_formula_items`).
#' @inheritParams label_expression
#' @noRd
items_expression <- function(items, map) {
  list_expression(lapply(items, function(item) {
    if (item$type == "do") {
      return(call("do", items_expression(item$items, map)))
    }
    e <- label_expression(item$label, map)
    if (!is.na(item$value)) {
      e <- call("==", e, as.numeric(item$value))
    }
    e
  }))
}

#' The Plotmath Expression of the Values of a Variant of a Formula
#'
#' @param values A `list` of labels and values (see `value_items`).
#' @inheritParams label_expression
#' @noRd
values_expression <- function(values, map) {
  list_expression(lapply(seq_along(values$label), function(i) {
    call(
      "==",
      label_expression(values$label[i], map),
      as.numeric(values$value[i])
    )
  }))
}

#' The Plotmath Expression of a Formula Tree
#'
#' @param node A node of a formula tree (see `formula_tree`).
#' @inheritParams label_expression
#' @noRd
tree_expression <- function(node, map) {
  switch(node$type,
    one = 1,
    binary = call(
      node$op,
      tree_expression(node$a, map),
      tree_expression(node$b, map)
    ),
    product = Reduce(
      function(a, b) call("*", a, b),
      lapply(node$factors, tree_expression, map)
    ),
    sum = call(
      "sum",
      tree_expression(node$body, map),
      items_expression(node$vars, map),
      ""
    ),
    frac = call(
      "frac",
      tree_expression(node$num, map),
      tree_expression(node$den, map)
    ),
    paren = call("bgroup", "(", tree_expression(node$inner, map), ")"),
    # The character 218 is the logical or in the symbol font. It is created
    # in the session that draws it, so that it is in the native encoding.
    case = call(
      "bgroup",
      "[",
      call(
        "~",
        call(
          "~",
          tree_expression(node$a, map),
          call("symbol", rawToChar(as.raw(218L)))
        ),
        tree_expression(node$b, map)
      ),
      "]"
    ),
    prob = {
      e <- items_expression(node$lhs, map)
      if (!is.null(node$rhs)) {
        e <- call("*", call("*", e, "|"), items_expression(node$rhs, map))
      }
      call("p", e)
    }
  )
}
