#' Identify a Causal Effect from Arbitrary Experiments And Observations
#'
#' Identify a causal `query` from available `data` in a semi-Markovian
#' causal model described by a `graph` that is a directed acyclic graph
#' (DAG) or a labeled directed acyclic graph (LDAG). In a semi-Markovian causal
#' model, each unobserved variable has exactly two children and they are denoted
#' by bidirected edges. For DAGs, special mechanisms related to
#' transportability of causal effects, recoverability from selection bias and
#' identifiability under missing data can also be included.
#' See 'Details' for the syntax of each argument. Note that all `character`
#' type arguments are case-sensitive.
#'
#' Argument `data` is used to list the available input distributions.
#' When `graph` is a DAG the distributions should be of the form
#' \deqn{P(A_i|do(B_i),C_i)}
#'
#' Individual variables within sets should be separated by a comma.
#' For example, three input distributions:
#' \deqn{P(Z|do(X)), P(W,Y|do(Z,X)), P(W,Y,X|Z)}
#'
#' should be given as follows:
#' \preformatted{
#' > data <- "
#' +  P(Z|do(X))
#' +  P(W,Y|do(Z,X))
#' +  P(W,Y,X|Z)
#' +"
#' }
#'
#' The use of multiple do-operators is not permitted. Furthermore, when both
#' conditioning variables and a do-operator are present, every conditioning
#' variable must either precede the do-operator or follow it. When `graph` is
#' an LDAG, the do-operation is represented by an intervention node, i.e.,
#'
#' \deqn{P(Y|do(X),Z) = P(Y|X,Z,I_X = 1)}
#'
#' For example, in the case of the previous example in an LDAG,
#' the three input distributions become:
#' \preformatted{
#' > data <- "
#' +  P(Z|X,I_X = 1)
#' +  P(W,Y|Z,X,I_X=1,I_Z=1)
#' +  P(W,Y,X|Z)
#' +"
#' }
#'
#' The intervention nodes \eqn{I_X} and \eqn{I_Z} must be explicitly defined in
#' the `graph` along with the relevant labels for the edges.
#'
#' Argument `query` is the target distribution of the search.
#' It has the same syntax as `data`, but only a single distribution should be
#' given. Multiple simultaneous target distributions are not supported.
#'
#' Argument `graph` is a description of a directed acyclic graph where directed
#' edges are denoted by `->` and bidirected arcs corresponding to unobserved
#' confounders are denoted by `<->` (or by `--`). As an example, a DAG with two
#' directed edges and one bidirected edge is constructed as follows:
#' \preformatted{
#' > graph <- "
#' +  X -> Z
#' +  Z -> Y
#' +  X <-> Y
#' +"
#' }
#' Some alternative formats for DAGs are supported as well. Graphs created
#' using the \pkg{igraph} package in the \pkg{causal.effect} package syntax can
#' be used for \pkg{dosearch} as well. DAGs created using the \pkg{dagitty}
#' package are also supported. Note that both time and space complexity of the
#' underlying search algorithm are exponential in the number of vertices,
#' but instances with up to ten nodes are routinely solved in under a second.
#'
#' LDAGs are constructed similarly with the addition of labels and with the
#' omission bidirected edges (latent variables must be explicitly defined).
#' As an example, an LDAG with two labeled edges can be constructed as follows:
#' \preformatted{
#' > graph <- "
#' +  X -> Z : A = 0
#' +  Z -> Y : A = 1
#' +  A -> Z
#' +  A -> Y
#' +"
#' }
#' Here the labels indicate that the edge from \eqn{X} to \eqn{Z} vanishes when
#' \eqn{A} has the value 0 and the edge from \eqn{Z} to \eqn{Y} vanishes when
#' \eqn{A} has the value 1. Multiple labels on the same edge should be
#' separated by a semi-colon, and individual assignments within each label
#' should be separated by a comma.
#'
#' Argument `transportability` enumerates the nodes that should be understood
#' as transportability nodes responsible for discrepancies between domains.
#' Individual variables should be separated by a comma.
#' See e.g., (Bareinboim and Pearl, 2014) for details on transportability.
#'
#' Argument `selection_bias` enumerates the nodes that should be understood as
#' selection bias nodes responsible for bias in the input data sets.
#' Individual variables should be separated by a comma. See e.g.,
#' (Bareinboim and Tian, 2015) for details on selection bias recoverability.
#'
#' Argument `missing_data` enumerates the missingness mechanisms of the model.
#' The syntax for a single mechanism is `M_X : X` where
#' \ifelse{html}{\out{M<sub>X</sub>}}{\eqn{M_X}} is the mechanism for \eqn{X}.
#' Individual mechanisms should be separated by a comma.
#' Note that both \ifelse{html}{\out{M<sub>X</sub>}}{\eqn{M_X}} and \eqn{X}
#' must be present in the graph if the corresponding mechanism is given as
#' input. Proxy variables should not be included in the graph, since they are
#' automatically generated based on \code{missing_data}. By default, a warning
#' is issued if a proxy variable is present in an input distribution but its
#' corresponding mechanism is not present in any input. See e.g.,
#' (Mohan, Pearl and Tian, 2013) for details on missing data as
#' a causal inference problem. Note that `dosearch` is not complete for
#' missing data problems, meaning that if `dosearch` is not able to identify
#' the `query`, it might still be identifiable via some other means.
#'
#' The `control` argument is a list that can supply any of the following
#' components:
#'
#' * `benchmark`: a `logical` value. If `TRUE`, the search time is
#'   recorded and returned (in milliseconds). Defaults to `FALSE`.
#' * `benchmark_rules`: a `logical` value. If `TRUE`, the time taken by each
#'   individual inference rule is also recorded in the benchmark
#'   (in milliseconds). Defaults to `FALSE`.
#' * `draw_derivation`: a `logical` value. If `TRUE`, a string representing
#'   the derivation steps as a DOT graph is returned. If the `DiagrammeR`
#'   package is installed, the DOT graph can be plotted by calling `plot` on
#'   the return object. The DOT graph can also be exported as an .svg file
#'   by using the `DOT` package. Defaults to `FALSE`.
#' * `draw_all`: a `logical` value. If `TRUE` and if `draw_derivation = TRUE`,
#'   the derivation will contain every step taken by the search. If `FALSE`,
#'   only the steps that resulted in an identifiable target are returned.
#'   Defaults to `FALSE`.
#'   `empty`: a `logical` value. If `TRUE`, an empty `dosearch` object is
#'   returned without running the search.
#' * `formula`: a `logical` value. If `TRUE`, a string representing the
#'   identifiable query is returned when the target query is identifiable.
#'   If `FALSE`, only a logical value is returned that takes the value `TRUE`
#'   for an identifiable target and `FALSE` otherwise. Defaults to `TRUE`.
#' * `heuristic`: a `logical` value. If `TRUE`, new distributions are expanded
#'   during the search according to a search heuristic
#'   (see Tikka et al., 2021, for details). Otherwise, distributions are
#'   expanded in the order in which they were identified. Defaults to `FALSE`.
#' * `md_sym`: a single `character` describing the symbol to use for active
#'   missing data mechanisms. Defaults to `"1"`.
#' * `time_limit`: a `numeric` value giving a time limit for the search
#'   (in hours). Defaults to a negative value that disables the time limit.
#' * `verbose`: a `logical` value. If `TRUE`, diagnostic information is printed
#'   to  the console during the search. Defaults to `FALSE`.
#' * `warn`: a `logical` value. If `TRUE`, a warning is issued for possibly
#'   unintentionally misspecified but syntactically correct input distributions.
#'   A warning is also raised if both lower-case and upper-case node or
#'   variable names are used simultaneously in the inputs
#'
#' @export
#' @param data A `character` string describing the available distributions in
#'   the package syntax. Alternatively, a list of character vectors.
#' @param query A `character` string describing the target distribution in the
#'   package syntax. Alternatively, a character vector.
#' @param graph A `character` string describing either a DAG or an LDAG in the
#'   package syntax. Alternatively, an \pkg{igraph} graph as used in the
#'   \pkg{causaleffect} package or a DAG constructed using the \pkg{dagitty}
#'   package.
#' @param transportability A `character` string describing the transportability
#'   nodes of the model in the package syntax (for DAGs only).
#' @param selection_bias A `character` string describing the selection bias
#'   nodes of the model in the package syntax (for DAGs only).
#' @param missing_data A `character` string describing the missing data
#'   mechanisms of the model in the package syntax (for DAGs only).
#' @param control A `list` of control parameters.
#' @return `dosearch` returns an object of class `dosearch` which is a list
#'   with the following components by default. See the `control` options
#'   on how to obtain a graphical representation of the derivation or
#'   how to benchmark the search.
#'
#'   * `identifiable`: a `logical` value that is `TRUE` if the target
#'     quantity is identifiable and `FALSE` otherwise.
#'   * `formula`: a `character` string describing the formula for an
#'     identifiable query or an empty character vector for
#'     a non-identifiable effect.
#'
#' @references
#' S. Tikka, A. Hyttinen, J. Karvanen. "Causal Effect Identification from
#' Multiple Incomplete Data Sources: A General Search-based Approach."
#' \emph{Journal of Statistical Software}, 99(5):1--40, 2021.
#'
#' E. Bareinboim, J. Pearl. "Transportability from Multiple
#' Environments with Limited Experiments: Completeness Results."
#' In \emph{Proceedings of the 27th Annual Conference on
#' Neural Information Processing Systems}, 280--288, 2014.
#'
#' E. Bareinboim, J. Tian. "Recovering Causal Effects from Selection
#' Bias " In \emph{Proceedings of the 29th AAAI Conference on Artificial
#' Intelligence}, 3475--3481, 2015.
#'
#' K. Mohan, J. Pearl, J. Tian. "Graphical Models for Inference with Missing
#' Data." In \emph{Proceedings of the 26th International Conference on
#' Neural Information Processing Systems}, 1277--1285, 2013.
#'
#' @srrstats {NW1.0} Domain of applicability is explained: semi-Markovian
#'   causal models that are described by directed acyclic graphs (DAGs) or
#'   labeled DAGs (LDAGs).
#' @srrstats {NW1.1} Exponential scaling is documented.
#' @srrstats {NW2.0} In addition to the primary `character` interface, both
#'   `igraph` and `dagitty` formats are supported and documented.
#' @srrstats {NW2.1} Expected forms of inputs are explained.
#' @srrstats {NW2.2} Expected forms are asserted and informative errors are
#'   issued.
#' @srrstats {NW2.4} Labels for edges can be included via the package syntax
#'   and their presence is automatically detected without a need for an
#'   additional parameter.
#' @srrstats {NW2.5, NW2.5a} The return object of `dosearch` includes the
#'   original call arguments thus preserving information.
#' @srrstats {NW2.6} All inputs are parsed and validated against expectations.
#' @srrstats {NW2.7} Non-finite or NA values are not allowed and appropriate
#'   errors are issued.
#' @srrstats {NW5.0} Return object includes the input network, preserving
#'   metadata.
#' @srrstats {NW5.1} An empty (in terms of search output) return object
#'   can be obtained via `control = list(empty = TRUE)`.
#' @srrstats {NW5.7} All control arguments are included in the return object.
#' @srrstats {NW6.3} The scalability simulations described in the `dosearch`
#'   paper can be replicated in both small scale and large scale variants and
#'   the code is included in the `rep` directory of the GitHub repo.
#' @examples
#' # A simple back-door formula
#' data1 <- "P(x,y,z)"
#' query1 <- "P(y|do(x))"
#' graph1 <- "
#'   x -> y
#'   z -> x
#'   z -> y
#' "
#' dosearch(data1, query1, graph1)
#'
#' # A simple front-door formula
#' data2 <- "P(x,y,z)"
#' query2 <- "P(y|do(x))"
#' graph2 <- "
#'   x -> z
#'   z -> y
#'   x <-> y
#' "
#' dosearch(data2, query2, graph2)
#'
#' # A scenario with combined transportability and selection bias
#' # in this case using the search heuristic provides a simpler formula
#' data <- "
#'   p(x,z,y|s)
#'   p(y,z|t,do(x))
#' "
#' query <- "p(y|do(x))"
#' graph <- "
#'   x -> z
#'   z -> y
#'   x -> s
#'   t -> z
#'   x <-> y
#' "
#' dosearch(
#'   data,
#'   query,
#'   graph,
#'   transportability = "t",
#'   selection_bias = "s",
#'   control = list(heuristic = TRUE, improve = FALSE)
#' )
#'
#' # A simple case-control design
#' data <- "
#'   p(x*,y*,r_x,r_y)
#'   p(y)
#' "
#' graph <- "
#'   x -> y
#'   y -> r_y
#'   r_y -> r_x
#' "
#' md <- "r_x : x, r_y : y"
#' dosearch(data, query, graph, missing_data = md)
#'
#' # Graph input using 'igraph' in the 'causaleffect' syntax
#' if (requireNamespace("igraph", quietly = TRUE)) {
#'   g_igraph <- igraph::graph_from_literal(
#'     x -+ z, z -+ y, x -+ y, y -+ x,
#'     simplify = FALSE
#'   )
#'   g_igraph <- igraph::set.edge.attribute(g_igraph, "description", 3:4, "U")
#'   dosearch(data2, query2, g_igraph)
#' }
#'
#' # Graph input with 'dagitty'
#' if (requireNamespace("dagitty", quietly = TRUE)) {
#'   g_dagitty <- dagitty::dagitty("dag{x -> z -> y; x <-> y}")
#'   dosearch(data2, query2, g_dagitty)
#' }
#'
#' # Alternative distribution input style using lists and vectors:
#' # Each element of the list describes a single distribution
#' # Each element is a character vector that describes the role
#' # of each variable in the distribution as follows:
#' # For a variable V and a distribution P(A|do(B),C) we have
#' #   V = 0, if V is in A
#' #   V = 1, if V is in B
#' #   V = 2, if V is in C
#' data_alt <- list(
#'   c(x = 0, y = 0, z = 0) # = P(x,y,z)
#' )
#' query_alt <- c(x = 1, y = 0) # = P(y|do(x))
#' dosearch(data_alt, query_alt, graph2)
#'
#' \dontrun{
#' # Additional examples
#' # Multiple input distributions (both observational and interventional)
#' data3 <- "
#'   p(z_2,x_2|do(x_1))
#'   p(z_1|x_2,do(x_1,y))
#'   p(x_1|w_1,do(x_2))
#'   p(y|z_1,z_2,x_1,do(x_2))
#'   p(w|y,x_1,do(x_2))
#' "
#' query3 <- "p(y,x_1|w,do(x_2))"
#' graph3 <- "
#'   x_1 -> z_2
#'   x_1 -> z_1
#'   x_2 -> z_1
#'   x_2 -> z_2
#'   z_1 -> y
#'   z_2 -> y
#'   x_1 -> w
#'   x_2 -> w
#'   z_1 -> w
#'   z_2 -> w
#' "
#' dosearch(data3, query3, graph3)
#'
#' # Selection bias
#' data4 <- "
#'   p(x,y,z_1,z_2|s)
#'   p(z_1,z_2)
#' "
#' query4 <- "p(y|do(x))"
#' graph4 <- "
#'   x   -> z_1
#'   z_1 -> z_2
#'   x   -> y
#'   y   -- z_2
#'   z_2 -> s
#' "
#' dosearch(data4, query4, graph4, selection_bias = "s")
#'
#' # Transportability
#' data5 <- "
#'   p(x,y,z_1,z_2)
#'   p(x,y,z_1|t_1,t_2,do(z_2))
#'   p(x,y,z_2|t_3,do(z_1))
#' "
#' query5 <- "p(y|do(x))"
#' graph5 <- "
#'   z_1 -> x
#'   x   -> z_2
#'   z_2 -> y
#'   z_1 <-> x
#'   z_1 <-> z_2
#'   z_1 <-> y
#'   t_1 -> z_1
#'   t_2 -> z_2
#'   t_3 -> y
#' "
#' dosearch(data5, query5, graph5, transportability = "t_1, t_2, t_3")
#'
#' # Missing data
#' # Proxy variables are denoted by an asterisk (*)
#' data6 <- "
#'   p(x*,y*,z*,m_x,m_y,m_z)
#' "
#' query6 <- "p(x,y,z)"
#' graph6 <- "
#'   z -> x
#'   x -> y
#'   x -> m_z
#'   y -> m_z
#'   y -> m_x
#'   z <-> y
#' "
#' dosearch(data6, query6, graph6, missing_data = "m_x : x, m_y : y, m_z : z")
#'
#' # An LDAG
#' data7 <- "P(X,Y,Z)"
#' query7 <- "P(Y|X,I_X=1)"
#' graph7 <- "
#'   X -> Y : Z = 1
#'   Z -> Y
#'   Z -> X : I_X = 1
#'   I_X -> X
#'   H -> X : I_X = 1
#'   H -> Z
#'   Q -> Z
#'   Q -> Y : Z = 0
#' "
#' dosearch(data7, query7, graph7)
#'
#' # A more complicated LDAG
#' # with multiple assignments for the edge X -> Z
#'
#' data8 <- "P(X,Y,Z,A,W)"
#' query8 <- "P(Y|X,I_X=1)"
#' graph8 <- "
#'   I_X -> X
#'   I_Z -> Z
#'   A -> W
#'   Z -> Y
#'   A -> Z
#'   X -> Z : I_Z = 1; A = 1
#'   X -> Y : A = 0
#'   W -> X : I_X = 1
#'   W -> Y : A = 0
#'   A -> Y
#'   U -> X : I_X = 1
#'   U -> Y : A = 1
#' "
#' dosearch(data8, query8, graph8)
#'
#' # Export the DOT diagram of the derivation as an SVG file
#' # to the working directory via the DOT package.
#' # By default, only the identifying part is plotted.
#' # PostScript format is also supported.
#' if (requireNamespace("DOT", quietly = TRUE)) {
#'   d <- get_derivation(
#'     data1,
#'     query1,
#'     graph1,
#'     control = list(draw_derivation = TRUE)
#'   )
#'   DOT::dot(d$derivation, "derivation.svg")
#' }
#' }
#'
dosearch <- function(data, query, graph, transportability = NULL,
                     selection_bias = NULL, missing_data = NULL,
                     control = list()) {
  if (!is.list(control)) {
    stop_("Argument `control` must be a list.")
  }
  if (missing(data)) {
    stop_("Argument `data` is missing.")
  }
  data <- parse_data(data)
  if (missing(query)) {
    stop_("Argument `query` is missing.")
  }
  query <- parse_query(query)
  if (missing(graph)) {
    stop_("Argument `graph` is missing.")
  }
  graph <- parse_graph(graph)
  validate_special("transportability", transportability)
  validate_special("selection_bias", selection_bias)
  validate_special("missing_data", missing_data)
  if (grepl(":", graph)) { # : denotes the presence of edge labels
    get_derivation_ldag(data, query, graph, control)
  } else {
    get_derivation_dag(
      data,
      query,
      graph,
      transportability,
      selection_bias,
      missing_data,
      control
    )
  }
}

#' Summary of a `dosearch` Object
#'
#' @export
#' @rdname dosearch
#' @param object An object of class `dosearch`.
#' @param ... Not used.
#' @return `summary` returns a `summary.dosearch` object.
#' @srrstats {NW5.4} The summary method for `dosearch` objects- Provides
#'   more details than the print method via `print.summary.dosearch`
#' @examples
#' data <- "p(x,y,z)"
#' query <- "p(y|do(x))"
#' graph <- "
#'   x -> y
#'   Z -> x
#'   z -> y
#' "
#' x <- dosearch(data, query, graph)
#' y <- summary(x)
#'
summary.dosearch <- function(object, ...) {
  if (!is_dosearch(object)) {
    stop_("Argument `object` must be an object of class `dosearch`.")
  }
  took <- NA
  units <- NA
  if (!is.null(object$time)) {
    took <- object$time / 1000.0
    units <- "seconds."
    if (took >= 60.0 && took < 3600.0) {
      took <- took / 60.0
      units <- "minutes."
    } else if (took >= 3600.0) {
      took <- took / 3600.0
      units <- "hours."
    }
  }
  d <- gsub("\n", "\n\t", trimws(object$call$data, which = "both"))
  d <- gsub(" ", "", d)
  g <- gsub("\n", "\n\t", trimws(object$call$graph, which = "both"))
  g <- gsub(" ", "", g)
  g <- gsub("->", " -> ", g)
  g <- gsub("--", " <-> ", g)
  structure(
    list(
      result = list(
        identifiable = object$identifiable,
        formula = object$formula
      ),
      call = object$call,
      time = took,
      units = units,
      data = d,
      graph = g
    ),
    class = "summary.dosearch"
  )
}

#' Plot of a `dosearch` Derivation
#'
#' @export
#' @rdname dosearch
#' @param x An object of class `dosearch`.
#' @param ... Additional arguments passed to [DiagrammeR::grViz()].
#' @return `plot` returns a `htmlwidget` object or `NULL` (invisibly)
#' @srrstats {NW5.5} The plot method for `dosearch` objects.
#' @srrstats {NW5.8} Can be used in HTML via e.g., R Markdown and Shiny.
#' @examples
#' \dontrun{
#' out <- dosearch(
#'   "p(x,y,z, w)",
#'   "p(y|do(x))",
#'   "x -> y \n z -> x \n w -> z \n x <-> w \n w <-> y",
#'   control = list(draw_derivation = TRUE)
#' )
#' if (requireNamespace("DiagrammeR", quietly = TRUE)) {
#'   plot(out)
#' }
#' }
#'
plot.dosearch <- function(x, ...) {
  out <- NULL
  if (is.null(x$derivation)) {
    message("No derivation is available to plot.")
  } else if (require_namespace("DiagrammeR", quietly = TRUE)) {
    out <- print(DiagrammeR::grViz(diagram = x$derivation, ...))
  } else {
    message("Please install the `DiagrammeR` package to plot derivations.")
  }
  invisible(out)
}

#' Print the Summary of a `dosearch` Object
#'
#' @export
#' @param x An object of class `summary.dosearch`.
#' @param max_chars Maximum number of characters of the formula to display. The
#'   default is 300.
#' @param ... Not used.
#' @returns `x` (invisibly)
#' @examples
#' data <- "p(x,y,z)"
#' query <- "p(y|do(x))"
#' graph <- "
#'   x -> y
#'   Z -> x
#'   z -> y
#' "
#' x <- dosearch(data, query, graph)
#' y <- summary(x)
#' print(y)
#'
print.summary.dosearch <- function(x, max_chars = 300L, ...) {
  if (!inherits(x, "summary.dosearch")) {
    stop_("Argument `x` must be an object of class `summary.dosearch`.")
  }
  res <- x$result
  y <- x$call
  cat(
    "The query", y$query, "is",
    ifelse(res$identifiable, "identifiable.", "not identifiable by do-search."),
    "\n"
  )
  if (identical(res$formula, "")) {
    cat("Formula: NA\n")
  } else {
    trunc_str <- ifelse(
      nchar(x$formula) > max_chars,
      "... [formula truncated]",
      ""
    )
    cat(
      "Formula:\n\t",
      substr(res$formula, 1L, max_chars),
      trunc_str,
      "\n",
      sep = ""
    )
  }
  if (!is.na(x$time)) {
    cat("Search took", x$time, x$units, "\n")
  }
  cat("Input data:\n")
  cat("\t", x$data, "\n", sep = "")
  cat("Input graph:\n")
  cat("\t", x$graph, "\n", sep = "")
  invisible(x)
}

#' Print a `dosearch` Object
#'
#' @export
#' @rdname dosearch
#' @param x An object of class `dosearch`.
#' @param max_chars Maximum number of characters of the formula to display. The
#'   default is 300.
#' @param ... Additional arguments passed to [base::format()].
#' @return `print` returns `x` invisibly.
#' @srrstats {NW5.2} The default print method for `dosearch` objects.
#' @srrstats {NW5.3} Only output the identifiability status and the formula,
#'   with an additional argument limiting the output length.
#' @examples
#' data <- "p(x,y,z)"
#' query <- "p(y|do(x))"
#' graph <- "
#'   x -> z
#'   Z -> y
#'   x <-> y
#' "
#' x <- dosearch(data, query, graph)
#' print(x)
#'
print.dosearch <- function(x, max_chars = 300L, ...) {
  if (!is_dosearch(x)) {
    stop_("Argument `x` must be an object of class `dosearch`.")
  }
  if (is.null(x$formula) || identical(x$formula, "")) {
    cat(
      "The query", x$call$query, "is",
      ifelse(x$identifiable, "identifiable", "not identifiable by do-search."),
      "\n"
    )
  } else {
    trunc_str <- ifelse(
      nchar(x$formula) > max_chars,
      "... [formula truncated]",
      ""
    )
    cat(format(substr(x$formula, 1L, max_chars), ...), trunc_str, "\n")
  }
  invisible(x)
}

#' Was the Target Distribution Identifiable?
#'
#' `is_identifiable` returns the a logical value describing the identifiability
#' of a causal query of an object of class `dosearch`.
#'
#' @export
#' @rdname dosearch
#' @param x An object of class `dosearch`.
#' @return `is_identifiable` returns a logical value. If `TRUE`, the target
#'   distribution was identifiable from the available inputs.
#' @examples
#' data <- "P(x,y,z)"
#' query <- "P(y|do(x))"
#' graph <- "
#'   x -> y
#'   z -> x
#'   z -> y
#' "
#' x <- dosearch(data, query, graph)
#' is_identifiable(x)
#' # TRUE
#'
is_identifiable <- function(x) {
  if (!is_dosearch(x)) {
    stop_("Argument `x` must be an object of class `dosearch`.")
  }
  x$identifiable
}

#' Retrieve the Identifying Formula of a Causal Query
#'
#' `get_formula` returns the identifying formula describing a causal query of
#' an object of class `dosearch`. If no formula is available, returns `NULL`.
#'
#' @export
#' @rdname dosearch
#' @param x An object of class `dosearch`.
#' @param run_again If `TRUE`, runs the search again in an attempt to obtain
#'   the formula, for example if `control$formula` was `FALSE` in the call to
#'   [dosearch::dosearch()], but the query itself is identifiable.
#' @return `get_formula` returns a `character` string representing the query
#'   in terms of the input data or `NULL` if the query is not identifiable.
#' @examples
#' data <- "P(x,y,z)"
#' query <- "P(y|do(x))"
#' graph <- "
#'   x -> y
#'   z -> x
#'   z -> y
#' "
#' x <- dosearch(data, query, graph, control = list(formula = FALSE))
#' get_formula(x, run_again = TRUE)
#'
get_formula <- function(x, run_again = FALSE) {
  if (!is_dosearch(x)) {
    stop_("Argument `x` must be an object of class `dosearch`.")
  }
  if (run_again) {
    y <- x$call
    y$control$formula <- TRUE
    z <- dosearch(
      y$data,
      y$query,
      y$graph,
      y$transportability,
      y$selection_bias,
      y$missing_data,
      y$control
    )
    z$formula
  } else if (!is.null(x$formula)) {
    x$formula
  } else {
    NULL
  }
}

#' Retrieve the Derivation of a Causal Query
#'
#' `get_derivation` returns the derivation of a causal query of an object of
#' class `dosearch`. If no derivation is available, returns `NULL`.
#'
#' @rdname dosearch
#' @param draw_all A logical value. If `TRUE`, the derivation will contain
#'   every step taken by the search. If `FALSE`, only steps that resulted in
#'   identification are returned.
#' @return `get_derivation` returns a graphical representation of the
#'   derivation steps that resulted in identification. The return object
#'   is a `character` string in `DOT` syntax.
#' @export
#' @examples
#' data <- "P(x,y,z)"
#' query <- "P(y|do(x))"
#' graph <- "
#'   x -> y
#'   z -> x
#'   z -> y
#' "
#' x <- dosearch(data, query, graph, control = list(draw_derivation = FALSE))
#' get_derivation(x, run_again = TRUE)
get_derivation <- function(x, run_again = FALSE, draw_all = FALSE) {
  if (!is_dosearch(x)) {
    stop_("Argument `x` must be an object of class `dosearch`.")
  }
  if (run_again) {
    y <- x$call
    y$control$draw_derivation <- TRUE
    y$control$draw_all <- draw_all
    z <- dosearch(
      y$data,
      y$query,
      y$graph,
      y$transportability,
      y$selection_bias,
      y$missing_data,
      y$control
    )
    z$derivation
  } else if (!is.null(x$derivation)) {
    x$derivation
  } else {
    NULL
  }
}

#' Benchmark a Specific Run of the Search
#'
#' `get_benchmark` returns the benchmarking information of an object of
#' class `dosearch`. If no benchmark is available, returns `NULL`.
#'
#' @export
#' @rdname dosearch
#' @param include_rules A `logical` value. If `TRUE`, also benchmarks the time
#'   taken by each inference rule separately.
#' @return `get_benchmark` returns a `list` with one or two elements or `NULL`.
#'   The first element of the list is always a numeric
#'   value of the total time taken by the search in milliseconds.
#'   The second is a numeric vector of the time taken by each inference rule
#'   (in the internal C++ implementation) of the search in milliseconds
#'   if `include_rules` is `TRUE`.
#' @examples
#' data <- "P(x,y,z)"
#' query <- "P(y|do(x))"
#' graph <- "
#'   x -> y
#'   z -> x
#'   z -> y
#' "
#' x <- dosearch(data, query, graph, control = list(benchmark = FALSE))
#' get_benchmark(x, run_again = TRUE)
#'
get_benchmark <- function(x, run_again = FALSE, include_rules = FALSE) {
  if (!is_dosearch(x)) {
    stop_("Argument `x` must be an object of class `dosearch`.")
  }
  if (run_again) {
    y <- x$call
    y$control$benchmark <- TRUE
    y$control$benchmark_rules <- include_rules
    z <- dosearch(
      y$data,
      y$query,
      y$graph,
      y$transportability,
      y$selection_bias,
      y$missing_data,
      y$control
    )
    get_benchmark(z, run_again = FALSE)
  } else if (!is.null(x$time)) {
    if (!is.null(x$rule_times)) {
      list(time = x$time, rule_times = x$rule_times)
    } else {
      list(time = x$time)
    }
  } else {
    NULL
  }
}
