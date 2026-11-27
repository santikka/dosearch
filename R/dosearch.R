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
#' If \ifelse{html}{\out{M<sub>X</sub>}}{\eqn{M_X}} or \eqn{X} is not present
#' in the graph, it is added to the graph with no edges other than the edge to
#' the proxy variable of \eqn{X}, i.e., it is independent of the other
#' variables of the graph, and a warning is issued by default. Proxy variables
#' should not be included in the graph, since they are automatically generated
#' based on \code{missing_data}. By default, a warning
#' is issued if a proxy variable is present in an input distribution but its
#' corresponding mechanism is not present in any input. See e.g.,
#' (Mohan, Pearl and Tian, 2013) for details on missing data as
#' a causal inference problem. Note that `dosearch` is not complete for
#' missing data problems, meaning that if `dosearch` is not able to identify
#' the `query`, it might still be identifiable via some other means.
#'
#' Missing data mechanisms can be assigned values in `data` and `query`.
#' The value 1 denotes an enabled mechanism, i.e., the variable is observed
#' and its proxy variable is equal to the true variable, and the value 0
#' denotes a disabled mechanism, i.e., the variable is missing. For example,
#' `P(Y*,R_Y = 1)` is the distribution of the observed values of \eqn{Y}, and
#' `P(Y|R_Y = 0)` is the distribution of \eqn{Y} among the units for which
#' \eqn{Y} is missing, as obtained from a follow-up study of non-respondents.
#' No other values are permitted. In addition to the rules of
#' Tikka et al. (2021), the search can use rules analogous to the
#' case-by-case, general-by-case and case-by-general reasoning of
#' Tikka et al. (2019) for the values of the mechanisms by setting
#' `case_reasoning = TRUE`. These rules are typically needed when mechanisms
#' are disabled in the input or target distributions, and they can also
#' identify queries that are not identifiable otherwise when the inputs
#' originate from different sources. However, they increase the size of the
#' search space, so they are not used by default.
#'
#' Before the search is started, the target is checked against criteria that
#' can show it to be non-identifiable directly from the inputs, in which case
#' the search is not carried out (when `improve = TRUE` in `control`). The
#' target is non-identifiable by do-search when a variable on the left-hand
#' side of the target does not appear on the left-hand side of any input
#' distribution (Tikka et al., 2021). For DAGs without missing data mechanisms,
#' two additional criteria are used. The first applies when a variable on the
#' right-hand side of the target does not appear in any input distribution
#' but the target depends on it, and the second applies when every input
#' distribution of some variable of the left-hand side of the target also
#' conditions on, or intervenes on, a neighbor of the variable in the graph
#' that the rules of the search can never remove, but the target does not.
#' See the vignette "New features in dosearch 1.1.0" for the precise criteria
#' and their proofs. With `verbose = TRUE` in `control`, the criterion that
#' applies is reported.
#'
#' The `control` argument is a list that can supply any of the following
#' components:
#'
#' * `benchmark`: a `logical` value. If `TRUE`, the search time is
#'   recorded and returned (in milliseconds). Defaults to `FALSE`.
#' * `benchmark_rules`: a `logical` value. If `TRUE`, the time taken by each
#'   individual inference rule is also recorded in the benchmark
#'   (in milliseconds). Defaults to `FALSE`.
#' * `cache`: a `logical` value. If `TRUE`, the results of the separation
#'   criteria evaluated during the search are cached (for LDAGs only).
#'   Defaults to `TRUE`.
#' * `con_vars`: a `character` vector of additional context variables for
#'   LDAGs, i.e., variables that can be assigned values in the input and target
#'   distributions even though they do not appear in any edge label.
#'   Defaults to an empty vector.
#' * `draw_derivation`: a `logical` value. If `TRUE`, a string representing
#'   the derivation steps as a DOT graph is returned. If the `DiagrammeR`
#'   package is installed, the DOT graph can be plotted by calling `plot` on
#'   the return object. Defaults to `FALSE`.
#' * `draw_all`: a `logical` value. If `TRUE` and if `draw_derivation = TRUE`,
#'   the derivation will contain every step taken by the search. If `FALSE`,
#'   only the steps that resulted in an identifiable target are returned.
#'   Defaults to `FALSE`.
#' * `empty`: a `logical` value. If `TRUE`, an empty `dosearch` object is
#'   returned without running the search.
#' * `formula`: a `logical` value. If `TRUE`, a string representing the
#'   identifiable query is returned when the target query is identifiable.
#'   If `FALSE`, only a logical value is returned that takes the value `TRUE`
#'   for an identifiable target and `FALSE` otherwise. Defaults to `TRUE`.
#' * `heuristic`: a `logical` value. If `TRUE`, new distributions are expanded
#'   during the search according to a search heuristic
#'   (see Tikka et al., 2021, for details). Otherwise, distributions are
#'   expanded in the order in which they were identified. Defaults to `FALSE`.
#' * `improve`: a `logical` value. If `TRUE`, the search is not carried out
#'   when the target can be shown to be non-identifiable directly from the
#'   inputs (see 'Details'), and the default rules for DAGs exclude the
#'   insertion and deletion of observations (rule 1 of do-calculus), which are
#'   implied by the other rules (see Tikka et al., 2021, for details). The
#'   observations of proxy variables can still be deleted in missing data
#'   problems, because the other rules do not apply to proxy variables.
#'   Defaults to `TRUE`.
#' * `md_sym`: deprecated and ignored. Previously, a single `character`
#'   describing the symbol to use for enabled missing data mechanisms.
#'   The value 1 now always denotes an enabled mechanism and the value 0 a
#'   disabled mechanism. A warning is issued if this argument is provided.
#' * `rules`: an `integer` vector of inference rules to use in the search
#'   instead of the default rules. The valid rules are -13, -11, -9, -8, -7,
#'   -6, -3, -2, -1 and 1 to 13 for DAGs and -8, -7, -5, -4, -3, -2 and 0 to 8
#'   for LDAGs. For DAGs, rules 9 to 13 and -13 to -9 are used only for
#'   missing data problems: rules 9 and -9 assign the values 1 and 0 to
#'   missing data mechanisms (case-by-general reasoning), rule 10 exchanges
#'   proxy variables for true variables, rules 11 and -11 combine the
#'   distributions for both values of a mechanism (case-by-case reasoning),
#'   and rules 12, 13 and -13 derive the distribution for one value of a
#'   mechanism from the distribution for the other value and the distribution
#'   where the mechanism is not assigned a value or is not present
#'   (general-by-case reasoning). Rules -9, 11, -11, 12, 13 and -13 are not
#'   among the default rules, but they are added to the rules that are used
#'   when `case_reasoning = TRUE`.
#'   Mainly intended for testing and benchmarking, as the search may fail to
#'   identify an identifiable query if a necessary rule is omitted.
#'   Defaults to an empty vector, which uses the default rules.
#' * `time_limit`: a `numeric` value giving a time limit for the search
#'   (in hours). Defaults to a negative value that disables the time limit.
#' * `verbose`: a `logical` value. If `TRUE`, diagnostic information is printed
#'   to  the console during the search. Defaults to `FALSE`.
#' * `warn`: a `logical` value. If `TRUE`, a warning is issued for possibly
#'   unintentionally misspecified but syntactically correct inputs, e.g., when
#'   a variable of an input distribution is not present in the graph, in any
#'   other input distribution or in the target distribution. A warning is
#'   also raised if both lower-case and upper-case node or variable names are
#'   used simultaneously in the inputs. Defaults to `TRUE`.
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
#' @param case_reasoning A `logical` value. If `TRUE`, the search also uses
#'   the rules for case-by-general, case-by-case and general-by-case
#'   reasoning on the values of the missing data mechanisms, i.e., rules -9,
#'   11, -11, 12, 13 and -13 (see `rules` of `control`). These rules are
#'   added to the default rules or to the rules given via `control`.
#'   The default is `FALSE` (for DAGs with missing data mechanisms only).
#' @return `dosearch` returns an object of class `dosearch` which is a list
#'   with the following components by default. See the `control` options
#'   on how to obtain a graphical representation of the derivation or
#'   how to benchmark the search.
#'
#'   * `identifiable`: a `logical` value that is `TRUE` if the target
#'     quantity is identifiable and `FALSE` otherwise.
#'   * `formula`: a `character` string describing the formula for an
#'     identifiable query in LaTeX syntax or an empty character vector for
#'     a non-identifiable effect. The formula shows the derivation of the
#'     target from the input distributions: every distribution in the formula
#'     is an input distribution or a marginal or conditional distribution of
#'     an input distribution, where the values assigned to the variables
#'     during the derivation are shown, and the steps of do-calculus do not
#'     appear as they do not change the distributions. A marginal or
#'     conditional distribution is given as such when the distribution that
#'     is marginalized or conditioned is derived from an input distribution
#'     by rules that have a single parent distribution, including the rules
#'     of do-calculus. The same holds for the division of a distribution by
#'     its marginal or conditional distribution (rules 7, -7, 8 and -8) when
#'     both are derived from the same input distribution by such rules.
#'     Case-by-case reasoning over a variable that is assigned a value
#'     reduces to the case of that value, so it counts as a rule with a
#'     single parent. Otherwise, the summations and quotients are given
#'     explicitly.
#'     A summation applies to every term that follows it in the same
#'     product, i.e., up to a minus or a plus sign, the end of the enclosing
#'     expression or the end of the formula. Products are not enclosed in
#'     parentheses, and differences are enclosed in parentheses only when they
#'     are terms of products or summations, when they are subtracted or when
#'     they are cases of case-by-case reasoning (see below). A variable that
#'     is bound by a summation is primed when a variable of the same name is
#'     already in scope, i.e., when it is a variable of the target
#'     distribution, it appears in the formula outside the summations that
#'     bind it, or another summation over a variable of the same name applies
#'     at the position of the summation.
#'     For example, the front-door formula for \eqn{P(y|do(x))} from the
#'     input \eqn{P(x,y,z)} is given as
#'     `\sum_{z}p(z|x)\sum_{x'}p(x')p(y|z,x')`.
#'     Variables bound by nested summations receive additional primes, and a
#'     superscript of the form `^{(k)}` is used instead when the number of
#'     primes \eqn{k} would exceed three. In missing data problems, the proxy
#'     variable `x*` of a variable `x` is written as `x^{\ast}` in the
#'     formula, and its primes are given in the same superscript, e.g.,
#'     `x^{\ast\prime}` or `x^{\ast(4)}`. A proxy variable that stands for
#'     the value of its true variable has the same primes as the true
#'     variable, and the summations over a true variable and over its proxy
#'     variable are distinguished in the same way as summations over the same
#'     variable. A proxy variable that is removed
#'     from a distribution by deleting an observation is not bound by a
#'     summation, but it is a variable of its own, which is primed in the
#'     same way when a variable of the same name is in scope. The formula
#'     holds for every value of such a variable that is possible given its
#'     missing data mechanism in the same distribution. When the mechanism
#'     has the value 0, the variable is missing, so it is determined by the
#'     mechanism, and it is omitted from the distribution, e.g.,
#'     `p(y|r_x = 0)` is given instead of `p(y|r_x = 0,x^{\ast})`. The
#'     variable is kept only when omitting it would change the distribution,
#'     i.e., when the mechanism is on the left-hand side and the variable is
#'     not, or when the variable is the only variable on the left-hand side.
#'     Furthermore,
#'     `\left[f_0 \vee f_1\right]` denotes the distribution given by `f_0`
#'     when a missing data mechanism has the value 0 and by `f_1` when it has
#'     the value 1 (case-by-case reasoning, see `variants`), where a case that
#'     is a difference is enclosed in parentheses, e.g.,
#'     `\left[\left(p(y) - p(r_x = 1,y)\right) \vee p(r_x = 1,y)\right]`.
#'   * `variants`: a named `character` vector of the variants of `formula`
#'     that is included when the formula is derived by case-by-case
#'     reasoning, i.e., by rules 11 and -11 on the values of missing data
#'     mechanisms or by rules 5 and -5 on the values of context variables of
#'     labeled DAGs. These variables are binary, and the variants give the
#'     formula for each combination of values of the variables that are
#'     free in the formula, named by the values, e.g., `"r_x = 0, r_y = 1"`.
#'     The values are assigned in the variants, so the case-by-case
#'     reasoning over a variable reduces to the case of its value, and a
#'     summation over a variable of case-by-case reasoning is written out as
#'     the sum of its terms for the values 0 and 1, which is enclosed in
#'     parentheses in the same way as a difference. For example, the formula
#'     for \eqn{P(y)} from the inputs \eqn{P(y^*,r_y)} and
#'     \eqn{P(y|r_y = 0)} in the graph `y -> r_y` is
#'     `\sum_{r_y}\left[p(r_y = 0)p(y|r_y = 0) \vee p(r_y = 1,y^{\ast})\right]`,
#'     and its variant is `p(r_y = 0)p(y|r_y = 0) + p(r_y = 1,y^{\ast})`. If
#'     none of the variables is free in the formula, as here, there is a
#'     single variant with an empty name. The variants are also available via
#'     [dosearch::get_formula()].
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
#' S. Tikka, A. Hyttinen, J. Karvanen. "Identifying Causal Effects via
#' Context-Specific Independence Relations." In \emph{Proceedings of the
#' 33rd Annual Conference on Neural Information Processing Systems}, 2019.
#'
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
#' # The value 0 denotes a disabled missing data mechanism. Here y is
#' # missing not at random, but a follow-up study of the non-respondents
#' # provides the distribution of y when it is missing in the original data.
#' # Combining the cases requires case-by-case reasoning.
#' data <- "
#'   p(y*,r_y)
#'   p(y|r_y = 0)
#' "
#' dosearch(
#'   data,
#'   "p(y)",
#'   "y -> r_y",
#'   missing_data = "r_y : y",
#'   case_reasoning = TRUE
#' )
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
#'   p(x_1|w,do(x_2))
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
#' # The mechanism m_y is independent of the other variables, so it is not
#' # present in the graph, which would produce a warning by default
#' dosearch(
#'   data6,
#'   query6,
#'   graph6,
#'   missing_data = "m_x : x, m_y : y, m_z : z",
#'   control = list(warn = FALSE)
#' )
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
#' }
#'
dosearch <- function(data, query, graph, transportability = NULL,
                     selection_bias = NULL, missing_data = NULL,
                     control = list(), case_reasoning = FALSE) {
  if (!is.list(control)) {
    stop_("Argument `control` must be a list.")
  }
  if (!is.logical(case_reasoning) || length(case_reasoning) != 1L ||
      is.na(case_reasoning)) {
    stop_("Argument `case_reasoning` must be either `TRUE` or `FALSE`.")
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
      control,
      case_reasoning
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
#' @examples
#' data <- "p(x,y,z)"
#' query <- "p(y|do(x))"
#' graph <- "
#'   x -> y
#'   z -> x
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
  g <- gsub("<->", "--", g)
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
#'   z -> x
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
  if (is.null(res$formula) || identical(res$formula, "")) {
    cat("Formula: NA\n")
  } else {
    trunc_str <- ifelse(
      nchar(res$formula) > max_chars,
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
#' @examples
#' data <- "p(x,y,z)"
#' query <- "p(y|do(x))"
#' graph <- "
#'   x -> z
#'   z -> y
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
#' @param variants If `TRUE`, returns the variants of the formula for the
#'   values of the variables of case-by-case reasoning instead (see
#'   `variants` in the value of [dosearch::dosearch()]). If case-by-case
#'   reasoning was not used, the formula itself is returned.
#' @return `get_formula` returns a `character` string representing the query
#'   in terms of the input data or `NULL` if the query is not identifiable.
#'   If `variants` is `TRUE`, the variants are returned as a named
#'   `character` vector when they are available.
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
#' # The variants of a formula derived by case-by-case reasoning
#' x <- dosearch(
#'   "p(y*,r_y = 1) \n p(y,r_y = 0)",
#'   "p(y|r_y)",
#'   "y -> r_y",
#'   missing_data = "r_y : y",
#'   case_reasoning = TRUE
#' )
#' get_formula(x, variants = TRUE)
#'
get_formula <- function(x, run_again = FALSE, variants = FALSE) {
  if (!is_dosearch(x)) {
    stop_("Argument `x` must be an object of class `dosearch`.")
  }
  if (run_again) {
    y <- x$call
    y$control$formula <- TRUE
    x <- dosearch(
      y$data,
      y$query,
      y$graph,
      y$transportability,
      y$selection_bias,
      y$missing_data,
      y$control,
      isTRUE(y$case_reasoning)
    )
  }
  if (variants && !is.null(x$variants)) {
    x$variants
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
      y$control,
      isTRUE(y$case_reasoning)
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
      y$control,
      isTRUE(y$case_reasoning)
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
