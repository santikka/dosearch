# dosearch 1.1.0

  * Missing data mechanisms can now also be disabled by assigning them the value 0 in the input and target distributions, e.g., `p(y|r_y = 0)` for the distribution of `y` among the units for which `y` is missing. The value 1 always denotes an enabled mechanism and the value 0 a disabled mechanism, and other values produce an error. Previously, only enabled mechanisms could be given. Identifying queries from inputs with disabled mechanisms or queries with disabled mechanisms typically requires `case_reasoning = TRUE` (see below).
  * The `md_sym` element of `control` is deprecated. It is ignored with a warning, and enabled mechanisms are always denoted by the value 1 in the inputs and in the formulas.
  * Added new inference rules for missing data problems that are analogous to the case-by-case, general-by-case and case-by-general reasoning used for LDAGs. Rule -9 disables missing data mechanisms (the counterpart of rule 9 that enables them), rules 11 and -11 combine the distributions for both values of a mechanism, and rules 12, 13 and -13 derive the distribution for one value of a mechanism from the distribution for the other value and the corresponding distribution where the mechanism is not assigned a value or is not present. The new rules increase the size of the search space, so they are not used by default. They can be enabled via the new argument `case_reasoning = TRUE` of `dosearch`, which adds them to the default rules or to the rules given via `rules` in `control`. They can also identify queries that are not identifiable by the default rules when no mechanism is disabled, e.g., `p(r_y = 1|y)` from `p(y*,r_y)` and `p(y)` in the graph `y -> r_y`. In the systematic analysis of bivariate missing data problems of Tikka et al. (2021), enabling the rules does not change the results, and the search takes about twice as long.
  * Fixed an issue in LDAGs where general-by-case reasoning produced an empty formula when the assigned variable was the only variable on the left-hand side and its distribution was derived using the insertion or deletion of observations. The formula is now `1 - f`, where `f` is the formula for the other value of the variable.
  * Formulas now show the derivation of the target from the input distributions. Previously, a distribution that was derived from the inputs by the rules of probability calculus alone was reported as a single distribution without showing how it was derived, e.g., `p(b|d,c)` for `p(b|c,d)` from the inputs `p(b,c,d|a)` and `p(a,d|c)`, which is now given as `\\sum_{a}p(a|d,c)p(b|a,d,c)`. Every distribution in a formula is now an input distribution or a marginal or conditional distribution of an input distribution. A marginal or conditional distribution is given as such when the distribution that is marginalized or conditioned is derived from an input distribution by rules that have a single parent distribution, including the rules of do-calculus. The same holds for the division of a distribution by its marginal or conditional distribution (rules 7, -7, 8 and -8) when both are derived from the same input distribution by such rules, e.g., `p(x^{\\ast},y|r_x = 1)` instead of `p(r_x = 1,x^{\\ast},y)` divided by `p(r_x = 1)`. Otherwise, the summations and quotients are given explicitly, e.g., for the marginal distribution of a product. The steps of do-calculus do not appear in the formulas, as they do not change the distributions. The values that are assigned to missing data mechanisms and to the context variables of LDAGs during the derivation are shown in the input distributions that they apply to, except under the summations over the assigned variables themselves, and case-by-case reasoning over a variable that is assigned a value reduces to the case of that value, also when deciding whether a distribution is derived from a single input distribution. The notation `\\left[f\\right]_{r_x = 1}` for a formula `f` evaluated at a value of a variable is no longer used. Previously, a missing data mechanism could also appear in a formula as if it had not been assigned a value. The terms of products are ordered by the lengths of their formulas as before, now using the lengths of the formulas as they are given. The derivations themselves are unchanged.
  * Formulas no longer enclose products in parentheses, and a summation now applies to every term that follows it in the same product, as is conventional. Differences are enclosed in parentheses only when they are terms of products or summations, when they are subtracted or when they are cases of case-by-case reasoning. Variables that are bound by summations in formulas are now distinguished from other variables of the same name by primes. A bound variable is primed only when a variable of the same name is already in scope, i.e., when it is a variable of the target distribution, it appears in the formula outside the summations that bind it, or another summation over a variable of the same name applies at the position of the summation, including the summations of the preceding terms of the same product. For example, the front-door formula `\\sum_{z}\\left(p(z|x)\\sum_{x}\\left(p(x)p(y|z,x)\\right)\\right)` is now `\\sum_{z}p(z|x)\\sum_{x'}p(x')p(y|z,x')`. Variables bound by nested summations receive additional primes, and a superscript of the form `^{(k)}` is used instead when more than three primes would be needed. In missing data problems, proxy variables that stand for the values of their true variables share the primes of the true variables, and summations over a true variable and over its proxy variable are primed in the same way as summations over the same variable.
  * Formulas now render correctly as LaTeX in missing data problems and with case-by-case reasoning. A proxy variable `x*` is now written as `x^{\\ast}` in formulas, and its primes are given in the same superscript, e.g., `x^{\\ast\\prime}`. The input and target distributions, the derivations and the diagnostic output still use `x*`. Case-by-case reasoning is now denoted by `\\left[f_0 \\vee f_1\\right]` instead of `\\left[f_0 /\\ f_1\\right]`, and a case that is a difference is enclosed in parentheses, e.g., `\\left[\\left(p(y) - p(r_x = 1,y)\\right) \\vee p(r_x = 1,y)\\right]`.
  * When the derivation uses case-by-case reasoning, i.e., rules 11 and -11 for missing data mechanisms or rules 5 and -5 for the context variables of LDAGs, the result of `dosearch` now also contains the variants of the formula (`variants`), a named character vector with a formula for each combination of values of the variables of case-by-case reasoning that are free in the formula, e.g., `r_x = 0` and `r_x = 1`. The values are assigned in the variants, so case-by-case reasoning reduces to the case of the value, and the marginal and conditional distributions of an input distribution of a case are given as such, e.g., `p(y^{\\ast}|r_y = 1)`. Summations over the variables of case-by-case reasoning are written out as sums of their terms for both values, e.g., `p(r_y = 0)p(y|r_y = 0) + p(r_y = 1,y^{\\ast})` instead of `\\sum_{r_y}\\left[p(r_y = 0)p(y|r_y = 0) \\vee p(r_y = 1,y^{\\ast})\\right]`. The variants can be retrieved via the new argument `variants` of `get_formula`. Printing the result still shows the general formula.
  * Added a new function `plot_formula` that draws the formula of a `dosearch` object on the current graphics device with the mathematics rendered, using the `plotmath` facility of R, so no LaTeX installation is needed. Summations are drawn with their limits, quotients as fractions, the part of a variable name after its first underscore as a subscript, proxy variables with an asterisk as a superscript and primes as prime marks. With `variants = TRUE`, the variants of the formula are drawn one below the other, each preceded by the values of the variables of case-by-case reasoning. The formula is drawn on a single line that is shrunk to fit the plot region, so the function is intended for formulas of moderate length, and formulas longer than `max_chars` characters are not plotted. Variable names that contain other characters than ASCII letters, digits, periods and underscores, or that begin or end with an underscore, are cleaned for plotting, and a warning lists the names that were changed. The package now imports `graphics`.
  * Graphs of arbitrary size are now supported for both DAGs and LDAGs. Sets of variables are now represented as dynamic bitsets in the C++ implementation and as vectors of vertex indices in the R interface, so the previous limit of 30 nodes (including the intervention nodes of DAGs and the proxy variables of missing data problems) no longer applies, and the corresponding error has been removed. The search is still exponential in the number of variables in the worst case, so for large graphs it may be necessary to restrict the inference rules via `rules` or to set a `time_limit` in `control`.
  * DAGs are now represented by a new data structure (`dag`) that replaces `dcongraph`. D-separation is now tested by a single linear-time traversal of the graph that takes the bidirected edges and the intervention nodes into account directly, instead of separate tests for every pair of variables that copied and eliminated the adjacency matrices of the graph.
  * The search is considerably faster. Distributions are now indexed by hash keys of their sets instead of strings, lookups no longer insert empty entries into the index, and each rule now only enumerates subsets of the variables it can apply to instead of every subset of all variables. The subsets are still enumerated in the same order, so the derivations are unchanged. The list of all subsets is no longer precomputed, which required memory exponential in the number of variables before the search started. In the simulation design of Tikka et al. (2021), the search is about 3 times faster for graphs with 5 nodes and about 10 times faster for graphs with 8 nodes.
  * Two new criteria are used to show that the target is non-identifiable before the search is started when `improve = TRUE` in `control` (the default) for DAGs without missing data mechanisms. The target is non-identifiable if a variable on the right-hand side of the target does not appear in any input distribution but is d-connected to the left-hand side of the target given the other variables of the right-hand side in the graph where the incoming edges of the intervened variables are removed. The target is also non-identifiable if every input distribution with a given variable of the left-hand side of the target on its left-hand side contains an anchor of the variable. The anchors of a variable are its parents that do not appear on the left-hand side of any input distribution or in the target, when they appear on the right-hand side of a distribution, and its children and the variables connected to it by a bidirected edge that do not appear on the left-hand side of any input distribution or among the observations of the target, when they appear among the observations of a distribution. The rules of the search never remove an anchor from a distribution, and the target contains no anchors by definition. For example, selection bias nodes that are children of a variable and transportability nodes that are parents of a variable are anchors of the variable. The criteria are not applied to missing data problems or to LDAGs, where only the criterion of Tikka et al. (2021) is used. See the vignette "New features in dosearch 1.1.0" for the proofs of the criteria.
  * When `verbose = TRUE` in `control`, the criterion that shows the target to be non-identifiable before the search is now reported.
  * Added a new vignette "New features in dosearch 1.1.0" that describes the performance of the new version, the new criteria for non-identifiability and their proofs, the explicit derivations, primes and parentheses in the formulas, the new rules for missing data problems, the variants of formulas and the plotting of formulas.
  * For LDAGs, evaluations of the CSI-separation criterion that fail are now cached as well, taking into account the contexts that were available, and the graph keeps the parents and children of each vertex as bitsets.
  * Preprocessing of LDAGs is much faster for edges whose endpoint has many other parents. For example, inferring the labels of an edge whose endpoint has 8 other parents took about 78 seconds and now takes less than a second.
  * Fixed an issue in LDAGs where edges that are absent in every context could be restored to the graph during the search, which could make the result depend on the search order (e.g., whether `heuristic = TRUE`) and cause identifiable queries to be reported as non-identifiable.
  * Fixed an issue in LDAGs where the context variables given via `con_vars` in `control` were not used to construct the contexts of the graph. Case-by-case reasoning over combinations of these variables with other context variables then succeeded vacuously, which could cause non-identifiable queries to be reported as identifiable. The search now also never accepts case-by-case reasoning over contexts that are not defined.
  * Fixed an issue in missing data problems where a missing data mechanism could be removed from a distribution while its proxy variable remained in it, e.g., by deleting a disabled mechanism from `p(y|x*,r_x = 0)` after exchanging it for an action, or by dividing `p(r_x = 1,x*)` by `p(r_x = 1|x*)`. A proxy variable is missing when its mechanism is disabled and equal to its true variable when its mechanism is enabled, so the distributions that such steps equated are defined for different values of the proxy variable, which could produce incorrect formulas. Rules -1, -3, -7 and -8 are no longer applied when they would remove a mechanism from a distribution that still contains its proxy variable on either side.
  * Fixed an issue in missing data problems where a proxy variable that was removed from a distribution by deleting an observation (rule -1) could be identified in the formula with another variable of the same name, e.g., with a proxy variable that a product introduced to the left-hand side, or bound by the summation over that variable. The formula of such a distribution holds only for the values of the removed proxy variable that are possible given its mechanism, so this could produce incorrect formulas, e.g., with `improve = FALSE` in `control`, where rule -1 is among the default rules. A removed proxy variable is now a variable of its own in the formula, which is primed when a variable of the same name is in scope. A removed proxy variable whose mechanism has the value 0 in the same distribution is missing, so it is determined by the mechanism, and it is omitted from the distribution in the formula, e.g., `p(y|r_x = 0)` instead of `p(y|r_x = 0,x^{\\ast})`. It is kept only when omitting it would change the distribution, i.e., when the mechanism is on the left-hand side and the proxy variable is not, or when the proxy variable is the only variable on the left-hand side.
  * When `improve = TRUE` in `control` (the default), the default rules for missing data problems now include the deletion of observations of proxy variables (rule -1 for proxy variables only). The deletion of observations is otherwise implied by the exchange of observations and actions and the deletion of actions (rules 2 and -3), but these rules cannot be applied to proxy variables. Some queries were previously identified only through the incorrect steps that removed a mechanism while its proxy variable remained (see above), and they are now identified with correct formulas.
  * Fixed an issue in missing data problems where the required distributions of the conditioning rules could be looked up with incorrect missing data mechanisms when a mechanism was already enabled, which could cause derivations to be missed.
  * The search can now be interrupted from R, and memory is released correctly when the search is interrupted or an error occurs.
  * The derivation (`draw_derivation = TRUE`) now contains each step only once, even if a distribution is used several times in the derivation. With `draw_all = TRUE`, the steps are now listed in the order in which they were derived, which no longer depends on the platform.
  * Transportability nodes and selection bias nodes can no longer be variables with missing data, missing data mechanisms or proxy variables.
  * Fixed an issue where case-by-case reasoning for LDAGs was skipped for distributions in which the first variable of the graph was the only variable assigned to 1, which could cause identifiable queries to be reported as non-identifiable.
  * The search heuristic (`heuristic = TRUE`) now uses the score designed for missing data problems when missing data mechanisms are present. Previously this score was always overwritten by the default score.
  * The search heuristic now also scores the input distributions of DAGs, as it already did for LDAGs. Previously, every input distribution of a DAG had a score of zero, so the order in which they were expanded did not depend on the target. Formulas obtained with `heuristic = TRUE` may differ from previous versions.
  * Variable names that contain "do" (e.g., `dose` or `random`) are now parsed correctly in input and target distributions.
  * The print method of `summary.dosearch` objects now truncates long formulas correctly and prints `NA` when the formula was not requested.
  * `summary` no longer garbles bidirected edges given as `<->` when displaying the graph.
  * A variable can no longer be both conditioned on and intervened on in the same distribution (e.g., `p(y|x,do(x))`). Such distributions were previously accepted and encoded incorrectly.
  * Value assignments such as `z = 0` in DAGs without missing data mechanisms now produce an error. Previously they were silently interpreted as a new variable named `z=0`.
  * Malformed value assignments (e.g., `r_x=`) and variables that are duplicated once value assignments are removed (e.g., `p(y|z=0,z=1)`) now produce an error.
  * For LDAGs, every value assignment in the input and target distributions must now be either 0 or 1. Previously only the first assignment of each distribution was checked.
  * Edge labels of LDAGs are now validated: each assignment must be of the form `A = 0` or `A = 1`. Previously, a label such as `A = 2` silently removed the edge in every context.
  * Edge labels of LDAGs are now always attached to the edge on the same line, even when the graph contains duplicated lines. Labels given on separate lines for the same edge are now combined.
  * Missing data mechanisms are now validated: each mechanism must be of the form `M_X : X`, each variable can have only one mechanism, a mechanism cannot be shared by several variables, and a variable cannot be both a mechanism and a variable with missing data.
  * Only `->`, `<->` and `--` are now accepted as edge types. Previously, for example, `-->` was accepted and treated as both a directed and a bidirected edge.
  * Bidirected edges of `igraph` graphs in the `causaleffect` syntax are no longer dropped when the edge is given in only one direction. The graph is now read with `igraph::ends()` instead of `igraph::get.edges()`.
  * Unnamed `igraph` graphs with a single vertex are now named correctly.
  * The `rules` element of `control` is now validated, and duplicated rules are removed. Previously, an unknown rule caused the search to reuse the result of the previous rule, and supplying more rules than there are rule types caused an out-of-bounds write when `benchmark_rules = TRUE`.
  * The `con_vars` element of `control` must now refer to variables of the graph or the input distributions.
  * `benchmark_rules = TRUE` in `control` now implies `benchmark = TRUE`. The `rule_times` element of the output now has one element per rule used, named by the rule identifiers.
  * A warning is now issued (unless `warn = FALSE` in `control`) when the target distribution contains variables that appear neither in the graph nor in any input distribution, which typically indicates a typo.
  * The warning about response indicators that are not present in any input distribution now also takes indicators on the right-hand side of the input distributions into account.
  * Empty node names in `transportability` and `selection_bias` (e.g., from a stray comma) are now ignored.
  * Error messages now refer to the query instead of an input distribution for invalid LDAG queries, and lists of unknown control arguments and duplicated variables are now separated by commas.
  * The `cache`, `con_vars`, `improve` and `rules` elements of `control` are now documented, and the `empty` element is now shown as its own item in the documentation.
  * A warning is now issued (unless `warn = FALSE` in `control`) when a missing data mechanism or a variable with missing data is not present in the graph. Such variables are added to the graph with no edges other than the edge to the proxy variable, as is now documented. Previously, the documentation stated that they must be present in the graph, but this was not checked.
  * A warning is now issued (unless `warn = FALSE` in `control`) when an input distribution contains a variable that is not present in the graph, in any other input distribution or in the target distribution, which typically indicates a typo.
  * Fixed typos in the examples of `summary` and `print` that added a separate node `Z` to the example graphs, a typo in an example of `dosearch` that added a separate node `w_1`, and a dangling sentence in the documentation of the return value of `print`.
  * The warning about using both lower case and upper case names now determines the case of a name by its letters only, so that, e.g., `X_1` and `x` are also detected.
  * The package no longer depends on the `DOT` package.

# dosearch 1.0.12

  * Fixed an issue related to an upcoming `igraph` update.

# dosearch 1.0.11

  * Fixed a memory allocation issue with LDAGs.

# dosearch 1.0.10

  * Added support for user-defined context-specific variables for LDAGs via `con_vars` in `control`.
  * Removed C++11 from system requirements (for CRAN).

# dosearch 1.0.9

  * Added several sanity checks for the input distributions and the graph.
  * Updated the documentation in various aspects. Importantly, it is now noted that `dosearch` is not complete for missing data problems.
  * A non-identifiable query is now reported as follows: "The query is non-identifiable by do-search" to further clarify that the search was not able to identify the query.
  * Added a new Vignette for the package.
  * Added a `plot` method via the `DiagrammeR` package to produce derivation plots more easily.
  * An error is now issued when the input graph is too large.
  * Fixed an example on transportability in the `dosearch` function documentation.

# dosearch 1.0.8

  * Added citation for the related article in the Journal of Statistical Software.

# dosearch 1.0.7

  * Changed the default option of the search heuristic to `FALSE` for all instances.
  * Implemented minor performance tweaks to the search algorithm.
  * Added more options for benchmarking.
  * Restored a legacy control parameter for internal use.
  * Moved `igraph` and `dagitty` packages to 'Suggests'.

# dosearch 1.0.6

  * Fixed a case where the search would continue unnecessarily for some time after discovering the target distribution.

# dosearch 1.0.5

  * The resulting list of the 'dosearch' function call now has the class `dosearch` with print and summary methods.
  * Added accessor functions `get_formula`, `get_benchmark` and `is_identifiable` to query an object of class `dosearch`.
  * The function `get_derivation` is now repurposed as an accessor function, and `dosearch` is now the only function for query identification.
  * The `graph` argument of `dosearch` now supports the `igraph` format used by the `causaleffect` package (see `dosearch` documentation for examples).
  * The `graph` argument of `dosearch` now supports DAGs created using the `dagitty` package (see `dosearch` documentation for examples).
  * Fixed an issue when using `benchmark = TRUE` in `control`.

# dosearch 1.0.4

  * Fixed compilation issues on some platforms.

# dosearch 1.0.3

  * Now supports context specific independence relations (CSIs) via labeled directed acyclic graphs (LDAGs). See the documentation and examples for more details.
  * The function `get_derivation` can now also be called via `dosearch`.
  * Bidirected edges can now also be given as `<->`.
  * Added a new control parameter, `time_limit`, which will terminate the search when the given amount of time (in hours) has passed (only when `benchmark = TRUE`).
  * Some redundant control options have been removed.
  * Fixed an issue with derivation rules when missing data was present.
  * Fixed an another minor issue regarding missing data.
  * Fixed a rare issue with formula generation.

# dosearch 1.0.2

  * Added an example on how to produce an image from the DOT derivation.
  * Added a warning when the response indicator for a proxy variable is not present in any data source.

# dosearch 1.0.1

  * Added a Vignette describing the search procedure.
