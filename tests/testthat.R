#' @srrstats {G5.2} Error and warning behaviour is tested.
#' @srrstats {G5.2a} Messages are unique.
#' @srrstats {G5.2b} Conditions are demonstrated.
#' @srrstats {G5.4, G5.4a, G5.6} Output of dosearch is compared against known
#'   theoretical identifiability and non-identifiability results.
#'   In other words, correctness can be tested independently of the
#'   implementation.
#' @srrstats {G5.5} The algorithm is deterministic, so this could also be NA
#' @srrstats {G5.6a} Because the package output is symbolic, the recovery
#'   of e.g., a causal effect is exact.
#' @srrstats {G5.7} Performance and scalability have been demonstrated
#'   in the JSS paper, and the simulations are too large to run on a single
#'   PC and require a cluster. The replication materials for this simulation
#'   are included in the 'rep' directory. The algorithm has exponential
#'   time and memory complexity in the number of the vertices of the graph,
#'   and it has been hypothesized that the general identifiability problem
#'   itself is NP-hard.
#' @srrstats {G5.8, G5.8a G5.8b, G5.8c, G5.8d} Edge conditions are tested.
#' @noRd
library(testthat)
library(dosearch)

test_check("dosearch")
