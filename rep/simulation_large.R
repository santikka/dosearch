## Large-scale Simulation R script for: 
## Causal Effect Identification from Multiple Incomplete Data Sources:
## A General Search-based Approach
## Santtu Tikka, Antti Hyttinen, Juha Karvanen
## 2021-08-09

## NOTE: Do not call directly! Use via dosearch_job.bash only!

## A function for generating a random causal model (the graph)
# n          number of observed variables (nodes of the graph)
# pedge      probability of a directed edge
# pconf      probability of a bidirected edge
# force      select whether path X -> ... -> Y should be forced
random_graph <- function(n, pedge = 0.5, pconf = 0.5, force = TRUE) {
  if (force) {
    link <- FALSE
    while (!link) {
      M <- random_graph(n, pedge, pconf, force = FALSE)
      G <- M$G
      if (any(rowSums(M$G) == 0 & 
              colSums(M$G) == 0 & 
              colSums(M$Ge) == 0)) {
        next  # all nodes must be connected by at least an arc
      }
      diag(G) <- 1
      for (i in 1:n) G <- G %*% G
      link <- G[2, 1] != 0
    }
    return(M)
  }
  M <- list(G = array(0, c(n, n)), Ge = array(0, c(n, n)))
  M$order <- sample(n, n)  #this is the causal order
  M$G <- array(sample(c(0, 1), nrow(M$G) * ncol(M$G), 
                      prob = c(1 - pedge, pedge),
                      replace = TRUE), 
               c(n, n))
  M$G[upper.tri(M$G)] <- 0
  diag(M$G) <- 0
  M$G[M$order, M$order] <- M$G
  M$Ge <- array(sample(c(0, 1), nrow(M$G) * ncol(M$G), 
                       prob = c(1 - pconf, pconf),
                       replace = TRUE), 
                c(n, n))
  M$Ge[upper.tri(M$Ge)] <- 0
  diag(M$Ge) <- 0
  M$Ge <- M$Ge + t(M$Ge)
  if (n <= 15) {
    M$names <- c("x", "y", "z", "w", "v", "a", 
                 "b", "c", "d", "e", "f", "g",
                 "h", "i", "j")[1:n]
  } else {
    M$names <- paste("x_", 1:n, sep = "")
  }
  return(M)
}

## A function to convert a causal model M into
## the string representation used by dosearch
G2str <- function(M) {
  di <- "\n "
  bi <- ""
  nr <- nrow(M$G)
  for (x in 1:nr) {
    for (y in 1:nr) {
      if (M$G[x, y])
        di <- paste(di, M$names[y], "->", M$names[x], "\n", sep = " ")
      if (y < x & M$Ge[x, y])
        bi <- paste(bi, M$names[y], "--", M$names[x], "\n", sep = " ")
    }
  }
  return(paste(di, bi, sep = " "))
}

## The main simulation function
# n          : number of observed variables (nodes of the graph)
# edgedegree : expected directed edge degree
# confdegree : expected bidirected edge degree
dosearch_simulation <- function(n, edgedegree = 2, confdegree = 1) {
  total <- (n * n - n) / 2
  pedge <- n * edgedegree / (2 * total)
  pconf <- n * confdegree / (2 * total)
  R <- list()
  Rprev <- list()
  results <- list()
  id <- list()
  query <- "p(y|do(x))"
  confs <- list()
  confs[[1]] <- list(name = "Heuristic & Improvements", 
                     heuristic = TRUE, improve = TRUE)
  confs[[2]] <- list(name = "Heuristic only", 
                     heuristic = TRUE, improve = FALSE)
  confs[[3]] <- list(name = "Improvements only", 
                     heuristic = FALSE, improve = TRUE)
  confs[[4]] <- list(name = "Baseline", 
                     heuristic = FALSE, improve = FALSE)
  for (a in 1:length(confs)) {
    results[[a]] <- list()
    M <- random_graph(n, pedge, pconf, force = TRUE)
    M$str <- G2str(M)
    P <- c("p(x)")
    k <- 0
    roles <- c("obs", "unobs", "cond", "do")
    if (a == 1) {
      while (TRUE) {
        k <- k + 1
        while (TRUE) {
          status <- sample(roles, n, replace = TRUE)
          if (k == 1 && (status[2] == "obs" || status[1] == "do")) {
            next
          }
          if (all(status != "obs")) {
            next
          }
          str <- paste("p(", paste(sort(M$names[status == "obs"]), 
                                   collapse = ",",
                                   sep = ""), 
                       sep = "")
          if (any(status == "cond") || any(status == "do")) {
            str <- paste(str, "|", sep = "")
          }
          if (any(status == "cond")) {
            str <- paste(str, paste(sort(M$names[status == "cond"]), 
                                    collapse = ",",
                                    sep = ""), 
                         sep = "")
          }
          if (any(status == "cond") && any(status == "do")) {
            str <- paste(str, ",", sep = "")
          }
          if (any(status == "do")) {
            str <- paste(str, "do(", 
                         paste(sort(M$names[status == "do"]),
                               collapse = ",",
                               sep = " "), 
                         ")", sep = "")
          }
          str <- paste(str, ")", sep = "")
          if (str %in% P) {
            next
          } else break
        }
        P <- c(P, str)
        graph <- M$str
        data <- paste(P, collapse = " \n ")
        Rprev <- R
        R <- list()
        R$data <- data
        R$query <- query
        R$graph <- M$str
        temp <- dosearch(R$data, R$query, R$graph,
                         control = list(
                           improve = confs[[a]]$improve,
                           heuristic = confs[[a]]$heuristic,
                           verbose = FALSE,
                           formula = FALSE,
                           time_limit = -1,
                           benchmark = TRUE))
        R$id <- temp$identifiable
        R$t <- temp$time/1000
        if (R$id) {
          id[[i]] <- list(id_data = R$data, 
                          nonid_data = Rprev$data, 
                          graph = R$graph)
          break
        }
      }
    } else {
      R <- list()
      R$data <- id[[i]]$id_data
      R$query <- query
      R$graph <- id[[i]]$graph
      temp <- dosearch(R$data, R$query, R$graph,
                       control = list(improve = confs[[a]]$improve,
                                      heuristic = confs[[a]]$heuristic,
                                      verbose = FALSE,
                                      formula = FALSE,
                                      time_limit = -1,
                                      benchmark = TRUE))
      R$id <- temp$identifiable
      R$t <- temp$time/1000
      Rprev <- list()
      Rprev$data <- id[[i]]$nonid_data
      Rprev$query <- query
      Rprev$graph <- id[[i]]$graph
      temp <- dosearch(Rprev$data, Rprev$query, Rprev$graph,
                       control = list(improve = confs[[a]]$improve,
                                      heuristic = confs[[a]]$heuristic,
                                      verbose = FALSE,
                                      formula = FALSE,
                                      time_limit = -1,
                                      benchmark = TRUE))
      Rprev$id <- temp$identifiable
      Rprev$t <- temp$time/1000
    }
    results[[a]] <- list(R = R, Rprev = Rprev)
  }
  return(list(n = n, confs = confs, results = results))
}

## Get the command arguments
args <- commandArgs(trailingOnly = TRUE)
lib_path <- as.character(args[1])
result_path <- as.character(args[2])
N <- 4:max(4, as.numeric(args[3]))
arr_id <- as.numeric(args[4])

## Add the package path to .libPaths
.libPaths(c(lib_path, .libPaths()))
options(warn = 1)

library(dosearch)

## Initialize the cluster and MPI
library(doMPI, quietly = TRUE)
cl <- startMPIcluster()
registerDoMPI(cl)
cl_size <- clusterSize(cl)

## Conduct the simulation
sim_result <- list()
for (n in N) {
  ## The argument .options.mpi.seed ensures reproducibility
  ## (up to element order in the resulting list)
  sim_result[[n - N[1] + 1]] <- 
    foreach(i = 1:cl_size, 
            .packages = "dosearch",
            .options.mpi = list(seed = arr_id)
    ) %dopar% dosearch_simulation(n = n)
}

## Save the simulation results
save(sim_result,
     file = paste0(result_path,
                   "/dosearch_simulation_results_", 
                   arr_id, ".RData"))

## Close the cluster and terminate MPI
closeCluster(cl)
mpi.quit()
