# Replication R script for:
# Causal Effect Identification from Multiple Incomplete Data Sources:
# A General Search-based Approach
# Santtu Tikka, Antti Hyttinen, Juha Karvanen
# 2021-08-09

# Install the dosearch package
# devtools::install_github("santikka/dosearch")
# install.packages("dosearch_1.0.7.tar.gz")
# install.packages("dosearch")

# Install other required packages
# install.packages(c("ggplot2", "scales", "foreach"))

# Load the packages
library("dosearch")
library("ggplot2")
library("scales")
library("foreach")

# Assume that the file path is the directory of this script
# and contains the directory "Results"
file_path <- "."

# Examples of Section 5
data <- "
  P(W)
  P(Y|X)
  P(Z|do(X),W)
"

query <- "P(Y|do(X),W)"
query <- c(Y = 0, X = 1, W = 2)

data <- list(
  c(W = 0),
  c(Y = 0, X = 2),
  c(Z = 0, X = 1, W = 2)
)

graph <- "
  X -> Y
  Z -> X
  Z -> Y
  X <-> Y
"

library("igraph")
graph <- graph_from_literal(X -+ Y, Z -+ X, Z -+ Y, X -+ Y, Y -+ X)
graph <- set.edge.attribute(graph, "description", 4:5, "U")

library("dagitty")
graph <- dagitty("dag{X -> Y; Z -> X; Z -> Y; X <-> Y}")


# Simulations
# Parsing and plotting functions

#' Combine the Individual Result Objects
#'
#' @param path Path to the 'Results' directory as a string
parse_results <- function(path) {
  file_paths <- list.files(path, full.names = TRUE)
  is_result <- grepl(".*dosearch_simulation_results.*", file_paths)
  result_files <- file_paths[is_result]
  x <- lapply(result_files, function(f) {
    load(f, envir = environment())
    sim_result
  })
  y <- do.call(mapply, args = c(list(FUN = c, SIMPLIFY = FALSE), x))
  confs <- y[[1]][[1]]$confs
  size_min <- y[[1]][[1]]$n
  size_max <- size_min + length(y) - 1L
  n_conf <- length(confs)
  n_inst <- length(y[[1]])
  res <- setNames(
    vector(mode = "list", length = length(y)),
    size_min:size_max
  )
  for (i in seq_along(y)) {
    res[[i]] <- setNames(
      vector(mode = "list", length = n_conf),
      sapply(confs, "[[", "name")
    )
    for (j in 1:n_conf) {
      res[[i]][[j]] <- vector(mode = "list", length = n_inst)
      for (k in 1:n_inst) {
        res[[i]][[j]][[k]] <- y[[i]][[k]]$results[[j]]
      }
    }
  }
  return(res)
}

#' Creates a Data Frame Representation of the Simulation Results
#'
#' @param res_single Simulation result object for a single graph size
result_dataframe <- function(res_single) {
  n_conf <- length(res_single)
  n_inst <- length(res_single[[1]])
  confs <- names(res_single)
  total <- n_conf * n_inst * 2
  res_df <- data.frame(
    time = numeric(total),
    config = factor(rep(NA, total), levels = confs),
    id = factor(rep(NA, total), levels = c("id", "nonid"))
  )
  for (j in 1:n_conf) {
    for (i in 1:n_inst) {
      x <- res_single[[j]][[i]]
      res_df[(j - 1) * n_inst + i, 1] <- x$R$t
      res_df[(j - 1) * n_inst + i, 2:3] <- c(confs[j], "id")
      res_df[n_inst * n_conf + (j - 1) * n_inst + i, 1] <- x$Rprev$t
      res_df[n_inst * n_conf + (j - 1) * n_inst + i, 2:3] <-
        c(confs[j], "nonid")
    }
  }
  return(res_df)
}

#' Compute Statistics of the Computation Times
#'
#' @param df A `data.frame` of the simulation results (via `result_dataframe`)
result_stats <- function(df) {
  with(df, {
    list(
      dominance = data.frame(
        mean_id = mean(df[id == "id" &
          config == "Heuristic & Improvements", "time"] <=
          df[id == "id" & config == "Baseline", "time"]
        ),
        mean_nonid = mean(df[id == "nonid" &
          config == "Heuristic & Improvements", "time"] <=
          df[id == "nonid" & config == "Baseline", "time"]
        ),
        total_id = sum(df[id == "id" &
          config == "Heuristic & Improvements", "time"] <=
          df[id == "id" & config == "Baseline", "time"]
        ),
        total_nonid = sum(df[id == "nonid" &
          config == "Heuristic & Improvements", "time"] <=
          df[id == "nonid" & config == "Baseline", "time"]
        )
      ),
      mean_times = aggregate(time ~ config + id, FUN = mean, data = df)
    )
  })
}

#' Create a Scatter Plot of the Computation Times
#'
#' Creates scatter plots of different configurations vs. baseline for
#' identifiable instances
#'
#' @param df A `data.frame` of the simulation results (via `result_dataframe`)
#' @param id Either `TRUE` for identifiable instances or `FALSE` if non-ID
#' @param conf The configuration of the search
#' @param sim_scale Either "small" or "large" depending on the scale of the
#'   simulation
plot_scatter <- function(df, id = TRUE,
                         sim_scale = c("large", "small"),
                         conf = "Heuristic & Improvements") {
  id_str <- if (id) "id" else "nonid"
  sim_scale <- match.arg(sim_scale)
  scale_limits <- c(0, 500)
  if (identical(sim_scale, "small")) {
    scale_limits <- c(0, 1)
  }
  df1 <- subset(df, id == id_str & config == conf)
  df2 <- subset(df, id == id_str & config == "Baseline")
  df1$btime <- df2$time
  keep <- function(x, ...) x
  p <- ggplot(df1, aes(x = time, y = btime)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1, lwd = 1.0) +
    theme_bw(base_size = 24) +
    theme(
      aspect.ratio = 1,
      axis.title.y = element_text(
        size = 22,
        margin = margin(t = 0, r = 12, b = 0, l = 0)
      ),
      axis.title.x = element_text(
        size = 22,
        margin = margin(t = 12, r = 0, b = 0, l = 0)
      )
    ) +
    ylab("Basic (s)") +
    xlab(paste(conf, "(s)")) +
    scale_y_continuous(
      limits = scale_limits,
      oob = keep,
      expand = expansion(mult = c(0.015, 0.01))
    ) +
    scale_x_continuous(
      limits = scale_limits,
      oob = keep,
      expand = expansion(mult = c(0.015, 0.01))
    )
  p
}

#' Create Boxplots of Computation Times for Each Graph Size
#'
#' @param res A simulation result object
#' @param sim_scale : either "small" or "large" depending on the scale of the
#'   simulation
scalability_boxplots <- function(res, sim_scale = c("small", "large")) {
  m <- length(res)
  N <- as.numeric(names(res))
  n_inst <- length(res[[1]][[1]])
  res_mat <- matrix(0, 2 * n_inst * m, 2)
  eps <- 1e-9
  sim_scale <- match.arg(sim_scale)
  scale_limits <- c(1e-4, 500)
  scale_breaks <-  c(0.01, 0.1, 1, 10, 100)
  if (identical(sim_scale, "small")) {
    scale_limits <- c(1e-6, 1)
    scale_breaks <- c(0.00001, 0.0001, 0.001, 0.01, 0.1, 1)
  }
  for (i in 1:m) {
    x <- res[[i]][[1]]
    for (j in 1:n_inst) {
      y <- x[[j]]
      res_mat[2 * n_inst * (i - 1) + j, 1] <- y$R$t + eps
      res_mat[2 * n_inst * (i - 1) + j, 2] <- N[i]
      res_mat[2 * n_inst * (i - 1) + n_inst + j, 1] <- y$Rprev$t + eps
      res_mat[2 * n_inst * (i - 1) + n_inst + j, 2] <- N[i]
    }
  }
  colnames(res_mat) <- c("time", "n")
  df <- as.data.frame(res_mat)
  df$n <- as.factor(df$n)
  point <- format_format(
    big.mark = " ",
    decimal.mark = ".",
    scientific = FALSE,
    drop0trailing = TRUE
  )
  p <- ggplot(df, aes(x = n, y = time)) +
    geom_boxplot() +
    theme_bw(base_size = 24) +
    theme(
      legend.key.width = unit(2, "cm"),
      axis.title.y = element_text(
        size = 18,
        margin = margin(t = 0, r = 12, b = 0, l = 0)
      ),
      axis.title.x = element_text(
        size = 18,
        margin = margin(t = 8, r = 0, b = 0, l = 0)
      ),
      panel.grid.major.x = element_blank()
    ) +
    coord_cartesian(ylim = scale_limits) +
    scale_y_continuous(
      trans = log10_trans(),
      breaks = scale_breaks,
      labels = point
    ) +
    ylab("Time (s)") +
    xlab("n")
  p
}

#' Generate All Plots of the Simulation
#'
#' @param res A Simulation result object
#' @param df A `data.frame` of the simulation results (via result_dataframe)
#' @param sim_scale Either "small" or "large" depending on the scale of the
#'   simulation
#' @param save_plots If `TRUE`, plots will be saved as .pdf to 'path'
#' @param path A directory to save the plots if save_plots is `TRUE`
generate_plots <- function(res, df, sim_scale,
                           save_plots = FALSE, path) {
  plots <- list()
  plots[[1]] <- plot_scatter(
    df,
    id = TRUE,
    sim_scale,
    conf = "Heuristic & Improvements"
  )
  plots[[2]] <- plot_scatter(
    df,
    id = TRUE,
    sim_scale,
    conf = "Heuristic only"
  )
  plots[[3]] <- plot_scatter(
    df,
    id = TRUE,
    sim_scale,
    conf = "Improvements only"
  )
  plots[[4]] <- plot_scatter(
    df,
    id = FALSE,
    sim_scale,
    conf = "Heuristic & Improvements"
  )
  plots[[5]] <- plot_scatter(
    df,
    id = FALSE,
    sim_scale,
    conf = "Heuristic only"
  )
  plots[[6]] <- plot_scatter(
    df,
    id = FALSE,
    sim_scale,
    conf = "Improvements only"
  )
  plots[[7]] <- scalability_boxplots(res, sim_scale)
  if (save_plots) {
    ggsave(
      paste0(path, "/scatter_id_hi.pdf"),
      plot = plots[[1]],
      width = 7,
      height = 7
    )
    ggsave(
      paste0(path, "/scatter_id_h.pdf"),
      plot = plots[[2]],
      width = 7,
      height = 7
    )
    ggsave(
      paste0(path, "/scatter_id_i.pdf"),
      plot = plots[[3]],
      width = 7,
      height = 7
    )
    ggsave(
      paste0(path, "/scatter_nonid_hi.pdf"),
      plots[[4]],
      width = 7,
      height = 7
    )
    ggsave(
      paste0(path, "/scatter_nonid_h.pdf"),
      plot = plots[[5]],
      width = 7,
      height = 7
    )
    ggsave(
      paste0(path, "/scatter_nonid_i.pdf"),
      plot = plots[[6]],
      width = 7,
      height = 7
    )
    ggsave(
      paste0(path, "/time_by_n.pdf"),
      plot = plots[[7]],
      width = 8.5,
      height = 7
    )
  } else {
    return(plots)
  }
  return(invisible(NULL))
}


result_path <- paste0(file_path, "/Results")
# Check that the "Results" directory exists, or create it
if (!dir.exists(result_path)) {
  dir.create(result_path)
}
# Check that there are result files in the directory
file_names <- list.files(result_path)
if (!length(file_names) ||
    all(!grepl("dosearch_simulation_results.*", file_names))) {
  print("No pre-existing simulation results found!")
  print("Running the small-scale simulation...")
  # This will generate an .RData file under "Results"
  source(paste0(file_path, "/simulation_small.R"))
}

# Generate the plots
res <- parse_results(result_path)
sim_scale <- if (tail(names(tail(res)), 1) == "7") "small" else "large"
df <- result_dataframe(res[[length(res)]])
plots <- generate_plots(res, df, sim_scale, save_plots = FALSE)

# View the individual plots

# Scatterplots
print(plots[[1]]) # ID instances, heuristic and improvements
print(plots[[2]]) # ID instances, heuristic only
print(plots[[3]]) # ID instances, improvements only
print(plots[[4]]) # Non-ID instances, heuristic and improvements
print(plots[[5]]) # Non-ID instances, heuristic only
print(plots[[6]]) # Non-ID instances, improvements only
print(plots[[7]]) # Boxplots of computation time

# Save plots as .pdf
generate_plots(
  res,
  df,
  sim_scale,
  save_plots = TRUE,
  path = result_path
)

# Get the simulation metrics (proportion of dominated instances and
# mean computation times)
# the first two elements of $dominance are the proportions of
# instances where heuristic and improvements are at least as good
# as the baseline search (id and non-id)
# the last two are the instance counts, respectively.
# $mean_times gives the mean computation times of instances
# for each configuration
result_stats(df)

# Examples of Section 6

# Subsection 6.1 HR department example
data1 <- "
   p(y,b,e,x)
   p(a,b,x)
"
query1 <- "p(y|do(x))"
graph1 <- "
    e -> x
    e -> y
    a -> b
    a -> x
    x -> b
    x -> y
    b -> y
"
dosearch(data1, query1, graph1, control = list(heuristic = TRUE))
# Joint distribution is not identifiable in this case
dosearch(data1, "p(y,b,e,x,a)", graph1, control = list(heuristic = TRUE))

# 2nd example
data2 <- "
  p(x_1,y_1,x_2,y_2,z,w)
  p(y_1,y_2|z,w,x_2,do(x_1))
  p(y_2|y_1,z,w,x_2,do(x_1))
  p(w|do(x_1,x_2))
  p(z|do(x_2))
"
query2 <- "p(y_1,y_2|do(x_1,x_2))"
graph2 <- "
  z -> y_1
  w -> y_1
  y_1 -> y_2
  x_2 -> z
  x_1 -> w
  y_1 <-> x_1
  y_1 <-> y_2
  y_2 <-> z
  y_1 <-> w
  y_2 <-> w
"
dosearch(data2, query2, graph2, control = list(heuristic = TRUE))

# 3rd example
data3 <- "
  p(x_1,y_1,x_2,y_2,z,w)
  p(y_1,y_2|w,x_1,x_2,do(z))
  p(y_1|y_2,w,z,x_2,do(x_1))
  p(y_2|x_1,w,x_2,do(z))
  p(x_2,w|do(x_1))
  p(x_2|do(x_1,w))
  p(y_2|z,w,x_2,do(x_1))
"
query3 <- "p(y_1,y_2|do(x_1,x_2))"
graph3 <- "
  y_2 -> y_1
  w -> y_1
  x_1 -> x_2
  x_1 -> y_2
  z -> y_2
  w -> y_2
  x_2 -> w
  x_1 <-> y_1
  x_1 <-> y_2
  y_1 <-> z
  x_2 <-> z
"
dosearch(data3, query3, graph3, control = list(heuristic = TRUE))

# Same effect but without the heuristic
dosearch(data3, query3, graph3)

# Subsection 6.2
data4 <- "
  p(x,z,y|s)
  p(y,z|t,do(x))
"
query4 <- "p(y|do(x))"
graph4 <- "
  x -> z
  z -> y
  x -> s
  t -> z
  x <-> y
"
dosearch(data4, query4, graph4,
         transportability = "t",
         selection_bias = "s",
         control = list(heuristic = TRUE))

# Subsection 6.3
data5 <- "
  p(x,y,z,w_1,w_2|s_1,s_2)
  p(z|s_1)
"
query5 <- "p(y|do(x))"
graph5 <- "
  w_1 -> w_2
  z -> w_2
  x -> y
  z -> y
  z -> s_2
  w_1 -> x
  w_2 -> x
  w_1 -> s_1
"
dosearch(data5, query5, graph5, selection_bias = "s_1, s_2")

# Subsection 6.4
data6 <- "p(x*,y*,r_x,r_y)"
graph61 <- "
  x -> y
  x -> r_y
  r_x -> r_y
  r_x <-> r_y
"
graph62 <- "
  x -> y
  r_y -> r_x
  r_x <-> r_y
  r_x <-> y
  x <-> y
"
graph63 <- "
  x -> y
  r_x <-> y
"
graph64 <- "
  x -> y
  x -> r_y
  r_y -> r_x
  y -> r_x
  x <-> y
"
graph65 <- "
  x -> y
  x -> r_y
  r_y -> r_x
  y -> r_x
  x <-> r_y
"
graph66 <- "
  x -> y
  x -> r_x
  r_x -> r_y
  x <-> y
"
md6 <- "r_x : x, r_y : y"

# Example 2, both identfiable
dosearch(data6, "p(x,y)", graph61, missing_data = md6)
dosearch(data6, "p(y|do(x))", graph61, missing_data = md6)

# Example 3, marginals identifiable, joint distribution is not identifiable and
# neither is the causal effect
dosearch(data6, "p(x)", graph62, missing_data = md6)
dosearch(data6, "p(y)", graph62, missing_data = md6)
dosearch(data6, "p(x,y)", graph62, missing_data = md6)
dosearch(data6, "p(y|do(x))", graph62, missing_data = md6)

# Minimal graph for Example 3
dosearch(data6, "p(x)", graph63, missing_data = md6)
dosearch(data6, "p(y)", graph63, missing_data = md6)
dosearch(data6, "p(x,y)", graph63, missing_data = md6)
dosearch(data6, "p(y|do(x))", graph63, missing_data = md6)

# Graph where marginals, joint and the causal effect are not identifiable, but
# the conditional is
dosearch(data6, "p(x)", graph64, missing_data = md6)
dosearch(data6, "p(y)", graph64, missing_data = md6)
dosearch(data6, "p(x,y)", graph64, missing_data = md6)
dosearch(data6, "p(y|do(x))", graph64, missing_data = md6)
dosearch(data6, "p(y|x)", graph64, missing_data = md6)

# Graph where only the causal effect and the conditional are identifiable
dosearch(data6, "p(x)", graph65, missing_data = md6)
dosearch(data6, "p(y)", graph65, missing_data = md6)
dosearch(data6, "p(x,y)", graph65, missing_data = md6)
dosearch(data6, "p(y|do(x))", graph65, missing_data = md6)
dosearch(data6, "p(y|x)", graph65, missing_data = md6)

# Graph where the joint, marginal of X and the causal effect are not
# identifiable
dosearch(data6, "p(x)", graph66, missing_data = md6)
dosearch(data6, "p(y)", graph66, missing_data = md6)
dosearch(data6, "p(x,y)", graph66, missing_data = md6)
dosearch(data6, "p(y|do(x))", graph66, missing_data = md6)
dosearch(data6, "p(y|x)", graph66, missing_data = md6)

# Subsection 6.5
data7 <- "
  p(x*,y*,z*,r_x,r_y,r_z)
  p(y)
"
graph7 <- "
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
md7 <- "r_x : x, r_y : y, r_z : z"
query71 <- "p(z|x)"
query72 <- "p(x)"
query73 <- "p(y|x,z)"

dosearch(data7, query71, graph7, missing_data = md7)
dosearch(data7, query72, graph7, missing_data = md7)
dosearch(data7, query73, graph7, missing_data = md7)

sessionInfo()
