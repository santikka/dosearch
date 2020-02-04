\name{get_benchmark}
\alias{get_benchmark}
\title{Benchmark of a specific run of the search}
\description{Returns the benchmarking information of an object of class "\link{dosearch}". }

\usage{get_benchmark(x, run_again = FALSE)}

\arguments{
  \item{x}{an object of class "\link{dosearch}".}
  \item{run_again}{a logical value. If \code{TRUE}, run the search again to obtain the benchmarking information if  not requested in the function call that produced \code{x}.}
}

\value{A list with two elements. The first describes the total time taken by the search in milliseconds. The second is a numeric vector of the time taken by each inference rule of the search in milliseconds.
}

\author{Santtu Tikka}

\examples{

data <- "P(x,y,z)"
query <- "P(y|do(x))"
graph <- "
  x -> y
  z -> x
  z -> y
"
x <- dosearch(data, query, graph, control = list(benchmark = FALSE))
get_benchmark(x, run_again = TRUE)
}