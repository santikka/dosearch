\name{get_benchmark}
\alias{get_benchmark}
\title{Benchmark a specific run of the search}
\description{Returns the benchmarking information of an object of class "\link{dosearch}". }

\usage{get_benchmark(x, run_again = FALSE, include_rules = FALSE)}

\arguments{
  \item{x}{an object of class "\link{dosearch}". }
  \item{run_again}{a logical value. If \code{TRUE}, run the search again to obtain the benchmarking information if it was not requested in the function call that produced \code{x}. }
  \item{include_rules}{A logical value. If \code{TRUE}, also benchmark the time taken by each inference rule separately. }
}

\value{A list with one or two elements. The first is always a numeric value of the total time taken by the search in milliseconds. The second is a numeric vector of the time taken by each inference rule (in the internal C++ implementation) of the search in milliseconds if \code{include_rules = TRUE}.
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