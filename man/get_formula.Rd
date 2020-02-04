\name{get_formula}
\alias{get_formula}
\title{Retrieve the identifying formula of a causal query}
\description{Returns the identifying formula describing a causal query of an object of class "\link{dosearch}". }

\usage{get_formula(x, run_again = FALSE)}

\arguments{
  \item{x}{an object of class "\link{dosearch}".}
  \item{run_again}{a logical value. If \code{TRUE}, run the search again to obtain a formula for the query if one was not requested in the function call that produced \code{x}.}
}

\value{A character string representing the query in terms of the input data. }

\author{Santtu Tikka}

\examples{

data <- "P(x,y,z)"
query <- "P(y|do(x))"
graph <- "
  x -> y
  z -> x
  z -> y
"
x <- dosearch(data, query, graph, control = list(formula = FALSE))
get_formula(x, run_again = TRUE)
}
