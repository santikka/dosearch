\name{is_identifiable}
\alias{is_identifiable}
\title{Query whether the target distribution was identifiable or not}
\description{Returns the a logical value describing the identifiability of a causal query of an object of class "\link{dosearch}". }

\usage{is_identifiable(x)}

\arguments{
  \item{x}{an object of class "\link{dosearch}".}
}

\value{A logical value. If \code{TRUE}, the target distribution is identifiable from the available inputs. }

\author{Santtu Tikka}

\examples{

data <- "P(x,y,z)"
query <- "P(y|do(x))"
graph <- "
  x -> y
  z -> x
  z -> y
"
x <- dosearch(data, query, graph)
is_identifiable(x)
# TRUE
}
