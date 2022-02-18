\name{dosearch}
\alias{dosearch}
\title{Identify a causal effect from arbitrary experiments and observations}
\description{Identify a causal \code{query} from available \code{data} in a causal model described by a \code{graph} that is a semi-Markovian DAG or a labeled directed acyclic graph (LDAG). For DAGs, special mechanisms related to transportability of causal effects, recoverability from selection bias and identifiability under missing data can also be included. }

\usage{dosearch(data, query, graph, 
         transportability, selection_bias, missing_data, 
         control)}

\arguments{
  \item{data}{a character string describing the available distributions in the package syntax. Alternatively, a list of character vectors. See \sQuote{Details}.}
  \item{query}{a character string describing the target distribution in the package syntax. Alternatively, a character vector. See \sQuote{Details}.}
  \item{graph}{a character string describing either a DAG or an LDAG in the package syntax. Alternatively, an "igraph" graph as used in the "causaleffect" package or a DAG constructed using the "dagitty" package. See \sQuote{Details}.}
  \item{transportability}{a character string describing the transportability nodes of the model in the package syntax (for DAGs only). See \sQuote{Details}.}
  \item{selection_bias}{a character string describing the selection bias nodes of the model in the package syntax (for DAGs only). See \sQuote{Details}.}
  \item{missing_data}{a character string describing the missing data mechanisms of the model in the package syntax (for DAGs only). See \sQuote{Details}.}
  \item{control}{a \link{list} of control parameters. See \sQuote{Details}. }
}

\details{

\code{data} is used to list the available input distributions. When \code{graph} is a DAG the distributions should be of the form

\ifelse{html}{\out{<p> P(A<sub>i</sub>|do(B<sub>i</sub>),C<sub>i</sub>) </p>}}{\deqn{P(A_i|do(B_i),C_i).}}

Individual variables within sets should be separated by a comma. For example, three input distributions

\ifelse{html}{\out{<p> P(Z|do(X)), P(W,Y|do(Z,X)), P(W,Y,X|Z) </p>}}{\deqn{P(Z|do(X)), P(W,Y|do(Z,X)), P(W,Y,X|Z), }}

should be given as follows: 
\preformatted{
> data <- "
+  P(Z|do(X))
+  P(W,Y|do(Z,X))
+  P(W,Y,X|Z)
+"
}

The use of multiple do-operators is not permitted. Furthermore, when both conditioning variables and a do-operator are present, every conditioning variable must either precede the do-operator or follow it. When \code{graph} is an LDAG, the do-operation is represented by an intervention node, i.e.,

\ifelse{html}{\out{<p>P(Y|do(X),Z) = P(Y|X,Z,I_X = 1)</p>}}{\deqn{P(Y|do(X),Z) = P(Y|X,Z,I_X = 1)}}

For example, in the case of the previous example in an LDAG, the three input distributions become:
\preformatted{
> data <- "
+  P(Z|X,I_X = 1)
+  P(W,Y|Z,X,I_X=1,I_Z=1)
+  P(W,Y,X|Z)
+"
}
The intervention nodes \eqn{I_X} and \eqn{I_Z} must be explicitly defined in the \code{graph} along with the relevant labels for the edges.

\code{query} is the target distribution of the search. It has the same syntax as \code{data}, but only a single distribution should be given.

\code{graph} is a description of a directed acyclic graph where directed edges are denoted by \code{->} and bidirected arcs corresponding to unobserved confounders are denoted by \code{<->} (or by \code{--}). As an example, a DAG with two directed edges and one bidirected edge is constructed as follows: 
\preformatted{
> graph <- "
+  X -> Z
+  Z -> Y
+  X <-> Y
+"
}
Some alternative formats for DAGs are supported as well. Graphs created using the \code{igraph} package in the \code{causal.effect} syntax can be used here. Similarly, DAGs created using \code{dagitty} are supported.

LDAGs are constructed similarly with the addition of labels and with the omission bidirected edges (latent variables must be explicitly defined). As an example, an LDAG with two labeled edges can be constructed as follows:
\preformatted{
  > graph <- "
+  X -> Z : A = 0
+  Z -> Y : A = 1
+  A -> Z
+  A -> Y
+"
}
Here the labels indicate that the edge from \eqn{X} to \eqn{Z} vanishes when \eqn{A} has the value 0 and the edge from \eqn{Z} to \eqn{Y} vanishes when A has the value 1. Multiple labels on the same edge should be separated by a semi-colon. 

\code{transportability} enumerates the nodes that should be understood as transportability nodes responsible for discrepancies between domains. Individual variables should be separated by a comma. See e.g., Bareinboim and Pearl (2014) for details on transportability.

\code{selection_bias} enumerates the nodes that should be understood as selection bias nodes responsible for bias in the input data sets. Individual variables should be separated by a comma. See e.g., Bareinboim and Pearl (2014) for details on selection bias recoverability.

\code{missing_data} enumerates the missingness mechanisms of the model. The syntax for a single mechanism is \code{M_X : X} where \ifelse{html}{\out{M<sub>X</sub>}}{\eqn{M_X}} is the mechanism for \eqn{X}. Individual mechanisms should be separated by a comma. Note that both \ifelse{html}{\out{M<sub>X</sub>}}{\eqn{M_X}} and \eqn{X} must be present in the graph if the corresponding mechanism is given as input. Proxy variables should not be included in the graph, since they are automatically generated based on \code{missing_data}. By default, a warning is issued if a proxy variable is present in an input distribution but its corresponding mechanism is not present in any input. See e.g., Mohan, Pearl and Tian (2013) for details on missing data as a causal inference problem.

The \code{control} argument is a list that can supply any of the following components:
\describe{
  \item{\code{benchmark}}{A logical value. If \code{TRUE}, the search time is recorded and returned (in milliseconds). Defaults to \code{FALSE}. }
  \item{\code{benchmark_rules}}{A logical value. If \code{TRUE}, the time taken by each individual inference rule is also recorded in the benchmark (in milliseconds). Defaults to \code{FALSE}. }
  \item{\code{draw_derivation}}{A logical value. If \code{TRUE}, a string representing the derivation steps as a DOT graph is returned. The graph can be exported as an image for example by using the \code{DOT} package. Defaults to \code{FALSE}. }
  \item{\code{draw_all}}{A logical value. If \code{TRUE} and if \code{draw_derivation = TRUE}, the derivation will contain every step taken by the search. If \code{FALSE}, only steps that resulted in an identifiable target are returned. Defaults to \code{FALSE}. }
  \item{\code{formula}}{A logical value. If \code{TRUE}, a string representing the identifiable query is returned when the target query is identifiable. If \code{FALSE}, only a logical value is returned that takes the value \code{TRUE} for an identifiable target and \code{FALSE} otherwise. Defaults to \code{TRUE}. }
  \item{\code{heuristic}}{A logical value. If \code{TRUE}, new distributions are expanded according to a search heuristic (see Tikka et al. (2019) for details). Otherwise, distributions are expanded in the order in which they were identified. Defaults to \code{FALSE}. }
  \item{\code{md_sym}}{A single character describing the symbol to use for active missing data mechanisms. Defaults to \code{"1"}. }
  \item{\code{time_limit}}{A numeric value giving a time limit for the search (in hours). Defaults to a negative value that disables the limit. }
  \item{\code{verbose}}{A logical value. If \code{TRUE}, diagnostic information is printed to the console during the search. Defaults to \code{FALSE}. }
  \item{\code{warn}}{A logical value. If \code{TRUE}, a warning is issued for possibly unintentionally misspecified but syntactically correct input distributions. }
}

} % END DETAILS

\value{An object of class \code{dosearch} which is a list with the following components by default. See the options of \code{control} for how to obtain a graphical representation of the derivation or how to benchmark the search. 

\describe{
  \item{\code{identifiable}}{A logical value that attains the value \code{TRUE} is the target quantity is identifiable and \code{FALSE} otherwise. }
  \item{\code{formula}}{A character string describing a formula for an identifiable query or an empty character vector for an unidentifiable effect. }
}

} % END VALUE

\author{Santtu Tikka}

\references{
S. Tikka, A. Hyttinen and J. Karvanen. Causal effect identification from multiple incomplete data sources: a general search-based approach. \emph{Journal of Statistical Software}, 99(5):1--40, 2021.
}

\examples{
## Simple back-door formula
data1 <- "P(x,y,z)"
query1 <- "P(y|do(x))"
graph1 <- "
  x -> y
  z -> x
  z -> y
"
dosearch(data1, query1, graph1)

## Simple front-door formula
data2 <- "P(x,y,z)"
query2 <- "P(y|do(x))"
graph2 <- "
  x -> z
  z -> y
  x <-> y
"
dosearch(data2, query2, graph2)

## Graph input using 'igraph' in the 'causaleffect' syntax
if (requireNamespace("igraph", quietly = TRUE)) {
  g_igraph <- igraph::graph.formula(x -+ z, z -+ y, x -+ y, y -+ x)
  g_igraph <- igraph::set.edge.attribute(g_igraph, "description", 3:4, "U")
  dosearch(data2, query2, g_igraph)
}

## Graph input with 'dagitty'
if (requireNamespace("dagitty", quietly = TRUE)) {
  g_dagitty <- dagitty::dagitty("dag{x -> z -> y; x <-> y}")
  dosearch(data2, query2, g_dagitty)
}

## Alternative distribution input style using lists and vectors:
## Each element of the list describes a single distribution
## Each element is a character vector that describes the role
## of each variable in the distribution as follows:
## For a variable V and a distribution P(A|do(B),C) we have
##   V = 0, if V is in A
##   V = 1, if V is in B
##   V = 2, if V is in C
data_alt <- list(
  c(x = 0, y = 0, z = 0) # = P(x,y,z)
)
query_alt <- c(x = 1, y = 0) # = P(y|do(x))
dosearch(data_alt, query_alt, graph2)

## Additional examples
\dontrun{

## Multiple input distributions (both observational and interventional)
data3 <- "
  p(z_2,x_2|do(x_1))
  p(z_1|x_2,do(x_1,y))
  p(x_1|w_1,do(x_2))
  p(y|z_1,z_2,x_1,do(x_2))
  p(w|y,x_1,do(x_2))
"
query3 <- "p(y,x_1|w,do(x_2))"
graph3 <- "
  x_1 -> z_2
  x_1 -> z_1
  x_2 -> z_1
  x_2 -> z_2
  z_1 -> y
  z_2 -> y
  x_1 -> w
  x_2 -> w
  z_1 -> w
  z_2 -> w
"
dosearch(data3, query3, graph3)

## Selection bias
data4 <- "
  p(x,y,z_1,z_2|s)
  p(z_1,z_2)
"
query4 <- "p(y|do(x))"
graph4 <- "
  x   -> z_1
  z_1 -> z_2
  x   -> y
  y   -- z_2
  z_2 -> s
"
dosearch(data4, query4, graph4, selection_bias = "s")

## Transportability
data5 <- "
  p(x,y,z_1,z_2)
  p(x,y,z_1|s_1,s_2,do(z_2))
  p(x,y,z_2|s_3,do(z_1))
"
query5 <- "p(y|do(x))"
graph5 <- "
  z_1 -> x
  x   -> z_2
  z_2 -> y
  z_1 <-> x
  z_1 <-> z_2
  z_1 <-> y
  t_1 -> z_1
  t_2 -> z_2
  t_3 -> y
"
dosearch(data5, query5, graph5, transportability = "t_1, t_2, t_3")

## Missing data
## Proxy variables are denoted by an asterisk (*)
data6 <- "
  p(x*,y*,z*,m_x,m_y,m_z)
"
query6 <- "p(x,y,z)"
graph6 <- "
  z -> x
  x -> y
  x -> m_z
  y -> m_z
  y -> m_x
  z <-> y
"
dosearch(data6, query6, graph6, missing_data = "m_x : x, m_y : y, m_z : z")

## An LDAG
data7 <- "P(X,Y,Z)"
query7 <- "P(Y|X,I_X=1)"
graph7 <- "
  X -> Y : Z = 1
  Z -> Y 
  Z -> X : I_X = 1
  I_X -> X
  H -> X : I_X = 1
  H -> Z
  Q -> Z
  Q -> Y : Z = 0
"
dosearch(data7, query7, graph7)

## A more complicated LDAG
## with multiple assignments for the edge X -> Z

data8 <- "P(X,Y,Z,A,W)"
query8 <- "P(Y|X,I_X=1)"
graph8 <- "
  I_X -> X
  I_Z -> Z
  A -> W
  Z -> Y
  A -> Z
  X -> Z : I_Z = 1; A = 1
  X -> Y : A = 0
  W -> X : I_X = 1
  W -> Y : A = 0
  A -> Y
  U -> X : I_X = 1
  U -> Y : A = 1
"
dosearch(data8, query8, graph8)

## Export the DOT diagram of the derivation as an SVG file
## to the working directory via the DOT package.
## By default, only the identifying part is plotted.
## PostScript format is also supported.
if (requireNamespace("DOT", quietly = TRUE)) {
  d <- get_derivation(data1, query1, graph1, 
                      control = list(draw_derivation = TRUE))
  DOT::dot(d$derivation, "derivation.svg")
}

} % END DONTRUN

} % END EXAMPLES
