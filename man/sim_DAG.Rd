% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/graph-sim.R
\name{sim_DAG}
\alias{sim_DAG}
\title{Generate Multi-connected DAG}
\usage{
sim_DAG(n, method = "ic-dag")
}
\arguments{
\item{n}{Number of nodes}

\item{method}{the algorithm used in simulation. The default is Ide's and Cozman's DAG algorithm 
in the bnlearn package, which generates DAGs with more connectivity. 
See the random.graph function in bnlearn for details.}
}
\value{
An igraph object
}
\description{
An alternative to the scale-free DAG generating Barabasi algorithm in igraph..
This function currently only outputs one leaf node.
}
\examples{
g <- sim_DAG(10)
igraphviz(g)
}

