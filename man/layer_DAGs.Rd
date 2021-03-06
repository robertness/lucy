% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/graph-sim.R
\name{layer_DAGs}
\alias{layer_DAGs}
\title{Generate graph comprised of sequentially layered randomly generated DAGs.}
\usage{
layer_DAGs(k, n, method = "ic-dag")
}
\arguments{
\item{k}{Number of layers.}

\item{n}{Number of nodes in each layer}

\item{method}{the algorithm used in simulation. The default is Ide's and Cozman's DAG algorithm 
in the bnlearn package, which generates DAGs with more connectivity. 
See the random.graph function in bnlearn for details.}
}
\value{
A DAG as an igraph object.
}
\description{
This is useful for generating a random network that is layered in structure. 
While the nodes in each subgraph layer is densely connected, only a subset of nodes in each layer,
connect to the next or previous layer.  Namely, all parentless nodes and their children in 
layer n are connected to all childless nodes and their parents in layer n - 1. 
Each subgraphs are generated using Ide's and Cozman's Generating Multi-connected DAGs algorithm, 
as implemented in the bnlearn pacakge.  The resulting network is still a DAG.
}
\examples{
g <- layer_DAGs(3, 4)
igraphviz(g)
}

