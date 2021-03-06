% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/graph-sim.R
\name{mlp_graph}
\alias{mlp_graph}
\title{Generate a Multilayer Perceptron Graph}
\usage{
mlp_graph(inputs, layers, outputs)
}
\arguments{
\item{inputs}{character, the names of the input nodes}

\item{outputs}{character, the names of the output nodes}

\item{layers}{a vector of integers specifying the number of hidden neurons (vertices) in each hidden layer}
}
\value{
igraph object, a structure with an input layer, an output layer and 
hidden layers in between.  Each node is named.  The graph has a vertex 
attribute called "layer" that describes whether it is an input, output, or
hidden layer node.
}
\description{
Generate a multi-layer perceptron graph structure.
}
\examples{
g <- mlp_graph(c("I1", "I2"), c("O1", "O2", "O3"), c(3, 2, 4))
igraphviz(g)
}

