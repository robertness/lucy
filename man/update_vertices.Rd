% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/propagation.R
\name{update_vertices}
\alias{update_edges}
\alias{update_vertices}
\title{Vertex/Edge Propagation in igraph
  
Foundation for any graph propagation algorithm, where the states of vertexes or edges propagate through the graph,
changing the states of others.  Traverse the graph, land on a vertex/edge, perform a callback function on the 
vertex/edge.  That callback can depend on the states of other 'determiner' vertices/edges.  When the callback is 
completed successfully, the 'updated' attribute of the object is changed to \code{TRUE}. If those determiners do 
not have the status \code{updated == TRUE}, then the function recursively performs the callback on the determiners until all 
their states are updated, before finalling performing the callback on the original vertex/edge.}
\usage{
update_vertices(g, get_determiners, callback, verbose = FALSE)

update_edges(g, get_determiners, callback, verbose = FALSE)
}
\arguments{
\item{g}{A graph object.}

\item{get_determiners}{A function returns the vertice/edge indices that must be updated for the callback to be 
executed.  Arguments: g - the graph object, i - the index of a vertex/edge in g. Returns: an array of indices
for the vertices/edges that are needed to perform the callback on vertex/edge i.}

\item{callback}{The function performed on each vertex/edge. Arguments: g - the graph object, i - the index of a graph/edge in g.
Returns: a graph object.}

\item{verbose}{if TRUE prints out details of propagation}
}
\value{
A igraph object with updated vertex/edge states. A warning message is returned if not all vertices 
could be updated.
}
\description{
\code{update_vertices} performs propagation on vertices. \code{updateEdges} performs propagation on edges.
}
\section{Functions}{
\itemize{
\item \code{update_edges}: 
}}

