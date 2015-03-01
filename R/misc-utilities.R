iparents <- function(g, v){
  checkVertex(v)
  V(g)[nei(v, mode="in")]$name
}

ichildren <- function(g, v){
  checkVertex(v)
  V(g)[nei(v, mode="out")]$name
}

#' In degree of a set of vertices in a directed graph
#' 
#' @param g an igraph directed graph
#' @param v.set a set of vertex names
#' @return numeric vector of degree counts
#' @examples
#' g <- ba.game(10)
#' inDegree(g, V(g))
inDegree <- function(g, v.set){
  if(!igraph::is.directed(g)) stop("Graph not directed.")
  igraph::degree(g, v = v.set,  mode = "in")
}
#' Out degree of a set of vertices in a directed graph
#' 
#' @param g an igraph directed graph
#' @param v.set a set of vertex names
#' @return numeric vector of degree counts
#' @examples
#' g <- ba.game(10)
#' inDegree(g, V(g))
outDegree <- function(g, v.set){
  if(!igraph::is.directed(g)) stop("Graph not directed.")
  igraph::degree(g, v = v.set,  mode = "out")
}

#' Find all the root vertices in a directed graph.
#' A root node has no parents
#' 
#' @param g an igraph directed graph
#' @return chacter vector of vertex names
#' @examples
#' g <- ba.game(10)
#' getRoots(g)
getRoots <- function(g){
  if(!igraph::is.directed(g)) stop("Must be a directed graph.")
  if(is.null(igraph::V(g)$name)) igraph::V(g)$name <- paste(igraph::V(g))
  igraph::V(g)[inDegree(g, igraph::V(g)) == 0]$name
}

#' Find all the leaf vertices in a directed graph.
#' A root node has no children
#' 
#' @param g an igraph directed graph
#' @return chacter vector of vertex names
#' @examples
#' g <- ba.game(10)
#' getLeaves(g)
getLeaves <- function(g){
  if(!igraph::is.directed(g)) stop("Must be a directed graph.")
  if(is.null(igraph::V(g)$name)) igraph::V(g)$name <- paste(igraph::V(g))
  igraph::V(g)[outDegree(g, igraph::V(g)) == 0]$name
}

igraph2bn <- function(g){
  bnlearn::as.bn(igraph::igraph.to.graphNEL(g))
}

bn2igraph <- function(net){
  igraph::igraph.from.graphNEL(bnlearn::as.graphNEL(net))
} 

