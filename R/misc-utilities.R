#' Type checking for igraph vertex arguments.
#' 
#' igraph fnctions that use igraph vertices as arguments can typicall take
#' the vertex object itself (igraph.vs), the vertex id (numeric),
#' or the vertex name (character).  This flexibility can lead to unsafe code.
#' The convention in this package is to use the vertex name.  
#' 
#' @param v Assumed to represent a vertex.
#' 
#' This function, returns an error if v is not of the class 'character'.
checkVertex <- function(v){
  if(is.character(v)){
    stop("Use of names instead of indices or igraph sequences as arguments may lead to problems.")
  }
  if(length(v) > 1){
    stop("Limited to one vertex.")
  }
  as.numeric(v)
}

#' Create an igraph Iterator Substitution Function
#' 
#' Takes either igraph iterator functions \code{E} and \code{V}, and creates a mirror function \code{S}
#' that can be used in code intended to work on either edges or vertices. Also creates the replacement 
#' function.  Creates the object in the parent environment.
#' @param iterator either "edge", or "vertex"
setIterator <- function(iterator){
  if(length(iterator) != 1 && !(iterator %in% c("edge", "vertex"))) {
    stop("iterator must be either 'edge' or 'vertex'")
  }
  if(iterator == "edge"){
    S <<- E
    `S<-` <<- `E<-`
  }else{
    S <<- V
    `S<-` <<- `V<-`
  }
}


# #' Extends Magrittr's "$" Pipe Operator igraph.
# #' 
# #' For more details Magrittr see \code{?magrittr}
# #' 
# #' @examples
# #' library(igraphr)
# #' g <- erdos.renyi.game(100, 2/100)
# #' V(g)$color <- "green"
# #' g %>% V %$% color
# #' @export 
# with.igraph.vs <- function(data, expr, ...) {
#   eval(substitute(data$c, list(c = substitute(expr))))
# }

#' Parents/Children of a Vertex
#' @param g igraph graph object, directed
#' @param v numeric vertex index or igraph.vs object
#' @export
iparents <- function(g, v){
  v <- checkVertex(v)
  if(!is.directed(g)) stop("g must be a directed graph")
  V(g)[nei(v, mode="in")] %>% as.numeric
}
#' @rdname iparents
ichildren <- function(g, v){
  v <- checkVertex(v)
  if(!is.directed(g)) stop("g must be a directed graph")
  V(g)[nei(v, mode="out")] %>% as.numeric
}

#' In/Out-degree of a set of vertices in a directed graph
#' 
#' @param g an igraph directed graph
#' @param v.set a set of vertex names
#' @return numeric vector of degree counts
#' @examples
#' g <- ba.game(10)
#' inDegree(g, V(g))
inDegree <- function(g, v.set){
  v.set %>% vapply(checkVertex, numeric(1)) 
  if(!is.directed(g)) stop("Graph not directed.")
  igraph::degree(g, v = v.set,  mode = "in")
}
#' @rdname inDegree
outDegree <- function(g, v.set){
  v.set %>% vapply(checkVertex, numeric(1))
  if(!is.directed(g)) stop("Graph not directed.")
  igraph::degree(g, v = v.set,  mode = "out")
}

#' Find all the leaves and root vertices in a directed graph.
#' A root node has no parents
#' 
#' @param g an igraph directed graph
#' @return chacter vector of vertex names
#' @examples
#' g <- ba.game(10)
#' getRoots(g)
getRoots <- function(g){
  checkDirected(g)
  g <- nameVertices(g)
  V(g)[inDegree(g, V(g)) == 0] %>%
    as.numeric
}
#' @rdname getRoots
getLeaves <- function(g){
  checkDirected(g)
  g <- nameVertices(g)
  V(g)[outDegree(g, V(g)) == 0] %>%
    as.numeric
}

#' Quick Naming of the Vertices/Edges in a Graph
#' 
#' Quickly add a name attribute to the vertices or edges in the graph.  
#' For vertices the name is the vertex id, the edges it is 'from->to'. 
#' 
#' @param g an igraph directed graph
#' @param v.set indices for the vertices that are to be named
#' @param e.set indices for the edges that that are to be named
#' @return chacter vector of vertex names
nameVertices <- function(g, v.set = V(g)){
  V(g)[v.set]$name <- paste(V(g)[v.set])
  g
}
#' @rdname nameVertices
nameEdges <- function(g, e.set = E(g)){
    el <- get.edgelist(g)
    E(g)[e.set]$name <- apply(el[e.set, , drop=F], 1 , paste, collapse="->") 
  g
}

# #' Enable indexing edges by name
# #' @export
# `[.igraph.es` <- function(x, i){
#   ret <- try(igraph::`[.igraph.es`(x, i), silent=TRUE)
#   if(class(ret) == "try-error"){
#     ret <- x[x$name == i]
#   }
#   ret
# }

checkDirected <- function(g){
  if(!is.directed(g)) stop("Must be a directed graph.")
}
  
#' Pull the Vertex from a Given Edge in Graph
#' Returns the index of a vertex in an edge
#' @param g igraph object
#' @param e integer/numeric/igraph.es edge index
#' @param node 'from' for from node, 'to' for to-node 
#' @return numeric of the vertex index
#' @export
getEdgeVertex <- function(g, e, node = c("from", "to")){
  el <- get.edgelist(g)
  colnames(el) <- c("from", "to")
  V(g)[el[e, node]] %>% as.numeric
}


