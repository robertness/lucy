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

#' Parents/Children/Markov Blanket of a Vertex
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
#' @rdname iparents
imb <- function(g, v){
  v <- checkVertex(v)
  if(!is.directed(g)) stop("g must be a directed graph")
  parents <- iparents(g, v)
  children <- ichildren(g, v)
  parents_of_children <- lapply(children, function(child) iparents(g, child)) %>% unlist %>% setdiff(v)
  unique(c(parents, children, parents_of_children))
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
#' get_roots(g)
get_roots <- function(g){
  checkDirected(g)
  g <- nameVertices(g)
  V(g)[inDegree(g, V(g)) == 0] %>%
    as.numeric
}
#' @rdname get_roots
get_leaves <- function(g){
  checkDirected(g)
  g <- nameVertices(g)
  V(g)[outDegree(g, V(g)) == 0] %>%
    as.numeric
}

#' Quick Naming of the Vertices/Edges in a Graph
#' 
#' If the vertex/edges of an igraph object have no name, quickly add a name 
#' based on the vertex index/vertices in the edge. For vertices the name is the 
#' vertex id, the edges it is 'from->to'. 
#' 
#' @param g an igraph directed graph
#' @export
#' @return the graph with the vertex attribute 'name'
nameVertices <- function(g){
  if(is.null(V(g)$name)){
    V(g)$name <- paste(V(g))
  }
  g
}
#' @rdname nameVertices
nameEdges <- function(g){
  if(is.null(E(g)$name)){
    el <- get.edgelist(g)
    E(g)$name <- apply(el, 1 , paste, collapse="->")
  }  
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


