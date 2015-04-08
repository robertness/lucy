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


#' Extends Magrittr's "$" Pipe Operator igraph.
#' 
#' For more details Magrittr see \code{?magrittr}
#' 
#' @examples
#' library(igraphr)
#' g <- erdos.renyi.game(100, 2/100)
#' V(g)$color <- "green"
#' g %>% V %$% color
#' @export 
with.igraph.vs <- function(data, expr, ...) {
  eval(substitute(data$c, list(c = substitute(expr))))
}

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
  if(!is.directed(g)) stop("Graph not directed.")
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
  if(!is.directed(g)) stop("Graph not directed.")
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
  checkDirected(g)
  g <- nameVertices(g)
  V(g)[inDegree(g, V(g)) == 0]$name
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
  checkDirected(g)
  g <- nameVertices(g)
  V(g)[outDegree(g, V(g)) == 0]$name
}

igraph2bn <- function(g){
  bnlearn::as.bn(igraph.to.graphNEL(g))
}

bn2igraph <- function(net){
  igraph.from.graphNEL(bnlearn::as.graphNEL(net))
} 

nameVertices <- function(g, v.set = V(g)){
  if(is.null(V(g)$name)){
    V(g)[v.set]$name <- paste(V(g)[v.set])
  }
  g
}

nameEdges <- function(g, e.set = E(g)){
  if(is.null(E(g)$name)){
    el <- get.edgelist(g)
    E(g)[e.set]$name <- apply(el[e.set, , drop=F], 1 , paste, collapse="->") 
  }
  g
}

# Enable indexing edges by name
#' @export
`[.igraph.es` <- function(x, i){
  ret <- try(igraph::`[.igraph.es`(x, i), silent=TRUE)
  if(class(ret) == "try-error"){
    ret <- x[x$name == i]
  }
  ret
}

checkDirected <- function(g){
  if(!is.directed(g)) stop("Must be a directed graph.")
}
  



