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
#' @export
ichildren <- function(g, v){
  v <- checkVertex(v)
  if(!is.directed(g)) stop("g must be a directed graph")
  V(g)[nei(v, mode="out")] %>% as.numeric
}
#' @rdname iparents
#' @export
imb <- function(g, v){
  v <- checkVertex(v)
  if(!is.directed(g)) stop("g must be a directed graph")
  parents <- iparents(g, v)
  children <- ichildren(g, v)
  parents_of_children <- lapply(children, function(child) iparents(g, child)) %>% unlist %>% setdiff(v)
  unique(c(parents, children, parents_of_children))
}

#' Select vertices from a sequence
#' 
#' Given a vertex sequence (class igraph.vs), selects the first, last, or a random vertex from
#' the sequence
#' @param g igraph object
#' @param s an object of class igraph.vs
#' @return the index of a vertex from the sequence
#' @export
first_v <- function(g, s){
  if(!(class(s) == "igraph.vs")) stop("Must supply object of class igraph.vs")
  g <- name_vertices(g)
  s <- V(g)[s]
  return(as.numeric(V(g)[V(g)[s]$name[1]]))
}
#' @rdname first_v
#' @export
last_v <- function(g, s){
  if(!(class(s) == "igraph.vs")) stop("Must supply object of class igraph.vs")
  g <- name_vertices(g)
  s <- V(g)[s]
  return(as.numeric(V(g)[V(g)[s]$name[length(s)]]))
}
#' @rdname first_v
#' @export
random_v <- function(g, s){
  if(!(class(s) == "igraph.vs")) stop("Must supply object of class igraph.vs")
  g <- name_vertices(g)
  s <- V(g)[s]
  return(V(g)[V(g)[s]$name[sample(length(s), 1)]])
}

#' In/Out-degree of a set of vertices in a directed graph
#' 
#' @param g an igraph directed graph
#' @param v.set a set of vertex names
#' @return numeric vector of degree counts
#' @examples
#' g <- ba.game(10)
#' inDegree(g, V(g))
in_degree <- function(g, v.set){
  v.set %>% vapply(checkVertex, numeric(1)) 
  if(!is.directed(g)) stop("Graph not directed.")
  igraph::degree(g, v = v.set,  mode = "in")
}
#' @rdname in_degree
out_degree <- function(g, v.set){
  v.set %>% vapply(checkVertex, numeric(1))
  if(!is.directed(g)) stop("Graph not directed.")
  igraph::degree(g, v = v.set,  mode = "out")
}

#' Find all the leaves and root vertices in a directed graph.
#' A root node has no parents
#' 
#' @param g an igraph directed graph
#' @return chacter vector of vertex names
#' @export
#' @examples
#' g <- ba.game(10)
#' get_roots(g)
get_roots <- function(g){
  check_directed(g)
  g <- name_vertices(g)
  V(g)[in_degree(g, V(g)) == 0] %>%
    as.numeric
}
#' @rdname get_roots
#' @export
get_leaves <- function(g){
  check_directed(g)
  g <- name_vertices(g)
  V(g)[out_degree(g, V(g)) == 0] %>%
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
name_vertices <- function(g){
  if(is.null(V(g)$name)){
    V(g)$name <- paste(V(g))
  }
  g
}
#' @rdname name_vertices
#' @export
name_edges <- function(g){
  if(is.null(E(g)$name)){
    el <- get.edgelist(g)
    E(g)$name <- apply(el, 1 , paste, collapse="->")
  }  
  g
}

check_directed <- function(g){
  if(!is.directed(g)) stop("Must be a directed graph.")
}
  
#' Pull the Vertex from a Given Edge in Graph
#' Returns the index of a vertex in an edge
#' @param g igraph object
#' @param e integer/numeric/igraph.es edge index
#' @param node 'from' for from node, 'to' for to-node 
#' @return numeric of the vertex index
#' @export
get_edge_vertex <- function(g, e, node = c("from", "to")){
  el <- get.edgelist(g)
  colnames(el) <- c("from", "to")
  V(g)[el[e, node]] %>% as.numeric
}

#' Reverse an edge in a directed graph
#' 
#' Return a new directed graph instance a given edge oriented in the opposite direction 
#' relative to the input graph.  Will not preserve graph, vertex,
#' and edge attributes.  
#' @param g a directed igraph object
#' @param e an edge (integer index or "igraph.es" object)
#' @return a new igraph object, the old graph with edges reverse.  
#' @export
reverse_edge <- function(g, e){
  check_directed(g)
  g %>%
    get.edgelist %>%
    {.[e, ] <- c(.[e, 2], .[e, 1]); .} %>%
    graph.edgelist
}

#' Reverse the edges of a directed graph
#' 
#' Return a new directed graph instance with each edge oriented in the opposite direction 
#' relative to the corresponding edge in the input graph.  Will not preserve graph, vertex,
#' and edge attributes.  
#' @param g a directed igraph object
#' @return a new igraph object, the old graph with edges reverse.  
#' @export
reverse_edges <- function(g){
  check_directed(g)
  g %>%
    get.edgelist %>%
    {cbind(.[, 2], .[, 1])} %>%
    graph.edgelist
}

#' Find a cycle
#' Returns the first cycle detected in the graph.  Relies on shortest path calculations.
#' If there is a "weight" edge attribute, it will be incorporated in the shortest edge
#' calculation.
#' @export
detect_cycle <- function(g){
  if(!is_directed(g)) stop("g is not directed.")
  for(v in V(g)){ 
    children <- V(g)[ichildren(g, v)]
    if(length(children) > 0){
      for(w in children){
        path <- suppressWarnings(shortest_paths(g, from = w, to = v, output = "epath"))$epath[[1]]
        if(length(path) != 0){
          path <- c(path, E(g)[get.edge.ids(g, c(v, w))])
          return(sort(path))
        } 
      }
    }
  }
  NULL
}
