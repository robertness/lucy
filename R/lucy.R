examineAttr <- function(g, property, formatAttr=NULL){
  #property = c("vertex", "edge", "graph")
  #formatAttr = optional formating function to be applied to return attribute value
  attributeLister <- list(vertex = list.vertex.attributes, 
                          edge = list.edge.attributes, 
                          graph=list.graph.attributes)[[property]]
  attributeGetter <- list(vertex = get.vertex.attribute, 
                          edge = get.edge.attribute, 
                          graph=get.graph.attribute)[[property]]
  obj.attributes <- attributeLister(g)
  obj.attribute.list <- lapply(obj.attributes, function(obj.attribute){
    output <- attributeGetter(g, obj.attribute)
    if(!is.null(formatAttr)) output <- formatAttr(output)
    output
  })
  names(obj.attribute.list) <- obj.attributes
  obj.attribute.list
}

examineGraph <- function(g, formatGraphAttr = NULL, formatVertexAttr= NULL, formatEdgeAttr=NULL){
  #A handy function for viewing the attributes in a graph 
  g.attribute.list <- examineAttr(g, "graph", formatGraphAttr)
  v.attribute.list <- examineAttr(g, "vertex", formatVertexAttr)
  e.attribute.list <- examineAttr(g, "edge", formatEdgeAttr)
  list(graph = g.attribute.list, vertices = v.attribute.list, edges = e.attribute.list)
}

#' Find all Vertices Upstream or Downstream of a Given Vertex in Directed Graph  
#' 
#' @param g igraph object
#' @param w character, the name of a vertex in g
#' @return character vector of vertex names.
#' @export
get_downstream_nodes <- function(g, w){
  w <- checkVertex(w)
  if(!is.directed(g)) stop("Graph must be directed.")
  g <- name_vertices(g)
  sp.mat <- shortest.paths(g, v = V(g), to = w, mode = "in") 
  if(is.null(dimnames(sp.mat))){
    dimnames(sp.mat) <- list(paste(1:vcount(g)), paste(w))
  }
  sp.mat[, V(g)[w]$name] %>% #Get the shortest path vector for all that have 
    Filter(f=is.finite) %>% #Keep online the finite length paths 
    names %>% # collect their nanmes
    {V(g)[.]} %>% # convert to vertex objects
    as.numeric %>% # convert to numeric
    setdiff(w) # exclude the source node itself
}
#' @rdname get_downstream_nodes
#' @export
get_upstream_nodes <- function(g, w){
  w <- checkVertex(w)
  if(!is.directed(g)) stop("Graph must be directed.")
  g <- name_vertices(g)
  sp.mat <- shortest.paths(g, v = V(g), to = w, mode = "out") 
  if(is.null(dimnames(sp.mat))){
    dimnames(sp.mat) <- list(paste(1:vcount(g)), paste(w))
  }
  sp.mat[, V(g)[w]$name] %>%
  #Get the shortest path vector for all that have 
    #paths coming into w
    Filter(f=is.finite) %>% #Keep online the finite length paths 
    names %>% # collect their nanmes
    {V(g)[.]} %>% # convert to vertex objects
    as.numeric %>% # convert to numeric
    setdiff(w) # exclude the source node itself
}

#' Test if a Vertex is Downstream or Upstream of Another
#' 
#' @param g igraph object
#' @param a numeric index or igraph.vs object, source vertex
#' @param end vertex
#' @return TRUE/FALSE value
#' @export
isBDownstreamOfA <- function(g, a, b){
  a <- checkVertex(a)
  b <- checkVertex(b)
  g <- name_vertices(g)
  sp <- suppressWarnings(shortest.paths(g, v = a, to = b, 
                       mode = "out",
                       algorithm = "unweighted"))[V(g)[a]$name, V(g)[b]$name]
  is.finite(sp)
}
#' @rdname get_connecting_nodes
#' @export
isBUpstreamOfA <- function(g, a, b){
  sp <- suppressWarnings(shortest.paths(g, v = a, to = b, 
                                        mode = "in",
                                        algorithm = "unweighted"))[V(g)[a]$name, V(g)[b]$name]
  is.finite(sp)
}

#' Find All Vertices or Edges on Paths Connecting Two Vertices    
#' 
#' Given two vertices, find all vertices lying on all paths between those two 
#' vertices.
#' 
#' @param g igraph object
#' @param src vertex index of class numeric or igraph.vs, starting vertex
#' @param trg vertex index of class numeric or igraph.vs, ending vertex
#' @return numeric vertex indices, including the src and trg indices
#' @export
get_connecting_nodes <- function(g, src, trg){
  v1 <- checkVertex(src)
  v2 <- checkVertex(trg)
  v1.downstream.nodes.except.v2 <- setdiff(get_downstream_nodes(g, v1), v2)
  downstream.nodes.upstream.to.v2 <- NULL
  if(length(v1.downstream.nodes.except.v2) > 0){
    bool <- sapply(v1.downstream.nodes.except.v2, function(node){
      edge.connectivity(g, node, v2) > 0
    })
    downstream.nodes.upstream.to.v2 <- v1.downstream.nodes.except.v2[bool]
  }
  c(v1, downstream.nodes.upstream.to.v2, v2)
}
#' @rdname get_connecting_nodes
get_connecting_edges <- function(g, src, trg){
  src <- checkVertex(src)
  trg <- checkVertex(trg)
  output.edges <- NULL
  if(V(g)[trg] != V(g)[src]){
    if(!(trg %in% get_downstream_nodes(g, src))) {
      stop("Target is not downstream of the source.")
    }
    v.set <- get_connecting_nodes(g, src, trg)
    output.edges <- E(g)[v.set %->% v.set] 
  }
  as.numeric(output.edges)
}

#' Random Walk with Restarts
rw_restarts <- function(g, starts, r = 0, use.weights = F){
  if(!use.weights) E(g)$weight <- 1
  starts.name <- V(g)[starts]$name
  restart.vec <- t(numeric(vcount(g)))
  colnames(restart.vec) <- V(g)$name
  restart.vec[, starts.name] <- 1
  restart.vec <- restart.vec / sum(restart.vec) #normalize (trivial in case of one restart node)
  adj.mat <- get.adjacency(g, attr = "weight", sparse = F) #edge attr
  out.degree <- apply(adj.mat, 1, function(row) sum(row > 0))
  empty.rows <- which(out.degree == 0)
  if(length(empty.rows) > 0){
    adj.mat[empty.rows, ] <- restart.vec
  }
  P <- t(apply(adj.mat, 1, function(row) row / sum(row)))
  ##Random Walk
  change <- 1
  tol <- 10 ^ (-6)
  rw.vec <- restart.vec
  i <- 0
  while(change > tol){
    rw.vec.old <- rw.vec
    rw.vec <- (1 - r) * rw.vec.old %*% P + r * restart.vec
    change <- sum(abs(rw.vec.old - rw.vec))
  }
  rw.vec <- as.numeric(rw.vec)
  names(rw.vec) <- colnames(restart.vec)
  rw.vec
}

reverseWalk <- function(g, use.weights = F){
  #reverse the edges
  el <- data.frame(get.edgelist(g)[, c(2, 1)])
  el$weight <- E(g)$weight
  new.g <- graph.data.frame(el) 
  V(new.g)[V(g)$name]$type <- V(g)$type 
  randomWalk(new.g, V(new.g)[type == "output"], use.weights = use.weights)
}

