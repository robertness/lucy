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
  if(!is.character(v)){
    stop("Use vertex names instead of vertex objects as arguments.")
  }
}


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

getDownstreamNodes <- function(g, v){
  v.bucket <- NULL
  v.children <- ichildren(g, v)
  if(length(v.children) > 0){
    v.bucket <- c(v.bucket, v.children)
    children.w.children <- v.children[sapply(v.children, function(child) length(ichildren(g, child)) > 0)]
    if(length(children.w.children) > 0){
      v.bucket <- c(v.bucket, unlist(sapply(children.w.children, getDownstreamNodes, g = g)))
    }
  }
  unique(v.bucket)
}
isBDownstreamOfA <- function(g, a, b){
  sp <- suppressWarnings(shortest.paths(g, v = a, to = b, 
                       mode = "out",
                       algorithm = "unweighted"))[a, b]
  is.finite(sp)
}

getConnectingNodes <- function(g, v1, v2){
  v1.downstream.nodes.except.v2 <- setdiff(getDownstreamNodes(g, v1), v2)
  downstream.nodes.upstream.to.v2 <- NULL
  if(length(v1.downstream.nodes.except.v2) > 0){
    downstream.nodes.upstream.to.v2 <- v1.downstream.nodes.except.v2[sapply(v1.downstream.nodes.except.v2, function(node) 
      edge.connectivity(g, node, v2) > 0)]
  }
  c(v1, downstream.nodes.upstream.to.v2, v2)
}
getConnectingEdges <- function(g, src, trg){
  output.edges <- NULL
  if(V(g)[trg] != V(g)[src]){
    if(!(trg %in% getDownstreamNodes(g, src))) {
      stop("Target is not downstream of the source.")
    }
    v.set <- getConnectingNodes(g, src, trg)
    output.edges <- E(g)[v.set %->% v.set]
  }
  output.edges
}

getDependentEdges <- function(g, e){
  #Determine the edges in g whose weights impact the 
  #optimization of the weight of edge e
  v.trg <- get.edgelist(g)[e, 2]
  output.v <- V(g)[type == "output"]
  dependent.edges <- NULL
  if(!(v.trg == output.v)){
    if(output.v %in% ichildren(g, v.trg)) {
      dependent.edges <- E(g)[v.trg %->% output.v]
    }else{
      dependent.edges <- getConnectingEdges(g, v.trg, output.v)
    }
  }
  dependent.edges
}

randomWalk <- function(g, starts, r = 0, use.weights = F){
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

