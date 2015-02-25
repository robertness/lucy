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

iparents <- function(g, v){
  V(g)[nei(v, mode="in")]
}

ichildren <- function(g, v){
  V(g)[nei(v, mode="out")]
}

updateNode <- function(g, v.index, getDeterminers, callback){  
  v <- V(g)[v.index]
  if(!v$updated){
    determiner.vertices <- getDeterminers(g, v)
    if(length(determiner.vertices) > 0){
      test.determiners.unupdated <- !determiner.vertices$updated
      if(any(test.determiners.unupdated)){
        unupdated.determiners <- determiner.vertices[test.determiners.unupdated]
        for(d in unupdated.determiners){
          g <- updateNode(g, d, getDeterminers, callback)
        }
      }
    }
    g <- callback(g, v)
    V(g)[v.index]$updated <- TRUE
  }
  g
}

updateVertices <- function(g, getDeterminers, callback){
  
  for(v.index in 1:vcount(g)){
    g <- updateNode(g, v.index, getDeterminers, callback) 
  }
  if(!all(V(g)$updated)){
    warning("The following nodes were not updated: ", 
            paste(V(g)[!updated]$name, collapse = ", "))
  }
  g <- resetUpdateAttributes(g)
  g
}
#In here I am sticking something to modify a caese when there is no determiners
#I'll clean this up when I combine updateEdge and updateVertex into updateProperty
#As a general rule, I will write a function for dealing directly with with an igraph base object
#then write an encapsulating function for working with a modified igraph objec
updateEdge <- function(g, e.index, getDeterminers, callback){
  e <- E(g)[e.index]
  message("Updating edge: ", e$name)
  if(!e$updated){
    determiner.edges <- getDeterminers(g, e)
    if(length(determiner.edges) > 0){
      test.determiners.unupdated <- !determiner.edges$updated    
      if(any(test.determiners.unupdated)){
        unupdated.determiners <- determiner.edges[test.determiners.unupdated]
        for(d in unupdated.determiners){
          message(e$name, " has unupdated downstream edges: ", paste(unupdated.determiners$name, collapse=", "))
          g <- updateEdge(g, d, getDeterminers, callback)
        }
      }
    }
    g <- callback(g, e)
    E(g)[e.index]$updated <- TRUE
  }
  g
}

updateEdges <- function(g, getDeterminers, callback){
  for(e.index in 1:ecount(g)){
    g <- updateEdge(g, e.index, getDeterminers, callback) 
  }
  if(!all(E(g)$updated)){
    warning("The following edges were not updated: ",
            paste(E(g)[!updated]$name, collapse = ", "))
  }
  g <- resetUpdateAttributes(g)
}

inDegree <- function(g, v.set){
  require(igraph)
  igraph::degree(g, v = v.set,  mode = "in")
}

nameEdges <- function(g, e.set){
  require(igraph)
  el <- get.edgelist(g)
  apply(el[e.set, , drop=F], 1 , paste, collapse="->")
}

generateMultiConnectedDAG <- function(n){  
  #An alternative to the Barabasi algorithm in igraph which 
  #generates a scale-free DAG.
  #This uses Ide's and Cozman's DAG algorithm in the bnlearn package, which generates DAGS with 
  #more connectivity. 
  #Limiting to one output node
  require(bnlearn, quietly = T, warn.conflicts = F)
  bn.net <- random.graph(paste(1:n), method = "ic-dag")
  simmed.leaves <- nodes(bn.net)[vapply(nodes(bn.net), 
                                        function(node) length(children(bn.net, node))==0,
                                        TRUE)]
  if(length(simmed.leaves) > 1){
    true.leaf <- sample(simmed.leaves, 1)
    arcs(bn.net) <- rbind(arcs(bn.net), cbind(setdiff(simmed.leaves, true.leaf), true.leaf))
  }
  gNEL.net <- as.graphNEL(bn.net)
  detach(package:bnlearn)
  require(igraph, quietly = T, warn.conflicts = F)
  igraph.net <- igraph.from.graphNEL(gNEL.net)
  igraph.net
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
  b %in% getDownstreamNodes(g, a)
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
igraphGraphvizPlot <- function(g, plot.args=NULL){
  #Plots an igraph DAG with Rraphviz via the bnlearn package
  #The default layout in Rgraphviz for DAGS is more interpretable than igraph 
  #layouts, especially when the DAGs are densely connected
  #bnlearn package is used as an intermediary because it has 
  #straightfowward DAG checking and Rgraphviz plotting
  require(igraph, quietly = T, warn.conflicts = F)  
  gNEL <- igraph.to.graphNEL(g)
  require(bnlearn, quietly = T, warn.conflicts = F)
  net <- as.bn(gNEL)
  args <- c(list(x = net), plot.args)
  do.call("graphviz.plot", args)
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

igraph.to.bn <- function(g){
  bnlearn::as.bn(igraph.to.graphNEL(g))
}