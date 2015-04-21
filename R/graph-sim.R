#' Generate a Multilayer Perceptron Graph 
#' 
#' Generate a directed graph structure like a multi-layer perceptron
#' 
generateMLP <- function(...){} 

#' Generate Multi-connected DAG
#' 
#' An alternative to the scale-free DAG generating Barabasi algorithm in igraph..
#' This uses Ide's and Cozman's DAG algorithm in the bnlearn package, which generates DAGS with 
#' more connectivity.  This function currently only outputs one leaf node.
#' @param n Number of nodes
#' @return An igraph object
#' @examples 
#' g <- generateMultiConnectedDag(10)
#' igraphVizPlot(g)
generateMultiConnectedDAG <- function(n){  
  bn.net <- bnlearn::random.graph(paste(1:n), method = "ic-dag")
  #Limit to one leaf -------------------
  simmed.leaves <- bnlearn::nodes(bn.net)[vapply(bnlearn::nodes(bn.net), 
                                        function(node) length(bnlearn::children(bn.net, node))==0,
                                        TRUE)]
  if(length(simmed.leaves) > 1){
    true.leaf <- sample(simmed.leaves, 1)
    bnlearn::arcs(bn.net) <- rbind(bnlearn::arcs(bn.net), 
                          cbind(setdiff(simmed.leaves, true.leaf), true.leaf))
  }
  bn.net %>%
    {bnlearn::as.graphNEL(.)} %>%
    igraph.from.graphNEL() %>%
    nameEdges() %>%
    nameVertices
}

#' Generate graph comprised of sequentially layered randomly generated DAGs.
#' 
#' This is useful for generating a random network that is layered in structure. 
#' While the nodes in each subgraph layer is densely connected, only a subset of nodes in each layer,
#' connect to the next or previous layer.  Namely, all parentless nodes and their children in 
#' layer n are connected to all childless nodes and their parents in layer n - 1. 
#' Each subgraphs are generated using Ide's and Cozman's Generating Multi-connected DAGs algorithm, 
#' as implemented in the bnlearn pacakge.  The resulting network is still a DAG.
#' @param k Number of layers.
#' @param n Number of nodes in each layer
#' @return A DAG as an igraph object.
#' @examples
#' g <- layerDAGs(3, 4)
#' igraphVizPlot(g)
layerDAGs <- function(k, n){
  subset.index <- rep(1:k, each = n)
  node.names <- paste(1:(k*n))
  node.name.list <- split(node.names, f=subset.index)
  nets <- lapply(node.name.list, function(node.names){
    if("package:bnlearn" %in% search()) stop("Run 'detach(package:bnlearn)' first.")
    g <- bnlearn::random.graph(node.names, method = "ic-dag") %>% #Convert bn to igraph by ...
      {bnlearn::as.graphNEL(.)} %>% # ... first converting to graphNEL
      igraph.from.graphNEL # ... then converting to an igraph
    V(g)$name <- node.names
    g
  })
  g <- graph.empty()
  for(i in 1:k){
    g <- g + nets[[i]]
  }
  for(i in 2:k){
    net.last <- nets[[i-1]]; net <- nets[[i]] #I will connect net.last to net 
    last.leaves <- getLeaves(net.last) 
    parents.of.leaves <- lapply(last.leaves, iparents, g = net.last) %>% unlist
    last.from <- c(last.leaves, parents.of.leaves)
    current.roots <- getRoots(net)
    children.of.roots <- lapply(current.roots, ichildren, g = net.last) %>% unlist
    current.to <- c(current.roots, children.of.roots)
    el <- expand.grid(V(net.last)[last.from]$name, V(net)[current.to]$name)
    names(el) <- c("from", "to")
    g <- g + igraph::edges(as.character(t(el)))
  }
  for(wgt in list.edge.attributes(g)){
    g <- remove.edge.attribute(g, wgt)
  }
  g 
}