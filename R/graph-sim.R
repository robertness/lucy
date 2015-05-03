#' Generate a Multilayer Perceptron Graph 
#' 
#' Generate a multi-layer perceptron graph structure.
#' @usage mlp_graph(inputs, layers, outputs)
#' @param inputs character, the names of the input nodes
#' @param layers a vector of integers specifying the number of hidden neurons (vertices) in each hidden layer
#' @param outputs character, the names of the output nodes
#' @return igraph object, a structure with an input layer, an output layer and 
#' hidden layers in between.  Each node is named.  The graph has a vertex 
#' attribute called "layer" that describes whether it is an input, output, or
#' hidden layer node.
#' @examples
#' g <- mlp_graph(c("I1", "I2"), c(3, 2, 4), c("O1", "O2", "O3"))
#' igraphviz(g)
#' @export
mlp_graph <- function(inputs, outputs, layers = NULL){
  if(!is.character(inputs) || !is.character(outputs)) stop("Use characters to label inputs and outputs.")
  conflicts <- intersect(inputs, outputs)
  if(length(conflicts) > 0) stop("There are inputs and outputs with the same name.")
  # If there are no hidden layers ...
  if(is.null(layers)){    
    vertex_table <- data.frame(v = c(inputs, outputs),
                               layer = c(rep("input", length(inputs)),
                                         rep("output", length(outputs)))
    )
    g <- list(inputs, outputs) %>%
      expand.grid %>%
      graph.data.frame(directed = T, vertices = vertex_table)
    return(g) #and stop here
  }
  # Else, start by making a list of all the layers
  layer_list <- seq_along(layers) %>% # 
    {paste("H", ., sep="")} %>%
    {Map(rep, ., layers)} %>%
    lapply(function(item) paste(item, 1:length(item), sep = ""))
  # Check name conflicts
  conflicts <- intersect(c(inputs, outputs), unlist(layer_list))
  if(length(conflicts) > 0) stop("Names of the inputs or outputs conflict with name of hidden nodes.")
  # Create and label a list of data 
  vertex_table <- layer_list %>%
    names %>%
    lapply(function(layer) cbind(v = layer_list[[layer]], layer)) %>%
    {do.call("rbind", .)} %>%
    {rbind(
      cbind(v = inputs, layer = "input"),
      .,
      cbind(v = outputs, layer = "output")
    )} %>% data.frame
  g <- layer_list %>%
    {c(list(inputs = inputs), ., list(outputs = outputs))} %>%
    {lapply(2:length(.), function(i){
      .[c(i - 1, i)]
    })} %>%
    lapply(expand.grid) %>%
    {lapply(., function(item) {names(item) <- c("from", "to"); item})} %>%
    {do.call("rbind", .)} %>%
    graph.data.frame(directed = T, vertices = vertex_table)
  g
} 

#' Generate Multi-connected DAG
#' 
#' An alternative to the scale-free DAG generating Barabasi algorithm in igraph..
#' This uses Ide's and Cozman's DAG algorithm in the bnlearn package, which generates DAGS with 
#' more connectivity.  This function currently only outputs one leaf node.
#' @param n Number of nodes
#' @return An igraph object
#' @examples 
#' g <- generateMultiConnectedDAG(10)
#' igraphviz(g)
generateMultiConnectedDAG <- function(n){
  # bnlearn makes a call 'setMethod("nodes", cl, function(object) .nodes(object))'
  # when loaded.  If the graph package is not attached to the search path, this call fails, seemingly
  # because a function with the name 'nodes' has to already exist in the path.
  # So I use require() and detach() to attach and detach the graph package.
  # I'd prefer not to use these functions since they may have side effects.  
  # But the only alternative I can think of is to include it in Depends, but this and other graph packages
  # have conflicts with igraph.
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
  g <- bn.net %>%
    {bnlearn::as.graphNEL(.)} %>%
    igraph.from.graphNEL() %>%
    nameEdges() %>%
    nameVertices
  g
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
#' igraphviz(g)
layerDAGs <- function(k, n){
  # bnlearn makes a call 'setMethod("nodes", cl, function(object) .nodes(object))'
  # when loaded.  If the graph package is not attached to the search path, this call fails, seemingly
  # because a function with the name 'nodes', from the 'graph' package, has to already exist in the path.
  # So I use require() and detach() to attach and detach the graph package.
  # I'd prefer not to use these functions since they may have side effects.  
  # But the only alternative I can think of is to include it in Depends, but this and other graph packages
  # have conflicts with igraph.
  subset.index <- rep(1:k, each = n)
  node.names <- paste(1:(k*n))
  node.name.list <- split(node.names, f=subset.index)
  nets <- lapply(node.name.list, function(node.names){
    g <- bnlearn::random.graph(node.names, method = "ic-dag") %>% #Convert bn to igraph by ...
      {bnlearn::as.graphNEL(.)} %>%# ... first converting to graphNEL
      igraph.from.graphNEL
    # ... then converting to an igraph
    V(g)$name <- node.names
    g
  })
  g <- graph.empty()
  for(i in 1:k){
    g <- g + nets[[i]]
  }
  for(i in 2:k){
    net.last <- nets[[i-1]]; net <- nets[[i]] #I will connect net.last to net 
    last.leaves <- get_leaves(net.last) 
    parents.of.leaves <- lapply(last.leaves, iparents, g = net.last) %>% unlist
    last.from <- c(last.leaves, parents.of.leaves)
    current.roots <- get_roots(net)
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