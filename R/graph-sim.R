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
#' g <- mlp_graph(c("I1", "I2"), c("O1", "O2", "O3"), c(3, 2, 4))
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
#'This function currently only outputs one leaf node.
#' @param n Number of nodes
#' @param method the algorithm used in simulation. The default is Ide's and Cozman's DAG algorithm 
#' in the bnlearn package, which generates DAGs with more connectivity. 
#' See the random.graph function in bnlearn for details.
#' @return An igraph object
#' @export
#' @examples 
#' g <- sim_DAG(10)
#' igraphviz(g)
sim_DAG <- function(n, method = "ic-dag"){
  bn.net <- bnlearn::random.graph(paste(1:n), method = method)
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
    name_edges() %>%
    name_vertices
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
#' @param method the algorithm used in simulation. The default is Ide's and Cozman's DAG algorithm 
#' in the bnlearn package, which generates DAGs with more connectivity. 
#' See the random.graph function in bnlearn for details.
#' @return A DAG as an igraph object.
#' @export
#' @examples
#' g <- layer_DAGs(3, 4)
#' igraphviz(g)
layer_DAGs <- function(k, n, method = "ic-dag"){
  subset.index <- rep(1:k, each = n)
  node.names <- paste(1:(k*n))
  node.name.list <- split(node.names, f=subset.index)
  nets <- lapply(node.name.list, function(node.names){
    g <- bnlearn::random.graph(node.names, method = method) %>% #Convert bn to igraph by ...
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

#' Simulate a scale-free network given an input network.
#' 
#' Fit a power law to the input network, and simulate a new network based on 
#' that fitted power law.  Assumes the input network has a power law degree distribution.
#' @param input graph
#' @param n number of desired vertices in the output graph
#' @return a new igraph object.
#' @export
power_law_sim <- function(g, n){  
  gamma <- fit_barabasi_power(g)
  out_degree_dist <- igraph::degree.distribution(g, mode = "out")
  igraph::barabasi.game(n, power = gamma, out.dist = out_degree_dist)
}

#' Use input graph to estimate power of preferential attachment for Barabasi-Albert algorithm.
#' Fits a power law to the input graph and returns the value to use as the power of
#' preferential attachment in a Barabasi-Albert simulation of a scale-free network.  This
#' is the 'power' argument in igraph's 'barabasi.game" function.
#' @param g an input igraph object, the power law will be simulated from this object.
#' @param mle if TRUE, estimate the power value of the out-degree distribution using mle via
#' BFGS optimization.  If FALSE, use the 'plfit' implementation (see the power.law.fit function
#' in igraph).  Default is FALSE.  FALSE option will generate a warning if a bad fit is detected
#' but the TRUE option will not.
#' @return a numeric value.
#' @export
fit_barabasi_power <- function(g, mle = FALSE){
  in_degree <- igraph::degree(g, mode = "in") 
  if(mle){
    fit <- igraph::power.law.fit(in_degree + 1, implementation = "r.mle")
    deviation <- abs(gamma_alpha_table$alpha - as.numeric(fit@coef))
  }else{
    fit <- igraph::power.law.fit(in_degree + 1, implementation = "plfit")
    deviation <- abs(gamma_alpha_table$alpha - fit$alpha)
    if(fit$KS.p < .1) warning("Graph degree distribution is a poor fit to a power law.")
  } 
  # Map fitted alpha back to power using a lookup table.
  gamma_alpha_table$gamma[deviation == min(deviation)][1]
}
