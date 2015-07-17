#' igraph plotting with Graphviz library
#' 
#' A simple plotter of igrpah objects with RGraphviz.  Nodes are labeled by 
#' their vertex names.  For more complicated graphing is need, use Graphviz 
#' directly. 

#' @param g an igraph object.
#' @param main character, the desired title of the graph. Default is NULL
#' @return graphviz.plot returns invisibly the graph object produced by Rgraphviz.
#' @examples
#' g <- ba.game(20)
#' igraphviz(g)
#' g <- layer_DAGs(3, 4)
#' igraphviz(g)
#' @export
igraphviz <- function(g, main = NULL){
  g <- name_vertices(g)
  gnell <- g %>% igraph.to.graphNEL %>% {Rgraphviz::layoutGraph(.)}
  Rgraphviz::layoutGraph(gnell, nodeAttrs=list(label=structure(V(g)$name, names=V(g)$name)))
  if(!is.null(main)) graph::graph.par(list(graph = list(main = main))) # Add a title if one is given
  Rgraphviz::renderGraph(gnell) # Render the graph
  graph::graph.par(list(graph = list(main = ""))) # Reset graph parameters
}

#' Plot path from an upstream vertex to a downstream vertex.
#'
#' @param g igraph object
#' @param src an upstream vertex in g
#' @param trg a downstream vertex in g
#' @param main character, the desired title of the graph. Default is NULL
#' @examples
#' set.seed(20)
#' g <- layer_DAGs(6, 3)
#' plot_path(g, 4, 11)
#' @export
plot_path <- function(g, src, trg, main = NULL){
  if(!(trg %in% get_downstream_nodes(g, src))) stop("Target is not downstream of the source.")
  node_list <- {rep("green", 2)} %>% 
    structure(names = V(g)[c(src, trg)]$name) %>%
    {list(fill = .)} 
  edge_list <- V(g)[get_connecting_nodes(g, src, trg)] %>%
    {E(g)[. %->% .]} %>%
    {get.edgelist(g)[., , drop = F]} %>% 
    apply(1, paste0, collapse="~") %>%
    {structure(rep("green", length(.)), names = .)} %>%
    {list(col = .)}
  g_out <- g %>% name_vertices %>% # Give vertices names if they do not have nay 
    igraph.to.graphNEL(.) %>% # convert to a graphNEL
    {Rgraphviz::layoutGraph(.)} %>% # lay the graph out
    {graph::`nodeRenderInfo<-`(., node_list)} %>% # add the node annotation
    {graph::`edgeRenderInfo<-`(., edge_list)} # add the edge annotation
    if(!is.null(main)) graph::graph.par(list(graph = list(main = main))) # Add a title if one is given
    Rgraphviz::renderGraph(g_out) # Render the graph
    graph::graph.par(list(graph = list(main = ""))) # Reset graph parameters
}