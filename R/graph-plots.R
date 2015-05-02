#' igraph plotting with Graphviz library
#' 
#' A simple plotter of igrpah objects with RGraphviz.  Nodes are labeled by 
#' their vertex names.  For more complicated graphing is need, use Graphviz 
#' directly. 

#' @param g an igraph object.
#' @return graphviz.plot returns invisibly the graph object produced by Rgraphviz.
#' @examples
#' g <- ba.game(20)
#' igraphviz(g)
#' g <- layerDAGs(3, 4)
#' igraphviz(g)
#' @export
igraphviz <- function(g){
  if(!requireNamespace("Rgraphviz", quietly = TRUE)) "RGraphviz not attached."
  g <- nameVertices(g)
  gnell <- g %>% igraph.to.graphNEL %>% layoutGraph
  layoutGraph(gnell, nodeAttrs=list(label=structure(V(g)$name, names=V(g)$name)))
  renderGraph(gnell)
}

#' Plot path from an upstream vertex to a downstream vertex.
#'
#' @param g igraph object
#' @param src an upstream vertex in g
#' @param trg a downstream vertex in g
#' @examples
#' set.seed(20)
#' g <- layerDAGs(6, 3)
#' plot_path(g, 4, 11)
#' @export
plot_path <- function(g, src, trg){
  requireNamespace("Rgraphviz")
  if(!(trg %in% getDownstreamNodes(g, src))) stop("Target is not downstream of the source.")
  node_list <- {rep("green", 2)} %>% 
    structure(names = V(g)[c(src, trg)]$name) %>%
    {list(fill = .)} 
  edge_list <- V(g)[getConnectingNodes(g, src, trg)] %>%
    {E(g)[. %->% .]} %>%
    {get.edgelist(g)[., , drop = F]} %>% 
    apply(1, paste0, collapse="~") %>%
    {structure(rep("green", length(.)), names = .)} %>%
    {list(col = .)}
  g %>% nameVertices %>% # Give vertices names if they do not have nay 
    igraph.to.graphNEL(.) %>% # convert to a graphNEL
    layoutGraph(.) %>% 
    {`nodeRenderInfo<-`(., node_list)} %>%
    {`edgeRenderInfo<-`(., edge_list)} %>%
    renderGraph
}