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