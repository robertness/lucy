#' igraph DAG plotting with graphviz library
#' 
#' The default layout for DAGs (directed acyclic graphs) in the graphviz library  is more 
#' interpretable than with igraph layouts,especially when the DAGs are densely connected.
#' 
#' In the current implementation, the bnlearn package is used as an intermediary, as it will
#' automatically check if the graph is a DAG.
#' @param g a DAG igraph object.
#' @param plot.args arguments for \code{bnlearn::graphviz.plot()}
#' @return graphviz.plot returns invisibly the graph object produced by Rgraphviz.
#' @examples
#' g <- ba.game(20)
#' igraphVizPlot(g)
#' g <- layerDAGs(3, 4)
#' igraphVizPlot(g)
igraphVizPlot <- function(g, plot.args=NULL){
  g <- nameVertices(g)
  net <- bnlearn::empty.graph(V(g)$name)
  bnlearn::arcs(net) <- get.edgelist(g)
  args <- c(list(x = net), plot.args)
  do.call("graphviz.plot", args, envir = as.environment("package:bnlearn"))
}

