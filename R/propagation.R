#' Check if updated attribute is present
#' 
#' @g igraph graph object
#' @S either of igraph sequence selector functions `E` or `V`
checkUpdateAttr <- function(g, S){
  check.null <- selector(g)$updated %>% is.null 
  if(check.null) stop("No update attribute is present.")
}

#' Propagation.
#' 
#' This closure generates functions for traversing a graph.
#'   
#' @g igraph graph object.
#' @S either of igraph sequence selector functions `E` or `V`; 
#' E if traversing over edges, or V if traversing over vertices
#' 
#' This function, returns an error if v is not of the class 'character'.
getUpdater <- function(g, S){
  checkUpdateAttr(g, S)
  updater <- function(g, obj, getDeterminers, callback){
    if(!S(g)[obj]$updated){
      determiners <- getDeterminers(g, obj)
      if(length(determiners) > 0){
        test.determiners.unupdated <- !S(g)[determiners]$updated
        if(any(test.determiners.unupdated)){
          unupdated.determiners <- determiners[test.determiners.unupdated]
          for(d in unupdated.determiners){
            g <- updater(g, d, getDeterminers, callback)
          }
        }
      }
      g <- callback(g, obj)
      S(g)[obj]$updated <- TRUE
    }
    g
  }
  updater
}
getTraverser <- function(g, S){
  traverser <- function(g, getDeterminers, callback){
    updater <- getUpdater(g, S)
    for(obj.name in S(g)$name){
      g <- updater(g, obj.name, getDeterminers, callback) 
    }
    if(!all(S(g)$updated)){
      warning("The following were not updated: ", 
              paste(S(g)[!updated]$name, collapse = ", "))
    }
    g
  }
  traverser
}
#' Vertex traversal for an iGraph.
#' 
#' Graph traversal is the problem of visiting all the vertices in a graph in a particular manner, 
#' updating and/or checking their values along the way.
#'   
#' @g A graph object.
#' @getDeterminers 
#' 
#' callback either of igraph sequence selector functions `E` or `V`; 
#' E if traversing over edges, or V if traversing over vertices
#' 
#' This function, returns an error if v is not of the class 'character'.
#' @export
updateVertices <- getTraverser(g, V)
#' @export
updateEdges <- getTraverser(g, E)

#' Graph Traversal
#' 
#' This uses the igraph traversal algorithms to produce a perhaps a better ordering of 
#' vertexs in the traversal done in the updating.  Not yet used, may be applied later to
#' improve speed, or when a callback is needed to stop the traversal in the event of cycles.   
getOrdering <- function(g, S, traversal = "depth.first"){
  list(breadth.first =
         function() graph.bfs(g, root = V(g)[1], 
                              neimode = "out", 
                              unreachable = TRUE, 
                              order = TRUE)$order,
       depth.first = 
         function() graph.dfs(g, root = V(g)[1], 
                              neimode = "out", 
                              unreachable = TRUE, 
                              order = TRUE)$order)[[traversal]]
}


