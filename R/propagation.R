checkIndex <- function(object){
  if(!(class(object) %in% c("integer","numeric", "igraph.vs", "igraph.es"))){
    stop("Refer to edges/vertices with an integer, numeric, or igraph iterators")
  } 
}

#' Check if updated attribute is present
#' 
#' @param g igraph graph object
checkAttributes <- function(g, S){
  check.null <- is.null(S(g)$updated)  || is.null(S(g)$name)
  if(check.null) stop("Either 'name' or 'updated' attribute missing.")
}

#' Propagator closure
#' 
#' A closure generates functions for doing propagations on a graph.
#'   
#' @param iterator either "edge", or "vertex"
#' @return A function that updates edges (S = E) or vertices (S = V)
getUpdater <- function(iterator){
  rec.level <- 0
  updater <- function(g, object, getDeterminers, callback){
    checkIndex(object)
    S <- ifelse(iterator == "edge", igraph::E, igraph::V)
    `S<-` <- ifelse(iterator == "edge", igraph::`E<-`, igraph::`V<-`)
    checkAttributes(g, S)
    iterator <- ifelse(length(formals(S)) == 4, "edge", "vertex") # for debugging purposes, can delete
    #message("#", rec.level, " level recursion for ", iterator, " updater")
    rec.level <<- rec.level + 1
    if(length(S(g)[object]$updated) == 0) browser()
    if(!(S(g)[object]$updated)){ #'S' is either E or V, set by setIterator()
      determiners <- getDeterminers(g, object)
      if(length(determiners) > 0){
        test.determiners.unupdated <- !S(g)[S(g) %in% determiners]$updated
        if(any(test.determiners.unupdated)){
          unupdated.determiners <- determiners[test.determiners.unupdated]
          for(d in unupdated.determiners){
            g <- updater(g, d, getDeterminers, callback)
          }
        }
      }
      g <- callback(g, object)
      if(!is.igraph(g)) stop("Your callback needs to return a valid igraph object.")
      S(g)[object]$updated <- TRUE
    }
    rec.level <<- 0
    g
  }
  updater
}

#' Traversal for Propogation
#' 
#' This closure creates a function that traverses the graph, applying the updater as it travels.
#'   
#' @param iterator either "edge", or "vertex"
#' E if traversing over edges, or V if traversing over vertices
getTraverser <- function(iterator){
  traverser <- function(g, getDeterminers, callback){
    S <- ifelse(iterator == "edge", igraph::E, igraph::V)
    `S<-` <- ifelse(iterator == "edge", igraph::`E<-`, igraph::`V<-`)
    #setIterator(iterator)
    updater <- getUpdater(iterator)
    for(object in S(g)){
      g <- updater(g, object, getDeterminers, callback) 
    }
    if(!all(S(g)$updated)){
      warning("The following were not updated: ", 
              paste(S(g)[!updated]$name, collapse = ", "))
    }
    g
  }
  traverser
}

#' Vertex/Edge Propagation in igraph
#'   
#' Foundation for any graph propagation algorithm, where the states of vertexes or edges propagate through the graph,
#' changing the states of others.  Traverse the graph, land on a vertex/edge, perform a callback function on the 
#' vertex/edge.  That callback can depend on the states of other 'determiner' vertices/edges.  When the callback is 
#' completed successfully, the 'updated' attribute of the object is changed to \code{TRUE}. If those determiners do 
#' not have the status \code{updated == TRUE}, then the function recursively performs the callback on the determiners until all 
#' their states are updated, before finalling performing the callback on the original vertex/edge.
#' 
#' \code{updateVertices} performs propagation on vertices. \code{updateEdges} performs propagation on edges.  
#'   
#' @param g A graph object.
#' @param getDeterminers The function that gives the vertices/edges that must be updated for the callback to be 
#' executed 
#' @param callback The function performed on each vertex/edge
#' @return A igraph object with updated vertex/edge states. A warning message is returned if not all vertices 
#' could be updated.
#' @export
updateVertices <- getTraverser("vertex")

#' @describeIn updateVertices 
updateEdges <- getTraverser("edge")

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



