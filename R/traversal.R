#' Check if updated attribute is present
#' 
#' @g igraph graph object
#' @selector functions E or V
checkUpdateAttr <- function(g, selector){
  check.null <- g %>% 
    selector %>% {
      .$updated
    } %>%
    is.null 
  if(check.null) stop("No update attribute is present.")
}

#' Traversal for an iGraph.
#' 
#' This closure generates functions for traversing a graph.
#'   
#' @g v Assumed to represent a vertex.
#' @selector function E if working with edges, or V if working with vertices
#' 
#' This function, returns an error if v is not of the class 'character'.
getUpdater <- function(g, selector){
  checkUpdateAttr(g, selector)
  if(!V(g)[v]$updated){
    determiner.vertices <- getDeterminers(g, v)
    if(length(determiner.vertices) > 0){
      test.determiners.unupdated <- !V(g)[determiner.vertices]$updated
      if(any(test.determiners.unupdated)){
        unupdated.determiners <- determiner.vertices[test.determiners.unupdated]
        for(d in unupdated.determiners){
          g <- updateNode(g, d, getDeterminers, callback)
        }
      }
    }
    g <- callback(g, v)
    V(g)[v]$updated <- TRUE
  }
  g
  
}

updateNode <- function(g, v, getDeterminers, callback){
  if(!V(g)[v]$updated){
    determiner.vertices <- getDeterminers(g, v)
    if(length(determiner.vertices) > 0){
      test.determiners.unupdated <- !V(g)[determiner.vertices]$updated
      if(any(test.determiners.unupdated)){
        unupdated.determiners <- determiner.vertices[test.determiners.unupdated]
        for(d in unupdated.determiners){
          g <- updateNode(g, d, getDeterminers, callback)
        }
      }
    }
    g <- callback(g, v)
    V(g)[v]$updated <- TRUE
  }
  g
}

updateVertices <- function(g, getDeterminers, callback){
  for(v.name in V(g)$name){
    g <- updateNode(g, v.name, getDeterminers, callback) 
  }
  if(!all(V(g)$updated)){
    warning("The following nodes were not updated: ", 
            paste(V(g)[!updated]$name, collapse = ", "))
  }
  g
}
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
