#' Enforce integer indexing
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


#' Edge propagation instance
#' Executes a callback on the edge that calculates a value.  The callback can rely
#' on values of other edges that are calculated by the same callback.  When the function
#' is applied to an edge, it checks that an 'update' attribute on all the edges that it
#' relies on is set to true before applying the callback.  If not, the callback is applied
#' to those unupdated edges first.
#' @param g an igraph object
#' @param e an index for an edge in g
#' @param get_determiners a function that returns the indices of the edges that must be updated
#' before the callback is executed.
#' @param verbose if TRUE prints out details of propagation
#' @param a function that performs an operation on an edge.
#' @export
vertex_updater <- function(g, v, get_determiners, callback, verbose = FALSE){
  checkIndex(v)
  checkAttributes(g, V)
  #message("#", rec.level, " level recursion for ", "vertex updater")
  if(!(V(g)[v]$updated)){
    determiners <- get_determiners(g, v)
    if(length(determiners) > 0){
      test.determiners.unupdated <- !V(g)[V(g) %in% determiners]$updated
      if(any(test.determiners.unupdated)){
        unupdated.determiners <- determiners[test.determiners.unupdated]
        if(verbose) {
          g <- name_vertices(g)
          message("propagating to vertices ",
                   paste(V(g)[unupdated.determiners]$name, collapse = ", "))
        }
        for(d in unupdated.determiners){
          g <- vertex_updater(g, d, get_determiners, callback)
        }
      }
    }
    g <- callback(g, v)
    if(!is.igraph(g)) stop("Your callback needs to return a valid igraph object.")
    V(g)[v]$updated <- TRUE
  }
  g
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
#' \code{update_vertices} performs propagation on vertices. \code{updateEdges} performs propagation on edges.
#'
#' @param g A graph object.
#' @param get_determiners A function returns the vertice/edge indices that must be updated for the callback to be
#' executed.  Arguments: g - the graph object, i - the index of a vertex/edge in g. Returns: an array of indices
#' for the vertices/edges that are needed to perform the callback on vertex/edge i.
#' @param callback The function performed on each vertex/edge. Arguments: g - the graph object, i - the index of a graph/edge in g.
#' Returns: a graph object.
#' @param verbose if TRUE prints out details of propagation
#' @return A igraph object with updated vertex/edge states. A warning message is returned if not all vertices
#' could be updated.
#' @export
update_vertices <- function(g, get_determiners, callback, verbose = FALSE){
  for(v in V(g)){
    g <- vertex_updater(g, v, get_determiners, callback, verbose = verbose)
  }
  if(!all(V(g)$updated)){
    warning("The following were not updated: ",
            paste(V(g)[!updated]$name, collapse = ", "))
  }
  g
}

#' Edge propagation instance
#' Executes a callback on the edge that calculates a value.  The callback can rely
#' on values of other edges that are calculated by the same callback.  When the function
#' is applied to an edge, it checks that an 'update' attribute on all the edges that it
#' relies on is set to true before applying the callback.  If not, the callback is applied
#' to those unupdated edges first.
#' @param g an igraph object
#' @param e an index for an edge in g
#' @param get_determiners a function that returns the indices of the edges that must be updated
#' before the callback is executed.
#' @param a function that performs an operation on an edge.
#' @param verbose if TRUE prints out details of propagation
#' @export
edge_updater <- function(g, e, get_determiners, callback, verbose = FALSE){
  checkIndex(e)
  checkAttributes(g, E)
  e <- as.numeric(e)
  if(!(E(g)[e]$updated)){
    determiners <- get_determiners(g, e)
    if(length(determiners) > 0){
      test.determiners.unupdated <- !E(g)[E(g) %in% determiners]$updated
      if(any(test.determiners.unupdated)){
        unupdated.determiners <- determiners[test.determiners.unupdated]
        if(verbose) {
          g <- name_vertices(g)
          unupdated.determiners.names <- unlist(
            lapply(unupdated.determiners, function(det_e){
              paste(V(g)[get_edge_vertex(g, det_e)]$name, collapse = "->")
            })
          )
          message("propagating to edges ", paste(unupdated.determiners.names, collapse = ", "))
        }
        for(d in unupdated.determiners){
          g <- edge_updater(g, d, get_determiners, callback)
        }
      }
    }
    g <- callback(g, e)
    if(!is.igraph(g)) stop("Your callback needs to return a valid igraph object.")
    E(g)[e]$updated <- TRUE
  }
  g
}

#' @describeIn update_vertices
#' @export
update_edges <- function(g, get_determiners, callback, verbose = FALSE){
  for(e in E(g)){
    g <- edge_updater(g, e, get_determiners, callback, verbose = verbose)
  }
  if(!all(V(g)$updated)){
    warning("The following were not updated: ",
            paste(paste(get_edge_vertex(E(g)[!updated]), collapse = "<-"), collapse = ", ")
    )
  }
  g
}
