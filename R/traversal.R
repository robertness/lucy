updateNode <- function(g, v.index, getDeterminers, callback){  
  v <- V(g)[v.index]
  if(!v$updated){
    determiner.vertices <- getDeterminers(g, v)
    if(length(determiner.vertices) > 0){
      test.determiners.unupdated <- !determiner.vertices$updated
      if(any(test.determiners.unupdated)){
        unupdated.determiners <- determiner.vertices[test.determiners.unupdated]
        for(d in unupdated.determiners){
          g <- updateNode(g, d, getDeterminers, callback)
        }
      }
    }
    g <- callback(g, v)
    V(g)[v.index]$updated <- TRUE
  }
  g
}

updateVertices <- function(g, getDeterminers, callback){ 
  for(v.index in 1:vcount(g)){
    g <- updateNode(g, v.index, getDeterminers, callback) 
  }
  if(!all(V(g)$updated)){
    warning("The following nodes were not updated: ", 
            paste(V(g)[!updated]$name, collapse = ", "))
  }
  g <- resetUpdateAttributes(g)
  g
}
#In here I am sticking something to modify a caese when there is no determiners
#I'll clean this up when I combine updateEdge and updateVertex into updateProperty
#As a general rule, I will write a function for dealing directly with with an igraph base object
#then write an encapsulating function for working with a modified igraph objec
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