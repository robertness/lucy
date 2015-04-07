context("Graph Propagation")

g <- ba.game(100)
graph.objects <- c("edge", "vertex")

test_that("setIterator sets both the iterator function and iterator replacement function.", {
  lapply(graph.objects, function(obj) {
    setIterator(obj)
    expect_true(class(S(g)) %in% c("igraph.es", "igraph.vs"))
    S(g)$new.attribute <- "foobar"
    expect_equal(S(g)$new.attribute[1], "foobar")
  })
})

test_that("getUpdater closure creates vertex updater and edge updater with the correct arguments.", {
  lapply(graph.objects, function(obj){ #For the E edge iterator and V vertex iterator functions
    getUpdater(obj) %>% #create an updater
      formals %>% names %>% #get the arguments of the resulting closure
      expect_equal(c("g", "obj", "getDeterminers","callback")) #make sure args are as expected.    
  })
})

test_that("edge/vertex propagation without a name or update attribute will throw error. ", {
  lapply(graph.objects, function(obj){
    setIterator(obj)
    determiner <- function(g, x)  {}
    callback <- function(g, x) g
    updater <- getUpdater(obj)
    #Should get an indexing error if trying to index without a name.
    expect_error(updater(g, S(g)[1], function(g, obj){}, function(g, obj){}), 
                 "Only indexing with character names at this time.")
    #In the future, this will be expanded to indexing with igraph.vs, igraph.es, and numeric indeces
    #Will have to write tests for that.  But in that case, this should return the followingerror 
    #expect_error(updater(g, S(g)[1], iparents, function(g, obj) {}), 
    #               "Either 'name' or 'updated' attribute missing.")
    # Confirm just having name attribute isn't sufficient
    g <- nameEdges(g)
    g <- nameVertices(g)
    expect_error(updater(g, S(g)[1]$name, determiner, callback),
                 "Either 'name' or 'updated' attribute missing.")
    # Confirm if both attributes are present, there is no issue
    S(g)$updated <- FALSE
    expect_true(is.igraph(updater(g, S(g)[1]$name, determiner, callback)))
  })
})

test_that("Product of parents on a DAG works", {
  
})
# test_that("Works on a cyclic directed graph with cycles", {
#   
# })
# test_that("warns if there are dependency loops", {
#   
# })
# ####
# 
# library(plyr)
# library(dplyr)
# library(reshape)
# data(infert, package="datasets")
# net.infert <- neuralnet(case ~ parity + induced + spontaneous, infert,
#                         hidden = 3,
#                         err.fct="sse", linear.output=FALSE, likelihood=TRUE)
# new.vals <- data.frame(parity = 3,
#                        induced = 2, 
#                        spontaneous = 1)
# net.prediction <- unlist(neuralnet::compute(net.infert, new.vals))
# # The callback
# calculateNode <- function(g, v){
#   #browser()
#   parents <- iparents(g, v)
#   wts <- E(g)[parents %->% v]$weight
#   V(g)[v]$value <- V(g)[parents]$value * wts %>% #Multiply values by weights
#     sum %>% # Sum them up
#     activate # Apply the activation function
#   g
# }
# V(g)$value <- NA
# V(g)[inputs]$value <- c(3, 2, 1)
# V(g)["bias1"]$value <- V(g)["bias2"]$value <- 1
# #updates
# V(g)$updated <- FALSE
# V(g)[c(inputs, "bias1", "bias2")]$updated <- TRUE
# g <- updateVertices(g, getDeterminers = iparents, callback = calculateNode)
# V(g)["case"]$value
# test_that("can perform neural network.", {
#   
# })