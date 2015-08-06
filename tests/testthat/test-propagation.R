context("Graph Propagation")

g <- ba.game(100)
devtools::load_all()

test_that("vertex propagation only works with numeric indices or igraph iterators.", {
  determiner <- function(g, x)  {}
  callback <- function(g, x) g
  g <- g %>% name_edges %>% name_vertices 
  V(g)$updated <- FALSE
  #Test the outcome is an igraph when using an iterator
  g.out <- vertex_updater(g, V(g)[1], determiner, callback)
  expect_true(is.igraph(g.out))
  #Test the same result comes out with a numeric index
  vertex_updater(g, as.numeric(V(g)[1]), determiner, callback) %>%
    identical(g.out) %>%
    expect_true
  #Should get an indexing error if trying to index with a name.
  expect_error(vertex_updater(g, V(g)[1]$name, determiner, callback),
               "Refer to edges/vertices with an integer, numeric, or igraph iterators")
})

test_that("edge propagation only works with numeric indices or igraph iterators.", {
  determiner <- function(g, x)  {}
  callback <- function(g, x) g
  g <- g %>% name_edges %>% name_vertices 
  E(g)$updated <- FALSE
  #Test the outcome is an igraph when using an iterator
  g.out <- edge_updater(g, E(g)[1], determiner, callback)
  expect_true(is.igraph(g.out))
  #Test the same result comes out with a numeric index
  edge_updater(g, as.numeric(E(g)[1]), determiner, callback) %>%
    identical(g.out) %>%
    expect_true
  #Should get an indexing error if trying to index with a name.
  expect_error(edge_updater(g, E(g)[1]$name, determiner, callback),
               "Refer to edges/vertices with an integer, numeric, or igraph iterators")
})

test_that("vertex propagation without a name or update attribute will throw error. ", {
  determiner <- function(g, x)  {}
  callback <- function(g, x) g
    # Confirm just having name attribute isn't sufficient
  g <- name_edges(g)
  g <- name_vertices(g)
  expect_error(vertex_updater(g, E(g)[1], determiner, callback),
                 "Either 'name' or 'updated' attribute missing.")
    # Confirm if both attributes are present, there is no issue
  V(g)$updated <- FALSE
  expect_true(is.igraph(vertex_updater(g, V(g)[1], determiner, callback)))
})

test_that("callback function returns a graph object otherwise an error is thrown", {
  g <- ba.game(30) %>% name_vertices
  V(g)$value <- runif(30)
  V(g)$updated <- FALSE
  get_product <- function(g, v) {
    V(g)[v]$value <- prod(V(g)[iparents(g, v)]$value)
    #This fails to return an igraph object, a common mistake
  }  
  expect_error(update_vertices(g, iparents, get_product), 
               "Your callback needs to return a valid igraph object.")
})

test_that("product of parents on a DAG works", {
  # See the vignette on proopagation for an explanation of this test.
  g <- ba.game(10) %>% name_vertices
  V(g)$value <- log(sample(1:50, 10))
  V(g)$updated <- FALSE
  getSum <- function(g, v) {
    parent.nodes <- iparents(g, v)
    if(length(parent.nodes) > 0) {
      V(g)[v]$value <- V(g)[v]$value + sum(V(g)[iparents(g, v)]$value)
    }
    g
  }
  g.final <- update_vertices(g, iparents, getSum)
  leaf <- get_leaves(g)[1]
  upstream.nodes <- get_upstream_nodes(g, leaf)
  expect_equal(V(g.final)[leaf]$value, sum(V(g)$value))
})

library(neuralnet, quietly=T)
library(reshape, quietly=T)
library(plyr, quietly=T)

data(infert, package="datasets")
test_that("method mirrors neural network prediction.",{

  net.infert <- neuralnet(case ~ parity + induced + spontaneous, infert,
                          hidden = 3,
                          err.fct="sse", linear.output=FALSE, likelihood=TRUE)
  #I can use this fitted model for prediction by passing it new input values to the *compute* function.
  nnet.prediction <- neuralnet::compute(net.infert, data.frame(parity = 3, 
                                                               induced = 2, 
                                                               spontaneous = 1)) %>%
    unlist %>% round(2) %>% #unlist and round the results
    `names<-`(c("bias1", "parity", "induced", "spontaneous", "bias2",
                "H1", "H2", "H3", "case")) #name the variables
  #The input values are propagated to the neurons (hidden nodes), and those values are propagated to the outcome I wish to predict.
  #Now I demonstrate the same predictive results with propagation.
  #I start by converting the neural net structure to an igraph object.
  inputs <- c("parity", "induced", "spontaneous")
  wts <- net.infert$weights[[1]]
  dimnames(wts[[1]]) <- list(c("bias1", inputs),
                             c("H1", "H2", "H3"))
  dimnames(wts[[2]]) <- list(c("bias2", "H1", "H2", "H3"), "case")
  g <- wts %>%
    lapply(melt) %>%
    rbind.fill %>%
    `names<-`(c("from", "to", "weight")) %>%
    graph.data.frame 
  #The weights are stored as the "weight" edge attribute.
  #I add a 'value' attribute to the vertices.  The values for the input are given as 3, 2, 1.  These will be used to calculate values for the hidden variables and ultimately the output variable "case". 
  V(g)$value <- NA
  V(g)[inputs]$value <- c(3, 2, 1)
  V(g)["bias1"]$value <- V(g)["bias2"]$value <- 1
  #I add a "updated" attribute to the vertices as well.  The biases will always have a value of 1, and the inputs are fixed, so I set their *updated* attribute to true.
  V(g)$updated <- FALSE
  V(g)[c(inputs, "bias1", "bias2")]$updated <- TRUE
  #The activation function in the neural network is the logistic function:
  activate <- function(x)  1 / (1 + exp(-x))
  #The activation function is what calculates the value of a node given its parents.  So I use it to create a callback called *calculateNode*. 
  calculateNode <- function(g, v){
    parents <- iparents(g, v)
    wts <- E(g)[parents %->% v]$weight
    #wts <- igraph::`[.igraph.es`(E(g), parents %->% v)$weight
    V(g)[v]$value <- activate(sum(V(g)[parents]$value * wts)) 
    g
  }
  #Since the state of each node is determined by the state of its parent nodes, the *get_determiners* function should only return the parents of the node.  
  #This function exists in *Lucy*, it is *iparents*.
  #Now we are ready for the propagation of values across the vertices.
  g.final <- update_vertices(g, get_determiners = iparents, callback = calculateNode)
  propagation.prediction <- round(V(g.final)$value, 2)
  names(propagation.prediction) <- V(g.final)$name
  #Comparing the original prediction to these results:
  expect_equal(nnet.prediction, propagation.prediction)
})

test_that("update_vertices doesn't have X wrong with it causing it to  go into
          some infinite recursion", {})

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
# g <- update_vertices(g, get_determiners = iparents, callback = calculateNode)
# V(g)["case"]$value
# test_that("can perform neural network.", {
#   
# })