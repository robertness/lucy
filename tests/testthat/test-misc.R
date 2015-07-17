context("Miscellaneous graph utility functions.")

test_that("Roots and leaves correctly defined", {
  g <- igraph::ba.game(20)
  roots <- get_roots(g)
  leaves <- get_leaves(g)
  lapply(roots, function(root){ # Define as node in directed network with no parents
    iparents(g, root) %>% 
      length %>%
      expect_equal(0)
  })
  lapply(leaves, function(leaf){ # Define as node in directed network with no children
    ichildren(g, leaf) %>% 
      length %>%
      expect_equal(0)
  })
})

test_that("All functions edges/vertices on iterators take an numeric
          index or edge/vertex sequence object.",{
            # These tests may look redundant since igraph already takes these kinds
            # of iterator arguments.  However, since I previously attempted to work
            # with names instead of indices and later scrapped this idea, the tests
            # are neccessary to suss out errors in other code.
            set.seed(10)
            g <- sim_DAG(50)
            checkVertexInputs <- function(v, up.str, down.str){
              get_downstream_nodes(g, v)[5:9] %>%
                identical(c(12, 14, 16, 17, 18)) %>%
                expect_true
              isBDownstreamOfA(g, a = up.str, b = down.str) %>%
                expect_true
              get_upstream_nodes(g, v) %>%
                identical(c(1, 2, 3, 4, 5, 6, 7, 13, 15)) %>%
                expect_true
              get_connecting_nodes(g, up.str, down.str)[15:20] %>% 
                identical(c(18, 19, 20, 21, 22, 23)) %>%
                expect_true
              get_connecting_edges(g, up.str, down.str)[81:85] %>%
                identical(c(120, 147, 148, 149, 150)) %>%
                expect_true
              iparents(g, down.str)[5:10] %>%
                identical(c(11, 12, 15, 17, 19, 22)) %>%
                expect_true
              ichildren(g, v)[5:10] %>%
                identical(c(16, 18, 19, 22, 24, 26)) %>%
                expect_true
              in_degree(g, v) %>% 
                identical(structure(9, names = "21")) %>%
                expect_true
              out_degree(g, v) %>%
                identical(structure(23, names = "21")) %>%
                expect_true
              V(name_vertices(g))[v] %>%
                as.numeric %>%
                identical(21) %>%
                expect_true
              E(name_edges(g))[to(v)] %>%
                as.numeric %>%
                identical(c(11, 32, 46, 63,  89, 110, 130,236, 275)) %>%
                expect_true
              expect_error(checkVertex("12"), 
                           "Use of names instead of indices or igraph sequences as arguments may lead to problems.")            
            }
            checkVertexInputs(21, 1, 40)
            checkVertexInputs(V(g)[21], V(g)[1], V(g)[40])
            (checkVertex(12) == 12) %>%
              expect_true
            (checkVertex(V(g)[12]) == 12) %>%
              expect_true
          })

test_that("All functions that return edges or vertices return numerics",{
  set.seed(10)
  g <- sim_DAG(50)
  v <- 21; up.str <- 1; down.str <- 40
  get_leaves(g) %>% is.numeric %>% expect_true
  get_roots(g) %>% is.numeric %>% expect_true
  get_downstream_nodes(g, v)[5:9] %>%
    identical(c(12, 14, 16, 17, 18)) %>%
    expect_true
  get_upstream_nodes(g, v) %>%
    identical(c(1, 2, 3, 4, 5, 6, 7, 13, 15)) %>%
    expect_true
  get_connecting_nodes(g, up.str, down.str)[15:20] %>% 
    identical(c(18, 19, 20, 21, 22, 23)) %>%
    expect_true
  get_connecting_edges(g, up.str, down.str)[81:85] %>%
    identical(c(120, 147, 148, 149, 150)) %>%
    expect_true
  iparents(g, down.str)[5:10] %>%
    identical(c(11, 12, 15, 17, 19, 22)) %>%
    expect_true
  ichildren(g, v)[5:10] %>%
    identical(c(16, 18, 19, 22, 24, 26)) %>%
    expect_true
  in_degree(g, v) %>% 
    identical(structure(9, names = "21")) %>%
    expect_true
  out_degree(g, v) %>%
    identical(structure(23, names = "21")) %>%
    expect_true
  V(name_vertices(g))[v] %>%
    as.numeric %>%
    identical(21) %>%
    expect_true
  E(name_edges(g))[to(v)] %>%
    as.numeric %>%
    identical(c(11, 32, 46, 63,  89, 110, 130,236, 275)) %>%
    expect_true
})

test_that("Functions that take single vertices/edges as arguments error out when
          more than one vertex or argument is passed.", {
            set.seed(10)
            g <- sim_DAG(50)
            v <- 21; down.str <- 40
            w <- c(20, 21, 22)
            vlist <- list(
              get_downstream_nodes,
              get_upstream_nodes,
              function(g, v) get_connecting_nodes(g, src = v, trg = down.str),
              iparents,
              ichildren)
            lapply(vlist, function(func){
              expect_error(func(g, w), "Limited to one vertex.")
            })
          })

test_that("edge name should not return any NAs", {
  g <- sim_DAG(50) %>% name_edges
  E(g)$name %>% is.na %>% any %>% expect_false
})