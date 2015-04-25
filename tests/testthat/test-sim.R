context("Graph Simulations")
test_that("layered DAG flows from top to bottom.", {
  g.skinny <- layerDAGs(10, 3)
  g.fat <- layerDAGs(3, 5)
  expect_true(igraph::is.dag(g.skinny))
  expect_true(igraph::is.dag(g.fat))
  expect_true(
    isBDownstreamOfA(g.skinny, getRoots(g.skinny)[1], getLeaves(g.skinny)[1])
  )
  expect_true(
    isBDownstreamOfA(g.fat, getRoots(g.fat)[1], getLeaves(g.fat)[1])
  )
})
test_that("mlpgeneration is robust", {
  #works with one layer
  g <- mlpgraph("I1", 10, "O1")
  (V(g)$layer %in% "H1") %>% sum %>% expect_equal(10)
  #Expect error if input or output name in the hidden nodes' names
  expect_error(mlpgraph("H12", 4, "O1"),
               "Names of the inputs or outputs conflict with name of hidden nodes.")
})