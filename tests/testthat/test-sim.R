
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