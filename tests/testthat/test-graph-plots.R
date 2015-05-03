context("test plots")
g1 <- layerDAGs(5, 3)
g2 <- generateMultiConnectedDAG(10)
test_that("igraphviz works", {
  igraphviz(g1)
  igraphviz(g2)
})
test_that("plot_path works", {
  v1 <- get_roots(g1)[1]
  v2 <- get_leaves(g1)[1]
  plot_path(g1, v1, v2)
})