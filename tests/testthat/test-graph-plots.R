context("test plots")
g1 <- layer_DAGs(5, 3)
g2 <- sim_DAG(10)
test_that("igraphviz works", {
  igraphviz(g1, main = "G1")
  igraphviz(g2, main = "G2")
})
test_that("plot_path works", {
  v1 <- get_roots(g1)[1]
  v2 <- get_leaves(g1)[1]
  plot_path(g1, v1, v2, main = "paths")
})