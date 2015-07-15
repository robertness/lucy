context("Graph Simulations")
test_that("layered DAG flows from top to bottom.", {
  g.skinny <- layer_DAGs(10, 3)
  g.fat <- layer_DAGs(3, 5)
  expect_true(igraph::is.dag(g.skinny))
  expect_true(igraph::is.dag(g.fat))
  expect_true(
    isBDownstreamOfA(g.skinny, get_roots(g.skinny)[1], get_leaves(g.skinny)[1])
  )
  expect_true(
    isBDownstreamOfA(g.fat, get_roots(g.fat)[1], get_leaves(g.fat)[1])
  )
})
test_that("mlp generation is robust", {
  #works with one layer
  g <- mlp_graph("I1", "O1", 10)
  (V(g)$layer %in% "H1") %>% sum %>% expect_equal(10)
  #Expect error if input or output name in the hidden nodes' names
  expect_error(mlp_graph("H12", "O1", 4),
               "Names of the inputs or outputs conflict with name of hidden nodes.")
})
test_that("mlp generation works without hidden layers", {
  g <- mlp_graph(paste(1:10), "Out")
  expect_true(ecount(g) == 10)
})

