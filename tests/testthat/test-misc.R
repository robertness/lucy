context("Miscellaneous graph utility functions.")
library(igraph)
test_that("Roots and leaves correctly defined", {
  g <- igraph::ba.game(20)
  roots <- getRoots(g)
  leaves <- getLeaves(g)
  expect_equal(length(iparents(g, roots)), 0)
  expect_equal(length(ichildren(g, leaves)), 0)
})