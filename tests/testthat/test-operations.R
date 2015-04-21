context("Graph Operations")
test_that("getUpstreamNodes errors out on an undirected graph.", {
  g0 <-  erdos.renyi.game(100, 1/10, directed = FALSE, loops = FALSE) %>% nameVertices
  expect_error(getUpstreamNodes(g0, V(g0)[5]), "Graph must be directed.")
})
g <- ba.game(30) %>% nameVertices
w <- V(g)[degree(g, mode = "in") > 0] %>%
  intersect(V(g)[degree(g, mode = "out") > 0]) %>%
  `[`(1)
upstream <- getUpstreamNodes(g, w)
downstream <- getDownstreamNodes(g, w)
test_that("getUpstreamNodes does not return vertices that are downstream in an acyclic graph.", {
  expect_true(length(intersect(upstream, downstream)) == 0)
})
test_that("getUpstreamNodes/getDownstreamNodes will return an ancestor/descendent of the source node.", {
  u <- neighborhood(g, w, order = 3, mode = "in")[[1]] %>% setdiff(w) %>% `[`(1)
  expect_true(u %in% upstream)
  u <- neighborhood(g, w, order = 3, mode = "out")[[1]] %>% setdiff(w) %>% `[`(1)
  expect_true(u %in% downstream)
})
