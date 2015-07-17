context("Graph Operations")
test_that("get_upstream_nodes errors out on an undirected graph.", {
  g0 <-  erdos.renyi.game(100, 1/10, directed = FALSE, loops = FALSE) %>% name_vertices
  expect_error(get_upstream_nodes(g0, V(g0)[5]), "Graph must be directed.")
})
g <- ba.game(30) %>% name_vertices
w <- V(g)[igraph::degree(g, mode = "in") > 0] %>%
  intersect(V(g)[igraph::degree(g, mode = "out") > 0]) %>%
  `[`(1)
upstream <- get_upstream_nodes(g, w)
downstream <- get_downstream_nodes(g, w)
test_that("get_upstream_nodes does not return vertices that are downstream in an acyclic graph.", {
  expect_true(length(intersect(upstream, downstream)) == 0)
})
test_that("get_upstream_nodes/get_downstream_nodes will return an ancestor/descendent of the source node.", {
  u <- neighborhood(g, w, order = 3, mode = "in")[[1]] %>% setdiff(w) %>% `[`(1)
  expect_true(u %in% upstream)
  u <- neighborhood(g, w, order = 3, mode = "out")[[1]] %>% setdiff(w) %>% `[`(1)
  expect_true(u %in% downstream)
})
