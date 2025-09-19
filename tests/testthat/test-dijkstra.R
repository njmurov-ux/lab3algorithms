wiki_graph <-
  data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
             v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
             w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))

test_that("outputs are correct in the Dijkstra algorithm.", {
  expect_equal(dijkstra(wiki_graph,1), c(0,7,9,20,20,11))
  expect_equal(dijkstra(wiki_graph,3), c(9,10,0,11,11,2))
})


test_that("Error messages are returned for erronous input in the Dijkstra algorithm.", {
  wiki_wrong_graph <- wiki_graph
  names(wiki_wrong_graph) <- c("v1, v3, w")
  expect_error(dijkstra(wiki_wrong_graph, 3))
  wiki_wrong_graph <- wiki_graph[1:2]
  expect_error(dijkstra(wiki_wrong_graph, 3))
  expect_error(dijkstra(wiki_graph, 7))
  expect_error(dijkstra(as.matrix(wiki_graph), 3))
})

neg_weight_graph <- data.frame(v1 = 1, v2 = 2, w = -1)

test_that("Negative edge weights should raise error", {
  expect_error(dijkstra(neg_weight_graph, 1))
})

too_few_cols <- data.frame(v1=1, v2=2)
too_many_cols <- data.frame(v1=1, v2=2, w=3, extra=4)

test_that("graph has too few or too many columns", {
  expect_error(dijkstra(too_few_cols, 1), "graph must have exactly three columns")
  expect_error(dijkstra(too_many_cols, 1), "graph must have exactly three columns")
})

inf_graph <- data.frame(v1=c(1), v2=c(2), w=c(Inf))

test_that("graph contains Inf", {
  expect_error(dijkstra(inf_graph, 1), "graph contains non-finite values")
})

test_that("init is not numeric", {
  expect_error(dijkstra(wiki_graph, "a"), "init must be a finite numeric scalar")
})

