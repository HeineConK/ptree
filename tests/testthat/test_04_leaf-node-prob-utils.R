library("testthat")
library("dplyr")

test_that("leaf probability computations works, with zero prob warning", {
  p <- path()
  p <- add_node(p, id = "A")
  p <- add_node(p, id = "B")
  p <- add_node(p, id = "C")
  p <- add_node(p, id = "D")
  p <- add_node(p, id = "E", text = "test")

  p <- add_root_edge(p, node_child = p$nodes$A)
  p <- add_root_edge(p, id_child = "B")
  p <- add_root_edge(p, id_child = "C")

  p <- add_edge(p, id_parent = "A", id_child = "D")
  p <- add_edge(p, id_parent = "D", id_child = "E")

  expect_warning( compute_leaf_probs( p ), regexp = "zero probability" )

})

test_that("leaf probability computations works", {
  p <- path()
  p <- add_node(p, id = "A")
  p <- add_node(p, id = "B")
  p <- add_node(p, id = "C")
  p <- add_node(p, id = "D")
  p <- add_node(p, id = "E", text = "test")

  p <- add_root_edge(p, node_child = p$nodes$A, prob = 0.75)
  p <- add_root_edge(p, id_child = "B")
  p <- add_root_edge(p, id_child = "C")

  p <- add_edge(p, id_parent = "A", id_child = "D", prob = 0.5)
  p <- add_edge(p, id_parent = "D", id_child = "E", prob = 0.25)

  nodes <- compute_leaf_probs( p )
  expect_equal(nodes$leaf_p[ nodes$is_leaf ],
               .75 * .5 * .25
  )
})

test_that("leaf probability is correclty invoked in compute tree",{
  p <- path()
  p <- add_node(p, id = "A")
  p <- add_node(p, id = "B")
  p <- add_node(p, id = "C")
  p <- add_node(p, id = "D")
  p <- add_node(p, id = "E", text = "test")

  p <- add_root_edge(p, node_child = p$nodes$A, prob = 0.75)
  p <- add_root_edge(p, id_child = "B")
  p <- add_root_edge(p, id_child = "C")

  p <- add_edge(p, id_parent = "A", id_child = "D", prob = 0.5)
  p <- add_edge(p, id_parent = "D", id_child = "E", prob = 0.25)

  ndigits <- 5
  tree <- compute_tree( p, compute_leaf_probs = T, round = ndigits )
  expect_equal(tree$leaf_p[ tree$is_leaf ],
               round(.75 * .5 * .25, ndigits)
  )

  expect_no_failure(
    draw_tree( tree )
  )

})



test_that("leaf probabilities are correclty computed at different tiers", {
  n_s1 <- path_node(id = "s1", text = "AoS")
  n_n1 <- path_node(id = "n1", text = "bar(AoS)")



  path1 <- path() %>%
    add_root_edge(node_child = n_s1, prob =  1/52, prob_str = "1/52") %>%
    add_root_edge(node_child = n_n1, prob = 51/52, prob_str = "51/52")


  expect_s3_class(path1, "path")

  expect_no_failure( compute_tree( path1 ) )

  n_s2 <- path_node(id = "s2", text = "A")
  n_n2 <- path_node(id = "n2", text = "bar(A)")


  path2 <- path1 %>%
    add_edge(path = .,
             node_parent = n_n1,
             node_child = n_s2,
             prob = 1/51,
             prob_str = "1/51") %>%
    add_edge(path = .,
             node_parent = n_n1,
             node_child = n_n2,
             prob = 50/51,
             prob_str = "50/51")








})


