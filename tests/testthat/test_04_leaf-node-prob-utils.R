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

test_that("leaf prbability is correclty invoked in compute tree",{
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
