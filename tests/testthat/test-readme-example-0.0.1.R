library(dplyr)
library(ptree)
library(testthat)

test_that("original example works", {
 ids <- c(
    "A1", "B1", "C1",
    "A2", "B2", "C2",
    "B3", "C3",
    "B4", "C4",
    "C5"
  )

  path_nodes <- lapply(ids, function(id) path_node(id))

  p <- path(path_nodes)
  p <- p %>%
    add_root_edge("A1", 1/2) %>%
    add_root_edge("A2", 1/2) %>%

    add_edge("A1", "B1", 2/3) %>%
    add_edge("B1", "C1", 1.0) %>%
    add_edge("A1", "B2", 1/3) %>%
    add_edge("B2", "C2", 1.0) %>%
    add_edge("A2", "B3", 1/4) %>%
    add_edge("B3", "C3", 1.0) %>%
    add_edge("A2", "B4", 3/4) %>%
    add_edge("B4", "C4", 1/2) %>%
    add_edge("B4", "C5", 1/2)

  # compute_tree( p ) %>% draw_tree()
  tree1 <- compute_tree( p )

  expect_no_failure( compute_tree( p ) %>% draw_tree() )

  nodes <- bind_rows(
    list(text = "A1", parent = 0, p = 1/2),
    list(text = "B1", parent = 1, p = 2/3),
    list(text = "C1", parent = 2, p = 1  ),
    list(text = "B2", parent = 1, p = 1/3),
    list(text = "C2", parent = 4, p = 1  ),
    list(text = "A2", parent = 0, p = 1/2),
    list(text = "B3", parent = 6, p = 1/4),
    list(text = "C3", parent = 7, p = 1  ),
    list(text = "B4", parent = 6, p = 3/4),
    list(text = "C4", parent = 9, p = 1/2),
    list(text = "C5", parent = 9, p = 1/2)
  )

  tree2 <- compute_tree( nodes )

  # expect_identical(tree1, tree2)
  expect_no_failure( draw_tree( tree2 ) )

})
