library("testthat")
library("dplyr")

test_that("computing and drawing trees from path object works",{
  p <- path() %>%
    add_node(, id = "A", text = "a") %>%
    add_node(, id = "B", text = "b") %>%
    add_node(, id = "C", text = "c") %>%
    add_node(, id = "D", text = "d") %>%
    add_node(, id = "E", text = "e") %>%
    add_node(, id = "F", text = "f") %>%

    add_root_edge(id_child = "A", prob = 0.5) %>%
    add_root_edge(id_child = "B", prob = 0.5) %>%
    add_root_edge(id_child = "C", prob = 0.5) %>%
    add_root_edge(id_child = "D", prob = 0.5) %>%

    add_edge(id_parent = "A", id_child = "E", prob = 0.2) %>%
    add_edge(id_parent = "A", id_child = "F", prob = 0.2)

  nodes <- as.data.frame(p)

  expect_s3_class( compute_tree( nodes ), "data.frame")
  expect_no_failure( draw_tree( compute_tree( nodes)) )
  expect_no_failure( draw_tree( compute_tree( p)) )
})
