library(ptree)
library(dplyr)
test_that("printing a path object through print.path flushes num of nodes and edges to console", {

  nl <- list(
    n1_A  = path_node(id = "1A", text = "A"),
    n1_Z  = path_node(id = "1Z", text = "Z"),
    n2_ZA = path_node(id = "2ZA", text = "A"),
    n2_ZZ = path_node(id = "2ZZ", text = "Z"),
    n2_AA = path_node(id = "2AA", text = "A"),
    n2_AZ = path_node(id = "2AZ", text = "Z")
  )


  testthat::expect_no_failure(
    path(  ) %>%
    add_root_edge(node_child = nl$n1_Z,
                  prob = 2/3,
                  prob_str = "2/3") %>%
    add_root_edge(node_child = nl$n1_A,
                  prob = 1/3,
                  prob_str = "1/3")
  )
})
