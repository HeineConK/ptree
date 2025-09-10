library(dplyr)
library(testthat)

test_that("edge probs can be overwritten by char values", {
  n_s1 <- path_node(id = "s1", text = "Pik Ass")
  n_n1 <- path_node(id = "n1", text = "kein Pik Ass")


  path1 <- path() %>%
    add_root_edge(node_child = n_s1, prob =  1/52, prob_str = "1/52") %>%
    add_root_edge(node_child = n_n1, prob = 51/52, prob_str = "51/52")


  expect_s3_class(path1, "path")

  expect_no_failure( compute_tree( path1 ) )
  expect_no_failure( compute_tree( path1 ) %>% draw_tree( path1 ) )
  expect_no_failure( plot( path1 ) )

})
