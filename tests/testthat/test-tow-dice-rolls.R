library(ptree)
library(dplyr)
library(testthat)

test_that("path diagram for dice roll outcomes is created", {

  p <- path()

  expect_no_failure(
    for(i in 1:6){
      p <- add_node(p, id = as.character(i), text = i)
      p <- add_root_edge(p, as.character(i), prob = 1 / 6)

      for(j in 1:6){
        id_ij <- paste(i,j,sep = ".")
        # print( id_ij )
        p <- add_node(p, id = id_ij, text = j)
        p <- add_edge(p, id_parent = as.character(i), id_child = id_ij, prob = 1/6)
      }
    }
  )

  expect_no_failure(
    compute_tree( p,round = 3 ) %>% draw_tree()
  )


})
