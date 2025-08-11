library("testthat")
library("dplyr")

test_that("compute_tree and draw_tree are working",{
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

  tree <- compute_tree(nodes)
  expect_s3_class(tree, "data.frame" )
  expect_true(nrow(tree) > 0)

  expect_error( draw_tree(tree), NA)
})

