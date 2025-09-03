test_that("path_nodes tests consitency correctly", {
  ids1 <- c("A", "B", "C")
  text1 <- c("a", "b", "c")
  ids2 <- 1:3

  testthat::expect_no_failure(
    nodes <- path_nodes( ids1, text1 )
  )

  testthat::expect_error(
    nodes <- path_nodes( ids2, text1 )
  )

  testthat::expect_error(
    nodes <- path_nodes( ids2[1:2], text1 )
  )

  testthat::expect_error(
    nodes <- path_nodes( ids1[1:2], text1 )
  )

  testthat::expect_error(
    nodes <- path_nodes( ids1, text1[1:2] )
  )

  testthat::expect_error(
    nodes <- path_nodes( ids2 )
  )

  testthat::expect_error(
    nodes <- path_nodes(ids = NULL, texts = text1)
  )

})
