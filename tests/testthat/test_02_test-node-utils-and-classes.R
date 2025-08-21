library("testthat")
library("dplyr")

test_that("creating a path by adding nodes works",{
  expect_no_failure({
    p <- path()
    p$nodes
    p %>% add_node(id = "A")

    add_node(p, id = "A")

    p <- add_node(p, id = "A")
    class(p)

    print( p )

    p2 <- path() %>%
      add_node(id = "A") %>%
      add_node(id = "B") %>%
      add_node(id = "C")

    p2 <- p2 %>% add_edge(node_parent = p2$nodes$A,
                          node_child = p2$nodes$B)

    p2$edges$`A-B`$node_child
  })
})

test_that("adding nodes with duplicate labels to a path fails",{
  expect_error({
    p <- path()
    p <- add_node(p, id = "A")
    p <- add_node(p, id = "B")
    p <- add_node(p, id = "C")
    p <- add_node(p, id = "A")
  })
})

test_that("adding root edges works",{
  expect_no_failure({
    p <- path()
    p <- add_node(p, id = "A")
    p <- add_node(p, id = "B")
    p <- add_node(p, id = "C")

    p <- add_root_edge(p, node_child = p$nodes$A)
    p <- add_root_edge(p, id_child = "B")

  })
})

test_that("adding a child node for a second time fails",{

    p <- path()
    p <- add_node(p, id = "A")
    p <- add_node(p, id = "B")
    p <- add_node(p, id = "C")

    p <- add_root_edge(p, node_child = p$nodes$A)
    p <- add_root_edge(p, id_child = "B")
    p <- add_root_edge(p, id_child = "C")

    expect_error({ p <- add_edge(p, id_parent = "A", id_child = "C" )})
    expect_error({ p <- add_root_edge(p, id_child = "C" )})

})



test_that("convertion of path to nodes data.frame works",{
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

  expect_s3_class(as.data.frame(p), "data.frame")
})

test_that("nodes missing in edge list are detected when converting path to data frame", {
  p <- path()
  p <- add_node(p, id = "A")
  p <- add_node(p, id = "B")
  p <- add_node(p, id = "C")
  p <- add_node(p, id = "D")
  p <- add_node(p, id = "E", text = "test")

  p <- add_root_edge(p, node_child = p$nodes$A)
  # p <- add_root_edge(p, id_child = "B")
  p <- add_root_edge(p, id_child = "C")

  p <- add_edge(p, id_parent = "A", id_child = "D")
  p <- add_edge(p, id_parent = "D", id_child = "E")

  expect_warning(
    as.data.frame( p )
  )

})
