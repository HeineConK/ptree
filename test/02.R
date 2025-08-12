library(dplyr)

n1 <- path_node(id = "1", text = "A")
class(n1)
n1
n1$id

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

as.data.frame( p )


