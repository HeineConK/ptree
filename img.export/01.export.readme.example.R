library("ptree")
library("dplyr")
library("ggplot2")

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



plt <- plot( p )
plt

ggsave(
  filename = "img.export/tree.svg",
  width = 5,
  height = 5,
  plot = plt,
  device = svg
)
