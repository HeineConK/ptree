#' @export path_node
path_node <- function(id, text){
  n <- list(id = id, text = text)
  class(n) <- "path_node"
  return( n )
}

#' @export path
path <- function(nodes = list()){
  p <- list(
    nodes = nodes,
    edges = list()
  )
  class(p) <- "path"
  return( p )
}
