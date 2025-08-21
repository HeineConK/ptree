#' @export path_node
path_node <- function(id, text = NULL){
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
  names( p$nodes ) <- sapply( nodes, function(n) n$id )
  class(p) <- "path"
  return( p )
}
