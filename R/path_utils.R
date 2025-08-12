#' @export
add_node <- function(path, node = NULL, id = NULL, text = NULL){
  if(!is.null(node)){
    path$nodes[[as.character(node$id)]] <- node
  }else if( !is.null(id)){
    node <- path_node(id = id, text = text)
    path$nodes[[as.character(node$id)]] <- node
  }
  path
}

#' @export add_edge
add_edge <- function( path, node_parent = NULL, id_parent = NULL, node_child = NULL, id_child = NULL, prob = 1){
  np <- if( !is.null(node_parent)){
    node_parent
  }else if(!is.null(id_parent)){
    stopifnot( id_parent %in% names(path$nodes))
    path$nodes[[ id_parent ]]
  }else{
    stop("No valid parent node object or parent node ID provided.")
  }

  nc <- if( !is.null(node_child)){
    node_child
  }else if(!is.null(id_child)){
    stopifnot( id_child %in% names(path$nodes))
    path$nodes[[ id_child ]]
  }else{
    stop("No valid child node object or child node ID provided.")
  }

  e <- list(
    node_parent = np,
    node_child  = nc,
    edge_id = paste0(np$id, "-", nc$id),
    prob = prob
  )
  class(e) <- "edge"
  path$edges[[ e$edge_id ]] <- e
  return( path )
}

#' @export
print.path <- function(p, ...){
  cat("Path object with", "\n")
  cat(length(p$nodes), " nodes", "\n")
  cat(length(p$edges), " edges", "\n")
}

#' @export
as.data.frame.path <- function(p, ...){
  return( iris ) # this is a simple test
}
