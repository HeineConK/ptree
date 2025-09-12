#' @export
path_nodes <- function(ids = NULL, texts = NULL){

  # checks
  ids_inull <- is.null( ids )
  texts_inull <- is.null( texts )

  if( ids_inull ){
    stop("No node ids given.")
  }

  if(!ids_inull & !texts_inull ){
    if(length( ids) != length( texts )){
      stop("Node ids and node texts have different lengths.")
    }
  }

  n <- length( ids )
  if( texts_inull ) texts <- rep(NULL, n)

  nodes <- lapply(1:n, function(i){
    id <- ids[ i ]
    txt <- texts[ i ]
    path_node(id = id, text = txt)
  })

  return( nodes )
}


#' @export
add_node <- function(path, node = NULL, id = NULL, text = NULL){
  if(!is.null(node)){
    path$nodes[[as.character(node$id)]] <- node
  }else if( !is.null(id)){
    node <- path_node(id = id, text = text)
    if( node$id %in% names(path$nodes) ){
      stop(paste("Node with id", id, "already in path's node list."))
    }
    path$nodes[[as.character(node$id)]] <- node
  }
  path
}

#' @export add_edge
add_edge <- function( path, id_parent = NULL, id_child = NULL, prob = 0, prob_str = NULL, node_parent = NULL,  node_child = NULL){
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

  child_ids <- sapply(path$edges, function(e) e$node_child$id)
  if(nc$id %in% child_ids) stop(paste("Node with id", nc$id, "has already been added as child."))

  e <- list(
    node_parent = np,
    node_child  = nc,
    edge_id = paste0(np$id, "-", nc$id),
    prob = prob,
    prob_str = prob_str,
    is_root_edge = FALSE
  )
  class(e) <- "edge"
  path$edges[[ e$edge_id ]] <- e

  if ( !np$id %in% sapply(path$nodes, function( n ) n$id) ) path <- add_node( path, np)
  if ( !nc$id %in% sapply(path$nodes, function( n ) n$id) ) path <- add_node( path, nc)

  return( path )
}

#' @export add_root_edge
add_root_edge <- function( path, id_child = NULL, prob = 0, prob_str = NULL, node_child = NULL){

  nc <- if( !is.null(node_child)){
    node_child
  }else if(!is.null(id_child)){
    ids <- sapply(path$nodes, function(n) n$id)
    stopifnot( id_child %in% ids)
    path$nodes[[ id_child ]]
  }else{
    stop("No valid child node object or child node ID provided.")
  }

  child_ids <- sapply(path$edges, function(e) e$node_child$id)
  if(nc$id %in% child_ids) stop(paste("Node with id", nc$id, "has already been added as root child."))


  e <- list(
    node_parent = NULL,
    node_child  = nc,
    edge_id = paste0("-", nc$id),
    prob = prob,
    prob_str = prob_str,
    is_root_edge = TRUE
  )
  class(e) <- "edge"
  path$edges[[ e$edge_id ]] <- e

  if ( !nc$id %in% sapply(path$nodes, function( n ) n$id) ) path <- add_node( path, nc)

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

  edges <- p$edges
  Ne <- length( edges)
  ids_parents  <- sapply(p$edges, function(e) e$node_parent$id)
  ids_children <- sapply(p$edges, function(e) e$node_child$id)
  ids <- sapply(p$nodes, function(n) n$id)
  d <- data.frame(
    text = character( Ne ),
    parent = rep(0, Ne),
    p = numeric( Ne ),
    p_str = character( Ne ),
    node_id = character( Ne )
  )

  for(i in 1:Ne){
    e <- edges[[i]]
    # d$text[ i ] <- ifelse(is.null( e$node_child$text), paste0("_", e$node_child$id, "_"), e$node_child$text)
    d$text[ i ] <- ifelse(is.null( e$node_child$text), e$node_child$id, e$node_child$text)
    d$parent[ i ] <- ifelse( e$is_root_edge, 0, which( e$node_parent$id == ids_children ))
    d$p[ i ] <- e$prob
    d$p_str[ i ] <- ifelse(!is.null(e$prob_str), e$prob_str, NA)
    d$node_id[ i ] <- e$node_child$id
  }

  if(length(setdiff(ids, d$node_id)) > 0){
    ids_miss <- character()
    for(id in ids){
      if(! id %in% d$node_id){
        ids_miss <- c( ids_miss, id )
      }
    }
    warning(cat("Missing root edges? Some node ids are not in the edge list.", "\n",
                "These are:", "\n",
                ids_miss, "\n"))

  }

  d

}
