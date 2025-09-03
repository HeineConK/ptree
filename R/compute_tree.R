col_id_tier <- function(nodes){
  if(!is.data.frame(nodes)){
    if(class(nodes) == "path"){
      nodes <- as.data.frame( nodes )
    }
  }

  if ( !("id" %in% colnames(nodes)) ) {
    nodes$id <- 1:nrow(nodes)
  } else {
    nodes <- nodes[order(nodes$id), ]
  }

  nodes$tier <- compute_tiers(nodes)
  nodes
}

#' A Tree Computer
#'
#' Compute all information (coordinates, angles) needed to draw a tree in a plotting package, e.g. ggplot2
#' @param nodes
#' @keywords x, y, angle, segment, line segment
#' @export
#' @examples
#' compute_tree(nodes, 2, .05, 1.4)

compute_tree <- function(nodes, round = 2, y_nudge = 0, aspect_ratio = 1.2, compute_leaf_probs = TRUE) {

  nodes <- col_id_tier(nodes)

  nodes <- place_nodes(nodes)
  nodes <- place_segments(nodes)

  if(compute_leaf_probs){
    nodes <- compute_leaf_probs( nodes )
  }

  if (("p" %in% colnames(nodes))) {
    nodes <- place_labels(nodes, y_nudge, aspect_ratio)

    if(!is.character(nodes$p) & round) {
      nodes$p <- round(nodes$p, round)
    }
  }

  nodes <- place_leaf_ps(nodes)
  nodes <- flip_vertical(nodes, y_nudge)

  if("leaf_p" %in% colnames(nodes)){
    if(!is.character(nodes$leaf_p) & round){
      nodes$leaf_p <- round(nodes$leaf_p, round)

    }
  }

  return(nodes)
}
