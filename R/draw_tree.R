#' A Probability Tree Plotter
#'
#' Draw a probability tree using ggplot2
#' @param nodes
#' @keywords x, y, angle, segment, line segment
#' @export
#' @examples
#' draw_tree(nodes)

draw_tree <- function(nodes, padding = text_padding_default(), leaf_p_fontface = "bold", show_leaf_probs = TRUE) {

  if ("numeric" %in% class(padding)) {
    padding <- text_padding(padding)
  }

  # if nodes df is built by hand, like in the readme example, prob strings might be missing
  # Thus add dummy column here
  if (!"p_str" %in% colnames(nodes)){
    nodes$p_str <- NA
  }

  # helper column containing probability labels
  nodes$prob_label_text <- sapply(1:nrow(nodes), function(i){
    if (!is.na(nodes$p_str[ i ])) {
      nodes$p_str[ i ]
    } else {
      nodes$p[ i ]
    }
  })



  p <- ggplot2::ggplot(nodes) +
    ggplot2::geom_segment( ggplot2::aes(x = x, y = y, xend = xend, yend = yend)) +
    ggplot2::geom_label( ggplot2::aes(x = x, y = y, label = text), label.size = NA, label.padding = padding, parse = TRUE) +
    ggplot2::theme_void() + ggplot2::ylim(0, 1)

  # add probabilty labels to edges
  if ("p" %in% colnames(nodes)) {

    # Function to generate a tilted rectangle
    tilted_rect <- function(x, y, width, height, angle) {
      # rectangle corners before rotation (centered at 0,0)
      rect <- data.frame(
        x = c(-width/2, width/2, width/2, -width/2),
        y = c(-height/2, -height/2, height/2, height/2)
      )

      # rotation matrix
      theta <- angle * pi/180
      rot <- matrix(c(cos(theta), -sin(theta), sin(theta), cos(theta)), ncol=2)

      # rotate and translate
      coords <- as.matrix(rect) %*% rot
      rect$x <- coords[,1] + x
      rect$y <- coords[,2] + y

      as.data.frame( rect )
    }

    label_rects <- lapply(1:nrow( nodes ), function(i){
        tilted_rect(
          x = nodes$p_x[ i ],
          y = nodes$p_y[ i ],
          width = nchar( as.character( nodes$p[ i ] ) ) * 0.030,
          height = 0.050,
          angle = nodes$p_angle[ i ]
        )
    })

    for(i in 1:nrow( nodes )){
      reci <- label_rects[[i]]
      p <- p + ggplot2::geom_polygon(data = reci, ggplot2::aes(x,y), fill = "white", color = "black")
    }

    p <- p + ggplot2::geom_text(ggplot2::aes(x = p_x, y = p_y, label = prob_label_text, angle = p_angle))

  }



  if ("leaf_p" %in% colnames(nodes)) {

    if (show_leaf_probs ) {
      leaves <- nodes[nodes$tier == max(nodes$tier), ]
      leaves$leaf_p[is.na(leaves$leaf_p)] <- ""
      p <- p + ggplot2::geom_text(ggplot2::aes(x = leaf_p_x, y = leaf_p_y, label = leaf_p),
                         data = leaves, fontface = leaf_p_fontface) +
        ggplot2::xlim(0, 1.2)
    }
  } else {
    p <- p + ggplot2::xlim(0, 1)
  }

  return(p)
}

#' @export
text_padding <- function(x, units = "lines"){
  ggplot2::unit(x, units)
}

#' @export
text_padding_default <- function(){
  text_padding( x = 0.75, units = "lines" )
}

#' @export
text_padding_none <- function(){
  text_padding( x = 0.00, units = "lines" )
}
