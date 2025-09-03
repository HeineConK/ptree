#' A Probability Tree Plotter
#'
#' Draw a probability tree using ggplot2
#' @param nodes
#' @keywords x, y, angle, segment, line segment
#' @export
#' @examples
#' draw_tree(nodes)

draw_tree <- function(nodes, padding = text_padding_default(), leaf_p_fontface = "bold") {

  if ("numeric" %in% class(padding)) {
    padding <- ggplot2::unit(padding, "lines")
  }

  p <- ggplot2::ggplot(nodes) +
    ggplot2::geom_segment( ggplot2::aes(x = x, y = y, xend = xend, yend = yend)) +
    ggplot2::geom_label( ggplot2::aes(x = x, y = y, label = text), label.size = NA, label.padding = padding) +
    ggplot2::theme_void() + ggplot2::ylim(0, 1)

  if ("p" %in% colnames(nodes)) {
    p <- p + ggplot2::geom_text(ggplot2::aes(x = p_x, y = p_y, label = p, angle = p_angle))
  }

  if ("leaf_p" %in% colnames(nodes)) {
    leaves <- nodes[nodes$tier == max(nodes$tier), ]
    leaves$leaf_p[is.na(leaves$leaf_p)] <- ""
    p <- p + ggplot2::geom_text(ggplot2::aes(x = leaf_p_x, y = leaf_p_y, label = leaf_p),
                       data = leaves, fontface = leaf_p_fontface) +
      ggplot2::xlim(0, 1.2)
  } else {
    p <- p + ggplot2::xlim(0, 1)
  }

  return(p)
}

text_padding <- function(x, units = "lines"){
  ggplot2::unit(x, units)
}

text_padding_default <- function(){
  text_padding( x = 0.75, units = "lines" )
}

text_padding_none <- function(){
  text_padding( x = 0.00, units = "lines" )
}
