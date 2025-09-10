#' S3 plot function for a given path object
#'
#' @param path Path object to be plotted
#'
#' @export
plot.path <- function(path, round = 2, y_nudge = 0, aspect_ratio = 1.2, compute_leaf_probs = TRUE, ...){
    tree <- compute_tree( path, round = round, y_nudge = y_nudge, aspect_ratio = aspect_ratio, compute_leaf_probs = compute_leaf_probs)
    draw_tree( tree, ... )
}
