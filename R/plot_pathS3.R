#' S3 plot function for a given path object
#'
#' @param path Path object to be plotted
#'
#' @export
plot.path <- function(path, ...){
    tree <- compute_tree( path )
    draw_tree( tree, ... )
}
