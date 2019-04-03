#=============================================#
##### Color header of Facet_wrap function #####
#=============================================#
#' @export

changeStrip <- function(gplot, color_swap, color_pallet = NULL){

  ## get the required packages

  require("ggplot2")
  require("grid")

  if(!is.vector(color_swap)){
    stop("color_swap must be a vector of length")
  }
  if(is.null(color_pallet)){
    number_col <- length(unique(color_swap))
    color <- grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]
    color <- unique(sub(color, pattern="[0-9]+", replacement=""))
    col <- sample(x = color, size = number_col)
  } else {
    col <- color_pallet
  }

  col_vect <- c()
  for (i in length(color_swap)) {
    col_vect[i] <- col[col_swap[i]]
  }

  stripped_plot <- ggplot_gtable(ggplot_build(gplot))
  strip_both <- which(grepl('strip-', stripped_plot$layout$name))
  fills <- col_vect
  k <- 1
  for (i in strip_both) {
    j <- which(grepl('rect', stripped_plot$grobs[[i]]$grobs[[1]]$childrenOrder))
    stripped_plot$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
    k <- k+1
  }
  grid.draw(stripped_plot)
}
