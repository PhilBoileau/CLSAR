#' Grid Arrange, Shared Legend
#'
#' Plot Multiple Graphs with Shared Legend in a Grid
#'
#' @param ...  Plots to be arranged in grid
#' @param ncol Number of columns in grid
#' @param nrow Number of rows in grid
#' @param position Gird position (bottom or right)
#'
#' @import grid gridExtra ggplot2
#' @return Grid of plots with common legend 
#' @export
#'
gridArrangeSharedLegend <- function(..., ncol = length(list(...)), nrow = 1, 
                                       position = c("bottom", "right")) {
  
  plots <- list(...)
  position <- match.arg(position)
  g <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  gl <- lapply(plots, function(x) x + theme(legend.position="none"))
  gl <- c(gl, ncol = ncol, nrow = nrow)
  
  combined <- switch(position,
                     "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
                                            legend,
                                            ncol = 1,
                                            heights = unit.c(unit(1, "npc") - lheight, lheight)),
                     "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                           legend,
                                           ncol = 2,
                                           widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
  
  grid.newpage()
  grid.draw(combined)
  
  # return gtable invisibly
  invisible(combined)
  
}
