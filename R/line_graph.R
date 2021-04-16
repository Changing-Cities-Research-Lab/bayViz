#' Produce a line graph of a variable.
#'
#' This function takes in data and produces a line graph of "yvar" along the "xvar" axis, categorized by "catvar". 
#'
#' @param dat Data with a columns containing variable of interest ("yvar"), x-axis variable ("xvar"), and grouping variable ("catvar").
#' @param var Name of variable to plot.
#' @param limits Y-axis limits
#' @param title Plot title
#' @param y_title Title to display along y-axis.
#' @param save T if user would like to return plot object and save file, F (default) to just return object.
#' @param savename File name of map for saving.
#' @param caption Caption for figure
#' @return Line graph of variable by periods, grouped by race, ethnoracial, income, or gentrification category.
#' @export

line_graph <- function(
  dat,
  colors = NULL,
  limits = NULL,
  y_type = scales::percent_format(accuracy = 1),
  title = NULL,
  y_title = NULL,
  caption = NULL
) {
  
  plot = 
    # plot variables
    ggplot(dat, aes(x = xvar, y = yvar, group = catvar)) +
    geom_line(aes(color = catvar))
  
  # adjust colors, if provided
  if (!is.null(colors)) {
    plot = plot + scale_color_manual(values = colors)
  }
  
  # adjust y-axis scales and limits, if provided
  plot = 
    plot + 
    scale_y_continuous(labels = y_type, 
                       limits = limits, 
                       expand = c(0, 0))
  
  # create titles and caption
  plot = 
    plot +
    ggtitle(title) +
    labs(y = y_title, caption = caption) 
  
  
  plot = plot + plot_theme()
    # plot + 
    # scale_x_discrete(expand = c(0.03, 0.03)) +
    # theme_bw() + 
    # theme(
    #   # Title
    #   legend.title = element_blank(),
    #   # Legend
    #   legend.text = element_text(size = 9),
    #   legend.position = "bottom",
    #   # Caption
    #   plot.caption = element_text(size = 7, hjust = 0, face = "italic"),
    #   # X-axis
    #   axis.ticks.x = element_blank(),
    #   axis.title.x = element_blank(),
    #   axis.text.x = element_text(size = 9),
    #   # Y-axis
    #   axis.ticks.y = element_blank(),
    #   axis.title.y = element_text(size = 9),
    #   # Background
    #   panel.grid.minor.y = element_line(color = "grey80"),
    #   panel.grid.major.x = element_blank(),
    #   panel.background = element_blank(),
    #   axis.line = element_line(colour = "black"),
    #   panel.border = element_blank()) +
    # guides(color = guide_legend(nrow = 1)) 

  return(plot)
}
