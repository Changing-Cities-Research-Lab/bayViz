#' Produce a line graph of a variable.
#'
#' This function takes in data and produces a line graph of "yvar" along the "xvar" axis, categorized by "catvar".
#'
#' @param dat Data with a columns containing variable of interest ("yvar"), x-axis variable ("xvar"), and grouping variable ("catvar").
#' @param colors Colors for "catvar." NULL (default) returns R's automatic coloring.
#' @param limits Y-axis limits. NULL (default) returns R's automatic limits.
#' @param y_type Data format of "yvar". Default returns percent to nearest unit value.
#' @param title Plot title. NULL (default) returns no title.
#' @param y_title Title to display along y-axis. NULL (default) returns no title.
#' @param caption Caption for figure. NULL (default) returns no caption.
#' @return Line graph of "yvar" over "xvar", categorized by "catvar", formatted per lab style.
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

  # plot variables
  plot =
    ggplot(dat, aes(x = xvar, y = yvar, group = catvar)) +
    geom_line(aes(color = catvar))

  # adjust colors, if provided
  if (!is.null(colors)) {
    plot = plot + scale_color_manual(values = colors)
  }

  # adjust scales and limits, if provided
  plot = plot +
    scale_y_continuous(labels = y_type,
                       limits = limits,
                       expand = c(0, 0)) +
    scale_x_discrete(expand = c(0.03, 0.03))

  # create titles and caption, if provided
  plot =  plot +
    ggtitle(title) +
    labs(y = y_title, caption = caption)


  plot = plot + plot_theme()
    # plot +
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
