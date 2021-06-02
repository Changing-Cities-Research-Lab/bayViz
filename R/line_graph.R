#' Produce a line graph of a variable.
#'
#' This function takes in data and produces a line graph of "yvar" along the "xvar" axis, categorized by "catvar" and facetted by "facet_label" if specified.
#'
#' @param dat Data with a columns containing variable of interest ("yvar"), x-axis variable ("xvar"), and grouping variable ("catvar").
#' @param colors Colors for "catvar." NULL (default) returns R's automatic coloring.
#' @param facet T to use "facet_label" column to create facetting panels. F (default) does not need "facet_label" column. 
#' @param limits Y-axis limits. NULL (default) returns R's automatic limits.
#' @param y_type Data format of "yvar". Default returns percent to nearest unit value.
#' @param line_type Line types for "catvar." NULL (default) returns R's automatic solid lines.
#' @param title Plot title. NULL (default) returns no title.
#' @param y_title Title to display along y-axis. NULL (default) returns no title.
#' @param caption Caption for figure. NULL (default) returns no caption.
#' @return Line graph of "yvar" over "xvar", categorized by "catvar" and facetted by "facet_label" if specified., formatted per lab style.
#' @export

line_graph <- function(
  dat,
  colors = NULL,
  facet = F,
  limits = NULL,
  y_type = scales::percent_format(accuracy = 1),
  line_type = NULL,
  title = NULL,
  y_title = NULL,
  caption = NULL
) {
  
  # plot variables
  plot =
    ggplot(dat, aes(x = xvar, y = yvar, group = catvar)) +
    geom_line(aes(color = catvar,
                  linetype = catvar))
  
  # adjust colors, if provided
  if (!is.null(colors)) {
    plot =
      plot +
      scale_color_manual(values = colors)
  }
  
  # facetting variable, if provided
  if (facet) {
    plot =
      plot +
      facet_wrap(vars(facet_label), 
                 nrow = 1,
                 scales = "free_y") 
  }
  
  # adjust line types, if provided
  if (!is.null(line_type)) {
    plot =
      plot +
      scale_linetype_manual(values = line_type)
  } else {
    plot =
      plot +
      scale_linetype_manual(values = rep("solid", length(unique(dat$catvar))))
  }
  
  # adjust scales and limits, if provided
  plot =
    plot +
    scale_y_continuous(labels = y_type,
                       limits = limits,
                       expand = c(0, 0)) +
    scale_x_discrete(expand = c(0.03, 0.03))
  
  # create titles and caption, if provided
  plot =
    plot +
    ggtitle(title) +
    labs(y = y_title,
         caption = caption)
  
  # set legend items all on one row
  plot = plot + guides(color = guide_legend(nrow = 1))
  
  # add custom lab theme (in DATASET.R)
  plot = plot + plot_theme()
  
  return(plot)
}
