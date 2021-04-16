
# PLOT THEME --------------------------------------------------------------

plot_theme <- function(plot) {

    theme_bw() +
    theme(
      # Title
      legend.title = element_blank(),
      # Legend
      legend.text = element_text(size = 9),
      legend.position = "bottom",
      # Caption
      plot.caption = element_text(size = 7, hjust = 0, face = "italic"),
      # X-axis
      axis.ticks.x = element_blank(),
      axis.title.x = element_blank(),
      axis.text.x = element_text(size = 9),
      # Y-axis
      axis.ticks.y = element_blank(),
      axis.title.y = element_text(size = 9),
      # Background
      panel.grid.minor.y = element_line(color = "grey80"),
      panel.grid.major.x = element_blank(),
      panel.background = element_blank(),
      axis.line = element_line(colour = "black"),
      panel.border = element_blank()) +
    guides(color = guide_legend(nrow = 1))

}


# COLOR VECTORS -----------------------------------------------------------
# all colors should be named "colors_[]" so it's easy for the user to find

colors_ses <- c(
  "Low" = "#D1EAE8",
  "Moderate" = "#8CCAC5",
  "Middle" = "#30A097",
  "High" = "#147870")

# these data ARE available to the user
usethis::use_data(colors_ses, overwrite = TRUE)

# these data are NOT available to the user
usethis::use_data(plot_theme, overwrite = TRUE, internal = TRUE)
