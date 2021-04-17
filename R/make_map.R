#' Produce a Bay Area map of continuous variable with gradient color scale
#'
#' This function takes in data and produces an HRA map of King County
#' representing the variable using a gradient color scale. HRA
#' column must be named "HRA".
#'
#' @param data Data with merged with census_shp (use create_mapping_data) and variable of interest.
#' @param area The Bay Area region to map (i.e., Bay Area, East Bay, South Bay, North Bay, Oakland, San Jose, San Francisco)
#' @param var Name of column containing variable to plot.
#' @param colors Either a list of colors, or use NULL/"sequential" and "diverging" for preset color palettes.
#' @param title Title of figure
#' @param caption Figure caption
#' @return Map of variable of interest in Bay Area region.
#' @export

# assumes data is filtered to the tracts of interest; area indicates the region to map
make_map <- function(
  data,
  area,
  var,
  colors = NULL,
  title = NULL,
  caption = NULL) {

  library(tidyverse)
  library(sf)
  library(ggmap)

  # check area is known
  if (!area %in% names(gmaps)) {
    return(str_glue("Invalid area. Use one of: ", str_c(names(gmaps), collapse = ", ")))
  }

  # select colors if user does not provide palette
  if(is.null(colors) | isTRUE(colors == "sequential")) {
    colors <- RColorBrewer::brewer.pal(n = 9, name = "Greens")
  } else if (isTRUE(colors == "diverging")) {
    colors <- RColorBrewer::brewer.pal(n = 9, name = "PRGn")
  }

  plot <-
    ggmap(gmaps[[area]]) +
    geom_sf(
      data = data,
      aes(fill = !! sym(var)),
      size = 0,
      alpha = 0.7,
      inherit.aes = FALSE
    )  +
    geom_sf(
      data = data,
      size = 0.3,
      alpha = 0,
      inherit.aes = FALSE,
      color = "black"
    ) +
    #facet_grid(cols = vars(period)) +
    #scale_fill_manual(
    #  breaks = quant_labels,
    #  values = quant_colors,
    #  labels = quant_labels,
    #  na.value = "grey60"
    #) +
    scale_fill_gradientn(colors = colors) +
    guides(
      fill =
        guide_colorbar(
          barheight = 0.5,
          barwidth = 19,
          title = NULL,
          frame.colour = "black"
        )
    ) +
    theme_void() +
    theme(
      legend.title = element_blank(),
      legend.text = element_text(size = 7),
      legend.position = "bottom",
      legend.box.margin = margin(3,0,0,0, unit = "pt"),
      plot.title = element_text(size = 10, hjust = 0),
      plot.margin = margin(3,1,3,1, unit = "pt"),
      plot.caption = element_text(size = 6, hjust = 0),
      panel.border = element_rect(colour = "black", fill=NA)
    ) +
    labs(caption = caption,
         title = title)

  return(plot)
}


# TO-DO INCORPORATE THIS ELSEWHERE!! i.e., breaks + colors + label type
# # Adjust color palette
# if (palette == "sequential") {
#   lim = NULL
#   type = "seq"
#   #MAP_COLORS <- RColorBrewer::brewer.pal(n = 9, name = "BuGn")
#   MAP_COLORS <- RColorBrewer::brewer.pal(n = 9, name = "Greens")
#   palette = "Greens"
#   direction = 1
#
#   # set number of Jenks breaks
#   if (jenksbreaks) {
#     breaks = data %>%
#       dplyr::pull({{var}}) %>%
#       BAMMtools::getJenksBreaks(k = 6)
#   }
#
# } else if (palette == "diverging") {
#
#   # Get limits to center diverging palette around 0
#   lim <- data %>%
#     select({{var}}) %>%
#     abs() %>%
#     max(na.rm = T) * c(-1, 1)
#   type = "div"
#   MAP_COLORS <- rev(RColorBrewer::brewer.pal(n = 9, name = "PRGn"))
#   palette = "PRGn"
#   direction = 1
#
#   # find Jenks breaks for negative and positive values separately, then combine
#
#   }
#
# } else {
#   return("Please select sequential or diverging color palette.")
# }
#
# # Overrides lim value if user inputs limits
# if (!is.null(limits)) {
#   lim = limits
# }











