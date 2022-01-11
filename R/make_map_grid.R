#' Produce a grid of maps based on two categorical variables
#'
#' This function takes in data and produces a map grid based on two categorical variables.
#'
#' @param data Data with a column containing tractid10, two categorical variables, and variable of interest.
#' @param var Name of column containing variable to plot.
#' @param xvar Name of column containing categorical variable to plot along x-axis.
#' @param yvar Name of column containing categorical variable to plot along y-axis.
#' @param shp_tracts "US_tract_2010.shp" loaded object
#' @param palette Color palette: "sequential" (default) or "diverging"
#' @param jenksbreaks Uses Jenks Breaks when T, otherwise uses continuous color scale
#' @param neg_bins For Jenks breaks, number of negative color bins. Default is 3.
#' @param pos_bins For Jenks breaks, number of positive color bins. Default is 3.
#' @param breaks Gradient scale breaks, either numeric vector or scales::extended_breaks(n = 6)
#' @param labels Gradient scale labels, either character vector or scales::percent or scales::comma
#' @param limits Gradient scale limits, c(min, max)
#' @param save T if user would like to return plot object and save file, F (default) to just return object.
#' @param savename File name of map for saving.
#' @param caption Figure caption
#' @param width Width of figure
#' @param height Height of figure
#' @return A grid of maps based on two categorical variables
#' @export
make_map_grid <- function(
  data,
  var,
  xvar,
  yvar,
  shp_tracts,
  palette = "sequential",
  jenksbreaks = T,
  neg_bins = 3,
  pos_bins = 3,
  breaks = scales::extended_breaks(n = 6),
  labels = scales::comma,
  limits = NULL,
  save = F,
  savename = "plot.png",
  caption = paste0(frb_caption, ses_caption, period_caption),
  width = 6.33,
  height = 4.97
) {
  
  library(sf)
  library(rgdal)
  library(foreach)
  library(ggmap)
  library(tidyverse)
  library(gridExtra)
  library(grid)
  library(BAMMtools)
  
  
  # Adjust color palette
  if (palette == "sequential") {
    
    # Get max and min values for common gradient scale
    max = data %>%
      select({{var}}) %>%
      max(na.rm = T)
    
    min = data %>%
      select({{var}}) %>%
      min(na.rm = T)
    
    range = c(min, max)
    
    MAP_COLORS <- RColorBrewer::brewer.pal(n = 9, name = "YlOrRd")
    type = "seq"
    palette = "YlOrRd"
    direction = 1
    
    # set number of Jenks breaks
    if (jenksbreaks) {
      breaks = data %>%
        dplyr::pull({{var}}) %>%
        BAMMtools::getJenksBreaks(k = 6)
    }
    
  } else if (palette == "diverging") {
    
    # Get limits to center diverging palette around 0
    range <- data %>%
      select({{var}}) %>%
      abs() %>%
      max(na.rm = T) * c(-1, 1)
    
    # Diverging palette
    MAP_COLORS <- rev(RColorBrewer::brewer.pal(n = 9, name = "RdBu"))
    type = "div"
    palette = "RdBu"
    direction = -1
    
    # find Jenks breaks for negative and positive values separately, then combine
    if (jenksbreaks) {
      values = data %>%
        dplyr::pull({{var}})
      
      # add 0 value to range of values to split negative and positive values
      values = c(0, values)
      
      neg_values = values[which(values <= 0)]
      pos_values = values[which(values >= 0)]
      
      neg_breaks = neg_values %>%
        getJenksBreaks(k = neg_bins + 1)
      pos_breaks = pos_values %>%
        getJenksBreaks(k = pos_bins + 1)
      
      # Removes duplicate 0's in breaks
      breaks = unique(c(neg_breaks, pos_breaks))
    }
    
  } else {
    return("Please select sequential or diverging color palette.")
  }
  
  # Overrides lim value if user inputs limits
  if (!is.null(limits)) {
    range = limits
  }
  
  # Combine with shapefiles
  
  # county tract map
  oak_tracts <-
    shp_tracts %>%
    filter(GEOID10S %in% oak_ids$trtid10)
  
  data = oak_tracts %>%
    right_join(data, by = c("GEOID10S" = "tractid10")) %>%
    st_transform(CRS("+proj=longlat +datum=WGS84"))
  
  # map data
  # Google Street Map for Oakland ----
  gmap_oak <- get_stamenmap(
    bbox = c(-122.3547, 37.6920, -122.1048, 37.890692),
    zoom = 12,
    maptype = "toner-lite",
    color = "bw")
  
  # produce map
  map <-
    ggmap(gmap_oak) +
    geom_sf(
      data = data,
      aes(fill = {{var}}),
      size = 0,
      alpha = 0.7,
      inherit.aes = FALSE
    ) +
    geom_sf(
      data = data,
      size = 0.1,
      alpha = 0,
      inherit.aes = FALSE,
      color = "black"
    ) +
    guides(
      fill =
        guide_colorbar(
          barheight = 0.5,
          barwidth = 17,
          title = NULL,
          frame.colour = "black")
    ) +
    facet_grid(rows = vars({{yvar}}),
               cols = vars({{xvar}}),
               switch = "y") +
    theme_void() +
    theme(
      legend.title = element_blank(),
      legend.position = "bottom",
      legend.text = element_text(size = 7),
      plot.title = element_text(size = 12, hjust = .5, vjust = 3),
      plot.margin = margin(3,-.5,3,-.5, unit = "pt"),
      plot.caption = element_text(size = 6, hjust = .5, face = "italic"),
      panel.border = element_rect(colour = "black", fill=NA),
      strip.text.y.left = element_text(angle = 0)
    ) +
    labs(caption = caption)
  
  # discrete color bar
  if (jenksbreaks) {
    
    # set colors manually if different number of negative and positive bins
    if (neg_bins != pos_bins) {
      # Custom palette
      # 1 negative, 3 positive bins:
      pal <- c("#67a9cf", "#fddbc7", "#ef8a62", "#b2182b")
      
      scale_fill_fermenter_custom <- function(pal,
                                              breaks,
                                              labels) {
        binned_scale("fill",
                     "fermenter",
                     ggplot2:::binned_pal(scales::manual_pal(unname(pal))),
                     breaks = breaks,
                     labels = labels)
      }
      map = map + scale_fill_fermenter_custom(pal,
                                              breaks = breaks,
                                              labels = labels)
    } else {
      map = map + scale_fill_fermenter(breaks = breaks,
                                       type = type,
                                       palette = palette,
                                       direction = direction,
                                       labels = labels)
    }
    
    # gradient color scale
  } else {
    map = map + scale_fill_gradientn(breaks = breaks,
                                     labels = labels,
                                     colors = alpha(MAP_COLORS, .8),
                                     limits = range,
                                     na.value = "grey60")
  }
  
  if (save) {
    ggsave(savename, map, height = height, width = width)
    return(map)
  } else {
    return(map)
  }
  return(data)
}
