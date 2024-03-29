#' Produce map panel of continuous variable with gradient or Jenks breaks color scale across different groups.
#'
#' This function takes in data and produces a panel of census tract maps
#' representing the variable using a gradient or discrete color scale.
#' Should have "tractid10" column for census tracts and "group" column
#' containing up to 6 distinct values. If "group" column is geographic regions,
#' then "region" parameter can be ignored.
#'
#' @param data Data with a "tractid10" column containing census tracts, "group" column containing panel titles; and variable of interest.
#' @param var Name of column containing variable to plot.
#' @param region If "group" column is not geographic region names, supply name of region to map for entire panel: "Bay Area", "San Francisco", "Oakland", "San Jose", "South Bay", "North Bay", or "East Bay"
#' @param shp_tracts "US_tract_2010.shp" loaded object
#' @param palette Color palette: "sequential" (default) or "diverging"
#' @param div_colors Specific color scheme for diverging palette. Defaults to "RdBu"
#' @param jenksbreaks Uses Jenks Breaks when T, otherwise uses continuous color scale
#' @param manualbreaks Numeric vector of breaks for a discrete color scale. Defaults to continuous scale (or uses jenks breaks if true)
#' @param neg_bins For Jenks breaks, number of negative color bins. Default is 3.
#' @param pos_bins For Jenks breaks, number of positive color bins. Default is 3.
#' @param breaks Gradient scale breaks, either numeric vector or scales::extended_breaks(n = 6)
#' @param colorscale Format of color key, either "colorbar" or "legend". Default is "colorbar".
#' @param labels Gradient scale labels, either character vector or scales::percent or scales::comma
#' @param limits Gradient scale limits, c(min, max)
#' @param coord T if plotting coordinate values (data frame must contain "lat", "lon" columns). Default is F.
#' @param save T if user would like to return plot object and save file, F (default) to just return object.
#' @param savename File name of map for saving.
#' @param caption Figure caption
#' @return Map panel of variable of interest across different groups.
#' @export

make_map_panel <- function(
  data,
  var,
  region = NULL,
  shp_tracts,
  palette = "sequential",
  div_colors = "RdPu",
  jenksbreaks = F,
  manualbreaks = NULL,
  neg_bins = 3,
  pos_bins = 3,
  breaks = scales::extended_breaks(n = 6),
  labels = scales::percent,
  colorscale = "colorbar",
  limits = NULL,
  coord = F,
  save = F,
  savename = "plot.png",
  caption = paste0(frb_caption, ses_caption, period_caption)
) {

  library(sf)
  library(rgdal)
  library(foreach)
  library(ggmap)
  library(tidyverse)
  library(gridExtra)
  library(grid)
  library(BAMMtools)

  # If supplied, check if region parameter is acceptable
  if (!is.null(region)) {
    if (!region %in% c("San Francisco", "Oakland", "San Jose", "South Bay", "North Bay", "East Bay")) {
      return("Please provide an acceptable region: San Francisco, Oakland, San Jose, South Bay, North Bay, East Bay.")
    }
  }

  # Adjust color palette
  if (palette == "sequential") {

    # Get max and min values for common gradient scale
    max = data %>%
      st_drop_geometry() %>%
      select({{var}})%>%
      max(na.rm = T)

    min = data %>%
      st_drop_geometry() %>%
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
    MAP_COLORS <- rev(RColorBrewer::brewer.pal(n = 9, name = div_colors))
    type = "div"
    palette = div_colors
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

      # Removes duplicates in breaks
      breaks = unique(c(neg_breaks, pos_breaks))

      # Reset neg bins and pos bins to match actual breaks
      neg_bins = length(breaks[which(breaks < 0)])
      pos_bins = length(breaks[which(breaks > 0)])
    }

  } else {
    return("Please select sequential or diverging color palette.")
  }

  # Overrides lim value if user inputs limits
  if (!is.null(limits)) {
    range = limits
  }

  maps_all = list()
  panels = unique(data$group)

  # Set base map for plotting legend (region doesn't matter)
  base_map = gmaps[["San Francisco"]]

  # Set region if parameter is supplied
  if (!is.null(region)) {
    base_map = gmaps[[region]]
  }

  # Get common legend ----
  legend_map <-
    ggmap(base_map) +
    geom_sf(
      data = data,
      aes(fill = {{var}}),
      size = 0,
      alpha = 0.5,
      inherit.aes = FALSE
    ) +
    guides(
      fill =
        guide_colorbar(
          barheight = 0.8,
          barwidth = 27,
          title = NULL,
          frame.colour = "black"
        )
    ) +
    theme(
      legend.text = element_text(size = 10),
      legend.title = element_blank(),
      legend.position = "bottom",
      legend.box.margin = margin(3,0,0,0, unit = "pt"),
      panel.border = element_rect(colour = "black", fill=NA)
    )

  # discrete color bar
  if (jenksbreaks) {

    # set colors manually if different number of negative and positive bins
    if (neg_bins != pos_bins) {
      # Custom palettes ***MUST SET CUSTOM COLORS

      # 1 negative, 3 positive bins:
      if (neg_bins == 1 & pos_bins == 3) {
        pal <- c("#67a9cf", "#fddbc7", "#ef8a62", "#b2182b")
        # 2 negative, 3 positive bins:
      } else if (neg_bins == 2 & pos_bins == 3) {
        pal <- c("#2166ac", "#67a9cf", "#fddbc7", "#ef8a62", "#b2182b")
      }

      scale_fill_fermenter_custom <- function(pal,
                                              na.value = "grey60",
                                              breaks,
                                              labels) {
        binned_scale(aesthetics = "fill",
                     scale_name = "fermenter",
                     palette = ggplot2:::binned_pal(scales::manual_pal(pal)),
                     na.value = na.value,
                     breaks = breaks,
                     labels = labels)
      }
      legend_map = legend_map +
        scale_fill_fermenter_custom(pal,
                                    breaks = breaks,
                                    labels = labels)

    } else {
      legend_map = legend_map +
        scale_fill_fermenter(breaks = breaks,
                             type = type,
                             palette = palette,
                             direction = direction,
                             labels = labels)
    }



  } else if (length(manualbreaks >= 1)) {
    # Add max value to user provided manual breaks
    max = data %>%
      st_drop_geometry() %>%
      dplyr::pull({{var}}) %>%
      max

    manualbreaks = c(manualbreaks, max)

    legend_map = legend_map +
      scale_fill_fermenter(breaks = manualbreaks,
                           palette = div_colors,
                           direction = direction,
                           labels = labels)

    # gradient color scale
  } else {
    legend_map = legend_map +
      scale_fill_gradientn(breaks = breaks,
                           labels = labels,
                           colors = alpha(MAP_COLORS, .8),
                           limits = range)
  }

  if (colorscale == "legend") {
    legend_map = legend_map +
      guides(fill = guide_legend())
  }

  # Save legend object
  tmp <- ggplot_gtable(ggplot_build(legend_map))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]

  # Plot maps ----
  foreach(i = 1:length(panels)) %do% {
    data_panel = data %>%
      dplyr::filter(group == panels[i]) %>%
      mutate(var = {{var}})

    background_map = NULL

    # If region parameter is supplied, use this as background map for entire panel
    # Otherwise, use "group" column containing geographic regions to select background map.
    if (!is.null(region)) {
      background_map = gmaps[[region]]

    } else {
      # Street Maps for Each Region
      if (panels[i] == "San Francisco") {
        background_map = gmaps[["San Francisco"]]

      } else if (panels[i] == "East Bay") {
        background_map = gmaps[["East Bay"]]

      } else if (panels[i] == "North Bay") {
        background_map = gmaps[["North Bay"]]

      } else if (panels[i] == "South Bay") {
        background_map = gmaps[["South Bay"]]

      } else if (panels[i] == "Oakland") {
        background_map = gmaps[["Oakland"]]

      } else if (panels[i] == "San Jose") {
        background_map = gmaps[["San Jose"]]
      }
    }

    ## Discrete color scale ----
    if (jenksbreaks) {
      # Force each jenks break point into the data, assigned to a non-Oakland tract
      # This prevents color palette from being used sequentially when certain breaks aren't present

      # Create column of breaks
      var_null = breaks

      # Get tractid data for tracts not in specified geography
      non_geo_tracts <-
        shp_tracts %>%
        filter(!GEOID10S %in% data$trtid10) %>%
        mutate(tractid10 = GEOID10S) %>%
        select(tractid10) %>%
        st_drop_geometry()

      # Get tracts not in specified geography
      tractid10 = non_geo_tracts[1:length(var_null),]

      # Create data frame
      df = data.frame(tractid10, var_null) %>%
        mutate(var = var_null) %>%
        select(tractid10, var)

      # Get geometry data
      df = shp_tracts %>%
        right_join(df, by = c("GEOID10S" = "tractid10")) %>%
        st_transform(CRS("+proj=longlat +datum=WGS84"))

      # Combine with original data frame
      data_panel = bind_rows(data_panel, df)

      map <-
        ggmap(background_map) +
        geom_sf(
          data = data_panel,
          aes(fill = var),
          size = 0,
          alpha = 0.7,
          inherit.aes = FALSE
        ) +
        geom_sf(
          data = data_panel,
          size = 0.3,
          alpha = 0,
          inherit.aes = FALSE,
          color = "black"
        ) +
        guides(
          fill =
            guide_colorbar(
              barheight = 0.5,
              barwidth = 15,
              title = NULL,
              frame.colour = "black"
            )
        ) +
        theme_void() +
        theme(
          legend.title = element_blank(),
          legend.position = "none",
          plot.title = element_text(size = 12, hjust = .5, vjust = 3),
          plot.margin = margin(3,-.5,3,-.5, unit = "pt"),
          plot.caption = element_text(size = 8),
          panel.border = element_rect(colour = "black", fill=NA)
        ) +
        labs(title = panels[i])

      # set colors manually if different number of negative and positive bins
      if (neg_bins != pos_bins) {
        # Custom palettes ***MUST SET CUSTOM COLORS

        # 1 negative, 3 positive bins:
        if (neg_bins == 1 & pos_bins == 3) {
          pal <- c("#67a9cf", "#fddbc7", "#ef8a62", "#b2182b")
          # 2 negative, 3 positive bins:
        } else if (neg_bins == 2 & pos_bins == 3) {
          pal <- c("#2166ac", "#67a9cf", "#fddbc7", "#ef8a62", "#b2182b")
        }

        scale_fill_fermenter_custom <- function(pal,
                                                na.value = "grey60",
                                                breaks,
                                                labels) {
          binned_scale(aesthetics = "fill",
                       scale_name = "fermenter",
                       palette = ggplot2:::binned_pal(scales::manual_pal(pal)),
                       na.value = na.value,
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

    } else if (length(manualbreaks) >= 1) {
      # Discrete color scale with breaks provided by user
      var_null = manualbreaks

      # Get tractid data for tracts not in specified geography
      non_geo_tracts <-
        shp_tracts %>%
        filter(!GEOID10S %in% data$trtid10) %>%
        mutate(tractid10 = GEOID10S) %>%
        select(tractid10) %>%
        st_drop_geometry()

      # Get tracts not in specified geography
      tractid10 = non_geo_tracts[1:length(var_null),]

      # Create data frame
      df = data.frame(tractid10, var_null) %>%
        mutate(var = var_null) %>%
        select(tractid10, var)

      # Get geometry data
      df = shp_tracts %>%
        right_join(df, by = c("GEOID10S" = "tractid10")) %>%
        st_transform(CRS("+proj=longlat +datum=WGS84"))

      # Combine with original data frame
      data_panel = bind_rows(data_panel, df)

      map <-
        ggmap(background_map) +
        geom_sf(
          data = data_panel,
          aes(fill = var),
          size = 0,
          alpha = 0.7,
          inherit.aes = FALSE
        ) +
        geom_sf(
          data = data_panel,
          size = 0.3,
          alpha = 0,
          inherit.aes = FALSE,
          color = "black"
        ) +
        guides(
          fill =
            guide_colorbar(
              barheight = 0.5,
              barwidth = 23,
              title = NULL,
              frame.colour = "black"
            )
        ) +
        theme_void() +
        theme(
          legend.title = element_blank(),
          legend.position = "none",
          plot.title = element_text(size = 12, hjust = .5, vjust = 3),
          plot.margin = margin(3,-.5,3,-.5, unit = "pt"),
          plot.caption = element_text(size = 8),
          panel.border = element_rect(colour = "black", fill=NA)
        ) +
        labs(title = panels[i]) +

        scale_fill_fermenter(breaks = manualbreaks,
                             palette = div_colors,
                             direction = direction,
                             labels = labels)

    }
    else {
      # Gradient color scale ----
      map <-
        ggmap(background_map) +
        geom_sf(
          data = data_panel,
          aes(fill = var),
          size = 0,
          alpha = 0.7,
          inherit.aes = FALSE
        ) +
        geom_sf(
          data = data_panel,
          size = 0.3,
          alpha = 0,
          inherit.aes = FALSE,
          color = "black"
        ) +
        guides(
          fill =
            guide_colorbar(
              barheight = 0.5,
              barwidth = 15,
              title = NULL,
              frame.colour = "black"
            )
        ) +
        theme_void() +
        theme(
          legend.title = element_blank(),
          legend.position = "none",
          plot.title = element_text(size = 12, hjust = .5, vjust = 3),
          plot.margin = margin(3,-.5,3,-.5, unit = "pt"),
          plot.caption = element_text(size = 8),
          panel.border = element_rect(colour = "black", fill=NA)
        ) +
        labs(title = panels[i])

      map = map + scale_fill_gradientn(breaks = breaks,
                                       labels = labels,
                                       colors = alpha(MAP_COLORS, .8),
                                       limits = range,
                                       na.value = "grey60")
    }

    # plot coordinate points
    if (coord == T) {
      + geom_point(
        data = data,
        aes(x = lon, y = lat),
        color = "navy", size = 2
      )
    }

    # add map to list of grobs
    maps_all = c(maps_all, list(map))
  }

  ## Arrange maps into panels ----
  map_number <- length(panels)

  map_panel = NULL
  width = 9
  height = 6

  if(map_number == 6) {
    layout <- rbind(c(1, 2, 3), c(4, 5, 6), c(7, 7, 7))
    map_panel =
      grid.arrange(maps_all[[1]], maps_all[[2]], maps_all[[3]],
                   maps_all[[4]], maps_all[[5]], maps_all[[6]],
                   legend,
                   nrow = 3, ncol = 3,
                   layout_matrix = layout,
                   heights = c(5, 5, 1.2),
                   bottom=textGrob(caption, gp=gpar(fontsize=9,font=3)))
    width = 9
    height = 6
  }

  if(map_number == 4) {
    layout <- rbind(c(1, 2), c(3, 4), c(5, 5))
    map_panel =
      grid.arrange(maps_all[[1]], maps_all[[2]], maps_all[[3]], maps_all[[4]],
                   legend,
                   nrow = 1 + ceiling(map_number/2), ncol = 2,
                   layout_matrix = layout,
                   heights = c(rep(5, ceiling(map_number/2)), 1.2),
                   bottom=textGrob(caption, gp=gpar(fontsize=9,font=3)))
    width = 7
    height = 7.8
  }

  if(map_number == 3) {
    layout <- rbind(c(1, 2, 3), c(4, 4, 4))
    map_panel =
      grid.arrange(maps_all[[1]], maps_all[[2]], maps_all[[3]],
                   legend,
                   nrow = 2, ncol = 3,
                   layout_matrix = layout,
                   heights = c(5.3, 1),
                   bottom=textGrob(caption, gp=gpar(fontsize=9,font=3)))
    width = 9.5
    height = 4.3
  }

  if(map_number == 2) {
    layout <- rbind(c(1, 2), c(3, 3))
    map_panel =
      grid.arrange(maps_all[[1]], maps_all[[2]],
                   legend,
                   nrow = 2, ncol = 2,
                   layout_matrix = layout,
                   heights = c(5.2, 1),
                   bottom=textGrob(caption, gp=gpar(fontsize=9,font=3)))
    width = 7
    height = 4.4
  }


  if (save) {
    ggsave(savename, map_panel, height = height, width = width)
    return(map_panel)
  } else {
    return(map_panel)
  }
}
