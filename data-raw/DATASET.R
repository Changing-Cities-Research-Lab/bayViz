
library(tidyverse)
library(ggmap)
currentdir <- dirname(rstudioapi::getSourceEditorContext()$path)

# SET UP MAP DATA ---------------------------------------------------------

# Define a function to fix the bbox to be in EPSG:3857
ggmap_bbox <- function(map) {
  if (!inherits(map, "ggmap")) stop("map must be a ggmap object")
  # Extract the bounding box (in lat/lon) from the ggmap to a numeric vector,
  # and set the names to what sf::st_bbox expects:
  map_bbox <- stats::setNames(unlist(attr(map, "bb")),
                       c("ymin", "xmin", "ymax", "xmax"))

  # Coonvert the bbox to an sf polygon, transform it to 3857,
  # and convert back to a bbox (convoluted, but it works)
  bbox_3857 <- sf::st_bbox(sf::st_transform(sf::st_as_sfc(sf::st_bbox(map_bbox, crs = 4326)), 3857))

  # Overwrite the bbox of the ggmap object with the transformed coordinates
  attr(map, "bb")$ll.lat <- bbox_3857["ymin"]
  attr(map, "bb")$ll.lon <- bbox_3857["xmin"]
  attr(map, "bb")$ur.lat <- bbox_3857["ymax"]
  attr(map, "bb")$ur.lon <- bbox_3857["xmax"]
  map
}

# Register Google API Key
# In order to use the geocoding features of `ggmap`, it is necessary to first
# register the Google API key. This only has to be done once.
ggmap::register_google("AIzaSyCO-hk4AjUgTdMKDuv18f66py8NIdrf4qU")

gmaps <- list()

## Entire Bay Area
gmaps[["Bay Area"]] <-
  ggmap::get_map(
    location = c(-123.539, 36.875, -121.3, 38.9),
    maptype = "roadmap",
    source = "google",
    color = "bw"
  ) %>%
  ggmap_bbox()

## San Francisco County
gmaps[["San Francisco"]] <-
  ggmap::get_map(
    location = c(-122.5559, 37.6876, -122.3331, 37.8448),
    maptype = "roadmap", source = "google", color = "bw"
  ) %>%
  ggmap_bbox()

## East Bay
gmaps[["East Bay"]] <-
  ggmap::get_map(
    location = c(-122.5992, 37.3898, -121.3797, 38.2404),
    maptype = "roadmap", source = "google", color = "bw"
  ) %>%
  ggmap_bbox()

## Oakland City
gmaps[["Oakland"]] <-
  ggmap::get_map(
    location = c(-122.3547, 37.6920, -122.1048, 37.8607),
    maptype = "roadmap", source = "google", color = "bw"
  ) %>%
  ggmap_bbox()

## South Bay: Santa Clara and San Mateo County
gmaps[["South Bay"]] <-
  ggmap::get_map(
    location = c(-122.5580, 37.0749, -121.5857, 37.7957),
    maptype = "roadmap", source = "google", color = "bw"
  ) %>%
  ggmap_bbox()

## San Jose City
gmaps[["San Jose"]] <-
  ggmap::get_map(
    location = c(-122.1027, 37.1138, -121.6276, 37.4748),
    maptype = "roadmap", source = "google", color = "bw"
  ) %>%
  ggmap_bbox()

## Northern Counties
gmaps[["North Bay"]] <-
  ggmap::get_map(
    location = c(-123.539, 37.614, -121.586, 39.045),
    maptype = "roadmap", source = "google", color = "bw" #watercolor
  ) %>%
  ggmap_bbox()

### Get shapefiles of each region ### ---

# City tracts
city_tracts <- readr::read_csv(paste0(currentdir, "/bayarea_majorcity_tracts_2010b.csv"))

# import dataset of tracts to keep for mapping (with sufficient population)
tracts_use <- readr::read_csv(paste0(currentdir, "/tracts_bayarea_min_pop_hu.csv"))

# # TO-DO: DO WE WANT TO SWITCH TO GET_STAMENMAP?
# data = kingcounty_hras %>%
#   right_join(data, by = c("HRA2010v2_" = "HRA")) %>%
#   st_transform(CRS("+proj=longlat +datum=WGS84"))
#
# # map data
# # Google Street Map for King County ----
# gmap <- get_stamenmap(
#   bbox = c(-122.65219845641234, 47.05811462511336, -121.05368763130899, 47.81607270131313),
#   # ^ is all king county
#   # c(-122.45262191072183, 47.48734893641715, -122.22946210910732, 47.73869829627044) # seattle
#   zoom = 10, # use 12 for seattle
#   maptype = "toner-lite",
#   color = "bw")

## TO-DO: TURN THIS INTO ITS OWN FUNCTION?
# join shapefiles with dataset to map (w/ column "tractid10"); split by BA regions
create_mapping_data <- function(dat, census_shp) {

  mapping_data <- list()

  mapping_data[["Bay Area"]] <-
    census_shp %>%
    filter(GEOID10S %in% tracts_use$trtid10)  %>%
    left_join(dat, by = c("GEOID10S" = "tractid10")) %>%
    sf::st_transform(3857)

  mapping_data[["San Francisco"]] <-
    mapping_data[["Bay Area"]] %>%
    filter(GEOID10S %in%
             (city_tracts %>% filter(city %in% "San Francisco") %>% pull(tractid10))
    )

  mapping_data[["East Bay"]] <-
    mapping_data[["Bay Area"]] %>%
    filter(GEOID10S %in%
             (tracts_use %>%
                filter(county %in% c("Alameda County", "Contra Costa County")) %>%
                pull(trtid10))
    )

  mapping_data[["South Bay"]] <-
    mapping_data[["Bay Area"]] %>%
    filter(GEOID10S %in%
             (tracts_use %>%
                filter(county %in% c("Santa Clara County", "San Mateo County")) %>%
                pull(trtid10))
    )

  mapping_data[["North Bay"]] <-
    mapping_data[["Bay Area"]] %>%
    filter(GEOID10S %in%
             (tracts_use %>%
                filter(county %in% c("Marin County", "Napa County", "Solano County", "Sonoma County")) %>%
                pull(trtid10))
    )

  mapping_data[["Oakland"]] <-
    mapping_data[["Bay Area"]] %>%
    filter(GEOID10S %in%
             (city_tracts %>% filter(city %in% "Oakland") %>% pull(tractid10))
    )

  mapping_data[["San Jose"]] <-
    mapping_data[["Bay Area"]] %>%
    filter(GEOID10S %in%
             (city_tracts %>% filter(city %in% "San Jose") %>% pull(tractid10))
    )

  return(mapping_data)
}

# PLOT THEME --------------------------------------------------------------

plot_theme <- function() {

    theme_bw() +
    theme(
      # Title
      legend.title = element_blank(),
      # Legend
      legend.text = element_text(size = 9),
      legend.position = "bottom",
      # Caption
      plot.caption = element_text(size = 7, hjust = 0.5, face = "italic"),
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
      panel.border = element_blank())

}


# COLOR VECTORS -----------------------------------------------------------
# all colors should be named "colors_[]" so it's easy for the user to find

### ADD SOME SAMPLE COLOR SETS, E.G. SEQUENTIAL, DIVERGING ###

colors_ses <- c(
  "Low" = "#D1EAE8",
  "Moderate" = "#8CCAC5",
  "Middle" = "#30A097",
  "High" = "#147870")

# these data ARE available to the user
usethis::use_data(colors_ses,
                  create_mapping_data,
                  tracts_use,
                  city_tracts,
                  overwrite = TRUE)

# these data are NOT available to the user
usethis::use_data(plot_theme,
                  gmaps,
                  overwrite = TRUE, internal = TRUE)
