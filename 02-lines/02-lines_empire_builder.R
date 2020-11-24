library(tidyverse)
library(sf)
library(tigris)

amtrak_lines <- read_sf("https://opendata.arcgis.com/datasets/d04fab6590c04feaa5f3955ef5abe929_0.geojson") %>%
  janitor::clean_names() %>%
  st_transform("+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs ")

amtrak_stations <- read_sf("https://opendata.arcgis.com/datasets/3e9daf681b154fb19372044f4d52941a_0.geojson") %>%
  janitor::clean_names() %>%
  st_transform("+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs ")


eb_line <- amtrak_lines %>%
  filter(name == "Empire Builder")

states <- states(cb = TRUE) %>%
  janitor::clean_names() %>%
  st_transform("+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs ")

# from https://www.sciencebase.gov/catalog/item/581d051de4b08da350d523c3
contours <- read_sf("02-lines/us_contours/us_contours.shp") %>%
  st_transform("+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs ")

# create filter area around amtrak line

eb_line_area <- eb_line %>%
  st_buffer(100000)

eb_contours <- st_crop(contours, st_bbox(eb_line_area))

# simplify for easier drawing

simp_contours <- rmapshaper::ms_simplify(eb_contours_300)
simp_eb_line <- rmapshaper::ms_simplify(eb_line)

stations <- amtrak_stations %>%
  filter(state %in% c("MN", "ND", "MT", "ID", "WA")) 

col_map <- ggplot() +
  geom_sf(data = simp_contours, lwd = 0.2, color = "#9DBEB7") +
  geom_sf(data = simp_eb_line, color = "#B2664E", lwd = 0.5) +
  theme_void()

ggsave( "02-lines/col_map.pdf", col_map, device = cairo_pdf)