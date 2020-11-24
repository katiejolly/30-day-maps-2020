library(tidyverse)
library(sf)
library(mapview)
library(osrm)
library(osmdata)
library(ggsflabel)
library(tigris)

public_art_q <- getbb("Seattle") %>% # within Seattle
  opq() %>% # create an overpass query
  add_osm_feature("tourism", "artwork") %>%  # with all public art features
  osmdata_sf() # and return that query as an sf object

public_art <- public_art_q$osm_points %>% # pull out just the point data
  st_transform(26910) # and use a UTM 10N projection

art <- read_csv("https://data.seattle.gov/api/views/j7sn-tdzk/rows.csv?accessType=DOWNLOAD") %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, agr = "constant") %>%
  st_transform(26910)
  

park_q <- getbb("Seattle") %>% # within Seattle
  opq() %>% # create an overpass query
  add_osm_feature("leisure", "park") %>%  # with all public art features
  osmdata_sf() # and return that query as an sf object

parks <- park_q$osm_polygons %>% # pull out just the point data
  st_transform(26910) # and use a UTM 10N projection

neighborhoods <- read_sf("https://opendata.arcgis.com/datasets/b76cdd45f7b54f2a96c5e97f2dda3408_2.geojson") %>%
  st_transform(26910) %>% 
  select(neighborhood = S_HOOD, parent = L_HOOD) %>%
  mutate(parent = str_to_title(parent),
         parent = if_else(parent == "No Broader Term", neighborhood, parent))

parks <- read_sf("https://opendata.arcgis.com/datasets/94adf91951234a80b124238533f9e042_5.geojson") %>%
  st_transform(26910)

osm_public_art_sf <- public_art %>%
  st_join(neighborhoods , left = FALSE) 

art_sf <- art %>%
  st_join(neighborhoods , left = FALSE) 

roads <- read_sf("https://opendata.arcgis.com/datasets/383027d103f042499693da22d72d10e3_0.geojson") %>%
  st_transform(26910)

## Fremont

fremont <- neighborhoods %>%
  filter(parent == "Fremont")

fremont_roads <- st_intersection(roads, st_buffer(fremont, 150))
fremont_parks <- st_intersection(parks, st_buffer(fremont, 150))

fremont_art <- osm_public_art_sf %>%
  filter(parent == "Fremont") %>%
  drop_na(name)

rownames(fremont_art) <- fremont_art$name

fremont_trip_obj <- osrmTrip(fremont_art, returnclass = "sf")

fremont_trip_sf <- fremont_trip_obj[[1]]$trip %>%
  mutate(order = row_number())

ggplot(fremont_trip_sf) +
  geom_sf(data = fremont, color = "white", fill = "white", lwd = 0.3) +
  geom_sf(data = fremont_roads, color = "#AABEBF", lwd = 0.15) +
  geom_sf(data = fremont_roads %>% filter(ARTDESCRIPT == "Principal Arterial"), color = "#AABEBF", lwd= 0.4) +
  geom_sf(data = fremont_parks, fill = "#AABEBF", color = "#AABEBF") +
  ggrepel::geom_text_repel(
    data = fremont_trip_sf,
    aes(label = end, geometry = geometry),
    stat = "sf_coordinates",
    min.segment.length = 0,
    colour = "#626F90",
    segment.colour = "#626F90",
    family = "Lato"
  ) +
  theme_void()

ggsave("fremont.pdf", device = cairo_pdf, height = 8, width = 8)


# Beacon Hill

beacon_hill <- neighborhoods %>%
  filter(parent == "Beacon Hill" & neighborhood == "North Beacon Hill")

beacon_hill_roads <- st_intersection(roads, st_buffer(beacon_hill, 150))
beacon_hill_parks <- st_intersection(parks, st_buffer(beacon_hill, 150))

beacon_hill_art <- osm_public_art_sf %>%
  filter(parent == "Beacon Hill" & neighborhood == "North Beacon Hill") %>%
  drop_na(name) %>%
  mutate(order = row_number())

rownames(beacon_hill_art) <- beacon_hill_art$name

mapview(beacon_hill_art)

beacon_hill_trip_obj <- osrmTrip(beacon_hill_art, returnclass = "sf")

beacon_hill_trip_sf <- beacon_hill_trip_obj[[1]]$trip %>%
  mutate(order = row_number())

ggplot(beacon_hill_trip_sf) +
  geom_sf(data = beacon_hill, color = "white", fill = "white", lwd = 0.3) +
  geom_sf(data = beacon_hill_roads, color = "#AABEBF", lwd = 0.15) +
  geom_sf(data = beacon_hill_roads %>% filter(ARTDESCRIPT == "Principal Arterial"), color = "#AABEBF", lwd= 0.4) +
  geom_sf(data = beacon_hill_parks, fill = "#AABEBF", color = "#AABEBF") +
  ggrepel::geom_text_repel(
    data = beacon_hill_art,
    aes(label = order, geometry = geometry),
    stat = "sf_coordinates",
    min.segment.length = 0,
    colour = "#626F90",
    segment.colour = "#626F90",
    family = "Lato"
  ) +
  theme_void()

ggsave("beacon_hill.pdf", device = cairo_pdf, height = 8, width = 8)
