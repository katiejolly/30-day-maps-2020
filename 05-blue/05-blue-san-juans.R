library(tidyverse)
library(tigris)
library(sf)

# download all the necessary data and reproject to UTM 10N

wa_counties <- counties(state = "WA", cb = TRUE) %>%
  janitor::clean_names() %>%
  st_transform(26910)

san_juans <- wa_counties %>%
  filter(name == "San Juan") %>%
  dplyr::select(name)

structures <- read_sf("https://opendata.arcgis.com/datasets/435072fe451e43ea93144e3ec08e93e5_2.geojson") %>%
  st_transform(26910)

san_juan_structures <- structures %>%
  st_intersection(st_buffer(st_centroid(st_union(san_juans)), 25000))

water <- read_sf("https://opendata.arcgis.com/datasets/28a0f93c33454297b4a9d3faf3da552a_1.geojson") %>%
  st_transform(26910)

# crop to just the area of interest

san_juan_ferry <- read_sf("https://opendata.arcgis.com/datasets/fad5f3f332f74b3388070e9c96a0ba35_1.geojson") %>%
  st_transform(26910) %>%
  st_intersection(st_buffer(st_centroid(st_union(san_juans)), 25000))
  

san_juan_water <- water %>%
  st_intersection(st_buffer(st_centroid(st_union(san_juans)), 25000))

san_juan_places <- tigris::landmarks(state = "WA") %>%
  st_transform(26910) %>%
  st_intersection(st_buffer(st_centroid(st_union(san_juans)), 25000))

san_juan_places_filtered <- san_juan_places %>%
  filter(FULLNAME %in% c("Friday Harbor", "Eastsound"))

surrounding_counties <- san_juans %>%
  st_union() %>%
  st_centroid() %>%
  st_buffer(25000) %>%
  st_as_sf() %>%
  st_intersection(wa_counties) %>%
  dplyr::filter(name != "San Juan")%>%
  dplyr::select(name)

# set colors

light_blue <- "#DDEDEE" #C3DBDD
dark_blue <- colorspace::darken("#DDEDEE", 0.2)
yellow <- "#FDDA56"
dark_yellow <- colorspace::darken(yellow)

ggplot() +
  geom_sf(data = st_buffer(st_centroid(st_union(san_juans)), 25000), color =light_blue, fill =light_blue) +
  geom_sf(data = san_juans, color = "gray97", fill = "gray97") +
  geom_sf(data = surrounding_counties, fill = "gray97", color = "gray97") +
  # geom_sf(data = shoreline, fill = "#B7D6C7") +
  geom_sf(data = san_juan_water, fill = light_blue, color =light_blue) +
  geom_sf(data = san_juan_ferry, color = dark_blue, linetype = 2, lwd = 0.25) +
  geom_sf(data = san_juan_structures, color = dark_yellow, fill = yellow, lwd = 0.2) +
  theme_void()

ggsave("05-blue/san_juans.pdf", height = 10, width = 10)  


# repeat for just the Friday Harbor area for inset map

friday_harbor_point <- san_juan_places_filtered %>%
  filter(FULLNAME == "Friday Harbor")

friday_harbor <- san_juans %>%
  st_intersection(st_buffer(friday_harbor_point, 1000))

friday_harbor_water <- san_juan_water %>%
  st_intersection(st_buffer(friday_harbor_point, 1000))

friday_harbor_structures <- san_juan_structures %>%
  st_intersection(st_buffer(friday_harbor_point, 1000))

friday_harbor_ferry <- san_juan_ferry %>%
  st_intersection(st_buffer(friday_harbor_point, 1000))

ggplot() +
  geom_sf(data = friday_harbor, color = "gray97", fill = "gray97") +
  geom_sf(data = friday_harbor_water, color = light_blue, fill = light_blue) +
  geom_sf(data = friday_harbor_ferry, color = dark_blue, linetype = 2) +
  geom_sf(data = friday_harbor_structures, color = dark_yellow, fill = yellow, lwd = 0.2) +
  geom_sf(data = st_buffer(friday_harbor_point, 1000), color = "gray50", fill = NA, lwd = 0.2) +
  theme_void()


ggsave("05-blue/friday_harbor.pdf", height = 10, width = 10)  
