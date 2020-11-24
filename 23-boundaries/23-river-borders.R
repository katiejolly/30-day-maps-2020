library(tidyverse)
library(urbnmapr)
library(sf)

states <- get_urbn_map(map = "states", sf = TRUE)

state_water_boundaries <- read_sf("23-boundaries/GSRB/GSRB_Level1.shp")

us_water_boundaries <- state_water_boundaries %>%
  filter(adm0_1 == "United States of America" | adm0_2 == "United States of America") %>%
  st_transform("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs")


border_map <- ggplot() +
  geom_sf(data = states, color = "gray80", lwd = 0.1, fill = NA) +
  geom_sf(data = us_water_boundaries, color = "#8BC4C1", lwd = .5) +
  
  theme_void()

border_map

ggsave("23-boundaries/border_map_draft.png", border_map, dpi = 300, width = 12, height = 10)
