library(patchwork)
library(tidyverse)
library(sf)

buildings <- read_sf("https://opendata.arcgis.com/datasets/8ffa9109cd9a4e37982cea67b289784d_0.geojson")

# List of housing types in the data
unique(buildings$House_Type)

# map of all the rowhouses-- red for highlight

row <- ggplot() +
  geom_sf(data = buildings, aes(fill = House_Type %in% c("Rowhouse", "rowhouse"), color = House_Type %in% c("Rowhouse", "rowhouse")), lwd = 0.001, show.legend = FALSE) +
  scale_fill_manual(values = c("#F9DDC5", "#D8404F")) +
  scale_color_manual(values = c("#F9DDC5", "#D8404F")) +
  theme_void() +
  labs(title = "rowhouses")


# map of all duplexes and triplexes
dup_trip <- ggplot() +
  geom_sf(data = buildings, aes(fill = House_Type %in% c("Triplex", "triplex", "Duplex", "duplex", "Durplex", "Tripledx"), color = House_Type %in% c("Triplex", "triplex", "Duplex", "duplex", "Durplex", "Tripledx")), lwd = 0.001, show.legend = FALSE) +
  scale_fill_manual(values = c("#F9DDC5", "#D8404F")) +
  scale_color_manual(values = c("#F9DDC5", "#D8404F")) +
  theme_void() +
  labs(title = "Duplex/triplex")

detached <- ggplot() +
  geom_sf(data = buildings, aes(fill = House_Type %in% c("Detached", "detached"), color = House_Type %in% c("Detached", "detached")), lwd = 0.001, show.legend = FALSE) +
  scale_fill_manual(values = c("#F9DDC5", "#D8404F")) +
  scale_color_manual(values = c("#F9DDC5", "#D8404F")) +
  theme_void() +
  labs(title = "detached")

# put them all together in a wide layout

map <- row + dup_trip + detached

# save as PDF for illustrator editing
ggsave("dc_map.pdf", map, width = 15, height = 5)
