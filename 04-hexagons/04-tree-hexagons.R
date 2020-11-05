library(tidyverse)
library(usethis)
library(sf)
library(tigris)

# use_zip("https://www.denvergov.org/media/gis/DataCatalog/tree_inventory/shape/tree_inventory.zip", destdir = "04-hexagons/")


co_trees <- read_sf("04-hexagons/tree_inventory/tree_inventory.shp") %>%
  janitor::clean_names() %>%
  st_transform(26913)

# denver outline

denver <- places(state = "CO", cb = TRUE) %>%
  janitor::clean_names() %>%
  filter(name == "Denver") %>%
  st_transform(26913)

# create hexagon grid with 200m hexagons
denver_grid <- st_make_grid(denver, cellsize = 200, square = FALSE) %>%
  st_as_sf() %>%
  mutate(hex_id = row_number())

# mapview::mapview(list(denver, denver_grid))

# is the blue spruce in the grid cell?
blue_spruce_grid <- st_join(denver_grid, co_trees, left = FALSE) %>%
  group_by(hex_id) %>%
  summarise(has_blue_spruce = sum(species_co == "Spruce, Blue") > 0)

ggplot(blue_spruce_grid) +
  geom_sf(aes(fill = as.factor(has_blue_spruce)), color = "white", lwd = 0.05) +
  scale_fill_manual(values = c("gray90", "#46728C")) +
  theme_void()

# plot with inner glow pattern

denver_plot <- ggplot() +
  geom_sf(data = denver, color = "gray80", fill = "gray90") +
  geom_sf(data = denver %>% st_buffer(-50), fill = "gray93", color = "transparent") +
  geom_sf(data = denver %>% st_buffer(-100), fill = "gray95", color = "transparent") +
  geom_sf(data = denver %>% st_buffer(-140), fill = "gray98", color = "transparent") +
  geom_sf(data = blue_spruce_grid, aes(fill = factor(has_blue_spruce)), color = "gray98", lwd = 0.02, show.legend = FALSE) +
  scale_fill_manual(values = c("gray90", "#52787D")) +
  theme_void()

ggsave("04-hexagons/denver_trees.pdf", denver_plot, height = 10, width = 10)


## repeat for DC

dc_trees <- read_sf("https://opendata.arcgis.com/datasets/f6c3c04113944f23a7993f2e603abaf2_23.geojson") %>%
  janitor::clean_names() %>%
  st_transform(26917)

View(unique(dc_trees$cmmn_nm))

dc <- places(state = "DC", cb = TRUE) %>%
  janitor::clean_names() %>%
  st_transform(26917)


dc_grid <- st_make_grid(dc, cellsize = 200, square = FALSE) %>%
  st_as_sf() %>%
  mutate(hex_id = row_number())

mapview::mapview(list(dc, dc_grid))

scarlet_oak_grid <- st_join(dc_grid, dc_trees, left = FALSE) %>%
  group_by(hex_id) %>%
  summarise(has_scarlet_oak = sum(cmmn_nm == "Scarlet oak") > 0)

ggplot(scarlet_oak_grid) +
  geom_sf(aes(fill = as.factor(has_scarlet_oak)), color = "white", lwd = 0.05) +
  scale_fill_manual(values = c("gray90", "#46728C")) +
  theme_void()

dc_plot <- ggplot() +
  geom_sf(data = dc, color = "gray80", fill = "gray90") +
  geom_sf(data = dc %>% st_buffer(-50), fill = "gray93", color = "transparent") +
  geom_sf(data = dc %>% st_buffer(-100), fill = "gray95", color = "transparent") +
  geom_sf(data = dc %>% st_buffer(-140), fill = "gray98", color = "transparent") +
  geom_sf(data = scarlet_oak_grid, aes(fill = factor(has_scarlet_oak)), color = "gray98", lwd = 0.02, show.legend = FALSE) +
  # geom_sf_text(data = st_centroid(dc_parks), aes(label = str_wrap(name, 20)), family = "Lato") +
  scale_fill_manual(values = c("gray90", "#A2343A")) +
  theme_void()

ggsave("04-hexagons/dc_trees.pdf", dc_plot, height = 10, width = 10)

### repeat for Seattle

seattle_trees <- read_sf("https://opendata.arcgis.com/datasets/0b8c124ace214943ab0379623937eccb_6.geojson") %>%
  janitor::clean_names() %>%
  st_transform(26910)


seattle <- places(state = "WA", cb = TRUE) %>%
  janitor::clean_names() %>%
  st_transform(26910) %>%
  filter(name == "Seattle")


seattle_grid <- st_make_grid(seattle, cellsize = 200, square = FALSE) %>%
  st_as_sf() %>%
  mutate(hex_id = row_number())

western_hemlock_grid <- st_join(seattle_grid, seattle_trees, left = FALSE) %>%
  group_by(hex_id) %>%
  summarise(has_hemlock = sum(common_name == "Western Hemlock") > 0)

ggplot(western_hemlock_grid) +
  geom_sf(aes(fill = as.factor(has_hemlock)), color = "white", lwd = 0.05) +
  scale_fill_manual(values = c("gray90", "#46728C")) +
  theme_void()

seattle_plot <- ggplot() +
  geom_sf(data = seattle, color = "gray80", fill = "gray90") +
  geom_sf(data = seattle %>% st_buffer(-50), fill = "gray93", color = "transparent") +
  geom_sf(data = seattle %>% st_buffer(-100), fill = "gray95", color = "transparent") +
  geom_sf(data = seattle %>% st_buffer(-140), fill = "gray98", color = "transparent") +
  geom_sf(data = western_hemlock_grid, aes(fill = factor(has_hemlock)), color = "gray98", lwd = 0.02, show.legend = FALSE) +
  # geom_sf_text(data = st_centroid(dc_parks), aes(label = str_wrap(name, 20)), family = "Lato") +
  scale_fill_manual(values = c("gray90", "#415842")) +
  theme_void()

seattle_plot

ggsave("04-hexagons/seattle_trees.pdf", seattle_plot, height = 10, width = 10)
