library(tigris)
library(sf)
library(tidyverse)
library(mapview)

bay_states <- states(cb = TRUE) %>%
  janitor::clean_names() %>%
  filter(name %in% c("Virginia", "Delaware", "Maryland", "District of Columbia")) %>%
  st_transform(26917)

# Chesapeake shoreline over time from https://data.imap.maryland.gov/datasets/maryland-shoreline-changes-legacy-historic-shorelines-by-years?geometry=-83.697%2C36.846%2C-71.370%2C39.861
shoreline <- read_sf("https://opendata.arcgis.com/datasets/b7dec3418668473c82002ee28e280eae_3.geojson") %>%
  janitor::clean_names() %>%
  st_transform(26917)

# just the part of the states that surrounds the bay
land <- bay_states %>%
  st_crop(st_bbox(shoreline))

# shoreline hazard index from https://data.imap.maryland.gov/datasets/maryland-coastal-resiliency-assessment-shoreline-hazard-index?geometry=-76.642%2C38.395%2C-75.872%2C38.584
hazard <- read_sf("https://opendata.arcgis.com/datasets/35a3cce465634531a43d6d01988a43cf_1.geojson") %>%
  st_transform(26917)

# all USA water bodies - very large file!
# https://hub.arcgis.com/datasets/esri::usa-detailed-water-bodies
water <- read_sf("https://services.arcgis.com/P3ePLMYs2RVChkJx/arcgis/rest/services/USA_Detailed_Water_Bodies/FeatureServer/0/query?where=1%3D1&outFields=*&geometry=-89.409%2C34.653%2C-62.229%2C40.732&geometryType=esriGeometryEnvelope&inSR=4326&spatialRel=esriSpatialRelIntersects&outSR=4326&f=json") %>%
  st_transform(26917)

chesapeake_water <- water %>%
  st_buffer(0) %>%
  st_crop(st_bbox(land))

ggplot(land) +
  geom_sf(data = land, fill = "#F1F0EA", color = "#4E4042",lwd = 0.15) +
  # only add significantly sized water bodies
  geom_sf(data = chesapeake_water %>% filter(FTYPE != "Swamp/Marsh", SQMI >= 0.1) %>% rmapshaper::ms_simplify(), fill = "#D7E9EE", color = "#D7E9EE") +
  # add back the boundaries that get hidden under the water
  geom_sf(data = land, fill = NA, color = "#4E4042",lwd = 0.15) +
  # add shoreline effect
  geom_sf(data = st_buffer(land %>% st_union(), 500), color = "#9FC2CB", fill = "transparent", lwd = 0.2) +
  geom_sf(data = st_buffer(land %>% st_union(), 1100), color = "#9FC2CB", fill = "transparent", lwd = 0.2) +
  geom_sf(data = st_buffer(land %>% st_union(), 1800), color = "#9FC2CB", fill = "transparent", lwd = 0.2) +
  geom_sf(data = st_buffer(land %>% st_union(), 2600), color = "#9FC2CB", fill = "transparent", lwd = 0.2) +
  geom_sf(data = st_buffer(land %>% st_union(), 3500), color = "#9FC2CB", fill = "transparent", lwd = 0.2) +
  geom_sf(data = hazard %>% filter(Hazard_with_Habitats %in% c("Moderate", "High")),  aes(color = Hazard_with_Habitats), size = 0.08, show.legend = FALSE) +
  scale_color_manual(values = c("Moderate" = colorspace::lighten("#F3AE79"), "High" = ("#CD4D46"))) +
  theme_void() +
  # artificially add water background
  theme(panel.background = element_rect(fill = "#D7E9EE"))

ggsave("17-islands/chesapeake_islands.png", width = 6, height = 7, dpi = 300)
