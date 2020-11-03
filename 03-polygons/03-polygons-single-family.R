library(tidyverse)
library(lubridate)
library(sf)

# load data from the open data site

zoning <- read_sf("https://opendata.arcgis.com/datasets/42863f1debdc47488a1c2b9edd38053e_9.geojson") %>%
  st_transform(26910)

dadus <- read_sf("https://opendata.arcgis.com/datasets/f59b69f4089448a68a0dedbd7dcf98a9_0.geojson") %>%
  mutate(APP_DATE = ymd_hms(APP_DATE)) %>%
  st_transform(26910)

adus <- read_sf("https://opendata.arcgis.com/datasets/b64a1cf17e0a4223a408e411a15029ae_0.geojson") %>%
  mutate(APP_DATE = ymd_hms(APP_DATE)) %>%
  st_transform(26910)

# filter out only the eligible single family and low rise zoning areas

zoning <- zoning %>%
  mutate(single_family_low_rise = str_starts(ZONING, "SF ") | ZONING == "RSL/TC" | str_starts(ZONING, "LR"))

zoning_simplified <- zoning %>%
  st_buffer(0) %>%
  group_by(single_family_low_rise) %>%
  summarise(areas = n())
  
# check that it looks right 

ggplot(zoning) +
  geom_sf(aes(fill = single_family_low_rise), color = "gray88", lwd = 0.1) +
  theme_void() +
  scale_fill_manual(values = c("gray98", "#6DA096"))

# construction permits since 2019

dadus_19_20 <- dadus %>%
  filter(APP_DATE >= "2019-01-01",
         APTYPE == "Construction Permit") %>%
  mutate(shape_id = sample(c(1:6), size = nrow(.), replace = TRUE), # randomize the size, color, and shape
         color_id = sample(c(1:6), size = nrow(.), replace = TRUE),
         size_id = sample(c(1:4), size = nrow(.), replace = TRUE))

adus_19_20 <- adus %>%
  filter(APP_DATE >= "2019-01-01",
         APTYPE == "Construction Permit") %>%
  mutate(shape_id = sample(c(1:6), size = nrow(.), replace = TRUE),
         color_id = sample(c(1:6), size = nrow(.), replace = TRUE),
         size_id = sample(c(1:4), size = nrow(.), replace = TRUE))

# color palette from design-seeds

pal <- c("#DAE5AF", "#9DC5A8", "#72A689", "#5A8F81", "#7BBDA8", "#A8D7CA")

ggplot() +
  geom_sf(data = zoning, aes(fill = single_family_low_rise), color = "white", lwd = 0.2, show.legend = FALSE) +
  scale_fill_manual(values = c("gray98", "gray94")) +
  geom_sf(data = dadus_19_20, aes(color = factor(color_id), shape = factor(shape_id), size = factor(size_id)), show.legend = FALSE) +
  geom_sf(data = adus_19_20, aes(color = factor(color_id), shape = factor(shape_id), size = factor(size_id)), show.legend = FALSE) +
  scale_size_manual(values = c(3.2,3.5,3.7,4)) +
  scale_color_manual(values = pal) +
  scale_shape_manual(values = c(15:20)) +
  theme_void()

# save as cairo pdf to be able to edit in illustrator

ggsave("03-polygons/dadu_map.pdf", device = cairo_pdf, width = 7, height = 10)

# after this step I pulled the map into illustrator to style the text


# permits per neighborhood

cra_totals <- dadus_19_20 %>%
  st_set_geometry(NULL) %>%
  bind_rows(adus_19_20 %>% st_set_geometry(NULL)) %>%
  group_by(cra = CRA) %>%
  summarise(total = n()) %>%
  mutate(cra = fct_reorder(factor(cra), total))
