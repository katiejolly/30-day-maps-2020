library(tidyverse)
library(tigris)
library(sf)
library(usethis)
library(ggnewscale)

# use_zip("https://www.huduser.gov/portal/sites/default/files/zip/UPSAI_050820.zip", destdir = "20-population")

ahs_survey_2017 <- read_csv("20-population/UPSAI_050820/UPSAI_050820.csv") %>%
  janitor::clean_names()

reds <- c("#733D47", "#BE7D88", "#D2A8AF")
greens <- c("#959E4A", "#C2CA8C", "#D7DDB4")
blues <- c("#0091C1", "#71B4D7", "#9ECBE5")

virginia_tracts <- tracts(state = "Virginia") %>%
  janitor::clean_names() %>%
  st_transform(26917)

virginia_counties <- counties(state = "Virginia") %>%
  janitor::clean_names() %>%
  st_transform(26917)

virginia_tracts_class <- virginia_tracts %>%
  select(geoid, countyfp) %>%
  left_join(ahs_survey_2017) %>%
  mutate(upsai_cat_clean = case_when(
    acs17_occupied_housing_units_est == 0 ~ NA_real_,
    TRUE ~ upsai_cat_controlled
    ),
         upsai_cat_desc = case_when(
           upsai_cat_clean == 1 ~ "Urban",
           upsai_cat_clean == 2 ~ "Suburban",
           upsai_cat_clean == 3 ~ "Rural",
           is.na(upsai_cat_clean) ~ "No occupied housing",
           TRUE ~ NA_character_
         ),
    urban_cat = cut(x = upsai_urban, breaks = c(0, .6, .85, 1)),
    suburban_cat = cut(x = upsai_suburban, breaks = c(0, .6, .85, 1)),
    rural_cat = cut(x = upsai_rural, breaks = c(0, .6, .85, 1)))


detail_map <- ggplot() +
  geom_sf(data = virginia_tracts_class %>% 
            filter(upsai_cat_desc == "Urban"), aes(fill = urban_cat), lwd = 0.08, color = "white", show.legend = FALSE) +
  scale_fill_manual(values = rev(reds)) +
  new_scale_fill() +
  geom_sf(data = virginia_tracts_class %>% 
            filter(upsai_cat_desc == "Suburban"), aes(fill = suburban_cat), lwd = 0.08, color = "white", show.legend = FALSE) +
  scale_fill_manual(values = rev(blues)) +
  new_scale_fill() +
  geom_sf(data = virginia_tracts_class %>% 
            filter(upsai_cat_desc == "Rural"), aes(fill = rural_cat), lwd = 0.08, color = "white", show.legend = FALSE) +
  scale_fill_manual(values = rev(greens)) +
  geom_sf(data = virginia_counties, color = "gray10", lwd = 0.08, fill = NA) +
  theme_void()

detail_map

ggsave("20-population/detailed_map.png", detail_map, dpi = 300, width = 12, height = 9)


dc_counties <- virginia_tracts_class[virginia_tracts_class$countyfp %in% c("510", "600", "610", "153", "107", "059", "013", "683", "685"), ]

dc_inset <- st_crop(virginia_tracts_class, st_bbox(dc_counties))

mapview::mapview(dc_inset)  

dc_map <- ggplot() +
  geom_sf(data = dc_inset %>% 
            filter(upsai_cat_desc == "Urban"), aes(fill = urban_cat), lwd = 0.08, color = "white", show.legend = FALSE) +
  scale_fill_manual(values = rev(reds)) +
  new_scale_fill() +
  geom_sf(data = dc_inset %>% 
            filter(upsai_cat_desc == "Suburban"), aes(fill = suburban_cat), lwd = 0.08, color = "white", show.legend = FALSE) +
  scale_fill_manual(values = rev(blues)) +
  new_scale_fill() +
  geom_sf(data = dc_inset %>% 
            filter(upsai_cat_desc == "Rural"), aes(fill = rural_cat), lwd = 0.08, color = "white", show.legend = FALSE) +
  scale_fill_manual(values = rev(greens)) +
  geom_sf(data = dc_counties, color = "gray10", lwd = 0.2, fill = NA) +
  theme_void()

ggsave("20-population/dc_map.png", dc_map, dpi = 300, width = 5, height = 5)

richmond_counties <- virginia_tracts_class[virginia_tracts_class$countyfp %in% c("041", "087", "085", "760"), ]

richmond_inset <- st_crop(virginia_tracts_class, st_bbox(richmond_counties))

richmond_map <- ggplot() +
  geom_sf(data = richmond_inset %>% 
            filter(upsai_cat_desc == "Urban"), aes(fill = urban_cat), lwd = 0.08, color = "white", show.legend = FALSE) +
  scale_fill_manual(values = rev(reds)) +
  new_scale_fill() +
  geom_sf(data = richmond_inset %>% 
            filter(upsai_cat_desc == "Suburban"), aes(fill = suburban_cat), lwd = 0.08, color = "white", show.legend = FALSE) +
  scale_fill_manual(values = rev(blues)) +
  new_scale_fill() +
  geom_sf(data = richmond_inset %>% 
            filter(upsai_cat_desc == "Rural"), aes(fill = rural_cat), lwd = 0.08, color = "white", show.legend = FALSE) +
  scale_fill_manual(values = rev(greens)) +
  geom_sf(data = richmond_counties, color = "gray10", lwd = 0.2, fill = NA) +
  theme_void()

ggsave("20-population/richmond_map.png", richmond_map, dpi = 300, width = 5, height = 5)
