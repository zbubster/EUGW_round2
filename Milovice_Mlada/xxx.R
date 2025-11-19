library(terra)
library(sf)
library(dplyr)
library(tidyr)

getwd()
setwd("AOPK/EUGW_round2/Milovice_Mlada/")

# vstupy
points <- st_read("MM_field_data.gpkg")
rast_model <- rast("EUGW_data/CZ_CON_CON_75111_20230101_20231231_GTYH_CLASS.tif")

# převod bodů na terra
points_v <- vect(points)

# připoj hodnotu rastru (predikce)
points$predicted <- terra::extract(rast_model, points_v)[,2]

# převod čísel na názvy
cat_map <- data.frame(
  predicted = 21:27,
  model_cat = c("Dry grassland",
                "Mesic grassland",
                "Wet and seasonally wet grassland",
                "Alpine and sub-alpine grassland",
                "Forest clearings",
                "Inland salt steppes",
                "Sparsely wooded grassland")
)

points <- left_join(points, cat_map, by = "predicted")


points_long <- points %>%
  st_drop_geometry() %>%
  pivot_longer(cols = c(EUNIS_LC, SECONDARY_EUNIS_LC, TERTIAIRY_EUNIS_LC), 
               names_to = "level", 
               values_to = "true_cat",
               values_drop_na = TRUE)


summary_tab <- points_long %>%
  group_by(level, true_cat, model_cat) %>%
  summarise(count = n(), .groups = "drop") %>%
  arrange(level, desc(count))


summary_tab <- summary_tab %>%
  group_by(level, true_cat) %>%
  mutate(share = count / sum(count)) %>%
  ungroup()


summary_tab
getwd()
write.csv(summary_tab, "summary_csv.csv")

library(ggplot2)

ggplot(summary_tab, aes(x = true_cat, y = model_cat, fill = share)) +
  geom_tile() +
  facet_wrap(~level) +
  scale_fill_viridis_c() +
  labs(x = "Terénní (true)", y = "Model (predicted)", fill = "Podíl") +
  theme_minimal()
