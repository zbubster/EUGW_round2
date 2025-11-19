library(sf)
library(dplyr)

g <- st_read("Milovice_Mlada/MM_field_data.gpkg")

col <- "EUNIS_LC"

# spočítej proporce
tab <- g %>%
  st_drop_geometry() %>%
  mutate(
    value = as.character(.data[[col]]),
    value = ifelse(is.na(value), "NA", value)
  ) %>%
  count(value, name = "n") %>%
  mutate(proportion = n / sum(n))

barplot(
  tab$proportion,
  names.arg = tab$value,
  col = rainbow(nrow(tab)),
  main = paste("Proporce kategorií –", col),
  ylab = "Proportion",
  las = 2
)


ggplot(tab, aes(x = "All", y = proportion, fill = value)) +
  geom_col(width = 0.6) +
  labs(
    x = "",
    y = "Proportion",
    fill = col,
    title = paste("Field data, n = 111")
  ) +
  theme_bw()

## with model

library(sf)
library(terra)
library(dplyr)
library(caret)
library(ggplot2)

v <- st_read("Milovice_Mlada/MM_field_data.gpkg")      # obsahuje sloupec col
r  <- rast("Milovice_Mlada/EUGW_data/CZ_CON_CON_75111_20230101_20231231_GTYH_CLASS.tif")   # klasifikační model

pts <- vect(v)

# extract vrací data frame s ID + hodnotou rastru
v$predicted <- extract(r, pts)[,2]

df <- v %>%
  st_drop_geometry() %>%
  mutate(
    observed = .$GT,             # číselný kód 21–27
    predicted = predicted
  )
df$predicted <- ifelse(is.na(df$predicted), "NA", df$predicted)
df$observed  <- ifelse(is.na(df$observed),  "NA", df$observed)
df$observed  <- ifelse(df$observed == 27,  "NA", df$observed)

table_cm <- table(df$observed, df$predicted)
table_cm


# obrazek

labels_map <- c(
  "21" = "Dry grassland",
  "22" = "Mesic grassland",
  "23" = "Wet and seasonally wet grassland",
  "24" = "Alpine and sub-alpine grassland",
  "25" = "Forest clearings",
  "26" = "Inland salt steppes",
  "27" = "Sparsely wooded grassland",
  "NA" = "Unclassified"
)

count_obs <- tab %>%
  group_by(observed) %>%
  summarise(n = sum(n)) %>% # n musíš mít v tab; pokud ne, řekni a upravím
  filter(observed != "27")
  



tab <- df %>%
  count(observed, predicted) %>%
  group_by(observed) %>%
  mutate(prop = n / sum(n)) %>%
  filter(observed != "27")

tab <- tab %>% 
  dplyr::filter(observed != "27")


ggplot(tab, aes(x = factor(observed), y = prop, fill = factor(predicted))) +
  geom_col() +
  geom_text(
    data = count_obs,
    aes(x = factor(observed), y = 1.02, label = paste0("n = ", n)),   # y = 1.02 → kousek nad 100 %
    inherit.aes = FALSE,
    size = 3
  ) +
  labs(
    x = "Observed class",
    y = "Proportion of predictions",
    fill = "Predicted class",
    title = "Field data ~ EUGW typology output"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_x_discrete(labels = labels_map) +      
  scale_fill_manual(
    values = c(
      "21" = "#f5ca7a",
      "22" = "#a5f57a",
      "23" = "#7ab6f5",
      "24" = "#ca7af5",
      "25" = "#5c8944",
      "26" = "#f57a7a",
      "27" = "#895a44",
      "NA" = "#cccccc"
    ),
    labels = labels_map                         
  )

## stability

library(terra)

# načtení rastru
r <- rast("____OUT_Milovice_stability/stable_pixels.tif")

# velikost jedné buňky v mapových jednotkách (u většiny CRS = m, takže výsledek bude v m²)
cell_area <- prod(res(r))

# počet buněk s hodnotou 1 STABLE
n_1 <- global(r == 1, "sum", na.rm = TRUE)[1]

# počet buněk s hodnotou 0 UNSTABLE
n_0 <- global(r == 0, "sum", na.rm = TRUE)[1]

# celkový počet buněk
n_total <- n_1 + n_0

# plochy
area_total <- n_total * cell_area
area_1     <- n_1 * cell_area
area_0     <- n_0 * cell_area

area_1_ha <- area_1 / 10000
area_0_ha <- area_0 / 10000
area_total_ha <- area_total / 10000

area_1_ha # stable
area_0_ha # unstable
area_total_ha

# percent
pct_1 <- (n_1 / n_total) * 100 # stable
pct_0 <- (n_0 / n_total) * 100 # unstable
pct_1
pct_0

#### stability2

r <- rast("____OUT_Milovice_stability/nchanges_per_pixel.tif")

# velikost jedné buňky v mapových jednotkách (u většiny CRS = m, takže výsledek bude v m²)
cell_area <- prod(res(r))

# získáme všechny hodnoty v rastru (bez NA)
vals <- sort(unique(values(r)))
vals <- vals[!is.na(vals)]

# připravíme výstupní data.frame
res <- data.frame(
  value    = vals,
  n_cells  = NA_integer_,
  area_m2  = NA_real_,
  pct      = NA_real_
)

# smyčka přes jednotlivé hodnoty
for (i in seq_along(vals)) {
  v <- vals[i]
  
  # počet buněk s danou hodnotou
  n_v <- global(r == v, "sum", na.rm = TRUE)[1]
  
  res$n_cells[i] <- n_v
  res$area_m2[i] <- n_v * cell_area
}

# celkový počet platných buněk (bez NA)
n_clean <- sum(res$n_cells)

# procenta z platných buněk
res$pct <- (res$n_cells / n_clean) * 100

# celková plocha očištěná o NA (součet všech kategorií)
total_clean_area <- sum(res$area_m2)

res
total_clean_area

