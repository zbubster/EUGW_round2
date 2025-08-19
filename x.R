library(terra)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggalluvial)

# -------------------
# NASTAVENÍ
# -------------------
# cesta k rastrům (jen CLASS.tif)
data_dir <- "Milovice_Mlada/EUGW_data/"
files_class <- list.files(data_dir, pattern = "CLASS\\.tif$", full.names = TRUE) |> sort()

# extrahuj roky z názvu souborů
extract_year <- function(x) substr(basename(x), 18, 21)  # přizpůsob dle struktury
years <- vapply(files_class, extract_year, character(1))

# načti stack
r_stack <- rast(files_class)
names(r_stack) <- years

# -------------------
# KONFIGURACE FILTRŮ
# -------------------
min_pixels_traj <- 1000   # minimální počet pixelů pro zachování trajektorie
min_pixels_node <- 1000     # minimální součet pixelů pro uzel (0 = vypnuto)

# -------------------
# VÝPOČET TRAJEKTORIÍ
# -------------------
# 1) pixel -> sekvence přes roky
df_seq <- as.data.frame(r_stack, na.rm = TRUE)
names(df_seq) <- years

# 2) spočítej četnosti unikátních trajektorií
df_counts <- df_seq %>%
  group_by(across(everything())) %>%
  summarise(freq = n(), .groups = "drop")

# 3) přidej ID trajektorie
df_counts$traj_id <- seq_len(nrow(df_counts))

# 4) pivot_longer do lodes formátu
df_long <- df_counts %>%
  pivot_longer(cols = all_of(years),
               names_to = "year", values_to = "class") %>%
  mutate(year  = factor(year, levels = years),
         class = factor(class, levels = as.character(21:27)))

# -------------------
# FILTRACE
# -------------------
df_long_filt <- df_long

# filtrování trajektorií
if (min_pixels_traj > 0) {
  df_long_filt <- df_long_filt %>%
    group_by(traj_id) %>%
    filter(unique(freq) >= min_pixels_traj) %>%
    ungroup()
}

# filtrování nodů
if (min_pixels_node > 0) {
  df_long_filt <- df_long_filt %>%
    group_by(year, class) %>%
    filter(sum(freq) >= min_pixels_node) %>%
    ungroup()
}

# -------------------
# VYKRESLENÍ
# -------------------
p <- ggplot(df_long_filt,
            aes(x = year, stratum = class, alluvium = traj_id, 
                y = freq, fill = class)) +
  geom_flow(stat = "alluvium", lode.guidance = "forward", knot.pos = 0.3) +
  geom_stratum(width = 1/12, color = "black") +
  scale_fill_brewer(type = "qual", palette = "Set1") +
  ggtitle("Grassland class transitions 2016–2023 (filtered)") +
  theme_minimal()

print(p)

# uložit do PNG
ggsave("grassland_transitions.png", p, width = 12, height = 7, dpi = 300)

################################################################################
# aggregated

library(terra)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggalluvial)

# -------------------
# NASTAVENÍ
# -------------------
data_dir <- "Milovice_Mlada/EUGW_data/"
files_class <- list.files(data_dir, pattern = "CLASS\\.tif$", full.names = TRUE) |> sort()

# extrakce roku z názvu souboru
extract_year <- function(x) substr(basename(x), 18, 21)  # přizpůsob podle tvého názvu
years <- vapply(files_class, extract_year, character(1))

# načtení stacku
r_stack <- rast(files_class)
names(r_stack) <- years

# Filtrační prahy
min_link <- 100   # minimální počet pixelů pro vykreslení linky
min_node <- 100   # minimální počet pixelů pro zachování nodu

# -------------------
# VÝPOČET TRANSITIONS
# -------------------
pairwise_list <- list()
for (i in 1:(nlyr(r_stack)-1)) {
  df <- as.data.frame(c(r_stack[[i]], r_stack[[i+1]]), na.rm = TRUE)
  names(df) <- c("from_class", "to_class")
  
  tab <- df %>%
    count(from_class, to_class, name = "freq") %>%
    mutate(year_from = names(r_stack)[i],
           year_to   = names(r_stack)[i+1])
  
  pairwise_list[[i]] <- tab
}
transitions_all <- bind_rows(pairwise_list)

# -------------------
# PŘEVOD DO LONG FORMÁTU
# -------------------
# nejdřív si uděláme unikátní ID pro každou kombinaci from→to
transitions_all <- transitions_all %>%
  mutate(group = paste(year_from, from_class, year_to, to_class, sep = "_"))

# pak teprve pivot_longer
df_long <- transitions_all %>%
  pivot_longer(cols = c(from_class, to_class),
               names_to = "pos", values_to = "class") %>%
  mutate(year = ifelse(pos == "from_class", year_from, year_to)) %>%
  select(year, class, freq, group)

desired_order <- as.character(21:27)
df_long <- df_long %>%
  mutate(year  = factor(year, levels = years),
         class = factor(class, levels = desired_order))

# -------------------
# FILTRACE LINKŮ A NODŮ
# -------------------
# Filtrace linků podle min_link
df_long_filt <- df_long %>%
  group_by(group) %>%
  filter(unique(freq) >= min_link) %>%
  ungroup()

# Součty pro nody
node_sums <- df_long_filt %>%
  group_by(year, class) %>%
  summarise(node_total = sum(freq), .groups = "drop")

keep_nodes <- node_sums %>%
  filter(node_total >= min_node) %>%
  mutate(node_id = paste(year, class)) %>%
  pull(node_id)

df_long_filt <- df_long_filt %>%
  mutate(node_id = paste(year, class)) %>%
  filter(node_id %in% keep_nodes) %>%
  select(-node_id)

# -------------------
# VYKRESLENÍ
# -------------------
p <- ggplot(df_long_filt,
            aes(x = year, stratum = class, alluvium = group,
                y = freq, fill = class, label = class)) +
  geom_flow(stat = "alluvium", lode.guidance = "forward", knot.pos = 0.3) +
  geom_stratum(width = 1/8, color = "black") +
  geom_text(stat = "stratum") +
  scale_fill_brewer(type = "qual", palette = "Set1") +
  ggtitle("Aggregated grassland class transitions 2016–2023 (filtered)") +
  theme_minimal()

print(p)

# uložit do PNG
ggsave("grassland_transitions_filtered.png", p, width = 14, height = 8, dpi = 300)

