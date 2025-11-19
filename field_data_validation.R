# ====== KROK 1: PŘÍPRAVA DAT A JOIN ======
# balíčky
pkgs <- c("sf", "terra", "dplyr", "tidyr", "stringr", "forcats", "readr", "purrr")
to_install <- setdiff(pkgs, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, repos = "https://cloud.r-project.org")
invisible(lapply(pkgs, library, character.only = TRUE))

# ---- 1) Načtení dat ----
field_path <- "Milovice_Mlada/MM_field_data.gpkg"
model_path <- "Milovice_Mlada/EUGW_data/CZ_CON_CON_75111_20230101_20231231_GTYH_CLASS.tif"
conf_path  <- "Milovice_Mlada/EUGW_data/CZ_CON_CON_75111_20230101_20231231_GTYH_CONF.tif"

field <- sf::st_read(field_path, quiet = TRUE)           # POINTS, ground-truth + atributy
model <- terra::rast(model_path)                         # 1 vrstva: predicted_label (kód 21–27)
conf  <- terra::rast(conf_path)                          # 7 vrstev: prob_... pro jednotlivé třídy

# ---- 2) Kontroly a harmonizace CRS ----
# (terra::rast má CRS z rastrů; sf objekt může mít jiný - reprojektujeme body do CRS rastrů)
if (!st_is_longlat(field)) {
  # OK; jen srovnáme s CRS rastru
  if (sf::st_crs(field)$wkt != terra::crs(model)) {
    message("Reprojekce 'field' do CRS rastru...")
    field <- sf::st_transform(field, sf::st_crs(terra::crs(model)))
  }
} else {
  message("Pozor: 'field' je v geografickém CRS, reprojekuju do CRS rastru...")
  field <- sf::st_transform(field, sf::st_crs(terra::crs(model)))
}

# ---- 3) Extrakce predikcí k bodům ----
# predicted_label (číselný kód) 
pred_df <- terra::extract(model, sf::st_coordinates(field) |> as.data.frame())
# extract() vrací sloupec 'ID' + hodnoty vrstev; dropneme ID a přilepíme
pred_df <- pred_df |>
  dplyr::select(-ID) |>
  dplyr::rename(predicted_code = dplyr::everything())

# ---- 4) Extrakce confidence/prob vrstev ----
conf_df <- terra::extract(conf, sf::st_coordinates(field) |> as.data.frame())
# Očistíme: necháme jen pravděpodobnosti, převedeme 0–100 → 0–1
prob_cols <- setdiff(names(conf_df), "ID")
conf_df <- conf_df |>
  dplyr::select(-ID) |>
  dplyr::mutate(dplyr::across(dplyr::everything(), ~ .x / 100)) 

# ---- 5) Sjednocení do jednoho data.frame ----
dat <- field |>
  # zachováme geometrii
  dplyr::bind_cols(pred_df, conf_df)

# ---- 6) Příprava „klíče“ kategorií (MAPOVÁNÍ KÓD → NÁZEV) ----
# POZN: Kódy 21–27 jsou z tvého modelu. Sem dosaď finální názvy přesně dle EUGW/EUNIS labelů.
# Zatím předvyplním běžné zkratky; uprav si dle svého modelu (DŮLEŽITÉ!):
label_map <- tibble::tibble(
  predicted_code = c(21L, 22L, 23L, 24L, 25L, 26L, 27L),
  pred_label     = c("Dry grassland", 
                     "Mesic grassland", 
                     "Seasonally wet and wet grasslands", 
                     "Alpine and subalpine grasslands", 
                     "Forest fringes and clearings and tall forb stands", 
                     "Inland salt steppes and salt marshes",
                     "Sparsely wooded grasslands")
)

dat <- dat |>
  dplyr::left_join(label_map, by = "predicted_code")

# ---- 7) Definuj „pravdu“ z terénu ----
# Vybereme primární terénní label (EUNIS_LC). U tebe se jmenuje přesně "EUNIS_LC".
# Můžeš doladit normalizaci názvů (trim, lower) pro robustnější shody.
norm_text <- function(x) {
  x |>
    stringr::str_squish()
}

dat <- dat |>
  dplyr::mutate(
    truth_label_raw = norm_text(EUNIS_LC),
    pred_label_raw  = norm_text(pred_label)
  )

# ---- 8) Zkontroluj soulad názvů pravda vs. pred_label ----
# Vytvoříme malý přehled, ať vidíš, co je potřeba případně přemapovat.
label_check <- dat |>
  dplyr::distinct(truth_label_raw, pred_label_raw) |>
  dplyr::arrange(truth_label_raw, pred_label_raw)
print(label_check, n = 50)

# ---- 9) Z prob_ vrstev vytvoříme vektor pravděpodobností, max_prob, margin, entropii ---
# Najdeme sloupce s pravděpodobnostmi:
prob_cols <- grep("^prob_", names(dat), value = TRUE)

# a) max pravděpodobnost (pro jakoukoli třídu)
dat <- dat |>
  dplyr::rowwise() |>
  dplyr::mutate(
    max_prob = if (all(is.na(c_across(all_of(prob_cols))))) NA_real_ else max(c_across(all_of(prob_cols)), na.rm = TRUE)
  ) |>
  dplyr::ungroup()

# b) margin = max_prob - druhá nejvyšší pravděpodobnost (větší → rozhodnutí je „ostřejší“)
second_max <- function(v) {
  v <- sort(v, decreasing = TRUE)
  if (length(v) >= 2) v[2] else NA_real_
}
dat <- dat |>
  dplyr::rowwise() |>
  dplyr::mutate(
    second_prob = if (all(is.na(c_across(all_of(prob_cols))))) NA_real_ else second_max(c_across(all_of(prob_cols))),
    margin = max_prob - second_prob
  ) |>
  dplyr::ungroup() |>
  dplyr::select(-second_prob)

# c) Shannon entropie (nižší → „jistější“ rozdělení)
shannon_entropy <- function(p) {
  p <- p[is.finite(p) & !is.na(p)]
  p <- p[p > 0]
  if (!length(p)) return(NA_real_)
  -sum(p * log(p))
}
dat <- dat |>
  dplyr::rowwise() |>
  dplyr::mutate(
    entropy = if (all(is.na(c_across(all_of(prob_cols))))) NA_real_ else 
      shannon_entropy(as.numeric(c_across(all_of(prob_cols))))
  ) |>
  dplyr::ungroup()

# ---- 10) „Shoda“ predikce s pravdou (zatím case-insensitive porovnání řetězců) ----
dat <- dat |>
  dplyr::mutate(
    truth_label = truth_label_raw,
    pred_label  = pred_label_raw,
    is_correct  = dplyr::case_when(
      is.na(truth_label) | is.na(pred_label) ~ NA,
      TRUE ~ tolower(truth_label) == tolower(pred_label)
    )
  )

# ---- 11) Uložení připravených dat pro další kroky ----
dir_out <- "Milovice_Mlada/validation_out"
if (!dir.exists(dir_out)) dir.create(dir_out, recursive = TRUE)

# a) tabulka bez geometrie (CSV)
dat_tbl <- dat |>
  sf::st_drop_geometry()
readr::write_csv(dat_tbl, file.path(dir_out, "field_pred_conf_join.csv"))

# b) geopackage s geometrií (snadné mapování v GIS)
out_gpkg <- file.path(dir_out, "field_pred_conf_join.gpkg")
if (file.exists(out_gpkg)) file.remove(out_gpkg)
sf::st_write(dat, out_gpkg, layer = "field_pred_conf_join", quiet = TRUE)

# ---- 12) Rychlý sumář pro kontrolu ----
message("Počet bodů: ", nrow(dat))
message("Podíl známé pravdy (EUNIS_LC ne-NA): ", round(mean(!is.na(dat$truth_label)), 3))
message("Podíl záznamů s predikcí (pred_label ne-NA): ", round(mean(!is.na(dat$pred_label)), 3))
message("Hrubá shoda (bez kalibrace názvů): ", round(mean(dat$is_correct, na.rm = TRUE), 3))

# Doporučení: projdi si 'label_check' a případně uprav 'label_map' nebo
# normalizaci 'truth_label' (např. sjednotit názvy synonym, odstranění plurálů atd.).

