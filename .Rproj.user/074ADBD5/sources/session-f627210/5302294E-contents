### MM transition matrix and sankey diagram

source("scripts/knihovnik.R")
co <- c("terra", "dplyr", "stringr", "tidyr", "networkD3", "htmlwidgets")
knihovnik(co)

# config
dir <- "Milovice_Mlada/EUGW_data/"
min_count_for_sankey <- 100

# load resters and sort them
files_class <- list.files(
  path = dir,
  pattern = "CLASS\\.tif$",
  full.names = TRUE
) %>% sort()

if (length(files_class) < 2) stop("ERROR: Less than 2 rasters found. At least 2 needed for transition matrix!")

# extract year from raster file name
extract_year <- function(x) {
  m <- str_match(basename(x), "_(\\d{8})_")
  if (is.na(m[1,2])) NA_character_ else substr(m[1,2], 1, 4)
}
years <- vapply(files_class, extract_year, character(1))
if (any(is.na(years))) stop("For some files year identification failed!")

# create raster stack
r_stack <- rast(files_class)
names(r_stack) <- years

# pairwise transition matrix between years
pairwise_list <- vector("list", nlyr(r_stack) - 1)

for (i in 1:(nlyr(r_stack) - 1)) {
  r_from <- r_stack[[i]]
  r_to   <- r_stack[[i + 1]]
  
  # spojíme do dvouvrstvého rastru a pojmenujeme
  rr <- c(r_from, r_to)
  names(rr) <- c("from_class", "to_class")
  
  # vyexportujeme dvojice hodnot pixelů, bez NA
  df <- terra::as.data.frame(rr, na.rm = TRUE)
  
  # omezíme na 21:27 (pokud chceš)
  df <- df |>
    filter(from_class %in% 21:27, to_class %in% 21:27)
  
  # spočítáme četnosti
  ct <- df |>
    count(from_class, to_class, name = "count") |>
    mutate(year_from = names(r_stack)[i],
           year_to   = names(r_stack)[i + 1])
  
  pairwise_list[[i]] <- ct
}

transitions_df <- bind_rows(pairwise_list)

# --- NODES + LINKS PRO SANKEY ---
src_names <- paste0(transitions_df$year_from, "_", transitions_df$from_class)
tgt_names <- paste0(transitions_df$year_to,   "_", transitions_df$to_class)
node_names <- unique(c(src_names, tgt_names))

nodes <- data.frame(name = node_names, stringsAsFactors = FALSE)
nodes$year  <- as.integer(substr(nodes$name, 1, 4))
nodes$class <- as.integer(sub(".*_(\\d+)$", "\\1", nodes$name))
# Seřadit: rok vzestupně, třída podle 21..27 (i když některé nemusí být přítomné)
desired_order <- 21:27
nodes$class_rank <- match(nodes$class, desired_order)
nodes <- nodes[order(nodes$year, nodes$class_rank, nodes$class), ]
nodes$id <- seq_len(nrow(nodes)) - 1
nodes$group <- as.factor(nodes$year)  # pro barvy
nodes <- nodes[, c("name", "id", "group")]

# --- LINKS: mapování na ID ---
links <- transitions_df |>
  mutate(source_name = paste0(year_from, "_", from_class),
         target_name = paste0(year_to,   "_", to_class)) |>
  left_join(nodes[, c("name","id")], by = c("source_name" = "name")) |>
  rename(source = id) |>
  left_join(nodes[, c("name","id")], by = c("target_name" = "name")) |>
  rename(target = id) |>
  transmute(
    source, target,
    value = count,
    year_from, year_to, from_class, to_class
  )


if (min_count_for_sankey > 0) {
  links <- links |> filter(value >= min_count_for_sankey)
}

# --- Barevná škála podle roku (doména = unikátní roky) ---
years_vec <- sort(unique(nodes$group))
# vyrobíme JS scale s 8 barvami (případně přidej další)
col_range <- c("#1f77b4","#ff7f0e","#2ca02c","#d62728",
               "#9467bd","#8c564b","#e377c2","#7f7f7f")
# když je víc roků než barev, zrecykluje se
colourScale_js <- htmlwidgets::JS(
  sprintf(
    "d3.scaleOrdinal().domain(%s).range(%s)",
    jsonlite::toJSON(as.character(years_vec)),
    jsonlite::toJSON(rep(col_range, length.out = length(years_vec)))
  )
)

# --- Vykreslení: iterations=0 zachová vstupní pořadí uvnitř sloupce ---
sankey <- networkD3::sankeyNetwork(
  Links = links,
  Nodes = nodes,
  Source = "source",
  Target = "target",
  Value  = "value",
  NodeID = "name",
  NodeGroup = "group",       # barvy podle roku
  colourScale = colourScale_js,
  fontSize = 12,
  nodeWidth = 26,
  nodePadding = 10,
  sinksRight = FALSE,
  iterations = 0             # důležité pro zachování pořadí (21:27)
)

# --- Přidat popisky roků nad sloupce (JS po vykreslení) ---
sankey <- htmlwidgets::onRender(
  sankey,
  "
  function(el, x) {
    var svg   = d3.select(el).select('svg');
    var nodes = d3.select(el).selectAll('.node').data();  // data s pozicemi
    if (!nodes || !nodes.length) return;

    // Získat unikátní roky a jejich x-pozice (sloupce)
    var cols = {};
    nodes.forEach(function(d){
      var year = d.name.split('_')[0];
      if(!cols[year]) cols[year] = [];
      cols[year].push(d.x);
    });

    // Vyrob popisek pro každý rok nad jeho sloupcem (x = min x v daném roce)
    Object.keys(cols).sort().forEach(function(year){
      var xs = cols[year];
      var xMin = d3.min(xs);
      // šířka uzlu je d.dx – vezmeme první uzel s daným rokem pro odhad středu
      var nd = nodes.find(function(n){ return n.name.indexOf(year + '_') === 0; });
      var dx = nd && nd.dx ? nd.dx : 0;
      var xCenter = xMin + dx/2 + 2;

      svg.append('text')
        .attr('x', xCenter)
        .attr('y', 14)                 // výška popisku
        .attr('text-anchor', 'middle')
        .style('font-size', '12px')
        .style('font-weight', '600')
        .text(year);
    });
  }
  "
)

# Zobrazit a uložit
sankey
saveWidget(sankey, file = "sankey_grasslands_2016_2023.html", selfcontained = TRUE)

# --- 5) (Volitelné) Agregace na jednu velkou matici pro každý pár let zvlášť i export ---
# Export všech párových matic jako CSV
dir.create("transition_csv", showWarnings = FALSE)
transitions_df %>%
  group_by(year_from, year_to) %>%
  arrange(from_class, to_class) %>%
  write.csv("transition_csv/ALL_transitions_long.csv", row.names = FALSE)

# Pro rychlou kontrolu: jedna maticová tabulka pro 2016->2023
if (all(c("2016","2023") %in% years)) {
  i2016 <- which(years == "2016")
  i2023 <- which(years == "2023")
  mat_16_23 <- crosstab(r_stack[[i2016]], r_stack[[i2023]])
  write.csv(as.data.frame(mat_16_23), "transition_csv/transition_2016_to_2023.csv", row.names = FALSE)
}
