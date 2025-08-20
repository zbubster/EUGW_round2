# skript MM stability maps

# load pckgs
source("scripts/knihovnik.R")
knihovnik(terra)

# load rasters
# models
files_class <- list.files(
  path = "Milovice_Mlada/EUGW_data/", 
  pattern = "CLASS\\.tif$", 
  full.names = TRUE
)
files_class <- sort(files_class)
all_class_rasters <- rast(files_class)

# buffer
buff <- vect("data/MM_buff1000.gpkg")

# crop rasters
all_class_rasters <- crop(all_class_rasters, buff, snap = "in", mask = T)

# rename rasters by years
years <- substr(basename(files_class), 18, 21)
names(all_class_rasters) <- years

## binary stability ‒ where class does not change through years
stable <- app(all_class_rasters, fun = function(x) all(x == x[1])) # all values same?
plot(stable, main="Stable (TRUE) vs Changed (FALSE)")
writeRaster(stable, "data/out/MM_stable_pixels.tif")

# numeric
freqs_stable <- freq(stable)
freqs_stable$percent <- freqs_stable$count / sum(freqs_stable$count) * 100
freqs_stable
write.table(freqs_stable, "data/out/MM_stable_freqs.csv", row.names = F)

## number of changes through years
nchanges <- app(all_class_rasters, fun = function(x) sum(diff(x) != 0))
plot(nchanges, main="Number of changes (2016–2023)")
writeRaster(nchanges, "data/out/MM_Nchanges_per_pixel.tif")

# numeric
freqs_nchanges <- freq(nchanges)
freqs_nchanges$percent <- freqs_nchanges$count / sum(freqs_nchanges$count) * 100
freqs_nchanges
write.table(freqs_nchanges, "data/out/MM_nchanges_freqs.csv", row.names = F)

