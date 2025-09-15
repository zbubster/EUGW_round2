# EUGW Senkey diagram

## CONFIG

### paths
dir <- "path/to/rasters"
# dir <- "../Milovice_Mlada/EUGW_data/"
# replace with path to directory with EUGW rasters of your locality

dir_out <- "output/location"
# dir_out <- "1000_MM_out"
# where should be output saved

### plotting options
min_count_for_nodes <- 1000
# minimal number of pixels per grassland cathegory (within whole raster) to be plotted
# drops EUNIS level2 cathegories with infrquent prediction

min_count_for_links <- 1000
# minimal number of pixels in transition between years to be plotted
# drop poor transitions between grassland types between years

## -- ## -- ## -- ## -- ## -- ##

## RUN
source("knihovnik.R")
source("senkey.R")
