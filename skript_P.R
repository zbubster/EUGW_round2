# Praded

### data prep
## libraries
source("scripts/knihovnik.R")
knihovnik(terra, sf, caret, pROC)

## load data
pred_2023 <- rast("Praded/EUGW_data/CZ_CON_CON_78422_20230101_20231231_GTYH_CLASS.tif")
#valid_2025 <- vect("Milovice_Mlada/COMPLETE_field_data.gpkg")

## reproject to same CRS
# source("scripts/CRS.R")
# check_same_crs(list(pred_2023, valid_2025))
# laylist <- harmonize_crs(list(pred_2023, valid_2025))
# check_same_crs(laylist)

# unlist
# pred_2023 <- laylist[[1]]
# valid_2025 <- laylist[[2]]

# check
plot(pred_2023)
# plot(valid_2025, add = T)

## buffer and crop rasters
buff <- vect("data/evl_buff1000.gpkg") # old qgis created layer
buff <- project(buff, crs(pred_2023)) # to EPSG 3035

# split to separete N2K sites
# buffMM <- buff[buff$SITECODE == "CZ0214006"]
buffP <- buff[buff$SITECODE == "CZ0714077"]
# writeVector(buffP, "data/P_buff1000.gpkg")

# crop original raster by buffered N2K site
pred_2023_croped <- crop(pred_2023, buffP, snap = "in", mask = T)

# check
plot(pred_2023_croped)
plot(buffMM, add = T)
# plot(valid_2025, add = T, pch = 16, col = "steelblue", cex = 1.3)
# plot(valid_2025, add = T, pch = "+", col = "orange", cex = 1)

## convert validation data information to grassland type code
# GT_char <- c("Dry grassland",
#              "Mesic grassland",
#              "Wet and seasonally wet grassland",
#              "Alpine and sub-alpine grassland",
#              "Forest clearings",
#              "Inland salt steppes",
#              "Sparsely wooded grassland"
# )
# 
# GT_code <- 21:27

# create new col with EUGW code
# valid_2025$code_EUNIS_LC <- GT_code[match(valid_2025$EUNIS_LC, GT_char)]

### confusion matrix
## extract inforamtion for validation sites
# data <- extract(c(pred_2023_croped), valid_2025, ID=TRUE)
# data <- cbind(data, valid = valid_2025$code_EUNIS_LC)

## confusion matrix
# cm <- confusionMatrix(
#   factor(data$predicted_label, levels=21:27),
#   factor(data$valid, levels=21:27)
# )
# cm

## AUC for "Dry grasslands"
# roc_21 <- roc(data$valid == 21, data$### confid for 21)  
# auc(roc_obj)
