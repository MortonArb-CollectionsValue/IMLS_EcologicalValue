#Loading in correct packages to extract
library(ggplot2)
library(rgdal); library(sp); library(raster)

#File Paths for Soil
path.dat.soil <- "D:/Data_IMLS_Ecological_Value/Climate_Extract_Drive/soil"

#Morton Arb Values in Soils
MortonArb_Data_soil <- read.csv(file.path(path.dat.soil,"0_MortonArb.csv"))


#SOIL Plots
#Malus Soil Data
malus_soil <- list.files(path = path.dat.soil,
                         pattern = "Malus", full.names = TRUE)
soilcols <- names(read.csv(malus_soil[1]))
# col.char <- which(soilcols %in% c("nativeDatabaseID", "MU.SOURCE1"))
coltype <- rep(NA, length(soilcols))
coltype <- "character"
malus_climate_soil <-  lapply(malus_soil, read.csv, colClasses=coltype) %>% bind_rows()
malus_climate_soil <- rbind.fill(malus_climate_soil, MortonArb_Data_soil)
malus_climate_soil <- tidyr::separate(malus_climate_soil, col = "species_name_acc", into=c("genus", "species"))
head(malus_climate_soil)
tail(malus_climate_soil)

#Quercus Soil Data
quercus_soil <- list.files(path = path.dat.soil,
                           pattern = "Quercus", full.names = TRUE)
soilcols <- names(read.csv(quercus_soil[1]))
# col.char <- which(soilcols %in% c("nativeDatabaseID", "MU.SOURCE1"))
coltype <- rep(NA, length(soilcols))
coltype[col.char] <- "character"
quercus_climate_soil <-  lapply(quercus_soil, read.csv, colClasses=coltype) %>% bind_rows()
quercus_climate_soil <- rbind.fill(quercus_climate_soil, MortonArb_Data_soil)
quercus_climate_soil <- tidyr::separate(quercus_climate_soil, col = "species_name_acc", into=c("genus", "species"))
head(quercus_climate_soil)
tail(quercus_climate_soil)

#Tilia Soil Data
tilia_soil <- list.files(path = path.dat.soil,
                         pattern = "Tilia", full.names = TRUE)
soilcols <- names(read.csv(tilia_soil[1]))
# col.char <- which(soilcols %in% c("nativeDatabaseID", "MU.SOURCE1"))
coltype <- rep(NA, length(soilcols))
coltype[col.char] <- "character"
tilia_climate_soil <-  lapply(tilia_soil, read.csv, colClasses=coltype) %>% bind_rows()
tilia_climate_soil <- rbind.fill(tilia_climate_soil, MortonArb_Data_soil)
tilia_climate_soil <- tidyr::separate(tilia_climate_soil, col = "species_name_acc", into=c("genus", "species"))
head(tilia_climate_soil)
tail(tilia_climate_soil)

#Ulmus Soil Data
ulmus_soil <- list.files(path = path.dat.soil,
                         pattern = "Ulmus", full.names = TRUE)
soilcols <- names(read.csv(ulmus_soil[1]))
# col.char <- which(soilcols %in% c("nativeDatabaseID", "MU.SOURCE1"))
coltype <- rep(NA, length(soilcols))
coltype[col.char] <- "character"
ulmus_climate_soil <-  lapply(ulmus_soil, read.csv, colClasses=coltype) %>% bind_rows()
ulmus_climate_soil <- rbind.fill(ulmus_climate_soil, MortonArb_Data_soil)
ulmus_climate_soil <- tidyr::separate(ulmus_climate_soil, col = "species_name_acc", into=c("genus", "species"))
head(ulmus_climate_soil)
tail(ulmus_climate_soil)