#Loading in correct packages to extract
library(ggplot2)
library(rgdal); library(sp); library(raster)

#File Paths for different folders
path.dat.tmin <- "D:/Data_IMLS_Ecological_Value/Climate_Extract_Drive/tmin"

#Morton Arb Values in different folders
MortonArb_Data_tmin <- read.csv(file.path(path.dat.tmin,"0_MortonArb.csv"))

#TMIN Plots
#Malus TMIN Data
malus_tmin <- list.files(path = path.dat.tmin,
                         pattern = "Malus", full.names = TRUE)
tmincols <- names(read.csv(malus_tmin[1]))
# col.char <- which(tmincols %in% c("nativeDatabaseID", "MU.SOURCE1"))
coltype <- rep(NA, length(tmincols))
coltype <- "character"
malus_climate_tmin <-  lapply(malus_tmin, read.csv, colClasses=coltype) %>% bind_rows()
malus_climate_tmin <- rbind.fill(malus_climate_tmin, MortonArb_Data_tmin)
malus_climate_tmin <- tidyr::separate(malus_climate_tmin, col = "species_name_acc", into=c("genus", "species"))
head(malus_climate_tmin)
tail(malus_climate_tmin)

#Quercus TMIN Data
quercus_tmin <- list.files(path = path.dat.tmin,
                           pattern = "Quercus", full.names = TRUE)
tmincols <- names(read.csv(quercus_tmin[1]))
# col.char <- which(tmincols %in% c("nativeDatabaseID", "MU.SOURCE1"))
coltype <- rep(NA, length(tmincols))
coltype <- "character"
quercus_climate_tmin <-  lapply(quercus_tmin, read.csv, colClasses=coltype) %>% bind_rows()
quercus_climate_tmin <- rbind.fill(quercus_climate_tmin, MortonArb_Data_tmin)
quercus_climate_tmin <- tidyr::separate(quercus_climate_tmin, col = "species_name_acc", into=c("genus", "species"))
head(quercus_climate_tmin)
tail(quercus_climate_tmin)

#Tilia TMIN Data
tilia_tmin <- list.files(path = path.dat.tmin,
                         pattern = "Tilia", full.names = TRUE)
tmincols <- names(read.csv(tilia_tmin[1]))
# col.char <- which(tmincols %in% c("nativeDatabaseID", "MU.SOURCE1"))
coltype <- rep(NA, length(tmincols))
coltype <- "character"
tilia_climate_tmin <-  lapply(tilia_tmin, read.csv, colClasses=coltype) %>% bind_rows()
tilia_climate_tmin <- rbind.fill(tilia_climate_tmin, MortonArb_Data_tmin)
tilia_climate_tmin <- tidyr::separate(tilia_climate_tmin, col = "species_name_acc", into=c("genus", "species"))
head(tilia_climate_tmin)
tail(tilia_climate_tmin)

#Ulmus TMIN Data
ulmus_tmin <- list.files(path = path.dat.tmin,
                         pattern = "Ulmus", full.names = TRUE)
tmincols <- names(read.csv(ulmus_tmin[1]))
# col.char <- which(tmincols %in% c("nativeDatabaseID", "MU.SOURCE1"))
coltype <- rep(NA, length(tmincols))
coltype <- "character"
ulmus_climate_tmin <-  lapply(ulmus_tmin, read.csv, colClasses=coltype) %>% bind_rows()
ulmus_climate_tmin <- rbind.fill(ulmus_climate_tmin, MortonArb_Data_tmin)
ulmus_climate_tmin <- tidyr::separate(ulmus_climate_tmin, col = "species_name_acc", into=c("genus", "species"))
head(ulmus_climate_tmin)
tail(ulmus_climate_tmin)
