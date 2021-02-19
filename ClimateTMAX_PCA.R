#Loading in correct packages to extract
library(ggplot2)
library(rgdal); library(sp); library(raster)

#File Paths for different folders
path.dat.tmax <- "D:/Data_IMLS_Ecological_Value/Climate_Extract_Drive/tmax"

#Morton Arb Values in different folders
MortonArb_Data_tmax <- read.csv(file.path(path.dat.tmax,"0_MortonArb.csv"))


#TMAX Plots
#Malus TMAX Data
malus_tmax <- list.files(path = path.dat.tmax,
                         pattern = "Malus", full.names = TRUE)
tmaxcols <- names(read.csv(malus_tmax[1]))
# col.char <- which(tmaxcols %in% c("nativeDatabaseID", "MU.SOURCE1"))
coltype <- rep(NA, length(tmaxcols))
coltype <- "character"
malus_climate_tmax <-  lapply(malus_tmax, read.csv, colClasses=coltype) %>% bind_rows()
malus_climate_tmax <- rbind.fill(malus_climate_tmax, MortonArb_Data_tmax)
malus_climate_tmax <- tidyr::separate(malus_climate_tmax, col = "species_name_acc", into=c("genus", "species"))
head(malus_climate_tmax)
tail(malus_climate_tmax)

#Quercus TMAX Data
quercus_tmax <- list.files(path = path.dat.tmax,
                         pattern = "Quercus", full.names = TRUE)
tmaxcols <- names(read.csv(quercus_tmax[1]))
# col.char <- which(tmaxcols %in% c("nativeDatabaseID", "MU.SOURCE1"))
coltype <- rep(NA, length(tmaxcols))
coltype <- "character"
quercus_climate_tmax <-  lapply(quercus_tmax, read.csv, colClasses=coltype) %>% bind_rows()
quercus_climate_tmax <- rbind.fill(quercus_climate_tmax, MortonArb_Data_tmax)
quercus_climate_tmax <- tidyr::separate(quercus_climate_tmax, col = "species_name_acc", into=c("genus", "species"))
head(quercus_climate_tmax)
tail(quercus_climate_tmax)

#Tilia TMAX Data
tilia_tmax <- list.files(path = path.dat.tmax,
                         pattern = "Tilia", full.names = TRUE)
tmaxcols <- names(read.csv(tilia_tmax[1]))
# col.char <- which(tmaxcols %in% c("nativeDatabaseID", "MU.SOURCE1"))
coltype <- rep(NA, length(tmaxcols))
coltype <- "character"
tilia_climate_tmax <-  lapply(tilia_tmax, read.csv, colClasses=coltype) %>% bind_rows()
tilia_climate_tmax <- rbind.fill(tilia_climate_tmax, MortonArb_Data_tmax)
tilia_climate_tmax <- tidyr::separate(tilia_climate_tmax, col = "species_name_acc", into=c("genus", "species"))
head(tilia_climate_tmax)
tail(tilia_climate_tmax)

#Ulmus TMAX Data
ulmus_tmax <- list.files(path = path.dat.tmax,
                         pattern = "Ulmus", full.names = TRUE)
tmaxcols <- names(read.csv(ulmus_tmax[1]))
# col.char <- which(tmaxcols %in% c("nativeDatabaseID", "MU.SOURCE1"))
coltype <- rep(NA, length(tmaxcols))
coltype <- "character"
ulmus_climate_tmax <-  lapply(ulmus_tmax, read.csv, colClasses=coltype) %>% bind_rows()
ulmus_climate_tmax <- rbind.fill(ulmus_climate_tmax, MortonArb_Data_tmax)
ulmus_climate_tmax <- tidyr::separate(ulmus_climate_tmax, col = "species_name_acc", into=c("genus", "species"))
head(ulmus_climate_tmax)
tail(ulmus_climate_tmax)
