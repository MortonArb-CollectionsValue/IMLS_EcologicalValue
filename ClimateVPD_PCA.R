#Loading in correct packages to extract
library(ggplot2)
library(rgdal); library(sp); library(raster)

#File Paths for different folders
path.dat.vpd <- "D:/Data_IMLS_Ecological_Value/Climate_Extract_Drive/vpd"

#Morton Arb Values in different folders
MortonArb_Data_vpd <- read.csv(file.path(path.dat.vpd,"0_MortonArb.csv"))

#VPD Plots
#Malus VPD Data
malus_vpd <- list.files(path = path.dat.vpd,
                         pattern = "Malus", full.names = TRUE)
vpdcols <- names(read.csv(malus_vpd[1]))
# col.char <- which(vpdcols %in% c("nativeDatabaseID", "MU.SOURCE1"))
coltype <- rep(NA, length(vpdcols))
coltype <- "character"
malus_climate_vpd <-  lapply(malus_vpd, read.csv, colClasses=coltype) %>% bind_rows()
malus_climate_vpd <- rbind.fill(malus_climate_vpd, MortonArb_Data_vpd)
malus_climate_vpd <- tidyr::separate(malus_climate_vpd, col = "species_name_acc", into=c("genus", "species"))
head(malus_climate_vpd)
tail(malus_climate_vpd)

#Quercus VDP Data
quercus_vpd <- list.files(path = path.dat.vpd,
                           pattern = "Quercus", full.names = TRUE)
vpdcols <- names(read.csv(quercus_vpd[1]))
# col.char <- which(vpdcols %in% c("nativeDatabaseID", "MU.SOURCE1"))
coltype <- rep(NA, length(vpdcols))
coltype <- "character"
quercus_climate_vpd <-  lapply(quercus_vpd, read.csv, colClasses=coltype) %>% bind_rows()
quercus_climate_vpd <- rbind.fill(quercus_climate_vpd, MortonArb_Data_vpd)
quercus_climate_vpd <- tidyr::separate(quercus_climate_vpd, col = "species_name_acc", into=c("genus", "species"))
head(quercus_climate_vpd)
tail(quercus_climate_vpd)

#Tilia VPD Data
tilia_vpd <- list.files(path = path.dat.vpd,
                         pattern = "Tilia", full.names = TRUE)
vpdcols <- names(read.csv(tilia_vpd[1]))
# col.char <- which(vpdcols %in% c("nativeDatabaseID", "MU.SOURCE1"))
coltype <- rep(NA, length(vpdcols))
coltype <- "character"
tilia_climate_vpd <-  lapply(tilia_vpd, read.csv, colClasses=coltype) %>% bind_rows()
tilia_climate_vpd <- rbind.fill(tilia_climate_vpd, MortonArb_Data_vpd)
tilia_climate_vpd <- tidyr::separate(tilia_climate_vpd, col = "species_name_acc", into=c("genus", "species"))
head(tilia_climate_vpd)
tail(tilia_climate_vpd)

#Ulmus VPD Data
ulmus_vpd <- list.files(path = path.dat.vpd,
                         pattern = "Ulmus", full.names = TRUE)
vpdcols <- names(read.csv(ulmus_vpd[1]))
# col.char <- which(vpdcols %in% c("nativeDatabaseID", "MU.SOURCE1"))
coltype <- rep(NA, length(vpdcols))
coltype <- "character"
ulmus_climate_vpd <-  lapply(ulmus_vpd, read.csv, colClasses=coltype) %>% bind_rows()
ulmus_climate_vpd <- rbind.fill(ulmus_climate_vpd, MortonArb_Data_vpd)
ulmus_climate_vpd <- tidyr::separate(ulmus_climate_vpd, col = "species_name_acc", into=c("genus", "species"))
head(ulmus_climate_vpd)
tail(ulmus_climate_vpd)
