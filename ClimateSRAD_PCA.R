#Loading in correct packages to extract
library(ggplot2)
library(rgdal); library(sp); library(raster)

#File Paths for different folders
path.dat.srad <- "D:/Data_IMLS_Ecological_Value/Climate_Extract_Drive/srad"

#Morton Arb Values in different folders
MortonArb_Data_srad <- read.csv(file.path(path.dat.srad,"0_MortonArb.csv"))


#SRAD Plots
#Malus SRAD Data
malus_srad <- list.files(path = path.dat.srad,
                         pattern = "Malus", full.names = TRUE)
sradcols <- names(read.csv(malus_srad[1]))
# col.char <- which(sradcols %in% c("nativeDatabaseID", "MU.SOURCE1"))
coltype <- rep(NA, length(sradcols))
coltype <- "character"
malus_climate_srad <-  lapply(malus_srad, read.csv, colClasses=coltype) %>% bind_rows()
malus_climate_srad <- rbind.fill(malus_climate_srad, MortonArb_Data_srad)
malus_climate_srad <- tidyr::separate(malus_climate_srad, col = "species_name_acc", into=c("genus", "species"))
head(malus_climate_srad)
tail(malus_climate_srad)

#Quercus SRAD Data
quercus_srad <- list.files(path = path.dat.srad,
                         pattern = "Quercus", full.names = TRUE)
sradcols <- names(read.csv(quercus_srad[1]))
# col.char <- which(sradcols %in% c("nativeDatabaseID", "MU.SOURCE1"))
coltype <- rep(NA, length(sradcols))
coltype <- "character"
quercus_climate_srad <-  lapply(quercus_srad, read.csv, colClasses=coltype) %>% bind_rows()
quercus_climate_srad <- rbind.fill(quercus_climate_srad, MortonArb_Data_srad)
quercus_climate_srad <- tidyr::separate(quercus_climate_srad, col = "species_name_acc", into=c("genus", "species"))
head(quercus_climate_srad)
tail(quercus_climate_srad)

#Tilia SRAD Data
tilia_srad <- list.files(path = path.dat.srad,
                           pattern = "Tilia", full.names = TRUE)
sradcols <- names(read.csv(tilia_srad[1]))
# col.char <- which(sradcols %in% c("nativeDatabaseID", "MU.SOURCE1"))
coltype <- rep(NA, length(sradcols))
coltype <- "character"
tilia_climate_srad <-  lapply(tilia_srad, read.csv, colClasses=coltype) %>% bind_rows()
tilia_climate_srad <- rbind.fill(tilia_climate_srad, MortonArb_Data_srad)
tilia_climate_srad <- tidyr::separate(tilia_climate_srad, col = "species_name_acc", into=c("genus", "species"))
head(tilia_climate_srad)
tail(tilia_climate_srad)

#Ulmus SRAD Data
ulmus_srad <- list.files(path = path.dat.srad,
                           pattern = "Ulmus", full.names = TRUE)
sradcols <- names(read.csv(ulmus_srad[1]))
# col.char <- which(sradcols %in% c("nativeDatabaseID", "MU.SOURCE1"))
coltype <- rep(NA, length(sradcols))
coltype <- "character"
ulmus_climate_srad <-  lapply(ulmus_srad, read.csv, colClasses=coltype) %>% bind_rows()
ulmus_climate_srad <- rbind.fill(ulmus_climate_srad, MortonArb_Data_srad)
ulmus_climate_srad <- tidyr::separate(ulmus_climate_srad, col = "species_name_acc", into=c("genus", "species"))
head(ulmus_climate_srad)
tail(ulmus_climate_srad)
