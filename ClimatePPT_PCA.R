#Loading in correct packages to extract
library(ggplot2)
library(rgdal); library(sp); library(raster)

#File Paths for PPT
path.dat.ppt <- "D:/Data_IMLS_Ecological_Value/Climate_Extract_Drive/ppt"  

#Morton Arb Values in PPT
MortonArb_Data_ppt <- read.csv(file.path(path.dat.ppt,"0_MortonArb.csv"))

#PPT Plots
#Malus PPT Data
malus_ppt <- list.files(path = path.dat.ppt,
                         pattern = "Malus", full.names = TRUE)
pptcols <- names(read.csv(malus_ppt[1]))
#col.char <- which(pptcols %in% c("nativeDatabaseID", "MU.SOURCE1"))
coltype <- rep(NA, length(pptcols))
coltype[col.char] <- "character"
malus_climate_ppt <-  lapply(malus_ppt, read.csv, colClasses=coltype) %>% bind_rows()
malus_climate_ppt <- rbind.fill(malus_climate_ppt, MortonArb_Data_ppt)
malus_climate_ppt <- tidyr::separate(malus_climate_ppt, col = "species_name_acc", into=c("genus", "species"))
head(malus_climate_ppt)
tail(malus_climate_ppt)

#Quercus PPT Data
quercus_ppt <- list.files(path = path.dat.ppt,
                        pattern = "Quercus", full.names = TRUE)
climatecols <- names(read.csv(quercus_ppt[1]))
# col.char <- which(pptcols %in% c("nativeDatabaseID", "MU.SOURCE1"))
coltype <- rep(NA, length(climatecols))
coltype[col.char] <- "character"
quercus_climate_ppt <-  lapply(quercus_ppt, read.csv, colClasses=coltype) %>% bind_rows()
quercus_climate_ppt <- rbind.fill(quercus_climate_ppt, MortonArb_Data_ppt)
quercus_climate_ppt <- tidyr::separate(quercus_climate_ppt, col = "species_name_acc", into=c("genus", "species"))
head(quercus_climate_ppt)
tail(quercus_climate_ppt)

#Tilia PPT Data
tilia_ppt <- list.files(path = path.dat.ppt,
                          pattern = "Tilia", full.names = TRUE)
pptcols <- names(read.csv(tilia_ppt[1]))
# col.char <- which(pptcols %in% c("nativeDatabaseID", "MU.SOURCE1"))
coltype <- rep(NA, length(pptcols))
coltype[col.char] <- "character"
tilia_climate_ppt <-  lapply(tilia_ppt, read.csv, colClasses=coltype) %>% bind_rows()
tilia_climate_ppt <- rbind.fill(tilia_climate_ppt, MortonArb_Data_ppt)
tilia_climate_ppt <- tidyr::separate(tilia_climate_ppt, col = "species_name_acc", into=c("genus", "species"))
head(tilia_climate_ppt)
tail(tilia_climate_ppt)

#Ulmus PPT Data
ulmus_ppt <- list.files(path = path.dat.ppt,
                        pattern = "Ulmus", full.names = TRUE)
pptcols <- names(read.csv(ulmus_ppt[1]))
# col.char <- which(pptcols %in% c("nativeDatabaseID", "MU.SOURCE1"))
coltype <- rep(NA, length(pptcols))
coltype[col.char] <- "character"
ulmus_climate_ppt <-  lapply(ulmus_ppt, read.csv, colClasses=coltype) %>% bind_rows()
ulmus_climate_ppt <- rbind.fill(ulmus_climate_ppt, MortonArb_Data_ppt)
ulmus_climate_ppt <- tidyr::separate(ulmus_climate_ppt, col = "species_name_acc", into=c("genus", "species"))
head(ulmus_climate_ppt)
tail(ulmus_climate_ppt)