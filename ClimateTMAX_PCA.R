#Loading in correct packages to extract
library(devtools)
# install_github("vqv/ggbiplot") #just used blank line when asked question
library(ggbiplot)
library("dplyr"); library("plyr"); library("readr")
library(ggplot2)
library(rgdal); library(sp); library(raster)
library(Hmisc)
library(data.table)

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



#PCA PLOTS
#choosing only the important traits: no categorical
important_traits <- c("tmax.ann.mean","tmax.ann.sd","tmax.ann.max","tmax.ann.min",
                      "tmax.max.mean","tmax.max.sd","tmax.max.max","tmax.max.min",
                      "tmax.min.mean","tmax.min.sd","tmax.min.max","tmax.min.min")

#Malus TMAX PCA
#converting all malus columns to numerics
sapply(malus_climate_tmax, mode)
malus_climate_tmax$tmax.ann.mean <- as.numeric(malus_climate_tmax$tmax.ann.mean)
malus_climate_tmax$tmax.ann.sd <- as.numeric(malus_climate_tmax$tmax.ann.sd)
malus_climate_tmax$tmax.ann.max <- as.numeric(malus_climate_tmax$tmax.ann.max)
malus_climate_tmax$tmax.ann.min <- as.numeric(malus_climate_tmax$tmax.ann.min)
malus_climate_tmax$tmax.max.mean <- as.numeric(malus_climate_tmax$tmax.max.mean)
malus_climate_tmax$tmax.max.sd <- as.numeric(malus_climate_tmax$tmax.max.sd)
malus_climate_tmax$tmax.max.max <- as.numeric(malus_climate_tmax$tmax.max.max)
malus_climate_tmax$tmax.max.min <- as.numeric(malus_climate_tmax$tmax.max.min)
malus_climate_tmax$tmax.min.mean <- as.numeric(malus_climate_tmax$tmax.min.mean)
malus_climate_tmax$tmax.min.sd <- as.numeric(malus_climate_tmax$tmax.min.sd)
malus_climate_tmax$tmax.min.max <- as.numeric(malus_climate_tmax$tmax.min.max)
malus_climate_tmax$tmax.min.min <- as.numeric(malus_climate_tmax$tmax.min.min)
sapply(malus_climate_tmax, mode)

#getting rid of the Malus NA values
malus_climate_tmax <- malus_climate_tmax[complete.cases(malus_climate_tmax[,important_traits]),]
#Reduction of Malus Variables: saved them to hard drive
MalusTMAX_Reduction1 <- cor(malus_climate_tmax[,important_traits])
#write.csv(MalusTMAX_Reduction1, "D:/Data_IMLS_Ecological_Value/Climate_Extract_Drive/tmax_Reductions/MalusTMAX_Reduction1.csv", row.names=TRUE)
#Malus PCA 1
malus.pca <- prcomp(malus_climate_tmax[,important_traits], center = TRUE,scale. = TRUE) 
summary(malus.pca)
malus.pca$rotation
#analysis of PCA Plots
ggbiplot(malus.pca) #basic plot


#Quercus TMAX PCA
#converting all quercus columns to numerics
sapply(quercus_climate_tmax, mode)
quercus_climate_tmax$tmax.ann.mean <- as.numeric(quercus_climate_tmax$tmax.ann.mean)
quercus_climate_tmax$tmax.ann.sd <- as.numeric(quercus_climate_tmax$tmax.ann.sd)
quercus_climate_tmax$tmax.ann.max <- as.numeric(quercus_climate_tmax$tmax.ann.max)
quercus_climate_tmax$tmax.ann.min <- as.numeric(quercus_climate_tmax$tmax.ann.min)
quercus_climate_tmax$tmax.max.mean <- as.numeric(quercus_climate_tmax$tmax.max.mean)
quercus_climate_tmax$tmax.max.sd <- as.numeric(quercus_climate_tmax$tmax.max.sd)
quercus_climate_tmax$tmax.max.max <- as.numeric(quercus_climate_tmax$tmax.max.max)
quercus_climate_tmax$tmax.max.min <- as.numeric(quercus_climate_tmax$tmax.max.min)
quercus_climate_tmax$tmax.min.mean <- as.numeric(quercus_climate_tmax$tmax.min.mean)
quercus_climate_tmax$tmax.min.sd <- as.numeric(quercus_climate_tmax$tmax.min.sd)
quercus_climate_tmax$tmax.min.max <- as.numeric(quercus_climate_tmax$tmax.min.max)
quercus_climate_tmax$tmax.min.min <- as.numeric(quercus_climate_tmax$tmax.min.min)
sapply(quercus_climate_tmax, mode)

#getting rid of the Quercus NA values
quercus_climate_tmax <- quercus_climate_tmax[complete.cases(quercus_climate_tmax[,important_traits]),]
#Reduction of Quercus Variables: saved them to hard drive
QuercusTMAX_Reduction1 <- cor(quercus_climate_tmax[,important_traits])
#write.csv(QuercusTMAX_Reduction1, "D:/Data_IMLS_Ecological_Value/Climate_Extract_Drive/tmax_Reductions/QuercusTMAX_Reduction1.csv", row.names=TRUE)
#Quercus PCA 1
quercus.pca <- prcomp(quercus_climate_tmax[,important_traits], center = TRUE,scale. = TRUE) 
summary(quercus.pca)
quercus.pca$rotation
#analysis of PCA Plots
ggbiplot(quercus.pca) #basic plot


#Tilia TMAX PCA
#converting all tilia columns to numerics
sapply(tilia_climate_tmax, mode)
tilia_climate_tmax$tmax.ann.mean <- as.numeric(tilia_climate_tmax$tmax.ann.mean)
tilia_climate_tmax$tmax.ann.sd <- as.numeric(tilia_climate_tmax$tmax.ann.sd)
tilia_climate_tmax$tmax.ann.max <- as.numeric(tilia_climate_tmax$tmax.ann.max)
tilia_climate_tmax$tmax.ann.min <- as.numeric(tilia_climate_tmax$tmax.ann.min)
tilia_climate_tmax$tmax.max.mean <- as.numeric(tilia_climate_tmax$tmax.max.mean)
tilia_climate_tmax$tmax.max.sd <- as.numeric(tilia_climate_tmax$tmax.max.sd)
tilia_climate_tmax$tmax.max.max <- as.numeric(tilia_climate_tmax$tmax.max.max)
tilia_climate_tmax$tmax.max.min <- as.numeric(tilia_climate_tmax$tmax.max.min)
tilia_climate_tmax$tmax.min.mean <- as.numeric(tilia_climate_tmax$tmax.min.mean)
tilia_climate_tmax$tmax.min.sd <- as.numeric(tilia_climate_tmax$tmax.min.sd)
tilia_climate_tmax$tmax.min.max <- as.numeric(tilia_climate_tmax$tmax.min.max)
tilia_climate_tmax$tmax.min.min <- as.numeric(tilia_climate_tmax$tmax.min.min)
sapply(tilia_climate_tmax, mode)

#getting rid of the Tilia NA values
tilia_climate_tmax <- tilia_climate_tmax[complete.cases(tilia_climate_tmax[,important_traits]),]
#Reduction of tilia Variables: saved them to hard drive
TiliaTMAX_Reduction1 <- cor(tilia_climate_tmax[,important_traits])
#write.csv(TiliaTMAX_Reduction1, "D:/Data_IMLS_Ecological_Value/Climate_Extract_Drive/tmax_Reductions/TiliaTMAX_Reduction1.csv", row.names=TRUE)
#tilia PCA 1
tilia.pca <- prcomp(tilia_climate_tmax[,important_traits], center = TRUE,scale. = TRUE) 
summary(tilia.pca)
tilia.pca$rotation
#analysis of PCA Plots
ggbiplot(tilia.pca) #basic plot


#Ulmus TMAX PCA
#converting all ulmus columns to numerics
sapply(ulmus_climate_tmax, mode)
ulmus_climate_tmax$tmax.ann.mean <- as.numeric(ulmus_climate_tmax$tmax.ann.mean)
ulmus_climate_tmax$tmax.ann.sd <- as.numeric(ulmus_climate_tmax$tmax.ann.sd)
ulmus_climate_tmax$tmax.ann.max <- as.numeric(ulmus_climate_tmax$tmax.ann.max)
ulmus_climate_tmax$tmax.ann.min <- as.numeric(ulmus_climate_tmax$tmax.ann.min)
ulmus_climate_tmax$tmax.max.mean <- as.numeric(ulmus_climate_tmax$tmax.max.mean)
ulmus_climate_tmax$tmax.max.sd <- as.numeric(ulmus_climate_tmax$tmax.max.sd)
ulmus_climate_tmax$tmax.max.max <- as.numeric(ulmus_climate_tmax$tmax.max.max)
ulmus_climate_tmax$tmax.max.min <- as.numeric(ulmus_climate_tmax$tmax.max.min)
ulmus_climate_tmax$tmax.min.mean <- as.numeric(ulmus_climate_tmax$tmax.min.mean)
ulmus_climate_tmax$tmax.min.sd <- as.numeric(ulmus_climate_tmax$tmax.min.sd)
ulmus_climate_tmax$tmax.min.max <- as.numeric(ulmus_climate_tmax$tmax.min.max)
ulmus_climate_tmax$tmax.min.min <- as.numeric(ulmus_climate_tmax$tmax.min.min)
sapply(ulmus_climate_tmax, mode)

#getting rid of the Ulmus NA values
ulmus_climate_tmax <- ulmus_climate_tmax[complete.cases(ulmus_climate_tmax[,important_traits]),]
#Reduction of Ulmus Variables: saved them to hard drive
UlmusTMAX_Reduction1 <- cor(ulmus_climate_tmax[,important_traits])
#write.csv(UlmusTMAX_Reduction1, "D:/Data_IMLS_Ecological_Value/Climate_Extract_Drive/tmax_Reductions/UlmusTMAX_Reduction1.csv", row.names=TRUE)
#Ulmus PCA 1
ulmus.pca <- prcomp(ulmus_climate_tmax[,important_traits], center = TRUE,scale. = TRUE) 
summary(ulmus.pca)
ulmus.pca$rotation
#analysis of PCA Plots
ggbiplot(ulmus.pca) #basic plot
