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
malus_climate_tmax_original <-  lapply(malus_tmax, read.csv, colClasses=coltype) %>% bind_rows()
malus_climate_tmax_original <- rbind.fill(malus_climate_tmax_original, MortonArb_Data_tmax)
malus_climate_tmax_original <- tidyr::separate(malus_climate_tmax_original, col = "species_name_acc", into=c("genus", "species"))
head(malus_climate_tmax_original)
tail(malus_climate_tmax_original)

#Quercus TMAX Data
quercus_tmax <- list.files(path = path.dat.tmax,
                         pattern = "Quercus", full.names = TRUE)
tmaxcols <- names(read.csv(quercus_tmax[1]))
# col.char <- which(tmaxcols %in% c("nativeDatabaseID", "MU.SOURCE1"))
coltype <- rep(NA, length(tmaxcols))
coltype <- "character"
quercus_climate_tmax_original <-  lapply(quercus_tmax, read.csv, colClasses=coltype) %>% bind_rows()
quercus_climate_tmax_original <- rbind.fill(quercus_climate_tmax_original, MortonArb_Data_tmax)
quercus_climate_tmax_original <- tidyr::separate(quercus_climate_tmax_original, col = "species_name_acc", into=c("genus", "species"))
head(quercus_climate_tmax_original)
tail(quercus_climate_tmax_original)

#Tilia TMAX Data
tilia_tmax <- list.files(path = path.dat.tmax,
                         pattern = "Tilia", full.names = TRUE)
tmaxcols <- names(read.csv(tilia_tmax[1]))
# col.char <- which(tmaxcols %in% c("nativeDatabaseID", "MU.SOURCE1"))
coltype <- rep(NA, length(tmaxcols))
coltype <- "character"
tilia_climate_tmax_original <-  lapply(tilia_tmax, read.csv, colClasses=coltype) %>% bind_rows()
tilia_climate_tmax_original <- rbind.fill(tilia_climate_tmax_original, MortonArb_Data_tmax)
tilia_climate_tmax_original <- tidyr::separate(tilia_climate_tmax_original, col = "species_name_acc", into=c("genus", "species"))
head(tilia_climate_tmax_original)
tail(tilia_climate_tmax_original)

#Ulmus TMAX Data
ulmus_tmax <- list.files(path = path.dat.tmax,
                         pattern = "Ulmus", full.names = TRUE)
tmaxcols <- names(read.csv(ulmus_tmax[1]))
# col.char <- which(tmaxcols %in% c("nativeDatabaseID", "MU.SOURCE1"))
coltype <- rep(NA, length(tmaxcols))
coltype <- "character"
ulmus_climate_tmax_original <-  lapply(ulmus_tmax, read.csv, colClasses=coltype) %>% bind_rows()
ulmus_climate_tmax_original <- rbind.fill(ulmus_climate_tmax_original, MortonArb_Data_tmax)
ulmus_climate_tmax_original <- tidyr::separate(ulmus_climate_tmax_original, col = "species_name_acc", into=c("genus", "species"))
head(ulmus_climate_tmax_original)
tail(ulmus_climate_tmax_original)



#PCA PLOTS
  #made each data frame end with original to indicate before NA values were removed
  #used to combine climate and soil data
#choosing only the important traits: no categorical
important_traits_tmax <- c("tmax.ann.mean","tmax.ann.sd","tmax.ann.max","tmax.ann.min",
                      "tmax.max.mean","tmax.max.sd","tmax.max.max","tmax.max.min",
                      "tmax.min.mean","tmax.min.sd","tmax.min.max","tmax.min.min")

#for reduction at sums of each trait and then took least sum from each category (ann, min, max)
important_traits_tmax2 <- c("tmax.ann.sd","tmax.max.sd", "tmax.min.sd")

#Malus TMAX PCA
#converting all malus columns to numerics
sapply(malus_climate_tmax_original, mode)
malus_climate_tmax_original$tmax.ann.mean <- as.numeric(malus_climate_tmax_original$tmax.ann.mean)
malus_climate_tmax_original$tmax.ann.sd <- as.numeric(malus_climate_tmax_original$tmax.ann.sd)
malus_climate_tmax_original$tmax.ann.max <- as.numeric(malus_climate_tmax_original$tmax.ann.max)
malus_climate_tmax_original$tmax.ann.min <- as.numeric(malus_climate_tmax_original$tmax.ann.min)
malus_climate_tmax_original$tmax.max.mean <- as.numeric(malus_climate_tmax_original$tmax.max.mean)
malus_climate_tmax_original$tmax.max.sd <- as.numeric(malus_climate_tmax_original$tmax.max.sd)
malus_climate_tmax_original$tmax.max.max <- as.numeric(malus_climate_tmax_original$tmax.max.max)
malus_climate_tmax_original$tmax.max.min <- as.numeric(malus_climate_tmax_original$tmax.max.min)
malus_climate_tmax_original$tmax.min.mean <- as.numeric(malus_climate_tmax_original$tmax.min.mean)
malus_climate_tmax_original$tmax.min.sd <- as.numeric(malus_climate_tmax_original$tmax.min.sd)
malus_climate_tmax_original$tmax.min.max <- as.numeric(malus_climate_tmax_original$tmax.min.max)
malus_climate_tmax_original$tmax.min.min <- as.numeric(malus_climate_tmax_original$tmax.min.min)
sapply(malus_climate_tmax_original, mode)

#getting rid of the Malus NA values
malus_climate_tmax <- malus_climate_tmax_original[complete.cases(malus_climate_tmax_original[,important_traits_tmax]),]
#Reduction of Malus Variables: saved them to hard drive
MalusTMAX_Reduction1 <- cor(malus_climate_tmax[,important_traits_tmax])
MalusTMAX_Reduction2 <- cor(malus_climate_tmax[,important_traits_tmax2])
#write.csv(MalusTMAX_Reduction1, "D:/Data_IMLS_Ecological_Value/Climate_Extract_Drive/tmax_Reductions/MalusTMAX_Reduction1.csv", row.names=TRUE)
#write.csv(MalusTMAX_Reduction2, "D:/Data_IMLS_Ecological_Value/Climate_Extract_Drive/tmax_Reductions/MalusTMAX_Reduction2.csv", row.names=TRUE)
#Malus PCA 1
malus.pca <- prcomp(malus_climate_tmax[,important_traits_tmax], center = TRUE,scale. = TRUE) 
summary(malus.pca)
malus.pca$rotation
#analysis of PCA Plots
ggbiplot(malus.pca) #basic plot
#Malus PCA 2
malus.pca <- prcomp(malus_climate_tmax[,important_traits_tmax2], center = TRUE,scale. = TRUE) 
summary(malus.pca)
malus.pca$rotation
#analysis of PCA Plots
ggbiplot(malus.pca) #basic plot


#Quercus TMAX PCA
#converting all quercus columns to numerics
sapply(quercus_climate_tmax_original, mode)
quercus_climate_tmax_original$tmax.ann.mean <- as.numeric(quercus_climate_tmax_original$tmax.ann.mean)
quercus_climate_tmax_original$tmax.ann.sd <- as.numeric(quercus_climate_tmax_original$tmax.ann.sd)
quercus_climate_tmax_original$tmax.ann.max <- as.numeric(quercus_climate_tmax_original$tmax.ann.max)
quercus_climate_tmax_original$tmax.ann.min <- as.numeric(quercus_climate_tmax_original$tmax.ann.min)
quercus_climate_tmax_original$tmax.max.mean <- as.numeric(quercus_climate_tmax_original$tmax.max.mean)
quercus_climate_tmax_original$tmax.max.sd <- as.numeric(quercus_climate_tmax_original$tmax.max.sd)
quercus_climate_tmax_original$tmax.max.max <- as.numeric(quercus_climate_tmax_original$tmax.max.max)
quercus_climate_tmax_original$tmax.max.min <- as.numeric(quercus_climate_tmax_original$tmax.max.min)
quercus_climate_tmax_original$tmax.min.mean <- as.numeric(quercus_climate_tmax_original$tmax.min.mean)
quercus_climate_tmax_original$tmax.min.sd <- as.numeric(quercus_climate_tmax_original$tmax.min.sd)
quercus_climate_tmax_original$tmax.min.max <- as.numeric(quercus_climate_tmax_original$tmax.min.max)
quercus_climate_tmax_original$tmax.min.min <- as.numeric(quercus_climate_tmax_original$tmax.min.min)
sapply(quercus_climate_tmax_original, mode)

#getting rid of the Quercus NA values
quercus_climate_tmax <- quercus_climate_tmax_original[complete.cases(quercus_climate_tmax_original[,important_traits_tmax]),]
#Reduction of Quercus Variables: saved them to hard drive
QuercusTMAX_Reduction1 <- cor(quercus_climate_tmax[,important_traits_tmax])
QuercusTMAX_Reduction2 <- cor(quercus_climate_tmax[,important_traits_tmax2])
#write.csv(QuercusTMAX_Reduction1, "D:/Data_IMLS_Ecological_Value/Climate_Extract_Drive/tmax_Reductions/QuercusTMAX_Reduction1.csv", row.names=TRUE)
#write.csv(QuercusTMAX_Reduction2, "D:/Data_IMLS_Ecological_Value/Climate_Extract_Drive/tmax_Reductions/QuercusTMAX_Reduction2.csv", row.names=TRUE)
#Quercus PCA 1
quercus.pca <- prcomp(quercus_climate_tmax[,important_traits_tmax], center = TRUE,scale. = TRUE) 
summary(quercus.pca)
quercus.pca$rotation
#analysis of PCA Plots
ggbiplot(quercus.pca) #basic plot
#Quercus PCA 2
quercus.pca <- prcomp(quercus_climate_tmax[,important_traits_tmax2], center = TRUE,scale. = TRUE) 
summary(quercus.pca)
quercus.pca$rotation
#analysis of PCA Plots
ggbiplot(quercus.pca) #basic plot



#Tilia TMAX PCA
#converting all tilia columns to numerics
sapply(tilia_climate_tmax_original, mode)
tilia_climate_tmax_original$tmax.ann.mean <- as.numeric(tilia_climate_tmax_original$tmax.ann.mean)
tilia_climate_tmax_original$tmax.ann.sd <- as.numeric(tilia_climate_tmax_original$tmax.ann.sd)
tilia_climate_tmax_original$tmax.ann.max <- as.numeric(tilia_climate_tmax_original$tmax.ann.max)
tilia_climate_tmax_original$tmax.ann.min <- as.numeric(tilia_climate_tmax_original$tmax.ann.min)
tilia_climate_tmax_original$tmax.max.mean <- as.numeric(tilia_climate_tmax_original$tmax.max.mean)
tilia_climate_tmax_original$tmax.max.sd <- as.numeric(tilia_climate_tmax_original$tmax.max.sd)
tilia_climate_tmax_original$tmax.max.max <- as.numeric(tilia_climate_tmax_original$tmax.max.max)
tilia_climate_tmax_original$tmax.max.min <- as.numeric(tilia_climate_tmax_original$tmax.max.min)
tilia_climate_tmax_original$tmax.min.mean <- as.numeric(tilia_climate_tmax_original$tmax.min.mean)
tilia_climate_tmax_original$tmax.min.sd <- as.numeric(tilia_climate_tmax_original$tmax.min.sd)
tilia_climate_tmax_original$tmax.min.max <- as.numeric(tilia_climate_tmax_original$tmax.min.max)
tilia_climate_tmax_original$tmax.min.min <- as.numeric(tilia_climate_tmax_original$tmax.min.min)
sapply(tilia_climate_tmax_original, mode)

#getting rid of the Tilia NA values
tilia_climate_tmax <- tilia_climate_tmax_original[complete.cases(tilia_climate_tmax_original[,important_traits_tmax]),]
#Reduction of tilia Variables: saved them to hard drive
TiliaTMAX_Reduction1 <- cor(tilia_climate_tmax[,important_traits_tmax])
TiliaTMAX_Reduction2 <- cor(tilia_climate_tmax[,important_traits_tmax2])
#write.csv(TiliaTMAX_Reduction1, "D:/Data_IMLS_Ecological_Value/Climate_Extract_Drive/tmax_Reductions/TiliaTMAX_Reduction1.csv", row.names=TRUE)
#write.csv(TiliaTMAX_Reduction2, "D:/Data_IMLS_Ecological_Value/Climate_Extract_Drive/tmax_Reductions/TiliaTMAX_Reduction2.csv", row.names=TRUE)
#tilia PCA 1
tilia.pca <- prcomp(tilia_climate_tmax[,important_traits_tmax], center = TRUE,scale. = TRUE) 
summary(tilia.pca)
tilia.pca$rotation
#analysis of PCA Plots
ggbiplot(tilia.pca) #basic plot
#tilia PCA 2
tilia.pca <- prcomp(tilia_climate_tmax[,important_traits_tmax2], center = TRUE,scale. = TRUE) 
summary(tilia.pca)
tilia.pca$rotation
#analysis of PCA Plots
ggbiplot(tilia.pca) #basic plot


#Ulmus TMAX PCA
#converting all ulmus columns to numerics
sapply(ulmus_climate_tmax_original, mode)
ulmus_climate_tmax_original$tmax.ann.mean <- as.numeric(ulmus_climate_tmax_original$tmax.ann.mean)
ulmus_climate_tmax_original$tmax.ann.sd <- as.numeric(ulmus_climate_tmax_original$tmax.ann.sd)
ulmus_climate_tmax_original$tmax.ann.max <- as.numeric(ulmus_climate_tmax_original$tmax.ann.max)
ulmus_climate_tmax_original$tmax.ann.min <- as.numeric(ulmus_climate_tmax_original$tmax.ann.min)
ulmus_climate_tmax_original$tmax.max.mean <- as.numeric(ulmus_climate_tmax_original$tmax.max.mean)
ulmus_climate_tmax_original$tmax.max.sd <- as.numeric(ulmus_climate_tmax_original$tmax.max.sd)
ulmus_climate_tmax_original$tmax.max.max <- as.numeric(ulmus_climate_tmax_original$tmax.max.max)
ulmus_climate_tmax_original$tmax.max.min <- as.numeric(ulmus_climate_tmax_original$tmax.max.min)
ulmus_climate_tmax_original$tmax.min.mean <- as.numeric(ulmus_climate_tmax_original$tmax.min.mean)
ulmus_climate_tmax_original$tmax.min.sd <- as.numeric(ulmus_climate_tmax_original$tmax.min.sd)
ulmus_climate_tmax_original$tmax.min.max <- as.numeric(ulmus_climate_tmax_original$tmax.min.max)
ulmus_climate_tmax_original$tmax.min.min <- as.numeric(ulmus_climate_tmax_original$tmax.min.min)
sapply(ulmus_climate_tmax_original, mode)

#getting rid of the Ulmus NA values
ulmus_climate_tmax <- ulmus_climate_tmax_original[complete.cases(ulmus_climate_tmax_original[,important_traits_tmax]),]
#Reduction of Ulmus Variables: saved them to hard drive
UlmusTMAX_Reduction1 <- cor(ulmus_climate_tmax[,important_traits_tmax])
UlmusTMAX_Reduction2 <- cor(ulmus_climate_tmax[,important_traits_tmax2])
#write.csv(UlmusTMAX_Reduction1, "D:/Data_IMLS_Ecological_Value/Climate_Extract_Drive/tmax_Reductions/UlmusTMAX_Reduction1.csv", row.names=TRUE)
#write.csv(UlmusTMAX_Reduction2, "D:/Data_IMLS_Ecological_Value/Climate_Extract_Drive/tmax_Reductions/UlmusTMAX_Reduction2.csv", row.names=TRUE)
#Ulmus PCA 1
ulmus.pca <- prcomp(ulmus_climate_tmax[,important_traits_tmax], center = TRUE,scale. = TRUE) 
summary(ulmus.pca)
ulmus.pca$rotation
#analysis of PCA Plots
ggbiplot(ulmus.pca) #basic plot
#Ulmus PCA 2
ulmus.pca <- prcomp(ulmus_climate_tmax[,important_traits_tmax2], center = TRUE,scale. = TRUE) 
summary(ulmus.pca)
ulmus.pca$rotation
#analysis of PCA Plots
ggbiplot(ulmus.pca) #basic plot
