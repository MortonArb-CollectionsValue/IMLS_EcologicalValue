#Loading in correct packages to extract
library(devtools)
# install_github("vqv/ggbiplot") #just used blank line when asked question
library(ggbiplot)
library("dplyr"); library("plyr"); library("readr")
library(ggplot2)
library(rgdal); library(sp); library(raster)
library(Hmisc)
library(data.table)

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



#PCA PLOTS
#choosing only the important traits: no categorical
important_traits_soil <- c("soil.ann.mean","soil.ann.sd","soil.ann.max","soil.ann.min",
                      "soil.max.mean","soil.max.sd","soil.max.max","soil.max.min",
                      "soil.min.mean","soil.min.sd","soil.min.max","soil.min.min")

#for reduction looked at sums of each trait and then took least sum from each category (ann, min, max)
important_traits_soil2 <- c("soil.ann.sd","soil.max.sd", "soil.min.sd")

#Malus Soil PCA
#converting all malus columns to numerics
sapply(malus_climate_soil, mode)
malus_climate_soil$soil.ann.mean <- as.numeric(malus_climate_soil$soil.ann.mean)
malus_climate_soil$soil.ann.sd <- as.numeric(malus_climate_soil$soil.ann.sd)
malus_climate_soil$soil.ann.max <- as.numeric(malus_climate_soil$soil.ann.max)
malus_climate_soil$soil.ann.min <- as.numeric(malus_climate_soil$soil.ann.min)
malus_climate_soil$soil.max.mean <- as.numeric(malus_climate_soil$soil.max.mean)
malus_climate_soil$soil.max.sd <- as.numeric(malus_climate_soil$soil.max.sd)
malus_climate_soil$soil.max.max <- as.numeric(malus_climate_soil$soil.max.max)
malus_climate_soil$soil.max.min <- as.numeric(malus_climate_soil$soil.max.min)
malus_climate_soil$soil.min.mean <- as.numeric(malus_climate_soil$soil.min.mean)
malus_climate_soil$soil.min.sd <- as.numeric(malus_climate_soil$soil.min.sd)
malus_climate_soil$soil.min.max <- as.numeric(malus_climate_soil$soil.min.max)
malus_climate_soil$soil.min.min <- as.numeric(malus_climate_soil$soil.min.min)
sapply(malus_climate_soil, mode)

#getting rid of the Malus NA values
malus_climate_soil <- malus_climate_soil[complete.cases(malus_climate_soil[,important_traits_soil]),]
#Reduction of Malus Variables: saved them to hard drive
MalusSoils_Reduction1 <- cor(malus_climate_soil[,important_traits_soil])
MalusSoils_Reduction2 <- cor(malus_climate_soil[,important_traits_soil2])
#write.csv(MalusSoils_Reduction1, "D:/Data_IMLS_Ecological_Value/Climate_Extract_Drive/soil_Reductions/MalusSoils_Reduction1.csv", row.names=TRUE)
#write.csv(MalusSoils_Reduction2, "D:/Data_IMLS_Ecological_Value/Climate_Extract_Drive/soil_Reductions/MalusSoils_Reduction2.csv", row.names=TRUE)
#Malus PCA 1
malus.pca <- prcomp(malus_climate_soil[,important_traits_soil], center = TRUE,scale. = TRUE) 
summary(malus.pca)
malus.pca$rotation
#analysis of PCA Plots
ggbiplot(malus.pca) #basic plot
#Malus PCA 2
malus.pca <- prcomp(malus_climate_soil[,important_traits_soil2], center = TRUE,scale. = TRUE) 
summary(malus.pca)
malus.pca$rotation
#analysis of PCA Plots
ggbiplot(malus.pca) #basic plot


#Quercus Soil PCA
#converting all quercus columns to numerics
sapply(quercus_climate_soil, mode)
quercus_climate_soil$soil.ann.mean <- as.numeric(quercus_climate_soil$soil.ann.mean)
quercus_climate_soil$soil.ann.sd <- as.numeric(quercus_climate_soil$soil.ann.sd)
quercus_climate_soil$soil.ann.max <- as.numeric(quercus_climate_soil$soil.ann.max)
quercus_climate_soil$soil.ann.min <- as.numeric(quercus_climate_soil$soil.ann.min)
quercus_climate_soil$soil.max.mean <- as.numeric(quercus_climate_soil$soil.max.mean)
quercus_climate_soil$soil.max.sd <- as.numeric(quercus_climate_soil$soil.max.sd)
quercus_climate_soil$soil.max.max <- as.numeric(quercus_climate_soil$soil.max.max)
quercus_climate_soil$soil.max.min <- as.numeric(quercus_climate_soil$soil.max.min)
quercus_climate_soil$soil.min.mean <- as.numeric(quercus_climate_soil$soil.min.mean)
quercus_climate_soil$soil.min.sd <- as.numeric(quercus_climate_soil$soil.min.sd)
quercus_climate_soil$soil.min.max <- as.numeric(quercus_climate_soil$soil.min.max)
quercus_climate_soil$soil.min.min <- as.numeric(quercus_climate_soil$soil.min.min)
sapply(quercus_climate_soil, mode)

#getting rid of the Quercus NA values
quercus_climate_soil <- quercus_climate_soil[complete.cases(quercus_climate_soil[,important_traits_soil]),]
#Reduction of Quercus Variables: saved them to hard drive
QuercusSoils_Reduction1 <- cor(quercus_climate_soil[,important_traits_soil])
QuercusSoils_Reduction2 <- cor(quercus_climate_soil[,important_traits_soil2])
#write.csv(QuercusSoils_Reduction1, "D:/Data_IMLS_Ecological_Value/Climate_Extract_Drive/soil_Reductions/QuercusSoils_Reduction1.csv", row.names=TRUE)
#write.csv(QuercusSoils_Reduction2, "D:/Data_IMLS_Ecological_Value/Climate_Extract_Drive/soil_Reductions/QuercusSoils_Reduction2.csv", row.names=TRUE)
#Quercus PCA 1
quercus.pca <- prcomp(quercus_climate_soil[,important_traits_soil], center = TRUE,scale. = TRUE) 
summary(quercus.pca)
quercus.pca$rotation
#analysis of PCA Plots
ggbiplot(quercus.pca) #basic plot
#Quercus PCA 2
quercus.pca <- prcomp(quercus_climate_soil[,important_traits_soil2], center = TRUE,scale. = TRUE) 
summary(quercus.pca)
quercus.pca$rotation
#analysis of PCA Plots
ggbiplot(quercus.pca) #basic plot


#Tilia Soils PCA
#converting all tilia columns to numerics
sapply(tilia_climate_soil, mode)
tilia_climate_soil$soil.ann.mean <- as.numeric(tilia_climate_soil$soil.ann.mean)
tilia_climate_soil$soil.ann.sd <- as.numeric(tilia_climate_soil$soil.ann.sd)
tilia_climate_soil$soil.ann.max <- as.numeric(tilia_climate_soil$soil.ann.max)
tilia_climate_soil$soil.ann.min <- as.numeric(tilia_climate_soil$soil.ann.min)
tilia_climate_soil$soil.max.mean <- as.numeric(tilia_climate_soil$soil.max.mean)
tilia_climate_soil$soil.max.sd <- as.numeric(tilia_climate_soil$soil.max.sd)
tilia_climate_soil$soil.max.max <- as.numeric(tilia_climate_soil$soil.max.max)
tilia_climate_soil$soil.max.min <- as.numeric(tilia_climate_soil$soil.max.min)
tilia_climate_soil$soil.min.mean <- as.numeric(tilia_climate_soil$soil.min.mean)
tilia_climate_soil$soil.min.sd <- as.numeric(tilia_climate_soil$soil.min.sd)
tilia_climate_soil$soil.min.max <- as.numeric(tilia_climate_soil$soil.min.max)
tilia_climate_soil$soil.min.min <- as.numeric(tilia_climate_soil$soil.min.min)
sapply(tilia_climate_soil, mode)

#getting rid of the Tilia NA values
tilia_climate_soil <- tilia_climate_soil[complete.cases(tilia_climate_soil[,important_traits_soil]),]
#Reduction of tilia Variables: saved them to hard drive
TiliaSoils_Reduction1 <- cor(tilia_climate_soil[,important_traits_soil])
TiliaSoils_Reduction2 <- cor(tilia_climate_soil[,important_traits_soil2])
#write.csv(TiliaSoils_Reduction1, "D:/Data_IMLS_Ecological_Value/Climate_Extract_Drive/soil_Reductions/TiliaSoils_Reduction1.csv", row.names=TRUE)
#write.csv(TiliaSoils_Reduction2, "D:/Data_IMLS_Ecological_Value/Climate_Extract_Drive/soil_Reductions/TiliaSoils_Reduction2.csv", row.names=TRUE)
#tilia PCA 1
tilia.pca <- prcomp(tilia_climate_soil[,important_traits_soil], center = TRUE,scale. = TRUE) 
summary(tilia.pca)
tilia.pca$rotation
#analysis of PCA Plots
ggbiplot(tilia.pca) #basic plot
#tilia PCA 2
tilia.pca <- prcomp(tilia_climate_soil[,important_traits_soil2], center = TRUE,scale. = TRUE) 
summary(tilia.pca)
tilia.pca$rotation
#analysis of PCA Plots
ggbiplot(tilia.pca) #basic plot


#Ulmus Soils PCA
#converting all ulmus columns to numerics
sapply(ulmus_climate_soil, mode)
ulmus_climate_soil$soil.ann.mean <- as.numeric(ulmus_climate_soil$soil.ann.mean)
ulmus_climate_soil$soil.ann.sd <- as.numeric(ulmus_climate_soil$soil.ann.sd)
ulmus_climate_soil$soil.ann.max <- as.numeric(ulmus_climate_soil$soil.ann.max)
ulmus_climate_soil$soil.ann.min <- as.numeric(ulmus_climate_soil$soil.ann.min)
ulmus_climate_soil$soil.max.mean <- as.numeric(ulmus_climate_soil$soil.max.mean)
ulmus_climate_soil$soil.max.sd <- as.numeric(ulmus_climate_soil$soil.max.sd)
ulmus_climate_soil$soil.max.max <- as.numeric(ulmus_climate_soil$soil.max.max)
ulmus_climate_soil$soil.max.min <- as.numeric(ulmus_climate_soil$soil.max.min)
ulmus_climate_soil$soil.min.mean <- as.numeric(ulmus_climate_soil$soil.min.mean)
ulmus_climate_soil$soil.min.sd <- as.numeric(ulmus_climate_soil$soil.min.sd)
ulmus_climate_soil$soil.min.max <- as.numeric(ulmus_climate_soil$soil.min.max)
ulmus_climate_soil$soil.min.min <- as.numeric(ulmus_climate_soil$soil.min.min)
sapply(ulmus_climate_soil, mode)

#getting rid of the Ulmus NA values
ulmus_climate_soil <- ulmus_climate_soil[complete.cases(ulmus_climate_soil[,important_traits_soil]),]
#Reduction of Ulmus Variables: saved them to hard drive
UlmusSoils_Reduction1 <- cor(ulmus_climate_soil[,important_traits_soil])
UlmusSoils_Reduction2 <- cor(ulmus_climate_soil[,important_traits_soil2])
#write.csv(UlmusSoils_Reduction1, "D:/Data_IMLS_Ecological_Value/Climate_Extract_Drive/soil_Reductions/UlmusSoils_Reduction1.csv", row.names=TRUE)
#write.csv(UlmusSoils_Reduction2, "D:/Data_IMLS_Ecological_Value/Climate_Extract_Drive/soil_Reductions/UlmusSoils_Reduction2.csv", row.names=TRUE)
#Ulmus PCA 1
ulmus.pca <- prcomp(ulmus_climate_soil[,important_traits_soil], center = TRUE,scale. = TRUE) 
summary(ulmus.pca)
ulmus.pca$rotation
#analysis of PCA Plots
ggbiplot(ulmus.pca) #basic plot
#Ulmus PCA 2
ulmus.pca <- prcomp(ulmus_climate_soil[,important_traits_soil2], center = TRUE,scale. = TRUE) 
summary(ulmus.pca)
ulmus.pca$rotation
#analysis of PCA Plots
ggbiplot(ulmus.pca) #basic plot

