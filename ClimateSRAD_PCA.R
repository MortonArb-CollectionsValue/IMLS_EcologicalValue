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


#PCA PLOTS
#choosing only the important traits: no categorical
important_traits_srad <- c("srad.ann.mean","srad.ann.sd","srad.ann.max","srad.ann.min",
                      "srad.max.mean","srad.max.sd","srad.max.max","srad.max.min",
                      "srad.min.mean","srad.min.sd","srad.min.max","srad.min.min")

#for reduction at sums of each trait and then took least sum from each category (ann, min, max)
important_traits_srad2 <- c("srad.ann.sd","srad.max.sd", "srad.min.min")

#Malus SRAD PCA
#converting all malus columns to numerics
sapply(malus_climate_srad, mode)
malus_climate_srad$srad.ann.mean <- as.numeric(malus_climate_srad$srad.ann.mean)
malus_climate_srad$srad.ann.sd <- as.numeric(malus_climate_srad$srad.ann.sd)
malus_climate_srad$srad.ann.max <- as.numeric(malus_climate_srad$srad.ann.max)
malus_climate_srad$srad.ann.min <- as.numeric(malus_climate_srad$srad.ann.min)
malus_climate_srad$srad.max.mean <- as.numeric(malus_climate_srad$srad.max.mean)
malus_climate_srad$srad.max.sd <- as.numeric(malus_climate_srad$srad.max.sd)
malus_climate_srad$srad.max.max <- as.numeric(malus_climate_srad$srad.max.max)
malus_climate_srad$srad.max.min <- as.numeric(malus_climate_srad$srad.max.min)
malus_climate_srad$srad.min.mean <- as.numeric(malus_climate_srad$srad.min.mean)
malus_climate_srad$srad.min.sd <- as.numeric(malus_climate_srad$srad.min.sd)
malus_climate_srad$srad.min.max <- as.numeric(malus_climate_srad$srad.min.max)
malus_climate_srad$srad.min.min <- as.numeric(malus_climate_srad$srad.min.min)
sapply(malus_climate_srad, mode)

#getting rid of the Malus NA values
malus_climate_srad <- malus_climate_srad[complete.cases(malus_climate_srad[,important_traits_srad]),]
#Reduction of Malus Variables: saved them to hard drive
MalusSRAD_Reduction1 <- cor(malus_climate_srad[,important_traits_srad])
MalusSRAD_Reduction2 <- cor(malus_climate_srad[,important_traits_srad2])
#write.csv(MalusSRAD_Reduction1, "D:/Data_IMLS_Ecological_Value/Climate_Extract_Drive/srad_Reductions/MalusSRAD_Reduction1.csv", row.names=TRUE)
#write.csv(MalusSRAD_Reduction2, "D:/Data_IMLS_Ecological_Value/Climate_Extract_Drive/srad_Reductions/MalusSRAD_Reduction2.csv", row.names=TRUE)
#Malus PCA 1
malus.pca <- prcomp(malus_climate_srad[,important_traits_srad], center = TRUE,scale. = TRUE) 
summary(malus.pca)
malus.pca$rotation
#analysis of PCA Plots
ggbiplot(malus.pca) #basic plot
#Malus PCA 2
malus.pca <- prcomp(malus_climate_srad[,important_traits_srad2], center = TRUE,scale. = TRUE) 
summary(malus.pca)
malus.pca$rotation
#analysis of PCA Plots
ggbiplot(malus.pca) #basic plot


#Quercus SRAD PCA
#converting all quercus columns to numerics
sapply(quercus_climate_srad, mode)
quercus_climate_srad$srad.ann.mean <- as.numeric(quercus_climate_srad$srad.ann.mean)
quercus_climate_srad$srad.ann.sd <- as.numeric(quercus_climate_srad$srad.ann.sd)
quercus_climate_srad$srad.ann.max <- as.numeric(quercus_climate_srad$srad.ann.max)
quercus_climate_srad$srad.ann.min <- as.numeric(quercus_climate_srad$srad.ann.min)
quercus_climate_srad$srad.max.mean <- as.numeric(quercus_climate_srad$srad.max.mean)
quercus_climate_srad$srad.max.sd <- as.numeric(quercus_climate_srad$srad.max.sd)
quercus_climate_srad$srad.max.max <- as.numeric(quercus_climate_srad$srad.max.max)
quercus_climate_srad$srad.max.min <- as.numeric(quercus_climate_srad$srad.max.min)
quercus_climate_srad$srad.min.mean <- as.numeric(quercus_climate_srad$srad.min.mean)
quercus_climate_srad$srad.min.sd <- as.numeric(quercus_climate_srad$srad.min.sd)
quercus_climate_srad$srad.min.max <- as.numeric(quercus_climate_srad$srad.min.max)
quercus_climate_srad$srad.min.min <- as.numeric(quercus_climate_srad$srad.min.min)
sapply(quercus_climate_srad, mode)

#getting rid of the Quercus NA values
quercus_climate_srad <- quercus_climate_srad[complete.cases(quercus_climate_srad[,important_traits_srad]),]
#Reduction of Quercus Variables: saved them to hard drive
QuercusSRAD_Reduction1 <- cor(quercus_climate_srad[,important_traits_srad])
QuercusSRAD_Reduction2 <- cor(quercus_climate_srad[,important_traits_srad2])
#write.csv(QuercusSRAD_Reduction1, "D:/Data_IMLS_Ecological_Value/Climate_Extract_Drive/srad_Reductions/QuercusSRAD_Reduction1.csv", row.names=TRUE)
#write.csv(QuercusSRAD_Reduction2, "D:/Data_IMLS_Ecological_Value/Climate_Extract_Drive/srad_Reductions/QuercusSRAD_Reduction2.csv", row.names=TRUE)
#Quercus PCA 1
quercus.pca <- prcomp(quercus_climate_srad[,important_traits_srad], center = TRUE,scale. = TRUE) 
summary(quercus.pca)
quercus.pca$rotation
#analysis of PCA Plots
ggbiplot(quercus.pca) #basic plot
#Quercus PCA 2
quercus.pca <- prcomp(quercus_climate_srad[,important_traits_srad2], center = TRUE,scale. = TRUE) 
summary(quercus.pca)
quercus.pca$rotation
#analysis of PCA Plots
ggbiplot(quercus.pca) #basic plot


#Tilia SRAD PCA
#converting all tilia columns to numerics
sapply(tilia_climate_srad, mode)
tilia_climate_srad$srad.ann.mean <- as.numeric(tilia_climate_srad$srad.ann.mean)
tilia_climate_srad$srad.ann.sd <- as.numeric(tilia_climate_srad$srad.ann.sd)
tilia_climate_srad$srad.ann.max <- as.numeric(tilia_climate_srad$srad.ann.max)
tilia_climate_srad$srad.ann.min <- as.numeric(tilia_climate_srad$srad.ann.min)
tilia_climate_srad$srad.max.mean <- as.numeric(tilia_climate_srad$srad.max.mean)
tilia_climate_srad$srad.max.sd <- as.numeric(tilia_climate_srad$srad.max.sd)
tilia_climate_srad$srad.max.max <- as.numeric(tilia_climate_srad$srad.max.max)
tilia_climate_srad$srad.max.min <- as.numeric(tilia_climate_srad$srad.max.min)
tilia_climate_srad$srad.min.mean <- as.numeric(tilia_climate_srad$srad.min.mean)
tilia_climate_srad$srad.min.sd <- as.numeric(tilia_climate_srad$srad.min.sd)
tilia_climate_srad$srad.min.max <- as.numeric(tilia_climate_srad$srad.min.max)
tilia_climate_srad$srad.min.min <- as.numeric(tilia_climate_srad$srad.min.min)
sapply(tilia_climate_srad, mode)

#getting rid of the Tilia NA values
tilia_climate_srad <- tilia_climate_srad[complete.cases(tilia_climate_srad[,important_traits_srad]),]
#Reduction of tilia Variables: saved them to hard drive
TiliaSRAD_Reduction1 <- cor(tilia_climate_srad[,important_traits_srad])
TiliaSRAD_Reduction2 <- cor(tilia_climate_srad[,important_traits_srad2])
#write.csv(TiliaSRAD_Reduction1, "D:/Data_IMLS_Ecological_Value/Climate_Extract_Drive/srad_Reductions/TiliaSRAD_Reduction1.csv", row.names=TRUE)
#write.csv(TiliaSRAD_Reduction2, "D:/Data_IMLS_Ecological_Value/Climate_Extract_Drive/srad_Reductions/TiliaSRAD_Reduction2.csv", row.names=TRUE)
#tilia PCA 1
tilia.pca <- prcomp(tilia_climate_srad[,important_traits_srad], center = TRUE,scale. = TRUE) 
summary(tilia.pca)
tilia.pca$rotation
#analysis of PCA Plots
ggbiplot(tilia.pca) #basic plot
#tilia PCA 2
tilia.pca <- prcomp(tilia_climate_srad[,important_traits_srad2], center = TRUE,scale. = TRUE) 
summary(tilia.pca)
tilia.pca$rotation
#analysis of PCA Plots
ggbiplot(tilia.pca) #basic plot

#Ulmus SRAD PCA
#converting all ulmus columns to numerics
sapply(ulmus_climate_srad, mode)
ulmus_climate_srad$srad.ann.mean <- as.numeric(ulmus_climate_srad$srad.ann.mean)
ulmus_climate_srad$srad.ann.sd <- as.numeric(ulmus_climate_srad$srad.ann.sd)
ulmus_climate_srad$srad.ann.max <- as.numeric(ulmus_climate_srad$srad.ann.max)
ulmus_climate_srad$srad.ann.min <- as.numeric(ulmus_climate_srad$srad.ann.min)
ulmus_climate_srad$srad.max.mean <- as.numeric(ulmus_climate_srad$srad.max.mean)
ulmus_climate_srad$srad.max.sd <- as.numeric(ulmus_climate_srad$srad.max.sd)
ulmus_climate_srad$srad.max.max <- as.numeric(ulmus_climate_srad$srad.max.max)
ulmus_climate_srad$srad.max.min <- as.numeric(ulmus_climate_srad$srad.max.min)
ulmus_climate_srad$srad.min.mean <- as.numeric(ulmus_climate_srad$srad.min.mean)
ulmus_climate_srad$srad.min.sd <- as.numeric(ulmus_climate_srad$srad.min.sd)
ulmus_climate_srad$srad.min.max <- as.numeric(ulmus_climate_srad$srad.min.max)
ulmus_climate_srad$srad.min.min <- as.numeric(ulmus_climate_srad$srad.min.min)
sapply(ulmus_climate_srad, mode)

#getting rid of the Ulmus NA values
ulmus_climate_srad <- ulmus_climate_srad[complete.cases(ulmus_climate_srad[,important_traits_srad]),]
#Reduction of Ulmus Variables: saved them to hard drive
UlmusSRAD_Reduction1 <- cor(ulmus_climate_srad[,important_traits_srad])
UlmusSRAD_Reduction2 <- cor(ulmus_climate_srad[,important_traits_srad2])
#write.csv(UlmusSRAD_Reduction1, "D:/Data_IMLS_Ecological_Value/Climate_Extract_Drive/srad_Reductions/UlmusSRAD_Reduction1.csv", row.names=TRUE)
#write.csv(UlmusSRAD_Reduction2, "D:/Data_IMLS_Ecological_Value/Climate_Extract_Drive/srad_Reductions/UlmusSRAD_Reduction2.csv", row.names=TRUE)
#Ulmus PCA 1
ulmus.pca <- prcomp(ulmus_climate_srad[,important_traits_srad], center = TRUE,scale. = TRUE) 
summary(ulmus.pca)
ulmus.pca$rotation
#analysis of PCA Plots
ggbiplot(ulmus.pca) #basic plot
#Ulmus PCA 2
ulmus.pca <- prcomp(ulmus_climate_srad[,important_traits_srad2], center = TRUE,scale. = TRUE) 
summary(ulmus.pca)
ulmus.pca$rotation
#analysis of PCA Plots
ggbiplot(ulmus.pca) #basic plot
