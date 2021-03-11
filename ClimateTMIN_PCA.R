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



#PCA PLOTS
#choosing only the important traits: no categorical
important_traits_tmin <- c("tmin.ann.mean","tmin.ann.sd","tmin.ann.max","tmin.ann.min",
                      "tmin.max.mean","tmin.max.sd","tmin.max.max","tmin.max.min",
                      "tmin.min.mean","tmin.min.sd","tmin.min.max","tmin.min.min")

#for reduction at sums of each trait and then took least sum from each category (ann, min, max)
important_traits_tmin2 <- c("tmin.ann.sd","tmin.max.sd", "tmin.min.")

#Malus tmin PCA
#converting all malus columns to numerics
sapply(malus_climate_tmin, mode)
malus_climate_tmin$tmin.ann.mean <- as.numeric(malus_climate_tmin$tmin.ann.mean)
malus_climate_tmin$tmin.ann.sd <- as.numeric(malus_climate_tmin$tmin.ann.sd)
malus_climate_tmin$tmin.ann.max <- as.numeric(malus_climate_tmin$tmin.ann.max)
malus_climate_tmin$tmin.ann.min <- as.numeric(malus_climate_tmin$tmin.ann.min)
malus_climate_tmin$tmin.max.mean <- as.numeric(malus_climate_tmin$tmin.max.mean)
malus_climate_tmin$tmin.max.sd <- as.numeric(malus_climate_tmin$tmin.max.sd)
malus_climate_tmin$tmin.max.max <- as.numeric(malus_climate_tmin$tmin.max.max)
malus_climate_tmin$tmin.max.min <- as.numeric(malus_climate_tmin$tmin.max.min)
malus_climate_tmin$tmin.min.mean <- as.numeric(malus_climate_tmin$tmin.min.mean)
malus_climate_tmin$tmin.min.sd <- as.numeric(malus_climate_tmin$tmin.min.sd)
malus_climate_tmin$tmin.min.max <- as.numeric(malus_climate_tmin$tmin.min.max)
malus_climate_tmin$tmin.min.min <- as.numeric(malus_climate_tmin$tmin.min.min)
sapply(malus_climate_tmin, mode)

#getting rid of the Malus NA values
malus_climate_tmin <- malus_climate_tmin[complete.cases(malus_climate_tmin[,important_traits_tmin]),]
#Reduction of Malus Variables: saved them to hard drive
MalusTMIN_Reduction1 <- cor(malus_climate_tmin[,important_traits_tmin])
MalusTMIN_Reduction2 <- cor(malus_climate_tmin[,important_traits_tmin2])
#write.csv(MalusTMIN_Reduction1, "D:/Data_IMLS_Ecological_Value/Climate_Extract_Drive/tmin_Reductions/MalusTMIN_Reduction1.csv", row.names=TRUE)
#write.csv(MalusTMIN_Reduction2, "D:/Data_IMLS_Ecological_Value/Climate_Extract_Drive/tmin_Reductions/MalusTMIN_Reduction2.csv", row.names=TRUE)
#Malus PCA 1
malus.pca <- prcomp(malus_climate_tmin[,important_traits_tmin], center = TRUE,scale. = TRUE) 
summary(malus.pca)
malus.pca$rotation
#analysis of PCA Plots
ggbiplot(malus.pca) #basic plot
#Malus PCA 2
malus.pca <- prcomp(malus_climate_tmin[,important_traits_tmin2], center = TRUE,scale. = TRUE) 
summary(malus.pca)
malus.pca$rotation
#analysis of PCA Plots
ggbiplot(malus.pca) #basic plot


#Quercus TMIN PCA
#converting all quercus columns to numerics
sapply(quercus_climate_tmin, mode)
quercus_climate_tmin$tmin.ann.mean <- as.numeric(quercus_climate_tmin$tmin.ann.mean)
quercus_climate_tmin$tmin.ann.sd <- as.numeric(quercus_climate_tmin$tmin.ann.sd)
quercus_climate_tmin$tmin.ann.max <- as.numeric(quercus_climate_tmin$tmin.ann.max)
quercus_climate_tmin$tmin.ann.min <- as.numeric(quercus_climate_tmin$tmin.ann.min)
quercus_climate_tmin$tmin.max.mean <- as.numeric(quercus_climate_tmin$tmin.max.mean)
quercus_climate_tmin$tmin.max.sd <- as.numeric(quercus_climate_tmin$tmin.max.sd)
quercus_climate_tmin$tmin.max.max <- as.numeric(quercus_climate_tmin$tmin.max.max)
quercus_climate_tmin$tmin.max.min <- as.numeric(quercus_climate_tmin$tmin.max.min)
quercus_climate_tmin$tmin.min.mean <- as.numeric(quercus_climate_tmin$tmin.min.mean)
quercus_climate_tmin$tmin.min.sd <- as.numeric(quercus_climate_tmin$tmin.min.sd)
quercus_climate_tmin$tmin.min.max <- as.numeric(quercus_climate_tmin$tmin.min.max)
quercus_climate_tmin$tmin.min.min <- as.numeric(quercus_climate_tmin$tmin.min.min)
sapply(quercus_climate_tmin, mode)

#getting rid of the Quercus NA values
quercus_climate_tmin <- quercus_climate_tmin[complete.cases(quercus_climate_tmin[,important_traits_tmin]),]
#Reduction of Quercus Variables: saved them to hard drive
QuercusTMIN_Reduction1 <- cor(quercus_climate_tmin[,important_traits_tmin])
QuercusTMIN_Reduction2 <- cor(quercus_climate_tmin[,important_traits_tmin2])
#write.csv(QuercusTMIN_Reduction1, "D:/Data_IMLS_Ecological_Value/Climate_Extract_Drive/tmin_Reductions/QuercusTMIN_Reduction1.csv", row.names=TRUE)
#write.csv(QuercusTMIN_Reduction2, "D:/Data_IMLS_Ecological_Value/Climate_Extract_Drive/tmin_Reductions/QuercusTMIN_Reduction2.csv", row.names=TRUE)
#Quercus PCA 1
quercus.pca <- prcomp(quercus_climate_tmin[,important_traits_tmin], center = TRUE,scale. = TRUE) 
summary(quercus.pca)
quercus.pca$rotation
#analysis of PCA Plots
ggbiplot(quercus.pca) #basic plot
#Quercus PCA 2
quercus.pca <- prcomp(quercus_climate_tmin[,important_traits_tmin2], center = TRUE,scale. = TRUE) 
summary(quercus.pca)
quercus.pca$rotation
#analysis of PCA Plots
ggbiplot(quercus.pca) #basic plot


#Tilia TMIN PCA
#converting all tilia columns to numerics
sapply(tilia_climate_tmin, mode)
tilia_climate_tmin$tmin.ann.mean <- as.numeric(tilia_climate_tmin$tmin.ann.mean)
tilia_climate_tmin$tmin.ann.sd <- as.numeric(tilia_climate_tmin$tmin.ann.sd)
tilia_climate_tmin$tmin.ann.max <- as.numeric(tilia_climate_tmin$tmin.ann.max)
tilia_climate_tmin$tmin.ann.min <- as.numeric(tilia_climate_tmin$tmin.ann.min)
tilia_climate_tmin$tmin.max.mean <- as.numeric(tilia_climate_tmin$tmin.max.mean)
tilia_climate_tmin$tmin.max.sd <- as.numeric(tilia_climate_tmin$tmin.max.sd)
tilia_climate_tmin$tmin.max.max <- as.numeric(tilia_climate_tmin$tmin.max.max)
tilia_climate_tmin$tmin.max.min <- as.numeric(tilia_climate_tmin$tmin.max.min)
tilia_climate_tmin$tmin.min.mean <- as.numeric(tilia_climate_tmin$tmin.min.mean)
tilia_climate_tmin$tmin.min.sd <- as.numeric(tilia_climate_tmin$tmin.min.sd)
tilia_climate_tmin$tmin.min.max <- as.numeric(tilia_climate_tmin$tmin.min.max)
tilia_climate_tmin$tmin.min.min <- as.numeric(tilia_climate_tmin$tmin.min.min)
sapply(tilia_climate_tmin, mode)

#getting rid of the Tilia NA values
tilia_climate_tmin <- tilia_climate_tmin[complete.cases(tilia_climate_tmin[,important_traits_tmin]),]
#Reduction of tilia Variables: saved them to hard drive
TiliaTMIN_Reduction1 <- cor(tilia_climate_tmin[,important_traits_tmin])
TiliaTMIN_Reduction2 <- cor(tilia_climate_tmin[,important_traits_tmin2])
#write.csv(TiliaTMIN_Reduction1, "D:/Data_IMLS_Ecological_Value/Climate_Extract_Drive/tmin_Reductions/TiliaTMIN_Reduction1.csv", row.names=TRUE)
#write.csv(TiliaTMIN_Reduction2, "D:/Data_IMLS_Ecological_Value/Climate_Extract_Drive/tmin_Reductions/TiliaTMIN_Reduction2.csv", row.names=TRUE)
#tilia PCA 1
tilia.pca <- prcomp(tilia_climate_tmin[,important_traits_tmin], center = TRUE,scale. = TRUE) 
summary(tilia.pca)
tilia.pca$rotation
#analysis of PCA Plots
ggbiplot(tilia.pca) #basic plot
#tilia PCA 2
tilia.pca <- prcomp(tilia_climate_tmin[,important_traits_tmin2], center = TRUE,scale. = TRUE) 
summary(tilia.pca)
tilia.pca$rotation
#analysis of PCA Plots
ggbiplot(tilia.pca) #basic plot


#Ulmus TMIN PCA
#converting all ulmus columns to numerics
sapply(ulmus_climate_tmin, mode)
ulmus_climate_tmin$tmin.ann.mean <- as.numeric(ulmus_climate_tmin$tmin.ann.mean)
ulmus_climate_tmin$tmin.ann.sd <- as.numeric(ulmus_climate_tmin$tmin.ann.sd)
ulmus_climate_tmin$tmin.ann.max <- as.numeric(ulmus_climate_tmin$tmin.ann.max)
ulmus_climate_tmin$tmin.ann.min <- as.numeric(ulmus_climate_tmin$tmin.ann.min)
ulmus_climate_tmin$tmin.max.mean <- as.numeric(ulmus_climate_tmin$tmin.max.mean)
ulmus_climate_tmin$tmin.max.sd <- as.numeric(ulmus_climate_tmin$tmin.max.sd)
ulmus_climate_tmin$tmin.max.max <- as.numeric(ulmus_climate_tmin$tmin.max.max)
ulmus_climate_tmin$tmin.max.min <- as.numeric(ulmus_climate_tmin$tmin.max.min)
ulmus_climate_tmin$tmin.min.mean <- as.numeric(ulmus_climate_tmin$tmin.min.mean)
ulmus_climate_tmin$tmin.min.sd <- as.numeric(ulmus_climate_tmin$tmin.min.sd)
ulmus_climate_tmin$tmin.min.max <- as.numeric(ulmus_climate_tmin$tmin.min.max)
ulmus_climate_tmin$tmin.min.min <- as.numeric(ulmus_climate_tmin$tmin.min.min)
sapply(ulmus_climate_tmin, mode)

#getting rid of the Ulmus NA values
ulmus_climate_tmin <- ulmus_climate_tmin[complete.cases(ulmus_climate_tmin[,important_traits_tmin]),]
#Reduction of Ulmus Variables: saved them to hard drive
UlmusTMIN_Reduction1 <- cor(ulmus_climate_tmin[,important_traits_tmin])
UlmusTMIN_Reduction2 <- cor(ulmus_climate_tmin[,important_traits_tmin2])
#write.csv(UlmusTMIN_Reduction1, "D:/Data_IMLS_Ecological_Value/Climate_Extract_Drive/tmin_Reductions/UlmusTMIN_Reduction1.csv", row.names=TRUE)
#write.csv(UlmusTMIN_Reduction2, "D:/Data_IMLS_Ecological_Value/Climate_Extract_Drive/tmin_Reductions/UlmusTMIN_Reduction2.csv", row.names=TRUE)
#Ulmus PCA 1
ulmus.pca <- prcomp(ulmus_climate_tmin[,important_traits_tmin], center = TRUE,scale. = TRUE) 
summary(ulmus.pca)
ulmus.pca$rotation
#analysis of PCA Plots
ggbiplot(ulmus.pca) #basic plot
#Ulmus PCA 2
ulmus.pca <- prcomp(ulmus_climate_tmin[,important_traits_tmin2], center = TRUE,scale. = TRUE) 
summary(ulmus.pca)
ulmus.pca$rotation
#analysis of PCA Plots
ggbiplot(ulmus.pca) #basic plot
