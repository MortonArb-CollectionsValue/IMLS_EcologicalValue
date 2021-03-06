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
path.dat.vpd <- "D:/Data_IMLS_Ecological_Value/Climate_Extract_Drive/vpd"

#Morton Arb Values in different folders
MortonArb_Data_vpd <- read.csv(file.path(path.dat.vpd,"0_MortonArb.csv"))

#VPD Plots
  #made each data frame end with original to indicate before NA values were removed
  #used to combine climate and soil data
#Malus VPD Data
malus_vpd <- list.files(path = path.dat.vpd,
                         pattern = "Malus", full.names = TRUE)
vpdcols <- names(read.csv(malus_vpd[1]))
# col.char <- which(vpdcols %in% c("nativeDatabaseID", "MU.SOURCE1"))
coltype <- rep(NA, length(vpdcols))
coltype <- "character"
malus_climate_vpd_original <-  lapply(malus_vpd, read.csv, colClasses=coltype) %>% bind_rows()
malus_climate_vpd_original <- rbind.fill(malus_climate_vpd_original, MortonArb_Data_vpd)
malus_climate_vpd_original <- tidyr::separate(malus_climate_vpd_original, col = "species_name_acc", into=c("genus", "species"))
head(malus_climate_vpd_original)
tail(malus_climate_vpd_original)

#Quercus VDP Data
quercus_vpd <- list.files(path = path.dat.vpd,
                           pattern = "Quercus", full.names = TRUE)
vpdcols <- names(read.csv(quercus_vpd[1]))
# col.char <- which(vpdcols %in% c("nativeDatabaseID", "MU.SOURCE1"))
coltype <- rep(NA, length(vpdcols))
coltype <- "character"
quercus_climate_vpd_original <-  lapply(quercus_vpd, read.csv, colClasses=coltype) %>% bind_rows()
quercus_climate_vpd_original <- rbind.fill(quercus_climate_vpd_original, MortonArb_Data_vpd)
quercus_climate_vpd_original <- tidyr::separate(quercus_climate_vpd_original, col = "species_name_acc", into=c("genus", "species"))
head(quercus_climate_vpd_original)
tail(quercus_climate_vpd_original)

#Tilia VPD Data
tilia_vpd <- list.files(path = path.dat.vpd,
                         pattern = "Tilia", full.names = TRUE)
vpdcols <- names(read.csv(tilia_vpd[1]))
# col.char <- which(vpdcols %in% c("nativeDatabaseID", "MU.SOURCE1"))
coltype <- rep(NA, length(vpdcols))
coltype <- "character"
tilia_climate_vpd_original <-  lapply(tilia_vpd, read.csv, colClasses=coltype) %>% bind_rows()
tilia_climate_vpd_original <- rbind.fill(tilia_climate_vpd_original, MortonArb_Data_vpd)
tilia_climate_vpd_original <- tidyr::separate(tilia_climate_vpd_original, col = "species_name_acc", into=c("genus", "species"))
head(tilia_climate_vpd_original)
tail(tilia_climate_vpd_original)

#Ulmus VPD Data
ulmus_vpd <- list.files(path = path.dat.vpd,
                         pattern = "Ulmus", full.names = TRUE)
vpdcols <- names(read.csv(ulmus_vpd[1]))
# col.char <- which(vpdcols %in% c("nativeDatabaseID", "MU.SOURCE1"))
coltype <- rep(NA, length(vpdcols))
coltype <- "character"
ulmus_climate_vpd_original <-  lapply(ulmus_vpd, read.csv, colClasses=coltype) %>% bind_rows()
ulmus_climate_vpd_original <- rbind.fill(ulmus_climate_vpd_original, MortonArb_Data_vpd)
ulmus_climate_vpd_original <- tidyr::separate(ulmus_climate_vpd_original, col = "species_name_acc", into=c("genus", "species"))
head(ulmus_climate_vpd_original)
tail(ulmus_climate_vpd_original)



#PCA PLOTS
#choosing only the important traits: no categorical
important_traits_vpd <- c("vpd.ann.mean","vpd.ann.sd","vpd.ann.max","vpd.ann.min",
                      "vpd.max.mean","vpd.max.sd","vpd.max.max","vpd.max.min",
                      "vpd.min.mean","vpd.min.sd","vpd.min.max","vpd.min.min")

#for reduction at sums of each trait and then took least sum from each category (ann, min, max)
important_traits_vpd2 <- c("vpd.ann.sd","vpd.max.sd", "vpd.min.min")


#Malus vpd PCA
#converting all malus columns to numerics
sapply(malus_climate_vpd_original, mode)
malus_climate_vpd_original$vpd.ann.mean <- as.numeric(malus_climate_vpd_original$vpd.ann.mean)
malus_climate_vpd_original$vpd.ann.sd <- as.numeric(malus_climate_vpd_original$vpd.ann.sd)
malus_climate_vpd_original$vpd.ann.max <- as.numeric(malus_climate_vpd_original$vpd.ann.max)
malus_climate_vpd_original$vpd.ann.min <- as.numeric(malus_climate_vpd_original$vpd.ann.min)
malus_climate_vpd_original$vpd.max.mean <- as.numeric(malus_climate_vpd_original$vpd.max.mean)
malus_climate_vpd_original$vpd.max.sd <- as.numeric(malus_climate_vpd_original$vpd.max.sd)
malus_climate_vpd_original$vpd.max.max <- as.numeric(malus_climate_vpd_original$vpd.max.max)
malus_climate_vpd_original$vpd.max.min <- as.numeric(malus_climate_vpd_original$vpd.max.min)
malus_climate_vpd_original$vpd.min.mean <- as.numeric(malus_climate_vpd_original$vpd.min.mean)
malus_climate_vpd_original$vpd.min.sd <- as.numeric(malus_climate_vpd_original$vpd.min.sd)
malus_climate_vpd_original$vpd.min.max <- as.numeric(malus_climate_vpd_original$vpd.min.max)
malus_climate_vpd_original$vpd.min.min <- as.numeric(malus_climate_vpd_original$vpd.min.min)
sapply(malus_climate_vpd_original, mode)

#getting rid of the Malus NA values
malus_climate_vpd <- malus_climate_vpd_original[complete.cases(malus_climate_vpd_original[,important_traits_vpd]),]
#Reduction of Malus Variables: saved them to hard drive
MalusVPD_Reduction1 <- cor(malus_climate_vpd[,important_traits_vpd])
MalusVPD_Reduction2 <- cor(malus_climate_vpd[,important_traits_vpd2])
#write.csv(MalusVPD_Reduction1, "D:/Data_IMLS_Ecological_Value/Climate_Extract_Drive/vpd_Reductions/MalusVPD_Reduction1.csv", row.names=TRUE)
#write.csv(MalusVPD_Reduction2, "D:/Data_IMLS_Ecological_Value/Climate_Extract_Drive/vpd_Reductions/MalusVPD_Reduction2.csv", row.names=TRUE)
# #Malus PCA 1
# malus.pca <- prcomp(malus_climate_vpd[,important_traits_vpd], center = TRUE,scale. = TRUE) 
# summary(malus.pca)
# malus.pca$rotation
# #analysis of PCA Plots
# ggbiplot(malus.pca) #basic plot
# #Malus PCA 2
# malus.pca <- prcomp(malus_climate_vpd[,important_traits_vpd2], center = TRUE,scale. = TRUE) 
# summary(malus.pca)
# malus.pca$rotation
# #analysis of PCA Plots
# ggbiplot(malus.pca) #basic plot


#Quercus VPD PCA
#converting all quercus columns to numerics
sapply(quercus_climate_vpd_original, mode)
quercus_climate_vpd_original$vpd.ann.mean <- as.numeric(quercus_climate_vpd_original$vpd.ann.mean)
quercus_climate_vpd_original$vpd.ann.sd <- as.numeric(quercus_climate_vpd_original$vpd.ann.sd)
quercus_climate_vpd_original$vpd.ann.max <- as.numeric(quercus_climate_vpd_original$vpd.ann.max)
quercus_climate_vpd_original$vpd.ann.min <- as.numeric(quercus_climate_vpd_original$vpd.ann.min)
quercus_climate_vpd_original$vpd.max.mean <- as.numeric(quercus_climate_vpd_original$vpd.max.mean)
quercus_climate_vpd_original$vpd.max.sd <- as.numeric(quercus_climate_vpd_original$vpd.max.sd)
quercus_climate_vpd_original$vpd.max.max <- as.numeric(quercus_climate_vpd_original$vpd.max.max)
quercus_climate_vpd_original$vpd.max.min <- as.numeric(quercus_climate_vpd_original$vpd.max.min)
quercus_climate_vpd_original$vpd.min.mean <- as.numeric(quercus_climate_vpd_original$vpd.min.mean)
quercus_climate_vpd_original$vpd.min.sd <- as.numeric(quercus_climate_vpd_original$vpd.min.sd)
quercus_climate_vpd_original$vpd.min.max <- as.numeric(quercus_climate_vpd_original$vpd.min.max)
quercus_climate_vpd_original$vpd.min.min <- as.numeric(quercus_climate_vpd_original$vpd.min.min)
sapply(quercus_climate_vpd_original, mode)

#getting rid of the Quercus NA values
quercus_climate_vpd <- quercus_climate_vpd_original[complete.cases(quercus_climate_vpd_original[,important_traits_vpd]),]
#Reduction of Quercus Variables: saved them to hard drive
QuercusVPD_Reduction1 <- cor(quercus_climate_vpd[,important_traits_vpd])
QuercusVPD_Reduction2 <- cor(quercus_climate_vpd[,important_traits_vpd2])
#write.csv(QuercusVPD_Reduction1, "D:/Data_IMLS_Ecological_Value/Climate_Extract_Drive/vpd_Reductions/QuercusVPD_Reduction1.csv", row.names=TRUE)
#write.csv(QuercusVPD_Reduction2, "D:/Data_IMLS_Ecological_Value/Climate_Extract_Drive/vpd_Reductions/QuercusVPD_Reduction2.csv", row.names=TRUE)
# #Quercus PCA 1
# quercus.pca <- prcomp(quercus_climate_vpd[,important_traits_vpd], center = TRUE,scale. = TRUE) 
# summary(quercus.pca)
# quercus.pca$rotation
# #analysis of PCA Plots
# ggbiplot(quercus.pca) #basic plot
# #Quercus PCA 2
# quercus.pca <- prcomp(quercus_climate_vpd[,important_traits_vpd2], center = TRUE,scale. = TRUE) 
# summary(quercus.pca)
# quercus.pca$rotation
# #analysis of PCA Plots
# ggbiplot(quercus.pca) #basic plot


#Tilia VPD PCA
#converting all tilia columns to numerics
sapply(tilia_climate_vpd_original, mode)
tilia_climate_vpd_original$vpd.ann.mean <- as.numeric(tilia_climate_vpd_original$vpd.ann.mean)
tilia_climate_vpd_original$vpd.ann.sd <- as.numeric(tilia_climate_vpd_original$vpd.ann.sd)
tilia_climate_vpd_original$vpd.ann.max <- as.numeric(tilia_climate_vpd_original$vpd.ann.max)
tilia_climate_vpd_original$vpd.ann.min <- as.numeric(tilia_climate_vpd_original$vpd.ann.min)
tilia_climate_vpd_original$vpd.max.mean <- as.numeric(tilia_climate_vpd_original$vpd.max.mean)
tilia_climate_vpd_original$vpd.max.sd <- as.numeric(tilia_climate_vpd_original$vpd.max.sd)
tilia_climate_vpd_original$vpd.max.max <- as.numeric(tilia_climate_vpd_original$vpd.max.max)
tilia_climate_vpd_original$vpd.max.min <- as.numeric(tilia_climate_vpd_original$vpd.max.min)
tilia_climate_vpd_original$vpd.min.mean <- as.numeric(tilia_climate_vpd_original$vpd.min.mean)
tilia_climate_vpd_original$vpd.min.sd <- as.numeric(tilia_climate_vpd_original$vpd.min.sd)
tilia_climate_vpd_original$vpd.min.max <- as.numeric(tilia_climate_vpd_original$vpd.min.max)
tilia_climate_vpd_original$vpd.min.min <- as.numeric(tilia_climate_vpd_original$vpd.min.min)
sapply(tilia_climate_vpd_original, mode)

#getting rid of the Tilia NA values
tilia_climate_vpd <- tilia_climate_vpd_original[complete.cases(tilia_climate_vpd_original[,important_traits_vpd]),]
#Reduction of tilia Variables: saved them to hard drive
TiliaVPD_Reduction1 <- cor(tilia_climate_vpd[,important_traits_vpd])
TiliaVPD_Reduction2 <- cor(tilia_climate_vpd[,important_traits_vpd2])
#write.csv(TiliaVPD_Reduction1, "D:/Data_IMLS_Ecological_Value/Climate_Extract_Drive/vpd_Reductions/TiliaVPD_Reduction1.csv", row.names=TRUE)
#write.csv(TiliaVPD_Reduction2, "D:/Data_IMLS_Ecological_Value/Climate_Extract_Drive/vpd_Reductions/TiliaVPD_Reduction2.csv", row.names=TRUE)
# #tilia PCA 1
# tilia.pca <- prcomp(tilia_climate_vpd[,important_traits_vpd], center = TRUE,scale. = TRUE) 
# summary(tilia.pca)
# tilia.pca$rotation
# #analysis of PCA Plots
# ggbiplot(tilia.pca) #basic plot
# #tilia PCA 2
# tilia.pca <- prcomp(tilia_climate_vpd[,important_traits_vpd2], center = TRUE,scale. = TRUE) 
# summary(tilia.pca)
# tilia.pca$rotation
# #analysis of PCA Plots
# ggbiplot(tilia.pca) #basic plot


#Ulmus VPD PCA
#converting all ulmus columns to numerics
sapply(ulmus_climate_vpd_original, mode)
ulmus_climate_vpd_original$vpd.ann.mean <- as.numeric(ulmus_climate_vpd_original$vpd.ann.mean)
ulmus_climate_vpd_original$vpd.ann.sd <- as.numeric(ulmus_climate_vpd_original$vpd.ann.sd)
ulmus_climate_vpd_original$vpd.ann.max <- as.numeric(ulmus_climate_vpd_original$vpd.ann.max)
ulmus_climate_vpd_original$vpd.ann.min <- as.numeric(ulmus_climate_vpd_original$vpd.ann.min)
ulmus_climate_vpd_original$vpd.max.mean <- as.numeric(ulmus_climate_vpd_original$vpd.max.mean)
ulmus_climate_vpd_original$vpd.max.sd <- as.numeric(ulmus_climate_vpd_original$vpd.max.sd)
ulmus_climate_vpd_original$vpd.max.max <- as.numeric(ulmus_climate_vpd_original$vpd.max.max)
ulmus_climate_vpd_original$vpd.max.min <- as.numeric(ulmus_climate_vpd_original$vpd.max.min)
ulmus_climate_vpd_original$vpd.min.mean <- as.numeric(ulmus_climate_vpd_original$vpd.min.mean)
ulmus_climate_vpd_original$vpd.min.sd <- as.numeric(ulmus_climate_vpd_original$vpd.min.sd)
ulmus_climate_vpd_original$vpd.min.max <- as.numeric(ulmus_climate_vpd_original$vpd.min.max)
ulmus_climate_vpd_original$vpd.min.min <- as.numeric(ulmus_climate_vpd_original$vpd.min.min)
sapply(ulmus_climate_vpd_original, mode)

#getting rid of the Ulmus NA values
ulmus_climate_vpd <- ulmus_climate_vpd_original[complete.cases(ulmus_climate_vpd_original[,important_traits_vpd]),]
#Reduction of Ulmus Variables: saved them to hard drive
UlmusVPD_Reduction1 <- cor(ulmus_climate_vpd[,important_traits_vpd])
UlmusVPD_Reduction2 <- cor(ulmus_climate_vpd[,important_traits_vpd2])
#write.csv(UlmusVPD_Reduction1, "D:/Data_IMLS_Ecological_Value/Climate_Extract_Drive/vpd_Reductions/UlmusVPD_Reduction1.csv", row.names=TRUE)
#write.csv(UlmusVPD_Reduction2, "D:/Data_IMLS_Ecological_Value/Climate_Extract_Drive/vpd_Reductions/UlmusVPD_Reduction2.csv", row.names=TRUE)
# #Ulmus PCA 1
# ulmus.pca <- prcomp(ulmus_climate_vpd[,important_traits_vpd], center = TRUE,scale. = TRUE) 
# summary(ulmus.pca)
# ulmus.pca$rotation
# #analysis of PCA Plots
# ggbiplot(ulmus.pca) #basic plot
# #Ulmus PCA 2
# ulmus.pca <- prcomp(ulmus_climate_vpd[,important_traits_vpd2], center = TRUE,scale. = TRUE) 
# summary(ulmus.pca)
# ulmus.pca$rotation
# #analysis of PCA Plots
# ggbiplot(ulmus.pca) #basic plot
