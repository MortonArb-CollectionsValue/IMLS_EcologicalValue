#beginning to implement techniques from mtcars into phenological data
#loading in packages used in soil graphs script
library(devtools)
# install_github("vqv/ggbiplot") #just used blank line when asked question
library(ggbiplot)
library("dplyr"); library("plyr"); library("readr")
library(ggplot2)
library(rgdal); library(sp); library(raster)
library(Hmisc)
library(data.table)

#Malus Collection with Morton Arb Data Point
path.dat <- "D:/Data_IMLS_Ecological_Value/Soil_Extract_Drive/Soil_Extract"  
#if data is on hard drive
#path.dat <- "/Volumes/GoogleDrive/Shared drives/IMLS MFA/Environmental Niche Value/Extracted Data/Soil_Extract/" 
#if data is from shared google folder
malus_soil <- list.files(path = path.dat,
                         pattern = "Malus", full.names = TRUE)
soilcols <- names(read.csv(malus_soil[1]))
col.char <- which(soilcols %in% c("nativeDatabaseID", "MU.SOURCE1"))
coltype <- rep(NA, length(soilcols))
coltype[col.char] <- "character"
malus_all <-  lapply(malus_soil, read.csv, colClasses=coltype) %>% bind_rows()
MortonArb_Data <- read.csv(file.path(path.dat,"0_MortonArb.csv"))
malus_all <- rbind.fill(malus_all, MortonArb_Data)
head(malus_all)
tail(malus_all)

#Quercus Collection with Morton Arb Data Point
quercus_soil <- list.files(path = path.dat,
                           pattern = "Quercus", full.names = TRUE)
soilcols <- names(read.csv(quercus_soil[1]))
col.char <- which(soilcols %in% c("nativeDatabaseID", "MU.SOURCE1"))
coltype <- rep(NA, length(soilcols))
coltype[col.char] <- "character"
quercus_all <-  lapply(quercus_soil, read.csv, colClasses=coltype) %>% bind_rows()
quercus_all <- rbind.fill(quercus_all, MortonArb_Data)
head(quercus_all)
tail(quercus_all)

#Tilia Collection with Morton Arb Data Point
tilia_soil <- list.files(path = path.dat,
                         pattern = "Tilia", full.names = TRUE)
soilcols <- names(read.csv(tilia_soil[1]))
col.char <- which(soilcols %in% c("nativeDatabaseID", "MU.SOURCE1"))
coltype <- rep(NA, length(soilcols))
coltype[col.char] <- "character"
tilia_all <-  lapply(tilia_soil, read.csv, colClasses=coltype) %>% bind_rows()
tilia_all <- rbind.fill(tilia_all, MortonArb_Data)
head(tilia_all)
tail(tilia_all)

#Ulmus Collection with Morton Arb Data Point
ulmus_soil <- list.files(path = path.dat,
                         pattern = "Ulmus", full.names = TRUE)
soilcols <- names(read.csv(ulmus_soil[1]))
col.char <- which(soilcols %in% c("nativeDatabaseID", "MU.SOURCE1"))
coltype <- rep(NA, length(soilcols))
coltype[col.char] <- "character"
ulmus_all <-  lapply(ulmus_soil, read.csv, colClasses=coltype) %>% bind_rows()
ulmus_all <- rbind.fill(ulmus_all, MortonArb_Data)
head(ulmus_all)
tail(ulmus_all)



#creating the PCA Plots for the Genera: considered the definite & potential characteristics from Work Plan
#choosing only the important traits: only looked at topsoil
#had no categorical variables: REF.DEPTH, T.USDA.TEX.CLASS
important_traits <- c("T.GRAVEL", "T.SAND", "T.SILT", "T.CLAY", "T.OC", "T.PH.H2O", 
                      "T.TEB", "T.ECE", "AWC_VALUE", "T.REF.BULK.DENSITY", "T.TEXTURE", 
                      "ROOTS", "T.CEC.CLAY", "T.CEC.SOIL", "T.CACO3", "T.CASO4", "T.ESP")
#revised to take out T.SAND, T.REF.BULK.DENSITY, T.TEXTURE
important_traits2 <- c("T.GRAVEL", 
                      #"T.SAND", 
                      "T.SILT", "T.CLAY", "T.OC", "T.PH.H2O", "T.TEB", "T.ECE", "AWC_VALUE", 
                      #"T.REF.BULK.DENSITY", "T.TEXTURE", 
                      "ROOTS", "T.CEC.CLAY", "T.CEC.SOIL", "T.CACO3", "T.CASO4", "T.ESP")


#getting rid of the Malus NA values
malus_all <- malus_all[complete.cases(malus_all[,important_traits]),]
malus_all <- malus_all[complete.cases(malus_all[,important_traits2]),]
#Reduction of Malus Variables: saved them to hard drive
MalusSoils_Reduction1 <- cor(malus_all[,important_traits])
#write.csv(MalusSoils_Reduction1, "D:/Data_IMLS_Ecological_Value/Soil_Extract_Drive/Soil_Reductions/MalusSoils_Reduction1.csv", row.names=TRUE)
MalusSoils_Reduction2 <- cor(malus_all[,important_traits2])
#write.csv(MalusSoils_Reduction2, "D:/Data_IMLS_Ecological_Value/Soil_Extract_Drive/Soil_Reductions/MalusSoils_Reduction2.csv", row.names=TRUE)
#Malus PCA 1
malus.pca <- prcomp(malus_all[,important_traits], center = TRUE,scale. = TRUE) 
summary(malus.pca)
malus.pca$rotation
#analysis of PCA Plots
ggbiplot(malus.pca) #basic plot
#Malus PCA 2
malus.pca2 <- prcomp(malus_all[,important_traits2], center = TRUE,scale. = TRUE) 
summary(malus.pca2)
malus.pca2$rotation
#analysis of PCA Plots
ggbiplot(malus.pca2) #basic plot

#getting rid of the Quercus NA values
quercus_all <- quercus_all[complete.cases(quercus_all[,important_traits]),]
quercus_all <- quercus_all[complete.cases(quercus_all[,important_traits2]),]
#Reduction of Quercus Variables: saved to Hard Drive
QuercusSoils_Reduction1 <- cor(quercus_all[,important_traits])
QuercusSoils_Reduction1
#write.csv(QuercusSoils_Reduction1, "D:/Data_IMLS_Ecological_Value/Soil_Extract_Drive/Soil_Reductions/QuercusSoils_Reduction1.csv", row.names=TRUE)
QuercusSoils_Reduction2 <- cor(quercus_all[,important_traits2])
QuercusSoils_Reduction2
#write.csv(QuercusSoils_Reduction2, "D:/Data_IMLS_Ecological_Value/Soil_Extract_Drive/Soil_Reductions/QuercusSoils_Reduction2.csv", row.names=TRUE)
#Quercus PCA 1
quercus.pca <- prcomp(quercus_all[,important_traits], center = TRUE,scale. = TRUE) 
summary(quercus.pca)
quercus.pca$rotation
#analysis of PCA Plots
ggbiplot(quercus.pca) #basic plot
#Quercus PCA 2
quercus.pca2 <- prcomp(quercus_all[,important_traits2], center = TRUE,scale. = TRUE) 
summary(quercus.pca2)
quercus.pca2$rotation
#analysis of PCA Plots: 
ggbiplot(quercus.pca2) + scale_x_continuous(limits=c(-3,2)) +
  scale_y_continuous(limits=c(-3,2))
#basic plot

pc1.mean <- mean(quercus.pca2$x[,1])
pc1.sd <- sd(quercus.pca2$x[,1])
#pc1.quant <- quantile(quercus.pca2$x[,1], c(0.01, 0.99))
pc1.lim <- c(pc1.mean-3*pc1.sd, pc1.mean+3*pc1.sd)
range(quercus.pca2$x[,1])


pc2.mean <- mean(quercus.pca2$x[,2])
pc2.sd <- sd(quercus.pca2$x[,2])
pc2.quant <- quantile(quercus.pca2$x[,2], c(0.01, 0.99))
pc2.lim <- c(pc1.mean-3*pc1.sd, pc1.mean+3*pc1.sd)
range(quercus.pca2$x[,2])

pc1.bound <- c(max(min(quercus.pca2$x[,1]), pc1.lim[1]), min(max(quercus.pca2$x[,1]), pc1.lim[2]))
pc2.bound <- c(max(min(quercus.pca2$x[,2]), pc2.lim[1]), min(max(quercus.pca2$x[,2]), pc2.lim[2]))




#getting rid of the Tilia NA values
tilia_all <- tilia_all[complete.cases(tilia_all[,important_traits]),]
tilia_all <- tilia_all[complete.cases(tilia_all[,important_traits2]),]
#Reduction of Tilia Variables: saved to Hard Drive
TiliaSoils_Reduction1 <- cor(tilia_all[,important_traits])
TiliaSoils_Reduction1
#write.csv(TiliaSoils_Reduction1, "D:/Data_IMLS_Ecological_Value/Soil_Extract_Drive/Soil_Reductions/TiliaSoils_Reduction1.csv", row.names=TRUE)
TiliaSoils_Reduction2 <- cor(tilia_all[,important_traits2])
TiliaSoils_Reduction2
#write.csv(TiliaSoils_Reduction2, "D:/Data_IMLS_Ecological_Value/Soil_Extract_Drive/Soil_Reductions/TiliaSoils_Reduction2.csv", row.names=TRUE)
#Tilia PCA 1
tilia.pca <- prcomp(tilia_all[,important_traits], center = TRUE,scale. = TRUE) 
summary(tilia.pca)
tilia.pca$rotation
#analysis of PCA Plots
ggbiplot(tilia.pca) #basic plot
#Tilia PCA 2
tilia.pca2 <- prcomp(tilia_all[,important_traits2], center = TRUE,scale. = TRUE) 
summary(tilia.pca2)
tilia.pca2$rotation
#analysis of PCA Plots
ggbiplot(.pca2) #basic plot

#getting rid of the Ulmus NA values
ulmus_all <- ulmus_all[complete.cases(ulmus_all[,important_traits]),]
ulmus_all <- ulmus_all[complete.cases(ulmus_all[,important_traits2]),]
#Reduction of Ulmus Variables
UlmusSoils_Reduction1 <- cor(ulmus_all[,important_traits])
UlmusSoils_Reduction1
#write.csv(UlmusSoils_Reduction1, "D:/Data_IMLS_Ecological_Value/Soil_Extract_Drive/Soil_Reductions/UlmusSoils_Reduction1.csv", row.names=TRUE)
UlmusSoils_Reduction2 <- cor(ulmus_all[,important_traits2])
UlmusSoils_Reduction2
#write.csv(TiliaSoils_Reduction2, "D:/Data_IMLS_Ecological_Value/Soil_Extract_Drive/Soil_Reductions/UlmusSoils_Reduction2.csv", row.names=TRUE)
#Ulmus PCA 1
ulmus.pca <- prcomp(ulmus_all[,important_traits], center = TRUE,scale. = TRUE) 
summary(ulmus.pca)
ulmus.pca$rotation
#analysis of PCA Plots
ggbiplot(ulmus.pca) #basic plot
#Ulmus PCA 2
ulmus.pca2 <- prcomp(ulmus_all[,important_traits2], center = TRUE,scale. = TRUE) 
summary(ulmus.pca2)
ulmus.pca2$rotation
#analysis of PCA Plots
ggbiplot(ulmus.pca2) #basic plot
