# Doing a script of some quick PCA/Permanova Analyses for an ESA abstract;
library(dplyr); library(ggplot2); library(vegan)
library(ggbiplot)

#Loading in Climate Data
malus.clim <- read.csv("D:/Data_IMLS_Ecological_Value/Total_PostReductions2/Malus_Climate_Final.csv")
quercus.clim <- read.csv("D:/Data_IMLS_Ecological_Value/Total_PostReductions2/Quercus_Climate_Final.csv")
tilia.clim <- read.csv("D:/Data_IMLS_Ecological_Value/Total_PostReductions2/Tilia_Climate_Final.csv")
ulmus.clim <- read.csv("D:/Data_IMLS_Ecological_Value/Total_PostReductions2/Ulmus_Climate_Final.csv")

#Loading in Soils Data
malus.soils <- read.csv("D:/Data_IMLS_Ecological_Value/Total_PostSoilReductions/Malus_Soil_Final.csv")
quercus.soils <- read.csv("D:/Data_IMLS_Ecological_Value/Total_PostSoilReductions/Quercus_Soil_Final.csv")
tilia.soils <- read.csv("D:/Data_IMLS_Ecological_Value/Total_PostSoilReductions/Tilia_Soil_Final.csv")
ulmus.soils <- read.csv("D:/Data_IMLS_Ecological_Value/Total_PostSoilReductions/Ulmus_Soil_Final.csv")

important.traits <- c("T.GRAVEL", "T.SILT", "T.CLAY", "T.OC", "T.PH.H2O", "T.TEB", "T.ECE", "AWC_VALUE", 
                      "ROOTS", "T.CEC.CLAY", "T.CEC.SOIL", "T.CACO3", "T.CASO4",	"T.ESP")

#Combining Climate & Soil Data: only using the variables for analysis
malus.all <-  merge(malus.clim[,6:17], malus.soils[,important.traits])
quercus.all <-  merge(quercus.clim[,6:17], quercus.soils[,important.traits])
tilia.all <-  merge(tilia.clim[,6:17], tilia.soils[,important.traits])
ulmus.all <-  merge(ulmus.clim[,6:17], ulmus.soils[,important.traits])

# PCA 1: kitchen sink approach -- throw it all in
set.seed(1608)
#Malus PCA
malus.pca1 <- prcomp(malus.all, center = TRUE,scale. = TRUE)
summary(malus.pca1)
save(malus.pca1, file = "D:/Data_IMLS_Ecological_Value/PCAs/maluspca1.RData")
ggbiplot(malus.pca1) #basic plot

#Quercus PCA
quercus.pca1 <- prcomp(quercus.all, center = TRUE,scale. = TRUE)
summary(quercus.pca1)
save(quercus.pca1, file = "D:/Data_IMLS_Ecological_Value/PCAs/quercuspca1.RData")
ggbiplot(quercus.pca1) #basic plot

#Tilia PCA
tilia.pca1 <- prcomp(tilia.all, center = TRUE,scale. = TRUE)
summary(tilia.pca1)
save(tilia.pca1, file = "D:/Data_IMLS_Ecological_Value/PCAs/tiliapca1.RData")
ggbiplot(tilia.pca1) #basic plot

#Ulmus PCA
ulmus.pca1 <- prcomp(ulmus.all, center = TRUE,scale. = TRUE)
summary(ulmus.pca1)
save(ulmus.pca1, file = "D:/Data_IMLS_Ecological_Value/PCAs/ulmuspca1.RData")
ggbiplot(ulmus.pca1) #basic plot


# #genus PCA 1
# genus.pca <- prcomp(genus_climate_total, center = TRUE,scale. = TRUE) 
# summary(genus.pca)
# genus.pca$rotation
# #analysis of PCA Plots
# ggbiplot(genus.pca) #basic plot