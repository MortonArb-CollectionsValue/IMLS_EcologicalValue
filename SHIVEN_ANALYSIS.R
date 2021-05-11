# Doing a script of some quick PCA/Permanova Analyses for an ESA abstract;
library(dplyr); library(ggplot2); library(vegan)
library(ggbiplot)

#Loading in Data
Malus_Climate_Final <- read.csv("D:/Data_IMLS_Ecological_Value/Total_PostReductions/Malus_Climate_Final.csv")
Quercus_Climate_Final <- read.csv("D:/Data_IMLS_Ecological_Value/Total_PostReductions/Quercus_Climate_Final.csv")
Tilia_Climate_Final <- read.csv("D:/Data_IMLS_Ecological_Value/Total_PostReductions/Tilia_Climate_Final.csv")
Ulmus_Climate_Final <- read.csv("D:/Data_IMLS_Ecological_Value/Total_PostReductions/Ulmus_Climate_Final.csv")

# PCA 1: kitchen sink approach -- throw it all in
set.seed(1608)
#Malus PCA
malus.pca1 <- prcomp(Malus_Climate_Final[,7:12], center = TRUE,scale. = TRUE)
summary(malus.pca1)
save(malus.pca1, file = "D:/Data_IMLS_Ecological_Value/PCAs/maluspca1.RData")
ggbiplot(malus.pca1) #basic plot

#Quercus PCA
quercus.pca1 <- prcomp(Quercus_Climate_Final[,7:12], center = TRUE,scale. = TRUE)
summary(quercus.pca1)
save(quercus.pca1, file = "D:/Data_IMLS_Ecological_Value/PCAs/quercuspca1.RData")
ggbiplot(quercus.pca1) #basic plot

#Tilia PCA
tilia.pca1 <- prcomp(Tilia_Climate_Final[,7:12], center = TRUE,scale. = TRUE)
summary(tilia.pca1)
save(tilia.pca1, file = "D:/Data_IMLS_Ecological_Value/PCAs/tiliapca1.RData")

#Ulmus PCA
ulmus.pca1 <- prcomp(Ulmus_Climate_Final[,7:12], center = TRUE,scale. = TRUE)
summary(ulmus.pca1)
save(ulmus.pca1, file = "D:/Data_IMLS_Ecological_Value/PCAs/ulmuspca1.RData")


# #genus PCA 1
# genus.pca <- prcomp(genus_climate_total, center = TRUE,scale. = TRUE) 
# summary(genus.pca)
# genus.pca$rotation
# #analysis of PCA Plots
# ggbiplot(genus.pca) #basic plot