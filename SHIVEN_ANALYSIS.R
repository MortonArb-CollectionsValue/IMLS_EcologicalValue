# Doing a script of some quick PCA/Permanova Analyses for an ESA abstract;
library(dplyr); library(ggplot2); library(vegan)
library(ggbiplot)

#Loading in Data
Malus_Climate_Final <- read.csv("D:/Data_IMLS_Ecological_Value/Total_PostReductions/Malus_Climate_Final.csv")
Quercus_Climate_Final <- read.csv("D:/Data_IMLS_Ecological_Value/Total_PostReductions/Quercus_Climate_Final.csv")
Tilia_Climate_Final <- read.csv("D:/Data_IMLS_Ecological_Value/Total_PostReductions/Tilia_Climate_Final.csv")
Ulmus_Climate_Final <- read.csv("D:/Data_IMLS_Ecological_Value/Total_PostReductions/Ulmus_Climate_Final.csv")

# PCA 1: kitchen sink approach -- throw it all in
#Malus PCA
set.seed(1608)
malus.pca1 <- prcomp(Malus_Climate_Final[,7:12], center = TRUE,scale. = TRUE)
save(malus.pca1, )

#Quercus PCA
set.seed(1608)
quercus.pca1 <- prcomp(Quercus_Climate_Final[,7:12], center = TRUE,scale. = TRUE)
save(quercus.pca1, )

#Tilia PCA
set.seed(1608)
tilia.pca1 <- prcomp(Tilia_Climate_Final[,7:12], center = TRUE,scale. = TRUE)
save(tilia.pca1, )

#Ulmus PCA
set.seed(1608)
ulmus.pca1 <- prcomp(Ulmus_Climate_Final[,7:12], center = TRUE,scale. = TRUE)
save(ulmus.pca1, )


# #genus PCA 1
# genus.pca <- prcomp(genus_climate_total, center = TRUE,scale. = TRUE) 
# summary(genus.pca)
# genus.pca$rotation
# #analysis of PCA Plots
# ggbiplot(genus.pca) #basic plot