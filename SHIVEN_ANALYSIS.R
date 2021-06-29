library(dplyr); library(ggplot2); library(vegan)
library(ggbiplot)

path.clim <- "D:/Data_IMLS_Ecological_Value/Total_PostReductions2/"
path.soils <- "D:/Data_IMLS_Ecological_Value/Total_PostSoilReductions/"
path.pcas <- "D:/Data_IMLS_Ecological_Value/PCAs"

#Loading in Climate Data
malus.clim <- read.csv(file.path(path.clim, "Malus_Climate_Final.csv"))
quercus.clim <- read.csv(file.path(path.clim, "Quercus_Climate_Final.csv"))
tilia.clim <- read.csv(file.path(path.clim, "Tilia_Climate_Final.csv"))
ulmus.clim <- read.csv(file.path(path.clim, "Ulmus_Climate_Final.csv"))

#Loading in Soils Data
malus.soils <- read.csv(file.path(path.soils, "Malus_Soil_Final.csv"))
quercus.soils <- read.csv(file.path(path.soils, "Quercus_Soil_Final.csv"))
tilia.soils <- read.csv(file.path(path.soils, "Tilia_Soil_Final.csv"))
ulmus.soils <- read.csv(file.path(path.soils, "Ulmus_Soil_Final.csv"))

#ROOTS was omitted because Arb Data is NA at this value
important.traits <- c("ppt.ann.mean", "ppt.min.min", "soil.ann.max", "soil.max.sd", "srad.ann.max", "srad.ann.sd", "tmax.ann.min",      
                      "tmax.min.sd", "tmin.ann.min", "tmin.ann.sd", "vpd.ann.max", "vpd.max.sd", "T.GRAVEL", "T.SILT", "T.CLAY", 
                      "T.OC", "T.PH.H2O", "T.TEB", "T.ECE", "AWC_VALUE", "T.CEC.CLAY", "T.CEC.SOIL", "T.CACO3", "T.CASO4",	"T.ESP")

#Combining Climate & Soil Data: only using the variables for analysis
  #Only takes the variables that are in both, gets rid of the variables that are only in 1
  #Filtering OUt Morton Arb Data since PCA does not work with Arb point: ROOTS is NA
  #Got rid of Arb data so it is not in data frame 4x
malus.all <-  merge(malus.clim, malus.soils, by.x="UID", by.y="UID")
morton.all <- malus.all[malus.all$genus.x=="MortonArb", ]
malus.all <- malus.all[!(malus.all$genus.x=="MortonArb"), ]
#malus.all2 <- filter(malus.all, !genus.x %in% c('MortonArb'))
#malus.all <- malus.all[,c(7:18, 22:35)]
quercus.all <-  merge(quercus.clim, quercus.soils, by.x="UID", by.y="UID")
quercus.all <- quercus.all[!(quercus.all$genus.x=="MortonArb"), ]
#quercus.all2 <- filter(quercus.all, !genus.x %in% c('MortonArb'))
#quercus.all <- quercus.all[,c(6:17, 22:35)]
tilia.all <-  merge(tilia.clim, tilia.soils, by.x="UID", by.y="UID")
tilia.all <- tilia.all[!(tilia.all$genus.x=="MortonArb"), ]
#tilia.all2 <- filter(tilia.all, !genus.x %in% c('MortonArb'))
#tilia.all <- tilia.all[,c(6:17, 22:35)]
ulmus.all <-  merge(ulmus.clim, ulmus.soils, by.x="UID", by.y="UID")
ulmus.all <- ulmus.all[!(ulmus.all$genus.x=="MortonArb"), ]
#ulmus.all2 <- filter(ulmus.all, !genus.x %in% c('MortonArb'))
#ulmus.all <- ulmus.all[,c(6:17, 22:35)]

total.all <- rbind(malus.all, quercus.all, tilia.all, ulmus.all, morton.all)
##WHY ARE ROW #'S ALL MESSED UP

# # PCA 1: kitchen sink approach -- throw it all in
# set.seed(1608)
# #Malus PCA
# malus.pca1 <- prcomp(malus.all2[,important.traits], center = TRUE, scale. = TRUE)
# summary(malus.pca1)
# #save(malus.pca1, file = "D:/Data_IMLS_Ecological_Value/PCA_Graphs/maluspca1.RData")
# malus.pca.plot <- ggbiplot(malus.pca1) #basic plot
# malus.pca.plot
# dev.copy(png, file.path(path.pcas, "malus_pca.png"))
# dev.off()
# 
# #Quercus PCA
# quercus.pca1 <- prcomp(quercus.all2[,important.traits], center = TRUE, scale. = TRUE)
# summary(quercus.pca1)
# #save(quercus.pca1, file = "D:/Data_IMLS_Ecological_Value/PCA_Graphs/quercuspca1.RData")
# quercus.pca.plot <- ggbiplot(quercus.pca1) #basic plot
# quercus.pca.plot
# dev.copy(png, file.path(path.pcas, "quercus_pca.png"))
# dev.off()
# 
# #Tilia PCA
# tilia.pca1 <- prcomp(tilia.all2[,important.traits], center = TRUE, scale. = TRUE)
# summary(tilia.pca1)
# #save(tilia.pca1, file = "D:/Data_IMLS_Ecological_Value/PCA_Graphs/tiliapca1.RData")
# tilia.pca.plot <- ggbiplot(tilia.pca1) #basic plot
# tilia.pca.plot
# dev.copy(png, file.path(path.pcas, "tilia_pca.png"))
# dev.off()
# 
# #Ulmus PCA
# ulmus.pca1 <- prcomp(ulmus.all2[,important.traits], center = TRUE, scale. = TRUE)
# summary(ulmus.pca1)
# #save(ulmus.pca1, file = "D:/Data_IMLS_Ecological_Value/PCA_Graphs/ulmuspca1.RData")
# ulmus.pca.plot <- ggbiplot(ulmus.pca1) #basic plot
# ulmus.pca.plot
# dev.copy(png, file.path(path.pcas, "ulmus_pca.png"))
# dev.off()


#Removing predictor outliers -- used ESA script lines 95-117

important.traits
meta.traits <- c("UID", "genus.x", "species.x", "decimalLatitude.x", "decimalLongitude.x")
  #Do I need to include the duplicate genus, species, latitude, & longtiude with the Y?


# Identify euclidean outliers from the first 2 PCs; based on 4 SDs
# - In future, could go and do this by each species
# scale() function makes data normally distributed with mean = 0 and standard deviation = 1 (puts things on common scale; overcomes challenges of different units)
# This needs to happen at the genus level: 1:2 is "meta.traits" while 3:ncol(malus.all2) is "important.traits"
total.scale <- cbind(total.all[,meta.traits], scale(total.all[,important.traits])) # putting descriptors w/ scaled data
summary(total.scale)

# Remove variables that are supreme outliers in any one variable
# NOTE: Because we have centered and scaled the data, it'll be normally distributed except for outliers!
# rows.remove <- which(malus.scale[,important.traits]>6)
# summary(rows.remove)

# Removing weirdos
# Being fairly stringent with the outlier number for our sanity
# Currently happening at the genus level --> down the road we'll try to adjust by species
rows.keep <- apply(total.scale[,important.traits], 1, FUN=function(x){all(abs(x)<=4)})
total.clean <- total.scale[rows.keep,]
summary(total.clean)


# PCA 2: kitchen sink approach -- throw it all in
# Used centered data without outliers (don't need to center or scale)
set.seed(1608)
#Malus PCA 2
total.pca2 <- prcomp(total.clean[,important.traits], center = FALSE, scale. = FALSE)
malus.pca2<- total.pca2 %>% genus.x=="Malus"
summary(total.pca2)
write.csv(as.data.frame.matrix(total.pca2$rotation), "D:/Data_IMLS_Ecological_Value/PCAs/Total_pca2.csv", row.names = TRUE)
total.pca.plot2 <- ggbiplot(total.pca2) #basic plot
total.pca.plot2
dev.copy(png, file.path(path.pcas, "total_pca2.png"))
dev.off()


# Identify euclidean outliers from the first 2 PCs; based on 4 SDs
# - In future, could go and do this by each species
# scale() function makes data normally distributed with mean = 0 and standard deviation = 1 (puts things on common scale; overcomes challenges of different units)
# This needs to happen at the genus level: 1:2 is "meta.traits" while 3:ncol(malus.all2) is "important.traits"
malus.scale <- cbind(malus.all[,meta.traits], scale(malus.all[,important.traits])) # putting descriptors w/ scaled data
summary(malus.scale)

# Remove variables that are supreme outliers in any one variable
# NOTE: Because we have centered and scaled the data, it'll be normally distributed except for outliers!
# rows.remove <- which(malus.scale[,important.traits]>6)
# summary(rows.remove)

# Removing weirdos
# Being fairly stringent with the outlier number for our sanity
# Currently happening at the genus level --> down the road we'll try to adjust by species
rows.keep <- apply(malus.scale[,important.traits], 1, FUN=function(x){all(abs(x)<=4)})
malus.clean <- malus.scale[rows.keep,]
summary(malus.clean)


# Identify euclidean outliers from the first 2 PCs; based on 4 SDs
# - In future, could go and do this by each species
# scale() function makes data normally distributed with mean = 0 and standard deviation = 1 (puts things on common scale; overcomes challenges of different units)
# This needs to happen at the genus level: 1:2 is "meta.traits" while 3:ncol(quercus.all2) is "important.traits"
quercus.scale <- cbind(quercus.all[,meta.traits], scale(quercus.all[,important.traits])) # putting descriptors w/ scaled data
summary(quercus.scale)

# Remove variables that are supreme outliers in any one variable
# NOTE: Because we have centered and scaled the data, it'll be normally distributed except for outliers!
# rows.remove <- which(quercus.scale[,important.traits]>6)
# summary(rows.remove)

# Removing weirdos
# Being fairly stringent with the outlier number for our sanity
# Currently happening at the genus level --> down the road we'll try to adjust by species
rows.keep <- apply(quercus.scale[,important.traits], 1, FUN=function(x){all(abs(x)<=4)})
quercus.clean <- quercus.scale[rows.keep,]
summary(quercus.clean)


# Identify euclidean outliers from the first 2 PCs; based on 4 SDs
# - In future, could go and do this by each species
# scale() function makes data normally distributed with mean = 0 and standard deviation = 1 (puts things on common scale; overcomes challenges of different units)
# This needs to happen at the genus level: 1:2 is "meta.traits" while 3:ncol(tilia.all2) is "important.traits"
tilia.scale <- cbind(tilia.all[,meta.traits], scale(tilia.all[,important.traits])) # putting descriptors w/ scaled data
summary(tilia.scale)

# Remove variables that are supreme outliers in any one variable
# NOTE: Because we have centered and scaled the data, it'll be normally distributed except for outliers!
# rows.remove <- which(tilia.scale[,important.traits]>6)
# summary(rows.remove)

# Removing weirdos
# Being fairly stringent with the outlier number for our sanity
# Currently happening at the genus level --> down the road we'll try to adjust by species
rows.keep <- apply(tilia.scale[,important.traits], 1, FUN=function(x){all(abs(x)<=4)})
tilia.clean <- tilia.scale[rows.keep,]
summary(tilia.clean)


# Identify euclidean outliers from the first 2 PCs; based on 4 SDs
# - In future, could go and do this by each species
# scale() function makes data normally distributed with mean = 0 and standard deviation = 1 (puts things on common scale; overcomes challenges of different units)
# This needs to happen at the genus level: 1:2 is "meta.traits" while 3:ncol(ulmus.all2) is "important.traits"
ulmus.scale <- cbind(ulmus.all[,meta.traits], scale(ulmus.all[,important.traits])) # putting descriptors w/ scaled data
summary(ulmus.scale)

# Remove variables that are supreme outliers in any one variable
# NOTE: Because we have centered and scaled the data, it'll be normally distributed except for outliers!
rows.remove <- which(ulmus.scale[,important.traits]>6)
summary(rows.remove)

# Removing weirdos
# Being fairly stringent with the outlier number for our sanity
# Currently happening at the genus level --> down the road we'll try to adjust by species
rows.keep <- apply(ulmus.scale[,important.traits], 1, FUN=function(x){all(abs(x)<=4)})
ulmus.clean <- ulmus.scale[rows.keep,]
summary(ulmus.clean)


# PCA 2: kitchen sink approach -- throw it all in
# Used centered data without outliers (don't need to center or scale)
set.seed(1608)
#Malus PCA 2
malus.pca2 <- prcomp(malus.clean[,important.traits], center = FALSE, scale. = FALSE)
summary(malus.pca2)
write.csv(as.data.frame.matrix(malus.pca2$rotation), "D:/Data_IMLS_Ecological_Value/PCAs/Malus_pca2.csv", row.names = TRUE)
malus.pca.plot2 <- ggbiplot(malus.pca2) #basic plot
malus.pca.plot2
dev.copy(png, file.path(path.pcas, "malus_pca2.png"))
dev.off()

#Quercus PCA 2
quercus.pca2 <- prcomp(quercus.clean[,important.traits], center = FALSE, scale. = FALSE)
summary(quercus.pca2)
write.csv(as.data.frame.matrix(quercus.pca2$rotation), "D:/Data_IMLS_Ecological_Value/PCAs/Quercus_pca2.csv", row.names = TRUE)
quercus.pca.plot2 <- ggbiplot(quercus.pca2) #basic plot
quercus.pca.plot2
dev.copy(png, file.path(path.pcas, "quercus_pca2.png"))
dev.off()

#Tilia PCA 2
tilia.pca2 <- prcomp(tilia.clean[,important.traits], center = FALSE, scale. = FALSE)
summary(tilia.pca2)
write.csv(as.data.frame.matrix(tilia.pca2$rotation), "D:/Data_IMLS_Ecological_Value/PCAs/Tilia_pca2.csv", row.names = TRUE)
tilia.pca.plot2 <- ggbiplot(tilia.pca2) #basic plot
tilia.pca.plot2
dev.copy(png, file.path(path.pcas, "tilia_pca2.png"))
dev.off()

#Ulmus PCA
ulmus.pca2 <- prcomp(ulmus.clean[,important.traits], center = FALSE, scale. = FALSE)
summary(ulmus.pca2)
write.csv(as.data.frame.matrix(ulmus.pca2$rotation), "D:/Data_IMLS_Ecological_Value/PCAs/Ulmus_pca2.csv", row.names = TRUE)
ulmus.pca.plot2 <- ggbiplot(ulmus.pca2) #basic plot
ulmus.pca.plot2
dev.copy(png, file.path(path.pcas, "ulmus_pca2.png"))
dev.off()
