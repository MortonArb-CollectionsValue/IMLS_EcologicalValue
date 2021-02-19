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


#getting rid of the malus NA values
malus_all <- malus_all[complete.cases(malus_all[,important_traits]),]
#Reduction of Malus Variables
cor(malus_all[,important_traits])
#Malus PCA
malus.pca <- prcomp(malus_all[,important_traits], center = TRUE,scale. = TRUE) 
#error with missing/infinite values: I think I need to get rid of the NAs or convert each column into a numeric
summary(malus.pca)
malus.pca$rotation
#analysis of PCA Plots
ggbiplot(malus.pca) #basic plot

#getting rid of the quercus NA values
quercus_all <- quercus_all[complete.cases(quercus_all[,important_traits]),]
#Reduction of Quercus Variables
cor(quercus_all[,important_traits])
#Quercus PCA
quercus.pca <- prcomp(quercus_all[,c(important_traits)], center = TRUE,scale. = TRUE)
summary(quercus.pca)
quercus.pca$rotation
#analysis of PCA Plots
ggbiplot(quercus,pca) #basic plot

#getting rid of the tilia NA values
tilia_all <- tilia_all[complete.cases(tilia_all[,important_traits]),]
#Reduction of Tilia Variables
cor(tilia_all[,important_traits])
#Tilia PCA
tilia.pca <- prcomp(tilia_all[,c(important_traits)], center = TRUE,scale. = TRUE)
summary(tilia.pca)
tilia.pca$rotation
#analysis of PCA Plots
ggbiplot(tilia.pc) #basic plot

#getting rid of the ulmus NA values
ulmus_all <- ulmus_all[complete.cases(ulmus_all[,important_traits]),]
#Reduction of Ulmus Variables
cor(ulmus_all[,important_traits])
#Ulmus PCA
ulmus.pca <- prcomp(ulmus_all[,c(important_traits)], center = TRUE,scale. = TRUE)
summary(ulmus.pca)
ulmus.pca$rotation
#analysis of PCA Plots
ggbiplot(ulmus.pca) #basic plot
