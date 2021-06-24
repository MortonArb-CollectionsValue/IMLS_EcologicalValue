####################################################################################################
####################################################################################################
# rm(list=ls())
### Load packages
my.packages <- c('ggplot2', 'plyr', 'readr', 'dplyr', 'sf', 'tidyverse', 'ks', 'vegan', 'ggbiplot')
# install.packages (my.packages) #Turn on to install current versions
lapply(my.packages, require, character.only=TRUE)
  rm(my.packages)

####################################################################################################
####################################################################################################
### Load functions
spatial.outlier<-function(data,x=data,threshold=0.05){
  spatial.depth=function(x,data){
    if(ncol(as.matrix(x))==1)
      x<-t(x)
    if(ncol(data)!=ncol(as.matrix(x))){
      sd<-"Dimensions do not match"
    }else{
      spd<-function(data,x)
      {
        v=array(0,ncol(data))
        for(j in 1:ncol(data))
        {
          for(i in 1:nrow(data))
          {
            if(sqrt(sum((x-data[i,])^2))!=0)
              v[j]=v[j]+((x[j]-data[i,j])/sqrt(sum((x-data[i,])^2)))
          }
          v[j]=v[j]/nrow(data)
        }
        sd=1-sqrt(sum(v^2))
      }
      sd<-apply(x,1,function(y){spd(data,y)})
    }
    sd
  }
  sd<-spatial.depth(x,data)
  if(length(dim(x))!=0){
    m<-list()
    m[[1]]<-which(sd<=threshold)
    m[[2]]<-x[m[[1]],]
    names(m)<-c("index","observation")
    if(length(m[[1]])==0)
      m<-"FALSE"
  }else{
    m<-ifelse(sd<=threshold,"TRUE","FALSE")
  }
  return(m)
}

####################################################################################################
####################################################################################################
### set paths/folders
  ## path to the shared Google Drive folder
  path.dat <- "/Volumes/GoogleDrive/Shared drives/IMLS MFA/Environmental Niche Value" ## path for Shannon
  # path.dat <- "D:/Data_IMLS_Ecological_Value"   ## path to data for Shiven D drive
  
  # path for the folder for figure output
  path.figs <- file.path(path.dat, "figures")

  # path on the local drive for accessing the data
  path.local <- "/Users/aesculus/Box/Research/Active_Projects/IMLS_MortonArb/local_data/PCA_Data"

####################################################################################################
####################################################################################################
## bring in data
path.clim <- file.path(path.dat, "Analysis/Total_PostReductions2")
path.soils <- file.path(path.dat, "Analysis/Total_PostSoilReductions")
path.pcas <- file.path(path.dat, "Analysis/PCAs")

# path.clim <- "D:/Data_IMLS_Ecological_Value/Total_PostReductions2/"
# path.soils <- "D:/Data_IMLS_Ecological_Value/Total_PostSoilReductions/"
# path.pcas <- "D:/Data_IMLS_Ecological_Value/PCAs"

#Loading Climate Data
malus.clim    <- read.csv(file.path(path.clim, "Malus_Climate_Final.csv"))
  head(malus.clim)
quercus.clim  <- read.csv(file.path(path.clim, "Quercus_Climate_Final.csv"))
tilia.clim    <- read.csv(file.path(path.clim, "Tilia_Climate_Final.csv"))
ulmus.clim    <- read.csv(file.path(path.clim, "Ulmus_Climate_Final.csv"))

#Loading Soils Data
malus.soils   <- read.csv(file.path(path.soils, "Malus_Soil_Final.csv"))
quercus.soils <- read.csv(file.path(path.soils, "Quercus_Soil_Final.csv"))
tilia.soils   <- read.csv(file.path(path.soils, "Tilia_Soil_Final.csv"))
ulmus.soils   <- read.csv(file.path(path.soils, "Ulmus_Soil_Final.csv"))

#ROOTS was omitted because Arb Data is NA at this value
important.traits <- c("ppt.ann.mean", "ppt.min.min", "soil.ann.max", "soil.max.sd", "srad.ann.max", "srad.ann.sd", "tmax.ann.min",      
                      "tmax.min.sd", "tmin.ann.min", "tmin.ann.sd", "vpd.ann.max", "vpd.max.sd", "T.GRAVEL", "T.SILT", "T.CLAY", 
                      "T.OC", "T.PH.H2O", "T.TEB", "T.ECE", "AWC_VALUE", "T.CEC.CLAY", "T.CEC.SOIL", "T.CACO3", "T.CASO4",	"T.ESP")

#Combining Climate & Soil Data: only using the variables for analysis
  #Only takes the variables that are in both, gets rid of the variables that are only in 1
  #Filtering out Morton Arb Data since PCA does not work with Arb point: ROOTS is NA
meta.traits <- c("genus", "species", "decimalLatitude", "decimalLongitude")

malus.all   <-  merge(malus.clim,   select(malus.soils,  -all_of(meta.traits)), by="UID")
quercus.all <-  merge(quercus.clim, select(quercus.soils,-all_of(meta.traits)), by="UID")
tilia.all   <-  merge(tilia.clim,   select(tilia.soils,  -all_of(meta.traits)), by="UID")
ulmus.all   <-  merge(ulmus.clim,   select(ulmus.soils,  -all_of(meta.traits)), by="UID") %>% select(-X)

#Removing predictor outliers -- used ESA script lines 95-117

# Identify euclidean outliers from the first 2 PCs; based on 4 SDs
# - In future, could go and do this by each species
# scale() function makes data normally distributed with mean = 0 and standard deviation = 1 (puts things on common scale; overcomes challenges of different units)
# This needs to happen at the genus level: 1:2 is "meta.traits" while 3:ncol(malus.all2) is "important.traits"
malus.scale <- cbind(malus.all[,c("UID", meta.traits)], scale(malus.all[,important.traits])) # putting descriptors w/ scaled data
  # summary(malus.scale)
  
# Remove variables that are supreme outliers in any one variable
# NOTE: Because we have centered and scaled the data, it'll be normally distributed except for outliers!
# rows.remove <- which(malus.scale[,important.traits]>6)
# summary(rows.remove)

# Removing weirdos
# Being fairly stringent with the outlier number for our sanity
# Currently happening at the genus level --> down the road we'll try to adjust by species
  rows.keep <- apply(malus.scale[,important.traits], 1, FUN=function(x){all(abs(x)<=4)})
  malus.clean <- malus.scale[rows.keep,]
    # summary(malus.clean)

quercus.scale <- cbind(quercus.all[,c("UID", meta.traits)], scale(quercus.all[,important.traits])) # putting descriptors w/ scaled data
  rows.keep <- apply(quercus.scale[,important.traits], 1, FUN=function(x){all(abs(x)<=4)})
  quercus.clean <- quercus.scale[rows.keep,]
    # summary(quercus.clean)

tilia.scale <- cbind(tilia.all[,c("UID", meta.traits)], scale(tilia.all[,important.traits])) # putting descriptors w/ scaled data
  rows.keep <- apply(tilia.scale[,important.traits], 1, FUN=function(x){all(abs(x)<=4)})
  tilia.clean <- tilia.scale[rows.keep,]
    # summary(tilia.clean)

ulmus.scale <- cbind(ulmus.all[,c("UID", meta.traits)], scale(ulmus.all[,important.traits])) # putting descriptors w/ scaled data
  rows.remove <- which(ulmus.scale[,important.traits]>6)
    # summary(rows.remove)
  rows.keep <- apply(ulmus.scale[,important.traits], 1, FUN=function(x){all(abs(x)<=4)})
  ulmus.clean <- ulmus.scale[rows.keep,]
    # summary(ulmus.clean)
  rm(rows.remove, rows.keep)

all.taxa <- rbind(malus.all, quercus.all, tilia.all, ulmus.all)
all.clean <- rbind(malus.clean, quercus.clean, tilia.clean, ulmus.clean)

  rm(malus.clean, quercus.clean, tilia.clean, ulmus.clean)
  rm(malus.soils, quercus.soils, tilia.soils, ulmus.soils)
  rm(malus.clim, quercus.clim, tilia.clim, ulmus.clim)
  rm(malus.scale, quercus.scale, tilia.scale, ulmus.scale)
  rm(malus.all, quercus.all, tilia.all, ulmus.all)
  
save(all.taxa, all.clean, file = file.path(path.dat, "Analysis", "PCA_data.RData"))

####################################################################################################
####################################################################################################

# PCA
  # Used centered data without outliers (don't need to center or scale)
set.seed(1608)

#Malus PCA
malus.pca <- prcomp(all.clean[all.clean$genus %in% "Malus",important.traits], center = FALSE, scale. = FALSE)
  # summary(malus.pca)
  
#Quercus PCA
quercus.pca <- prcomp(all.clean[all.clean$genus %in% "Quercus",important.traits], center = FALSE, scale. = FALSE)
  # summary(quercus.pca)

#Tilia PCA
tilia.pca <- prcomp(all.clean[all.clean$genus %in% "Tilia",important.traits], center = FALSE, scale. = FALSE)
  # summary(tilia.pca)

#Ulmus PCA
ulmus.pca <- prcomp(all.clean[all.clean$genus %in% "Ulmus",important.traits], center = FALSE, scale. = FALSE)
  # summary(ulmus.pca)

####################################################################################################
####################################################################################################
save(all.taxa, all.clean, important.traits, malus.pca, quercus.pca, tilia.pca, ulmus.pca, file = file.path(path.dat, "Analysis", "PCA_data.RData"))
