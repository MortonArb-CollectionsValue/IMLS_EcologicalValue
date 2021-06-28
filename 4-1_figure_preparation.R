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



my.packages <- c('ggplot2', 'plyr', 'readr', 'dplyr', 'sf', 'tidyverse')
# install.packages (my.packages) #Turn on to install current versions
lapply(my.packages, require, character.only=TRUE)
rm(my.packages)

## set pasths/folders
path.dat1 <- "/Volumes/GoogleDrive/Shared drives/IMLS MFA/Environmental Niche Value"
path.figs <- file.path(path.dat1, "figures")
path.local <- "/Users/aesculus/Box/Research/Active_Projects/IMLS_MortonArb/local_data/PCA_Data"

###################################################################################################


# Load paths/folders
path.dat <- "/Volumes/GoogleDrive/Shared drives/IMLS MFA/Environmental Niche Value/Extracted Data/Soil_Extract"
# path.figs <- file.path(path.dat, "..")

fsoils <- dir(path.dat, "Quercus")
dat.arb <- read.csv(file.path(path.dat, "0_MortonArb.csv"))

cols.soils <- c("T.GRAVEL", "T.SAND", "T.SILT", "T.CLAY", "T.OC", "T.PH.H2O", "T.TEB", "T.ECE", "AWC_VALUE", "T.REF.BULK.DENSITY")

# Setting up some column stuff to make it work when things get weird
soilcols <- names(dat.arb)
col.char <- which(soilcols %in% c("nativeDatabaseID", "MU.SOURCE1"))
coltype <- rep(NA, length(soilcols))
coltype[col.char] <- "character"


f.all <- list.files(path = path.dat,
                           pattern = "Quercus", full.names = TRUE)
dat.all <-  lapply(f.all, read.csv, colClasses=coltype) %>% bind_rows()

# Add the Arb in here
dat.all <- rbind(dat.arb, dat.all)

# Cut down to just the variables we care about
dat.long <- stack(dat.all[,cols.soils])
dat.long$UID <- dat.long$UID
dat.long$Species <- dat.long$species_name_acc

dim(dat.long)

# T.ECE is looking really weird
# dat.long[dat.long$ind=="T.ECE" & dat.long$values>1 & !is.na(dat.long$values), "values"] <- NA

## Commenting out the pdf creation as takes awhile. However, this code is good.
# pdf(file.path("Quercus_VariableSummary.pdf"), height=11, width=8)
# for(SPP in unique(dat.long$Species[dat.long$UID!="MORTONARB"])){
#   print(
#     ggplot(data=dat.long[dat.long$UID!="MORTONARB",], aes(x=values)) +
#       facet_wrap(~ind, scales="free") +
#       geom_density(aes(fill="All Genus"), alpha=0.5) +
#       geom_density(data=dat.long[dat.long$Species==SPP,], aes(fill=Species), alpha=0.5) +
#       geom_vline(data=dat.long[dat.long$UID=="MORTONARB",], aes(xintercept=values),color="green4", size=1.5) +
#       scale_fill_manual(values=c("All Genus"="black", "blue")) +
#       scale_y_continuous(expand=c(0,0)) +
#       ggtitle(SPP) +
#       theme(legend.position="bottom",
#             legend.title=element_blank())
#     )
# }
# dev.off()

# Trying an ordination
# randomly subset from our data frame to see if that works
dat.all <- dat.all[complete.cases(dat.all[, cols.soils]),] %>% group_by(species_name_acc)

# pts.samp <- sample(1:nrow(dat.all), 1e6, replace = F)
pc.t1 <- prcomp(dat.all[, cols.soils])
summary(pc.t1)
pc.t1

soil.cor <- cor(dat.all[, cols.soils])
soil.cor
# plot(dat.all[, cols.soils])


soil.cv <- cov(dat.all[, cols.soils])
soil.cv

pc.t2 <- prcomp(dat.all[, cols.soils[!cols.soils %in% c("T.SAND")]])
summary(pc.t2)
pc.t2

pc.t2 <- princomp(dat.all[, cols.soils[!cols.soils %in% c("AWC_VALUE", "T.SAND")]], scores=T)
summary(pc.t2)
summary(pc.t2$scores)
pc.t2$loadings

dat.all$PC1 <- pc.t2$scores[,1]
dat.all$PC2 <- pc.t2$scores[,2]

# Calculating convex hulls for Q. alba, Q. velutina, Q. boyntonii
# library(tidyverse)
# pc.hulls <- dat.all %>% select(species_name_acc, PC1, PC2) %>% group_by(species_name_acc) %>% slice(chull(PC1, PC2))
# summary(pc.hulls)
# 
## Commenting out the ordination as takes awhile. However, this code is good.
# png("TEST_Ordination_Quercus.png", height=8, width=8, units="in", res=320)
# ggplot(data=dat.all, aes(x=PC1, y=PC2)) +
#   geom_point(aes(x=PC1, y=PC2), size=0.25, color="gray50")  +
#   geom_polygon(data=pc.hulls[pc.hulls$species_name_acc %in% c("Quercus alba"),], aes(fill=species_name_acc), alpha=0.25) +
#   geom_polygon(data=pc.hulls[pc.hulls$species_name_acc %in% c("Quercus boyntonii"),], aes(fill=species_name_acc), alpha=0.5) +
#   geom_point(data=dat.all[dat.all$species_name_acc %in% c("Quercus alba"),], aes(color=species_name_acc)) +
#   geom_point(data=dat.all[dat.all$species_name_acc=="Quercus boyntonii",], aes(color=species_name_acc), size=2) +
#   geom_point(data=dat.all[dat.all$species_name_acc=="MortonArb",], aes(color=species_name_acc), size=8) +
#   scale_color_manual(name="Species", values=c("MortonArb"="green4", "Quercus alba"="blue2", "Quercus boyntonii"="red2", "Quercus velutina"="cadetblue3")) +
#   scale_fill_manual(name="Species", values=c("MortonArb"="green4", "Quercus alba"="blue2", "Quercus boyntonii"="red2", "Quercus velutina"="cadetblue3"))
# dev.off()


###################################################################################################
###################################################################################################
###################################################################################################
## set pasths/folders
path.dat1 <- "/Volumes/GoogleDrive/Shared drives/IMLS MFA/Environmental Niche Value"
path.figs <- file.path(path.dat1, "figures")
path.local <- "/Users/aesculus/Box/Research/Active_Projects/IMLS_MortonArb/local_data/PCA_Data"

###################################################################################################
## calculating convex hulls
  ## creating convex hulls using sf method
pca.df <- dat.all %>% select(species_name_acc, PC1, PC2)

  library(sf)
# Convert object to an sf object
  # remove the Morton Arb row as it is a point
    pc.xy <- st_as_sf(pca.df[-1,], coords=c("PC1","PC2"))

# make the sf polygon object containing all species convex hull polygons
    # .ch for convex hull
polys.ch <- pc.xy %>% dplyr::group_by(species_name_acc) %>% dplyr::summarise() %>% 
                  st_cast("POLYGON") %>% st_convex_hull() 
  names(polys.ch)[1] <- "species_name_acc"

# create df for taxa and convex hull areas
pca_ch_areas <- polys.ch %>% st_drop_geometry()
pca_ch_areas$area.ch <- polys.ch %>% st_area()

# save data
save(dat.all, dat.arb, dat.long, pc.t1, pc.t2, pc.xy, pca.df, pca_ch_areas, polys.ch, 
                    file=file.path(path.dat1, "Extracted Data", "PCA_output.RData"))

# write out area data
  write.csv(pca_ch_areas, file.path(path.dat1, "Extracted Data", "areas_convex_hull.csv"), 
                  row.names = FALSE)


####################
load(file.path(path.local, "PCA_output.RData"))

# # define two taxa you want to compare
# spp.a <- "Quercus alba"
# spp.b <- "Quercus boyntonii"
# spp.c <- "Quercus brantii"

####################
# ## going to make polygons and calculate areas using sf package
# # using sf package
# 
# # # make polygons of two taxa you want to compare
# poly.ch.a <- polys.ch %>% dplyr::filter(species_name_acc == spp.a)
# poly.ch.b <- polys.ch %>% dplyr::filter(species_name_acc == spp.b)
# # 
# # # calculate the area of that overlap for the 2 polygons
# overlap.area <- st_intersection(poly.ch.a, poly.ch.b) %>% st_area()

## function to calculate polygon overlap
overlap2spp_polys <- function(a=spp.a, b=spp.b, polys=polys.ch){
    # load libraries
    require(sf); require(dplyr)
  
      # make polygons of two taxa you want to compare
      poly.ch.a <- polys.ch %>% dplyr::filter(species_name_acc == a)
      poly.ch.b <- polys.ch %>% dplyr::filter(species_name_acc == b)
    
      # calculate the area of that overlap for the 2 polygons
      overlap.area <- st_intersection(poly.ch.a, poly.ch.b) %>% st_area()
    # return value    
      if(length(overlap.area) == 0L){overlap.area <- 0}
      
        return(overlap.area)
}


# create table (spp.pairs) for every combination of polygons to overlap
spp_comb <- as_data_frame(unique(polys.ch$species_name_acc))
k <- 2    #k is 2 because want 2 columns in combined dataset
rows <- spp_comb %>% group_by_all() %>% group_split()
row_combinations <- t(combn(x = 1:nrow(spp_comb), m = k)) %>% as_tibble() %>% 
                      rename(spp.a=V1, spp.b=V2) ; rm(spp_comb)

spp.pairs <- row_combinations %>%
  mutate_all(~ map(., ~ pluck(rows, .x))) %>% 
    unnest(cols=c(spp.a, spp.b), names_sep = "_")
  names(spp.pairs) <- c('spp.a', 'spp.b')

# return value for overlap of convex hull polygons for species a (spp.a) and b (spp.b)
  # iterate through all polygon combinations
all_spp_pairs <- spp.pairs %>% rowwise() %>% mutate(area_overlap=
                                                    overlap2spp_polys(spp.a, spp.b, polys.ch))

# add in area for both spp.a and spp.b
  # might want to have species area here to help calculate percentage of overlap
## STILL NEED TO DO, IF WANTED IN SAME DATASET AS AREA OVERLAP

# save data
save(dat.all, dat.arb, dat.long, pc.t1, pc.t2, pc.xy, pca.df, pca_ch_areas, polys.ch, 
          all_spp_pairs, overlap2spp_polys, 
              file=file.path(path.dat1, "Extracted Data", "PCA_output.RData"))

# write out area data
  write.csv(all_spp_pairs, file.path(path.dat1, "Extracted Data", "areas_convex_hull_overlap.csv"), 
              row.names = FALSE)

###############################

load(file.path(path.dat1, "Extracted Data", "PCA_output.RData"))

# Distance from Morton Arboretum to centroid of convex hull for a taxon
  # make st object for the PCA point for Morton Arboretum
    ma.pt <- st_as_sf(pca.df[pca.df$species_name_acc=="MortonArb",], coords=c("PC1","PC2"))
    
  # define PCA point for a taxon
    # subset each species polygon
      # centroid will be calculated in the function (how it is)
        #alternatively, could set centroid outside and feed into function

## function to calculate distance between two points, 
        # where one is defined point and other is a polygon
# dist_pt2centroid <- function(a=ma.pt, t.spp="Quercus alba", polys=polys.ch){
dist_pt2centroid <- function(a=ma.pt, b=species_name_acc, polys=polys.ch){
    # load libraries
    require(sf); require(dplyr)
  
      # calculate the area of that overlap for the 2 polygons
        dist2points <- st_distance(ma.pt, st_centroid(polys.ch %>% 
                                                      dplyr::filter(species_name_acc == b)))
    # return value    
      if(length(dist2points) == 0L){dist2points <- 0}
      
        return(dist2points)
  }

# dist_pt2centroid(ma.pt, species_name_acc, polys=polys.ch)
  
  pca_ch_areas <- pca_ch_areas %>% rowwise() %>% mutate(distMortArb2centroid=
          dist_pt2centroid(ma.pt, species_name_acc, polys.ch))
  
# save data
save(dat.all, dat.arb, dat.long, pc.t1, pc.t2, pc.xy, pca.df, pca_ch_areas, polys.ch, 
          all_spp_pairs, overlap2spp_polys, dist_pt2centroid, 
              file=file.path(path.dat1, "Extracted Data", "PCA_output.RData"))

# write out area and distance data
  write.csv(pca_ch_areas, file.path(path.dat1, "Extracted Data", "areas_distance_convex_hulls.csv"), 
              row.names = FALSE)


###############################  
# Calculate distance from point to nearest edge of polygon  

  dist_pt2edge <- function(a=ma.pt, b=species_name_acc, polys=polys.ch){
    # load libraries
    require(sf); require(dplyr)
  
      # calculate the distance between Morton Arb point and the closest edge of polygon
        pt2edge <- st_geometry(obj = polys.ch %>% dplyr::filter(species_name_acc == b)) %>% 
                        st_cast(to = 'LINESTRING') %>% st_distance(y = ma.pt)

    # return value    
      if(length(pt2edge) == 0L){pt2edge <- 0}
      
        return(pt2edge)
    }

  pca_ch_areas <- pca_ch_areas %>% rowwise() %>% mutate(dist_MortArb2edge=
          dist_pt2edge(ma.pt, species_name_acc, polys.ch))
  pca_ch_areas <- as_data_frame(pca_ch_areas) 
  names(pca_ch_areas) <- c('species_name_acc', 'area.ch', 'distMortArb2centroid', 'dist_MortArb2edge')
  # save data
save(dat.all, dat.arb, dat.long, pc.t1, pc.t2, pc.xy, pca.df, pca_ch_areas, polys.ch, 
          all_spp_pairs, overlap2spp_polys, 
              file=file.path(path.dat1, "Extracted Data", "PCA_output.RData"))

# write out area data
  write.csv(pca_ch_areas, file.path(path.dat1, "Extracted Data", "areas_distance_convex_hulls.csv"), 
              row.names = FALSE)
# path.clim <- "D:/Data_IMLS_Ecological_Value/Total_PostReductions2/"
# path.soils <- "D:/Data_IMLS_Ecological_Value/Total_PostSoilReductions/"
# path.pcas <- "D:/Data_IMLS_Ecological_Value/PCAs"
# 
# #Loading Climate Data
# malus.clim    <- read.csv(file.path(path.clim, "Malus_Climate_Final.csv"))
#   head(malus.clim)
# quercus.clim  <- read.csv(file.path(path.clim, "Quercus_Climate_Final.csv"))
# tilia.clim    <- read.csv(file.path(path.clim, "Tilia_Climate_Final.csv"))
# ulmus.clim    <- read.csv(file.path(path.clim, "Ulmus_Climate_Final.csv"))
# 
# #Loading Soils Data
# malus.soils   <- read.csv(file.path(path.soils, "Malus_Soil_Final.csv"))
# quercus.soils <- read.csv(file.path(path.soils, "Quercus_Soil_Final.csv"))
# tilia.soils   <- read.csv(file.path(path.soils, "Tilia_Soil_Final.csv"))
# ulmus.soils   <- read.csv(file.path(path.soils, "Ulmus_Soil_Final.csv"))
# 
# #ROOTS was omitted because Arb Data is NA at this value
# important.traits <- c("ppt.ann.mean", "ppt.min.min", "soil.ann.max", "soil.max.sd", "srad.ann.max", "srad.ann.sd", "tmax.ann.min",      
#                       "tmax.min.sd", "tmin.ann.min", "tmin.ann.sd", "vpd.ann.max", "vpd.max.sd", "T.GRAVEL", "T.SILT", "T.CLAY", 
#                       "T.OC", "T.PH.H2O", "T.TEB", "T.ECE", "AWC_VALUE", "T.CEC.CLAY", "T.CEC.SOIL", "T.CACO3", "T.CASO4",	"T.ESP")
# 
# #Combining Climate & Soil Data: only using the variables for analysis
#   #Only takes the variables that are in both, gets rid of the variables that are only in 1
#   #Filtering out Morton Arb Data since PCA does not work with Arb point: ROOTS is NA
# meta.traits <- c("genus", "species", "decimalLatitude", "decimalLongitude")
# 
# malus.all   <-  merge(malus.clim,   select(malus.soils,  -all_of(meta.traits)), by="UID")
# quercus.all <-  merge(quercus.clim, select(quercus.soils,-all_of(meta.traits)), by="UID")
# tilia.all   <-  merge(tilia.clim,   select(tilia.soils,  -all_of(meta.traits)), by="UID")
# ulmus.all   <-  merge(ulmus.clim,   select(ulmus.soils,  -all_of(meta.traits)), by="UID") %>% select(-X)
# 
# #Removing predictor outliers -- used ESA script lines 95-117
# 
# # Identify euclidean outliers from the first 2 PCs; based on 4 SDs
# # - In future, could go and do this by each species
# # scale() function makes data normally distributed with mean = 0 and standard deviation = 1 (puts things on common scale; overcomes challenges of different units)
# # This needs to happen at the genus level: 1:2 is "meta.traits" while 3:ncol(malus.all2) is "important.traits"
# malus.scale <- cbind(malus.all[,c("UID", meta.traits)], scale(malus.all[,important.traits])) # putting descriptors w/ scaled data
#   # summary(malus.scale)
#   
# # Remove variables that are supreme outliers in any one variable
# # NOTE: Because we have centered and scaled the data, it'll be normally distributed except for outliers!
# # rows.remove <- which(malus.scale[,important.traits]>6)
# # summary(rows.remove)
# 
# # Removing weirdos
# # Being fairly stringent with the outlier number for our sanity
# # Currently happening at the genus level --> down the road we'll try to adjust by species
#   rows.keep <- apply(malus.scale[,important.traits], 1, FUN=function(x){all(abs(x)<=4)})
#   malus.clean <- malus.scale[rows.keep,]
#     # summary(malus.clean)
# 
# quercus.scale <- cbind(quercus.all[,c("UID", meta.traits)], scale(quercus.all[,important.traits])) # putting descriptors w/ scaled data
#   rows.keep <- apply(quercus.scale[,important.traits], 1, FUN=function(x){all(abs(x)<=4)})
#   quercus.clean <- quercus.scale[rows.keep,]
#     # summary(quercus.clean)
# 
# tilia.scale <- cbind(tilia.all[,c("UID", meta.traits)], scale(tilia.all[,important.traits])) # putting descriptors w/ scaled data
#   rows.keep <- apply(tilia.scale[,important.traits], 1, FUN=function(x){all(abs(x)<=4)})
#   tilia.clean <- tilia.scale[rows.keep,]
#     # summary(tilia.clean)
# 
# ulmus.scale <- cbind(ulmus.all[,c("UID", meta.traits)], scale(ulmus.all[,important.traits])) # putting descriptors w/ scaled data
#   rows.remove <- which(ulmus.scale[,important.traits]>6)
#     # summary(rows.remove)
#   rows.keep <- apply(ulmus.scale[,important.traits], 1, FUN=function(x){all(abs(x)<=4)})
#   ulmus.clean <- ulmus.scale[rows.keep,]
#     # summary(ulmus.clean)
#   rm(rows.remove, rows.keep)
# 
# all.taxa <- rbind(malus.all, quercus.all, tilia.all, ulmus.all)
# all.clean <- rbind(malus.clean, quercus.clean, tilia.clean, ulmus.clean)
# 
#   rm(malus.clean, quercus.clean, tilia.clean, ulmus.clean)
#   rm(malus.soils, quercus.soils, tilia.soils, ulmus.soils)
#   rm(malus.clim, quercus.clim, tilia.clim, ulmus.clim)
#   rm(malus.scale, quercus.scale, tilia.scale, ulmus.scale)
#   rm(malus.all, quercus.all, tilia.all, ulmus.all)
#   
# save(all.taxa, all.clean, file = file.path(path.dat, "Analysis", "PCA_data.RData"))
# 
# ####################################################################################################
# ####################################################################################################
# 
# # PCA
#   # Used centered data without outliers (don't need to center or scale)
# set.seed(1608)
# 
# #Malus PCA
# malus.pca <- prcomp(all.clean[all.clean$genus %in% "Malus",important.traits], center = FALSE, scale. = FALSE)
#   # summary(malus.pca)
#   
# #Quercus PCA
# quercus.pca <- prcomp(all.clean[all.clean$genus %in% "Quercus",important.traits], center = FALSE, scale. = FALSE)
#   # summary(quercus.pca)
# 
# #Tilia PCA
# tilia.pca <- prcomp(all.clean[all.clean$genus %in% "Tilia",important.traits], center = FALSE, scale. = FALSE)
#   # summary(tilia.pca)
# 
# #Ulmus PCA
# ulmus.pca <- prcomp(all.clean[all.clean$genus %in% "Ulmus",important.traits], center = FALSE, scale. = FALSE)
#   # summary(ulmus.pca)
# 
# ####################################################################################################
# ####################################################################################################
# save(all.taxa, all.clean, important.traits, malus.pca, quercus.pca, tilia.pca, ulmus.pca, file = file.path(path.dat, "Analysis", "PCA_data.RData"))
# 
# ####################################################################################################
# ####################################################################################################
# # ### Load functions
# # spatial.outlier<-function(data,x=data,threshold=0.05){
# #   spatial.depth=function(x,data){
# #     if(ncol(as.matrix(x))==1)
# #       x<-t(x)
# #     if(ncol(data)!=ncol(as.matrix(x))){
# #       sd<-"Dimensions do not match"
# #     }else{
# #       spd<-function(data,x)
# #       {
# #         v=array(0,ncol(data))
# #         for(j in 1:ncol(data))
# #         {
# #           for(i in 1:nrow(data))
# #           {
# #             if(sqrt(sum((x-data[i,])^2))!=0)
# #               v[j]=v[j]+((x[j]-data[i,j])/sqrt(sum((x-data[i,])^2)))
# #           }
# #           v[j]=v[j]/nrow(data)
# #         }
# #         sd=1-sqrt(sum(v^2))
# #       }
# #       sd<-apply(x,1,function(y){spd(data,y)})
# #     }
# #     sd
# #   }
# #   sd<-spatial.depth(x,data)
# #   if(length(dim(x))!=0){
# #     m<-list()
# #     m[[1]]<-which(sd<=threshold)
# #     m[[2]]<-x[m[[1]],]
# #     names(m)<-c("index","observation")
# #     if(length(m[[1]])==0)
# #       m<-"FALSE"
# #   }else{
# #     m<-ifelse(sd<=threshold,"TRUE","FALSE")
# #   }
# #   return(m)
# # }
# # 
# ####################################################################################################
# 
