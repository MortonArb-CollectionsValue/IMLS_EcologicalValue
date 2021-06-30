####################################################################################################
## 4-2_run_PCA_save.R

####################################################################################################
rm(list=ls())
### Load packages
my.packages <- c('ggplot2', 'plyr', 'readr', 'readxl', 'dplyr', 'sf', 'tidyverse', 'ks', 'vegan', 'ggbiplot')
# install.packages (my.packages) #Turn on to install current versions
lapply(my.packages, require, character.only=TRUE)
  rm(my.packages)

####################################################################################################
####################################################################################################
### set paths/folders
  ## path to the shared Google Drive folder
  path.dat <- "/Volumes/GoogleDrive/Shared drives/IMLS MFA/Environmental Niche Value" ## path for Shannon
  # path.dat <- "D:/Data_IMLS_Ecological_Value"   ## path to data for Shiven D drive
  
  # path for the folder for figure output
  path.figs <- file.path(path.dat, "figures")

  # # path on the local drive for accessing the data
  # path.local <- "/Users/aesculus/Box/Research/Active_Projects/IMLS_MortonArb/local_data/PCA_Data"
  # 
####################################################################################################
####################################################################################################
## bring in data
path.clims <- file.path(path.dat, "Extracted Data/Climate_Extract")
path.soils <- file.path(path.dat, "Extracted Data/Soil_Extract")
# path.pcas   <- file.path(path.dat, "Analysis/PCAs")

####################################################################################################
####################################################################################################
## load data
load(file.path(path.dat, "Extracted Data", "PCA_output.RData"))
save(dat.all, dat.red, meta.traits, important.traits,
              file=file.path(path.dat, "Extracted Data", "PCA_output.RData"))
  
## mark rows that are >= 4 standard deviations away
  rows.keep <- apply(tilia.scale[,important.traits], 1, FUN=function(x){all(abs(x)<=4)})
tilia.clean <- tilia.scale[rows.keep,]

  
####################################################################################################
####################################################################################################
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
####################################################################################################
####################################################################################################

  gen.ls <- c('Malus', 'Quercus', 'Tilia', 'Ulmus')

fsoils <- dir(path.dat)

  i.ge <- 'Malus'

  for(i.ge in gen.ls){
    i.ge
    pc.i <- df.in %>% filter(genus==i.ge)
    
  }



  dim(dat.long)