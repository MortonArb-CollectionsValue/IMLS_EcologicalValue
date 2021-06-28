####################################################################################################
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
heads.clim <- read_excel(file.path(path.dat, 'Extracted Data/MortonArb_headers.xlsx'), sheet='MortonArb_headers_clims')
heads.soil <- read_excel(file.path(path.dat, 'Extracted Data/MortonArb_headers.xlsx'), sheet='MortonArb_headers_soils')
climcols <- names(heads.clim)
soilcols <- names(heads.soil)

# Setting up some column stuff to make it work when things get weird
col.char <- which(soilcols %in% c("UID", "species_name_acc", "nativeDatabaseID", "MU.SOURCE1"))
coltype <- rep(NA, length(soilcols))
  coltype[col.char] <- "character"
f.soils   <- list.files(path = path.soils, full.names = TRUE)
  dat.soils <-  lapply(f.soils, read.csv, colClasses=coltype) %>% bind_rows()

  dat.all <- dat.soils %>% select(-c(decimalLatitude.1, decimalLongitude.1)) %>% mutate(species_name_acc2 = species_name_acc) %>% 
    relocate(UID) %>% relocate(AWC.CLASS, hwsd_code, .after = last_col())  %>% relocate(species_name_acc2, .after = species_name_acc)
  dat.all <- tidyr::separate(dat.all, col = "species_name_acc2", into=c("genus", "species"), " ", extra="merge")
    names(dat.all)
    head(dat.all)
unique(dat.all$species_name_acc)
names(dat.all)  
clims <- c('ppt', 'soil', 'srad', 'tmax', 'tmin', 'vpd')

      for(clim in clims){
            col.char <- which(climcols %in% c("UID", "species_name_acc"))
            coltype <- rep(NA, length(climcols))
              coltype[col.char] <- "character"
        f.clim <- list.files(path = file.path(path.clims, clim), full.names=TRUE)
          dat.clim <-  lapply(f.clim, read.csv, colClasses=coltype) %>% bind_rows() #
          nms.yes <- names(dat.clim)[!names(dat.clim) %in% names(dat.all)]
        dat.clim <- dat.clim %>% select(c("UID", all_of(nms.yes)))
        dat.all <- full_join(dat.all, dat.clim, by="UID")
        }
rm(dat.clim, dat.soils)
# dat.all <- tidyr::separate(dat.all, col = "species_name_acc", into=c("genus", "species"))
#   names(dat.all)
# 
save(dat.all,
              file=file.path(path.dat, "Extracted Data", "PCA_output.RData"))
# write out area data
  write.csv(dat.all, file.path(path.dat, "Extracted Data", "all_combined_data.csv"), 
                  row.names = FALSE)

    rm(soilcols, col.char, coltype, climcols, col.char2, coltype2, heads.clim, heads.soil, f.clims, 
            f.soils, path.clims, path.soils, clims, nms.yes)
####################################################################################################
####################################################################################################
# Cut down to just the variables we care about
important.traits <- c("ppt.ann.mean", "ppt.min.min",  "soil.ann.max", "soil.max.sd",  "srad.ann.max", 
                      "srad.ann.sd",  "tmax.ann.min", "tmax.min.sd",  "tmin.ann.min", "tmin.ann.sd", 
                      "vpd.ann.max",  "vpd.max.sd",   "T.GRAVEL",     "T.SILT",       "T.CLAY", 
                      "T.OC",         "T.PH.H2O",     "T.TEB",        "T.ECE",        "AWC_VALUE", 
                      "T.CEC.CLAY",   "T.CEC.SOIL",   "T.CACO3",      "T.CASO4",	    "T.ESP")

meta.traits <- c("species_name_acc", "genus", "species", "decimalLatitude", "decimalLongitude")
dat.red <- dat.all %>% select("UID", all_of(meta.traits), all_of(important.traits))

save(dat.all, dat.red, meta.traits, important.traits,
              file=file.path(path.dat, "Extracted Data", "PCA_output.RData"))
# write out area data
  write.csv(dat.red, file.path(path.dat, "Extracted Data", "all_reduced_data_for_PCA.csv"), 
                  row.names = FALSE)

####################################################################################################
####################################################################################################
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