####################################################################################################
####################################################################################################
# 4-1_PCA_data_prep1.R
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
  path.out <- file.path(path.dat, "Analysis/PrelimPCA/figures")
  # path for the folder for figure output
  path.figs <- file.path(path.dat, "figures")
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
dat.all[dat.all$species_name_acc =='MortonArb',]$species <- 'MortonArb'

## bring in climate data, which are in different folders
## make vecot of folder names to iterate
clims <- c('ppt', 'soil', 'srad', 'tmax', 'tmin', 'vpd')

# We were getting some heading issues with the current loop because the data 
#    being read read in doesn't have all of the climcols.  So lets reference off of an example
df.test <- read.csv(f.clim[1])
cols.use <- names(df.test)

## iterate through climate folders
for(clim in clims){
  col.char <- which(cols.use %in% c("UID", "species_name_acc"))
  coltype <- rep(NA, length(cols.use))
  coltype[col.char] <- "character"
  f.clim <- list.files(path = file.path(path.clims, clim), full.names=TRUE)
  dat.clim <-  lapply(f.clim, read.csv, colClasses=coltype) %>% bind_rows() #
  nms.yes <- names(dat.clim)[!names(dat.clim) %in% names(dat.all)]
  dat.clim <- dat.clim %>% select(c("UID", all_of(nms.yes)))
  dat.all <- full_join(dat.all, dat.clim, by="UID")
}
rm(dat.clim, dat.soils, clim)

## save RData object
save(dat.all, clims,
              file=file.path(path.dat, "Extracted Data", "PCA_output.RData"))
# write out all data
  write.csv(dat.all, file.path(path.dat, "Extracted Data", "all_combined_data.csv"), 
                  row.names = FALSE)

    rm(soilcols, col.char, coltype, climcols, col.char2, coltype2, heads.clim, heads.soil, f.clims, 
            f.soils, path.clims, path.soils, nms.yes)
####################################################################################################
####################################################################################################
# Cut down to just the variables we care about
important.traits <- c("ppt.ann.mean", "ppt.min.min",  "soil.ann.max", "soil.max.sd",  "srad.ann.max", 
                      "srad.ann.sd",  "tmax.ann.min", "tmax.min.sd",  "tmin.ann.min", "tmin.ann.sd", 
                      "vpd.ann.max",  "vpd.max.sd",   "T.GRAVEL",     "T.SILT",       "T.CLAY", 
                      "T.OC",         "T.PH.H2O",     "T.TEB",        "T.ECE",        "AWC_VALUE", 
                      "T.CEC.CLAY",   "T.CEC.SOIL",   "T.CACO3",      "T.CASO4",	    "T.ESP")

meta.traits <- c("species_name_acc", "genus", "species", "decimalLatitude", "decimalLongitude")

## reduce dataframe to important variables for PCA
dat.red <- dat.all %>% select("UID", all_of(meta.traits), all_of(important.traits))

## save RData object
save(dat.all, dat.red, meta.traits, important.traits, clims, 
              file=file.path(path.dat, "Extracted Data", "PCA_output.RData"))
# write out area data
  write.csv(dat.red, file.path(path.dat, "Extracted Data", "all_reduced_data_for_PCA.csv"), 
                  row.names = FALSE)
