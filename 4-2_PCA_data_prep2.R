####################################################################################################
####################################################################################################
# 4-2_PCA_data_prep2.R
####################################################################################################
####################################################################################################
rm(list=ls())
### Load packages
my.packages <- c('ggplot2', 'plyr', 'readr', 'readxl', 'dplyr', 'sf', 'tidyverse', 'ks', 'vegan', 
                 'ggbiplot')
# install.packages (my.packages) #Turn on to install current versions
lapply(my.packages, require, character.only=TRUE)
  rm(my.packages)

  
important.traits <- c("ppt.ann.mean", "ppt.min.min",  "soil.ann.max", "soil.max.sd",  "srad.ann.max","srad.ann.sd",  "tmax.ann.min", "tmax.min.sd",  "tmin.ann.min", "tmin.ann.sd","vpd.ann.max",  "vpd.max.sd", "T.GRAVEL", "T.SILT", "T.CLAY", "T.OC", "T.PH.H2O", "T.TEB", "T.ECE", "AWC_VALUE", "T.CEC.CLAY", "T.CEC.SOIL", "T.CACO3", "T.CASO4", "T.ESP")
important.traits.CR <- c("ppt.ann.mean", "ppt.min.min", "soil.ann.max", "soil.max.sd", "srad.ann.max", "srad.ann.sd", "tmax.ann.max", "tmax.max.sd", "tmin.ann.min", "tmin.min.sd", "vpd.ann.max", "vpd.max.sd", "T.SILT", "T.CLAY", "T.OC", "T.PH.H2O", "T.ECE", "AWC_VALUE", "T.CEC.SOIL", "T.CACO3")

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
# load data and further prep
load(file.path(path.dat, "Extracted Data", "PCA_output.RData"))
####################################################################################################
####################################################################################################
## load functions
source("0-X_Ecological_Value_functions.R")
####################################################################################################
####################################################################################################
# filter to complete cases of reduced data (dat.red) to create complete cases for all genera (all.gen)
all.gen <- dat.red %>% filter(complete.cases(dat.red[,all_of(important.traits.CR)]))
row.arb <- which(all.gen$UID=="MORTONARB")

##For each genus, separately center and scale and then and recombine into one dataset
## set genera
  gen.ls <- unique(all.gen$genus)[!unique(all.gen$genus) %in% "MortonArb"]
  # gen.ls <- unique(all.gen$genus)
## set species
  spp.ls <- unique(all.gen$species_name_acc)[!unique(all.gen$species_name_acc) %in% "MortonArb"]
  # spp.ls <- unique(all.gen$species_name_acc)

## need to find records that are outliers and mark so that can remove for PCA
  ## first, scale all by genus and then return as one large data.frame (gen.scale)
    ## also add MortonArb back into df as 1st row
gen.scale <- lapply(gen.ls, center.scale.one, df.all=all.gen, 
                      meta.traits=meta.traits, important.traits=important.traits.CR, 
                      calc.level='genus', center.scale='both') %>% bind_rows


  ## second, identify rows to keep and add column/field to indicate within abs value for 
  ##          genus "in_gen" or for species "in_spp"
  ##    also add MortonArb back into df as 1st row
gen.clean <- lapply(gen.ls, abs_rows2keep, df.all=gen.scale, abs.val=4, meta.traits=meta.traits,
                          important.traits=important.traits.CR, calc.level='genus') %>% bind_rows
####################################################################################################
####################################################################################################
row.arb <- which(gen.clean$UID=="MORTONARB")

save(dat.all, dat.red, meta.traits, important.traits, important.traits.CR, clims, gen.ls, spp.ls, gen.scale, gen.clean, 
     row.arb,
              file=file.path(path.dat, "Extracted Data", "PCA_output.RData"))
  rm(gen.scale, all.gen)
  
# write out area data
  write.csv(gen.clean, file.path(path.dat, "Extracted Data", "data_cleaned_for_PCA.csv"),
                  row.names = FALSE)

####################################################################################################
####################################################################################################
# ## center.scale.one
#   ##  function to center and/or scale values by taxon
#   ## define: 
#     ## df.all; data set to use for the PCA analyses; should be complete cases or without missing values 
#     ## meta.traits; traits that are metadata
#     ## important.traits; traits that are used for PCA
#     ## calc.level; genus/species; should the PCA be done at genus or species level
#     ## center.scale; default="both"; values may be "center", "scale", "both"
#   center.scale.one <- function(x, df.all=all.gen, meta.traits=meta.traits, important.traits=important.traits, 
#                         calc.level='genus', center.scale='both', ...){
#     
#       # if(center.scale=='scale'){
#       #     scaleTRUE  <- TRUE
#       #     centerTRUE <- FALSE
#       #   } else if(center.scale=='center'){
#       #     scaleTRUE  <- FALSE
#       #     centerTRUE <- TRUE
#       #     } else if(center.scale=='both'){
#       #     scaleTRUE  <- TRUE
#       #     centerTRUE <- TRUE
#       #       } else 
#       #       scaleTRUE  <- FALSE
#       #       centerTRUE <- FALSE
# 
#         if(calc.level=='genus'){
#             one.scale <- cbind(df.all[df.all$genus %in% c('MortonArb', x), c("UID", meta.traits)], 
#                                scale(df.all[df.all$genus %in% c('MortonArb', x), important.traits]))
#             # one.scale <- cbind(df.all[df.all$genus %in% c('MortonArb', x), c("UID", meta.traits)], 
#             #                    scale(df.all[df.all$genus %in% c('MortonArb', x), important.traits], 
#             #                         scale=scaleTRUE, center=centerTRUE))
#             one.scale$genus[one.scale$UID %in% 'MORTONARB'] <- paste0('MortonArb_', x)
#         } else if(calc.level=='species'){
#             one.scale <- cbind(df.all[df.all$species_name_acc %in% c('MortonArb', x), 
#                                 c("UID", meta.traits)], 
#                                scale(df.all[df.all$species_name_acc %in% 
#                                 c('MortonArb', x), important.traits]))
#             # one.scale <- cbind(df.all[df.all$species_name_acc %in% c('MortonArb', x), 
#             #                     c("UID", meta.traits)], 
#             #                    scale(df.all[df.all$species_name_acc %in% 
#             #                     c('MortonArb', x), important.traits], 
#             #                         scale=scaleTRUE, center=centerTRUE))
#             one.scale$species_name_acc[one.scale$UID %in% 'MORTONARB'] <- paste0('MortonArb_', x)
#         }
# 
#           if(nrow(one.scale) == 0L){} else return(one.scale)
#         rm(one.scale)
#     }
####################################################################################################
####################################################################################################
# ## abs_rows2keep
#   ##  function to calculate the records that are within a distance of a multiple of absolute value 
#   ##          and then set new field that defines whether the values are in or out of that distance
#   ## define: 
#     ## df.all; data set to use; should be complete cases or without missing values
#     ## meta.traits; traits that are metadata
#     ## important.traits; traits that are used for PCA
#     ## calc.level; genus/species; should the PCA be done at genus or species level
#     ## abs.val: value to define whether records is within or out of the total distance of a multiple 
#     ##        of the absolute value for that field
#   abs_rows2keep <- function(x, df.all=all.gen, abs.val=4, meta.traits=meta.traits, 
#                             important.traits=important.traits, calc.level='genus', ...){
#           if(calc.level=='genus'){
#               one.val <- df.all %>% filter(genus %in% c(x, paste0('MortonArb_', x)))
#               keepers <- apply(one.val[, important.traits], 1, 
#                                FUN=function(x){all(abs(x)<=abs.val)})
#               keep.UID <- one.val[keepers,"UID"]
#               one.keep <- one.val %>% mutate(absval=if_else(one.val$UID %in% keep.UID, 
#                                 paste0("in_gen_", abs.val), paste0("out_gen_", abs.val)))
#           } else if(calc.level=='species'){
#               one.val <- df.all %>% filter(species_name_acc %in% c(x, paste0('MortonArb_', x)))
#               keepers <- apply(one.val[, important.traits], 1, 
#                                FUN=function(x){all(abs(x)<=abs.val)})
#               keep.UID <- one.val[keepers,"UID"]
#               one.keep <- one.val %>% mutate(absval=if_else(one.val$UID %in% keep.UID, 
#                                 paste0("in_spp_", abs.val), paste0("out_spp_", abs.val)))
#           }
#               if(nrow(one.keep) == 0L){} else return(one.keep)
#             rm(one.keep)
#     }
#     
####################################################################################################
####################################################################################################