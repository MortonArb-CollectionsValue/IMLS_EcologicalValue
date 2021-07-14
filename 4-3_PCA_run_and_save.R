####################################################################################################
## 4-3_PCA_run_and_save.R
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
# load data for PCA
load(file.path(path.dat, "Extracted Data", "PCA_output.RData"))
####################################################################################################
####################################################################################################
## load functions
source("0-X_Ecological_Value_functions.R")
  
## CR updating important Traits
important.traits.CR <- c("ppt.ann.mean", "ppt.min.min", "soil.ann.max", "soil.max.sd", "srad.ann.max", "srad.ann.sd", "tmax.ann.max", "tmax.max.sd", "tmin.ann.min", "tmin.min.sd", "vpd.ann.max", "vpd.max.sd", "T.SILT", "T.CLAY", "T.OC", "T.PH.H2O", "T.ECE", "AWC_VALUE", "T.CEC.SOIL", "T.CACO3")
####################################################################################################
####################################################################################################
## make list of PCA objects by genus, 
    ##  with each PCA object in list named by taxon
  gen.pcas <- lapply(gen.ls, prcomp_calcs, df.all=gen.clean[gen.clean$absval=='in_gen_4',], meta.traits=meta.traits, 
                    important.traits=important.traits.CR, center.scale="none", calc.level='genus')
  names(gen.pcas) <- gen.ls
####################################################################################################
####################################################################################################
## make one df with PCs included
gen.clean.all <- lapply(gen.ls, bind.objects.cols) %>% bind_rows
gen.clean.pca <- left_join(gen.clean, gen.clean.all, by=names(gen.clean))
  rm(gen.clean.all)
####################################################################################################
####################################################################################################
## save all data
save(dat.all, dat.red, meta.traits, important.traits, important.traits.CR, gen.ls, spp.ls, gen.scale, gen.clean, row.arb,
          gen.pcas, gen.clean.pca,
          file=file.path(path.dat, "Extracted Data", "PCA_output.RData"))

####################################################################################################
####################################################################################################

# Calculating convex hulls for species
  ## define which PCs to use for chull  
    pc.incl1 <- 'PC1'
    pc.incl2 <- 'PC2'
  
  pc.hulls <- gen.clean.pca %>% filter(genus %in% gen.ls, absval=='in_gen_4') %>% select(c("species_name_acc", all_of(pc.incl1), all_of(pc.incl2))) %>% group_by(species_name_acc) %>% slice(chull(PC1, PC2))
    nms <- gen.clean.pca %>% select(species_name_acc, genus, species) %>% distinct()
    pc.hulls2 <- left_join(pc.hulls, nms, by="species_name_acc") %>% relocate(species_name_acc, genus, species)
    
   assign(paste0("pc.hulls", "_", pc.incl1, "_", pc.incl2), pc.hulls2); rm(pc.hulls2)
   
####################################################################################################
####################################################################################################
## save all data
save(dat.all, dat.red, meta.traits, important.traits, gen.ls, spp.ls, gen.scale, gen.clean, row.arb,
          gen.pcas, gen.clean.pca, pc.hulls_PC1_PC2,
          file=file.path(path.dat, "Extracted Data", "PCA_output.RData"))

   
####################################################################################################
####################################################################################################
# ## prcomp_calcs
#   ##  function to run PCA by taxon and then return PCA object
#   ## define: 
#     ## df.all; data set to use for the PCA analyses; should be complete cases or without missing values 
#     ## meta.traits; traits that are metadata
#     ## important.traits; traits that are used for PCA
#     ## calc.level; genus/species; should the PCA be done at genus or species level
#     ## center.scale; default="both"; values may be "center", "scale", "both"; 
#     ##        should the PCA center and/or scale the data
#     prcomp_calcs <- function(x, df.all=gen.clean, meta.traits=meta.traits, important.traits=important.traits, 
#                              center.scale="both", calc.level='genus', ...){
#       
#       if(center.scale=='scale'){
#           scaleTRUE  <- TRUE
#           centerTRUE <- FALSE
#         } else if(center.scale=='center'){
#           scaleTRUE  <- FALSE
#           centerTRUE <- TRUE
#           } else if(center.scale=='both'){
#           scaleTRUE  <- TRUE
#           centerTRUE <- TRUE
#             } else 
#             scaleTRUE  <- FALSE
#             centerTRUE <- FALSE
# 
#           if(calc.level=='genus'){
#               one.pca <- prcomp(df.all[df.all$genus %in% c(x, paste0('MortonArb_', x)), 
#                                        important.traits], center=centerTRUE, scale.=scaleTRUE)
#           } else if(calc.level=='species'){
#               one.pca <- prcomp(df.all[df.all$species_name_acc %in% c(x, paste0('MortonArb_', x)), 
#                                        important.traits], center=centerTRUE, scale.=scaleTRUE)
#           }
#               return(one.pca)
#             rm(one.pca)
#     }
#   
####################################################################################################
####################################################################################################