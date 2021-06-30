

# 4-2_PCA_data_prep2.R

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
# load data and further prep
load(file.path(path.dat, "Extracted Data", "PCA_output.RData"))

####################################################################################################
####################################################################################################
# filter to complete cases of reduced data (dat.red) to create complete cases for all genera (all.gen)
all.gen <- dat.red %>% filter(complete.cases(dat.red[,all_of(important.traits)]))
row.arb <- which(all.gen$UID=="MORTONARB")
## entire dataset without thje MORTONARB row
  no.arb <- all.gen[-row.arb,]

##For each genus, separately center and scale and then and recombine into one dataset
## set genera
  gen.ls <- unique(all.gen$genus)[!unique(all.gen$genus) %in% "MortonArb"]
## set species
  spp.ls <- unique(all.gen$species_name_acc)[!unique(all.gen$species_name_acc) %in% "MortonArb"]

####################################################################################################
####################################################################################################
  
    scale.one <- function(x, df.all=no.arb, meta.traits=meta.traits, important.traits=important.traits, calc.level='genus'){
          if(calc.level=='genus'){
            # one.val <- df.all %>% filter(genus==x1)
              one.scale <- cbind(df.all[df.all$genus %in% x, c("UID", meta.traits)], scale(df.all[df.all$genus %in% x, important.traits]))
          } else if(calc.level=='species'){
              one.scale <- cbind(df.all[df.all$species_name_acc %in% x, c("UID", meta.traits)], scale(df.all[df.all$species_name_acc %in% x, important.traits]))
          }
              if(nrow(one.scale) == 0L){} else return(one.scale)
            rm(one.scale)
    }

  abs_rows2keep <- function(x, df.all=no.arb, abs.val=4, meta.traits=meta.traits, important.traits=important.traits, calc.level='genus', ...){
          if(calc.level=='genus'){
            # one.val <- df.all %>% filter(genus==x1)
            keepers <- apply(df.all[df.all$genus %in% x, important.traits], 1, FUN=function(x){all(abs(x)<=abs.val)})
            keep.UID <- df.all[keepers,"UID"]
            one.keep <- df.all %>% mutate(absval_in=if_else(df.all$UID %in% keep.UID, "in", "out"))
          } else if(calc.level=='species'){
            keepers <- apply(df.all[df.all$species_name_acc %in% x, important.traits], 1, FUN=function(x){all(abs(x)<=abs.val)})
            keep.UID <- df.all[keepers,"UID"]
            one.keep <- df.all %>% mutate(absval_in=if_else(df.all$UID %in% keep.UID, "in", "out"))
          }
              if(nrow(one.keep) == 0L){} else return(one.keep)
            rm(one.keep)
    }
    
####################################################################################################
####################################################################################################
## need to find records that are outliers and mark so that can remove for PCA
  ## first, scale all by genus and then return as one large data.frame (gen.scale)
    ## also add MortonArb back into df as 1st row
gen.scale <- bind_rows(all.gen[row.arb,], lapply(gen.ls, scale.one, df.all=no.arb, meta.traits=meta.traits, 
                    important.traits=important.traits, calc.level='genus') %>% bind_rows)
  rm(no.arb)
  ## second, identify rows to keep and add column/field to indicate within abs value
    ## also add MortonArb back into df as 1st row
    no.arb <- gen.scale[-row.arb,]

gen.clean <- bind_rows(c(gen.scale[row.arb,], absval_in="in"), lapply(gen.ls, abs_rows2keep, df.all=no.arb, abs.val=4, meta.traits=meta.traits, 
                    important.traits=important.traits, calc.level='genus') %>% bind_rows)
  rm(no.arb)

####################################################################################################
####################################################################################################
  
save(dat.all, dat.red, meta.traits, important.traits, clims, gen.ls, spp.ls, gen.scale, gen.clean,
      
              file=file.path(path.dat, "Extracted Data", "PCA_output.RData"))
# write out area data
  write.csv(dat.red, file.path(path.dat, "Extracted Data", "all_reduced_data_for_PCA.csv"),
                  row.names = FALSE)

####################################################################################################
####################################################################################################
  
  
#     # scale.one(x1=gen.ls[1], df.all=no.arb, meta.traits=meta.traits, important.traits=important.traits, calc.level='genus')
#     # 
#     
# 
# 
# 
#     vapply(gen.ls, scale.one, c(x1=gen.ls, df.all=no.arb, meta.traits=meta.traits, important.traits=important.traits, calc.level='genus'))
# 
#     all.scale <- apply(gen.ls, FUN=scale.one(x1, df.all=no.arb, calc.level='genus')) %>% bind_rows()
#     # all.scale <- lapply(gen.ls, FUN=scale.genus(all.gen)) %>% bind_rows()
# 
#     # quercus.scale[,3:ncol(quercus.scale)], 1, FUN=function(x){all(abs(x)<=4)}
#     
#     
#     one.gen <- all.gen %>% filter(genus==g)
#       gen.scale <- cbind(one.gen[,meta.traits], scale(one.gen[,important.traits])) # putting descriptors w/ scaled data
#     rm(one.gen)
#   
# 
#   all.scale <- cbind(all.gen[,meta.traits], scale(all.gen[,important.traits])) # putting descriptors w/ scaled data
#     summary(all.scale)
#   names(all.gen[, important.traits])
# 
#   
# # Remove variables that are supreme outliers in any one variable
# # NOTE: Because we have centered and scaled the data, it'll be normally distributed except for outliers!
# #rows.remove <- which(quercus.scale[,3:ncol(quercus.scale)]>6)
# #summary(rows.remove)
# 
# # Removing weirdos
# # Being fairly stringent with the outlier number for our sanity
# # Currently happening at the genus level --> down the road we'll try to adjust by species
# rows.keep <- apply(quercus.scale[,3:ncol(quercus.scale)], 1, FUN=function(x){all(abs(x)<=4)})
# 
# quercus.clean <- quercus.scale[rows.keep,]
# summary(quercus.clean)
#   
#   
#   tilia.pca1 <- prcomp(tilia.all2[,important.traits], center = TRUE, scale. = TRUE)
# 
#   pc.t1 <- prcomp(dat.red[, important.traits], center = TRUE, scale. = TRUE)
# summary(pc.t1)
# pc.t1
# 
# soil.cor <- cor(dat.all[, cols.soils])
# soil.cor
# plot(dat.all[, cols.soils])
# 
# 
# soil.cv <- cov(dat.all[, cols.soils])
# soil.cv
# 
# pc.t2 <- prcomp(dat.all[, cols.soils[!cols.soils %in% c("T.SAND")]])
# summary(pc.t2)
# pc.t2
# 
# pc.t2 <- princomp(dat.all[, cols.soils[!cols.soils %in% c("AWC_VALUE", "T.SAND")]], scores=T)
# summary(pc.t2)
# summary(pc.t2$scores)
# pc.t2$loadings
# 
# dat.all$PC1 <- pc.t2$scores[,1]
# dat.all$PC2 <- pc.t2$scores[,2]
# 
# ####################################################################################################
# ####################################################################################################
# ## save data
# 
# save(dat.all, dat.red, meta.traits, important.traits, clims, gen.ls, spp.ls,
#               file=file.path(path.dat, "Extracted Data", "PCA_output.RData"))
# # write out area data
#   write.csv(dat.red, file.path(path.dat, "Extracted Data", "all_reduced_data_for_PCA.csv"), 
#                   row.names = FALSE)
# 
# ####################################################################################################
# ####################################################################################################
