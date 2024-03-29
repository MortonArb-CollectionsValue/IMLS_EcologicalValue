# Setting up a new script to play around with the calculation of hulls and do the stats

####################################################################################################
## 4-4_Figures_PCA_Clouds_by_genus_species.R
####################################################################################################
####################################################################################################
rm(list=ls())
### Load packages
my.packages <- c('ggplot2', 'plyr', 'readr', 'dplyr', 'sf', 'tidyverse', 'ks', 'vegan', 'ggbiplot', "tictoc")
library(sp)
library(rgeos)
# install.packages (my.packages) #Turn on to install current versions
lapply(my.packages, require, character.only=TRUE)
rm(my.packages)
####################################################################################################
####################################################################################################
### set paths/folders
## path to the shared Google Drive folder
path.dat <- "/Volumes/GoogleDrive/Shared drives/IMLS MFA/Environmental Niche Value" ## path for Shannon & Christy
# path.dat <- "D:/Data_IMLS_Ecological_Value"   ## path to data for Shiven D drive
path.out <- file.path(path.dat, "Analysis/PrelimPCA/figures")
# path for the folder for figure output
path.figs <- file.path(path.dat, "figures")
####################################################################################################
####################################################################################################
# load PCA data for figures
load(file.path(path.dat, "Extracted Data", "PCA_output.RData"))

# Create a simplified PCA for graphing
# gen.clean.pca$PC1.round <- round(gen.clean.pca$PC1, 2)
# gen.clean.pca$PC2.round <- round(gen.clean.pca$PC2, 2)
# dim(gen.clean.pca)
# head(gen.clean.pca)

# gen.simple.pca <- aggregate(UID ~ genus + PC1.round + PC2.round, data=gen.clean.pca, FUN=length)
# gen.simple.pca <- rbind(gen.simple.pca, gen.clean.pca[gen.clean.pca$species=="MortonArb",names(gen.simple.pca)])
# dim(gen.simple.pca)

####################################################################################################
####################################################################################################
## load functions
source("0-X_Ecological_Value_functions.R")


####################################################################################################
# Test stats with different options and see how they compare
# 1. Hull Identification: 
#    1.1 full convex; 
#    1.2. outliers: A) 4 sd; B) 6 sd); 
#    1.3. Percentiles: 50%, 75%, 90%, 95%
#    -- in all cases, try the trimming options based: A) on distance from centroid; B) PC1/2 values
# 2. Calculate Hull Stats: 
#    2.1 Area; #
#    2.2 congeneric pairwise overlap; 
#    2.3 Arb distance to A) perimeter; B) centroid
# 3. 
####################################################################################################
# # Code from original analysis script (4-3_PCA_run_and_save.R)
# pc.incl1 <- 'PC1'
# pc.incl2 <- 'PC2'
# 
# pc.hulls <- gen.clean.pca %>% filter(genus %in% gen.ls, absval=='in_gen_4') %>% select(c("species_name_acc", all_of(pc.incl1), all_of(pc.incl2))) %>% group_by(species_name_acc) %>% slice(chull(PC1, PC2))
# nms <- gen.clean.pca %>% select(species_name_acc, genus, species) %>% distinct()
# pc.hulls2 <- left_join(pc.hulls, nms, by="species_name_acc") %>% relocate(species_name_acc, genus, species)
gen.clean.pca$mpd.outlier <- NA
hull.coords <- list()
pca.hulls <- list()
boot.npts=500
for(SPP in unique(gen.clean.pca$species_name_acc)){
  if(SPP == "MortonArb") next
  print(SPP)
  # SPP="Quercus robur"
  
  rows.spp <- which(gen.clean.pca$species_name_acc==SPP & gen.clean.pca$absval=="in_gen_4")
  
  if(length(rows.spp)<=5) next # skip over really small species
  
  # Finding the niche outliers for each species
  dat.spp <- gen.clean.pca[rows.spp,]
  
  # Going to need to do something different for very large datasets
  if(length(rows.spp)>=100*boot.npts){
    # For very large datasets, we'll need to run the algorithm on a subset of points
    # This won't be ideal, but hopefully the randomization will make it work out
    n.grps <- round(length(rows.spp)/(100*boot.npts), 0)+1 # This means we'll always be rounding up essentially
    n.samp <- round(length(rows.spp)/n.grps, 0) # this will make sure our groups are relatively balanced
    todo <- 1:length(rows.spp) # Rows that still need to be processed
    for(i in 1:n.grps){
      donow <- sample(todo, min(n.samp, length(todo)), replace=F) # Sample from the points that need distances
      dat.spp$mpd.outlier[donow] <- trimOutliers(dat.spp[donow,], pc.incl1="PC1", pc.incl2="PC2", sd.out=4, boot.npts=boot.npts, boot.nsamp=10)
      todo <- todo[!todo %in% donow] # Remove the points we just did
    }
  } else {
    # It's small enough we can just 
    dat.spp$mpd.outlier  <- trimOutliers(dat.spp, pc.incl1="PC1", pc.incl2="PC2", sd.out=4, boot.npts=boot.npts, boot.nsamp=10)
  }
  
  gen.clean.pca$mpd.outlier[rows.spp] <- dat.spp$mpd.outlier
    
  
  # Calculating the hull for each species
  dat.spp <- dat.spp[!dat.spp$mpd.outlier & !is.na(dat.spp$mpd.outlier),]
  if(nrow(dat.spp)<5) next
  pc.hulls <- chull(dat.spp[,c("PC1", "PC2")])
  # hull.coords <- c(pc.hulls, pc.hulls[1])
  hull.coords[[SPP]] <- dat.spp[c(pc.hulls, pc.hulls[1]),c("PC1", "PC2")]
  pca.hulls[[SPP]] <- xy2SP(hull.coords[[SPP]][,c("PC1", "PC2")])
  
}

# save(gen.clean.pca, pca.hulls,
     # file=file.path(path.dat, "Extracted Data", "HullAnaly.RData"))


###### Calculating the overlap statistics; this is an ugly way to do it, but :shrug:
gen.stats <- list()
gen.overlap <- list()

for(GEN in c("Malus", "Quercus", "Tilia", "Ulmus")){
  spp.gen <- unique(gen.clean.pca$species_name_acc[gen.clean.pca$genus==GEN])
  dat.gen <- data.frame(species=spp.gen, hull.TMA=NA, area=NA, over.min=NA, over.mean=NA, over.max=NA)
  mat.overlap <- array(dim=c(length(spp.gen), length(spp.gen)), dimnames=list(spp.gen, spp.gen))
  pt.arb <- gen.clean.pca[gen.clean.pca$genus==paste("MortonArb", GEN, sep="_"), ]
  
  for(i in 1:length(spp.gen)){
    dat.spp <- gen.clean.pca[gen.clean.pca$species_name_acc==spp.gen[i] & gen.clean.pca$absval=="in_gen_4",]
    dat.gen$dist.TMA[i] <- sqrt((pt.arb$PC1 - mean(dat.spp$PC1, na.rm=T))^2 + (pt.arb$PC2 - mean(dat.spp$PC2, na.rm=T))^2)
    
    if(!spp.gen[i] %in% names(pca.hulls)) next

    # This is a really round about way of doing something that should be simple, but so it goes
    spp.pts <- hull.coords[[spp.gen[i]]]
    dat.gen$hull.TMA[i] <- ifelse(point.in.polygon(point.x=pt.arb$PC1, point.y=pt.arb$PC2,
                                                    pol.x=spp.pts[,"PC1"],
                                                    pol.y=spp.pts[,"PC2"]),
                                   T, F)
    
    dat.gen$area[i] <- rgeos::gArea(pca.hulls[[spp.gen[i]]]) # Extract the hull area
    
    
    
    for(j in 1:length(spp.gen)){
      if(!spp.gen[j] %in% names(pca.hulls) | i==j) next
      spp.int <- rgeos::gIntersection(pca.hulls[[spp.gen[i]]], pca.hulls[[spp.gen[j]]])
      if(is.null(spp.int)){
        mat.overlap[i,j] <- 0
      } else {
        mat.overlap[i,j] <- rgeos::gArea(spp.int) 
      }
    } # End j loop
    dat.gen$over.max[i] <- max(mat.overlap[i,], na.rm=T)
    dat.gen$over.mean[i] <- mean(mat.overlap[i,], na.rm=T)
    dat.gen$over.min[i] <- min(mat.overlap[i,], na.rm=T)
  }# End i loop
  
  dat.gen$p.over.mean <- dat.gen$over.mean/dat.gen$area
  dat.gen$p.over.max <- dat.gen$over.max/dat.gen$area
  
  gen.stats[[GEN]] <- dat.gen
  gen.overlap[[GEN]] <- mat.overlap
} # End Genus loop


save(gen.clean.pca, hull.coords, pca.hulls, gen.stats, gen.overlap,
     file=file.path(path.dat, "Extracted Data", "HullAnaly.RData"))

save(gen.stats, file=file.path(path.dat, "Extracted Data", "HullAnaly_GeneraSppStats.RData"))
