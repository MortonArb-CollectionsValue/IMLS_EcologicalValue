# Script to compile and launch the Shiny App
# 1. Read in PCA data and then push just the components needed for graphing to make life easier.

## Potential enhancements: redo hulls to allow the user to specify a confidence interval; but this is a good spot for now
library(ggplot2)

# -----------------------------
# Read the PCA object from Google --> 
# **NOTE: this is a BIG file (>1 GB), so it takes a while and we do NOT want to push the whole thing to the shiny app.
path.dat <- "/Volumes/GoogleDrive/Shared drives/IMLS MFA/Environmental Niche Value" 
load(file.path(path.dat, "Extracted Data", "PCA_output.RData"))

#### Cleaning up the data
names(gen.clean.pca)
# To Find Morton, UID = MORTONARB; Species = MortonArb
gen.clean.pca[grep("Morton", gen.clean.pca$genus),"genus"] <- unlist(lapply(strsplit(gen.clean.pca$genus[grep("Morton", gen.clean.pca$genus)], "_"), function(x) x[2]))

# Getting rid of NAs just for sanity
gen.clean.pca <- gen.clean.pca[!is.na(gen.clean.pca$PC1),]

# For the sake of cleaning data calculate whether a point is an outlier or not based on distance from the centroid.
for(GEN in unique(gen.clean.pca$genus)){
  for(SPP in unique(gen.clean.pca$species[gen.clean.pca$genus==GEN])){
    spp.ind <- which(gen.clean.pca$genus==GEN & gen.clean.pca$species==SPP)
    if(SPP == "MortonArb") {
      gen.clean.pca[spp.ind, c("outlier.4sig", "outlier.6sig")] <- F } 
    
    # Get the data for the species to make things easier
    dat.spp <- gen.clean.pca[spp.ind,]
    
    # Calcuate the XY Centroid & then the euclidean distance
    cent.x <- mean(dat.spp$PC1)
    cent.y <- mean(dat.spp$PC2)
    dat.spp$dist.euc <- sqrt((dat.spp$PC1-cent.x)^2 + (dat.spp$PC2-cent.y)^2)
    
    dist.mean  <- mean(dat.spp$dist.euc)
    dist.sd <- sd(dat.spp$dist.euc)
    
    dat.spp$outlier.4sig <- ifelse(dat.spp$dist.euc>(dist.mean + 4*dist.sd), T, F)
    dat.spp$outlier.6sig <- ifelse(dat.spp$dist.euc>(dist.mean + 6*dist.sd), T, F)
    
    gen.clean.pca[spp.ind,c("outlier.4sig", "outlier.6sig")] <- dat.spp[,c("outlier.4sig", "outlier.6sig")]
  }
}


### Rounding values to make it a lot easier to plot
gen.clean.pca$PC1.round <- round(gen.clean.pca$PC1, 2)
gen.clean.pca$PC2.round <- round(gen.clean.pca$PC2, 2)
gen.clean.pca$PC3.round <- round(gen.clean.pca$PC3, 2)
dim(gen.clean.pca)
head(gen.clean.pca)

gen.simple.pca <- aggregate(UID ~ genus + species + species_name_acc + PC1.round + PC2.round + outlier.4sig + outlier.6sig, data=gen.clean.pca, FUN=length)
gen.simple.pca <- rbind(gen.simple.pca, gen.clean.pca[gen.clean.pca$species=="MortonArb",names(gen.simple.pca)])
dim(gen.simple.pca)
summary(gen.simple.pca)

# Just a quick test plot to make sure the outlier identificaiton makes sense
ggplot(data=gen.simple.pca[gen.simple.pca$genus=="Quercus" & gen.simple.pca$species=="alba",]) +
  geom_point(aes(x=PC1.round, y=PC2.round, color=outlier.4sig)) +
  geom_point(aes(x=mean(PC1.round), y=median(PC2.round)), color="black", size=2) 

# Save our clean, compiled datasets
write.csv(gen.simple.pca, "data/PCA_Points.csv", row.names=F)

summary(pc.hulls_PC1_PC2)
write.csv(pc.hulls_PC1_PC2, "data/PCA_Hulls.csv", row.names=F)

rsconnect::deployApp(forceUpdate = T, launch.browser = T)
