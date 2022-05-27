# Script to compile and launch the Shiny App
# 1. Read in PCA data and then push just the components needed for graphing to make life easier.

## Potential enhancements: redo hulls to allow the user to specify a confidence interval; but this is a good spot for now
library(ggplot2)

# -----------------------------
# Read the PCA object from Google --> 
# **NOTE: this is a BIG file (>1 GB), so it takes a while and we do NOT want to push the whole thing to the shiny app.
path.dat <- "/Volumes/GoogleDrive/Shared drives/IMLS MFA/Environmental Niche Value" 
load(file.path(path.dat, "Extracted Data", "PCA_output.RData"))
load(file.path(path.dat, "Extracted Data", "HullAnaly.RData")) # Note: this will overwite gen.clean.pca

#### Cleaning up the data
names(gen.clean.pca)
# To Find Morton, UID = MORTONARB; Species = MortonArb
gen.clean.pca[grep("Morton", gen.clean.pca$genus),"genus"] <- unlist(lapply(strsplit(gen.clean.pca$genus[grep("Morton", gen.clean.pca$genus)], "_"), function(x) x[2]))

# Getting rid of NAs just for sanity
gen.clean.pca <- gen.clean.pca[!is.na(gen.clean.pca$PC1) & gen.clean.pca$absval=='in_gen_4',]


### Rounding values to make it a lot easier to plot
gen.clean.pca$PC1.round <- round(gen.clean.pca$PC1, 2)
gen.clean.pca$PC2.round <- round(gen.clean.pca$PC2, 2)
gen.clean.pca$PC3.round <- round(gen.clean.pca$PC3, 2)
dim(gen.clean.pca)
head(gen.clean.pca)

gen.simple.pca <- aggregate(UID ~ genus + species + species_name_acc + PC1.round + PC2.round + mpd.outlier, data=gen.clean.pca, FUN=length)
gen.simple.pca <- rbind(gen.simple.pca, gen.clean.pca[gen.clean.pca$species=="MortonArb",names(gen.simple.pca)])
dim(gen.simple.pca)
summary(gen.simple.pca)

# Just a quick test plot to make sure the outlier identificaiton makes sense
# gen.stats$Quercus[gen.stats$Quercus$species=="Quercus arizonica",]

ggplot(data=gen.simple.pca[gen.simple.pca$genus=="Quercus" & gen.simple.pca$species=="arizonica",]) +
  geom_point(aes(x=PC1.round, y=PC2.round, color=mpd.outlier)) +
  geom_point(aes(x=mean(PC1.round), y=median(PC2.round)), color="black", size=2) +
  geom_point(data=gen.simple.pca[gen.simple.pca$genus=="Quercus" & gen.simple.pca$species=="MortonArb",], aes(x=PC1.round, y=PC2.round), color="orange3", size=5)

# Save our clean, compiled datasets
write.csv(gen.simple.pca, "data/PCA_Points.csv", row.names=F)

save(hull.coords, file="data/PCA_Hulls_Coords.RData")
save(gen.stats, file="data/PCA_OverlapStats.RData")

# Clean up our hulls object
hulls.df <- data.frame()
for(i in 1:length(hull.coords)){
  spp.now <- names(hull.coords)[i]
  hulls.df <- rbind(hulls.df, data.frame(species_name=spp.now, 
                                         genus=stringr::str_split(spp.now, " ")[[1]][1], 
                                         species=stringr::str_split(spp.now, " ")[[1]][2],
                                         hull.coords[[i]]))
}
head(hulls.df)
# summary(pc.hulls_PC1_PC2)
write.csv(hulls.df, "data/PCA_Hulls.csv", row.names=F)

ggplot(data=gen.simple.pca[gen.simple.pca$genus=="Quercus" & gen.simple.pca$species=="arizonica",]) +
  geom_point(aes(x=PC1.round, y=PC2.round, color=mpd.outlier)) +
  geom_point(aes(x=mean(PC1.round), y=median(PC2.round)), color="black", size=2) +
  geom_polygon(data=hulls.df[hulls.df$genus=="Quercus" & hulls.df$species=="arizonica",], aes(x=PC1, y=PC2), alpha=0.5, color="black") +
  geom_point(data=gen.simple.pca[gen.simple.pca$genus=="Quercus" & gen.simple.pca$species=="MortonArb",], aes(x=PC1.round, y=PC2.round), color="orange3", size=5)


# Adding in the option to graph predictors
##Trying to add Eigenvectors to Graph
#Sample Eigenvectors
gen.pcas$Quercus$rotation

#Scaling them
gen.load <- data.frame()
pc.expvar <- data.frame()
for(i in 1:length(gen.pcas)){
  df.tmp <- data.frame(genus=names(gen.pcas)[i], 
                       env.var=row.names(gen.pcas[[i]]$rotation), 
                       gen.pcas[[i]]$rotation[,1:2])
  df.tmp$labx <- df.tmp$PC1*(max(gen.pcas[[i]]$x[,1])+0.5)
  df.tmp$laby <- df.tmp$PC2*(max(gen.pcas[[i]]$x[,2])+0.5)
  df.tmp$xend <- df.tmp$PC1*(max(gen.pcas[[i]]$x[,1])-0.5)
  df.tmp$yend <- df.tmp$PC2*(max(gen.pcas[[i]]$x[,2]))
  
  # Getting the top predictors
  pc.sum <- summary(gen.pcas[[i]])$importance
  
  sum.tmp <- data.frame(genus=names(gen.pcas)[i], PC1=pc.sum[2,1], PC2=pc.sum[2,2], Pcum1.2=pc.sum[3,2])
  df.tmp$dist <- sqrt((df.tmp$PC1*pc.sum[2,1]/pc.sum[3,2])^2 + (df.tmp$PC2*pc.sum[2,2]/pc.sum[3,2])^2)# How long the combined arrow is
  df.tmp$rank <- order(df.tmp$dist)
  
  # Formatting the summary
  
  # Put it together
  gen.load <- rbind(gen.load, df.tmp)
  pc.expvar <- rbind(pc.expvar, sum.tmp)
}
summary(gen.load)
pc.expvar

# Adding some grouping classification to env vars
vars.soil <- c("T.SILT", "T.CLAY", "AWC_VALUE", "T.OC", "T.PH.H2O", "T.ECE", "T.CEC.SOIL", "T.CACO3")
gen.load$var.type <- ifelse(gen.load$env.var %in% c(vars.soil), "Soil", "Climate")
# gen.load

# gen.load$env.var[!gen.load$env.var %in% c("tmax.ann.amx", "tmax.max.sd", "tmin.ann.min", "tmin.min.sd", "ppt.ann.mean", "ppt.min.min", "vpd.ann.max", "vpd.max.sd", "srad.ann.max", "srad.ann.sd", "soil.ann.max", "soil.max.sd", "T.SILT", "T.CLAY", "AWC_VALUE", "T.OC", "T.PH.H2O", "T.ECE", "T.CEC.SOIL", "T.CACO3")]
gen.load$env.var <- factor(gen.load$env.var, levels=c("tmax.ann.max", "tmax.max.sd", "tmin.ann.min", "tmin.min.sd", "ppt.ann.mean", "ppt.min.min", "vpd.ann.max", "vpd.max.sd", "srad.ann.max", "srad.ann.sd", "soil.ann.max", "soil.max.sd", "T.SILT", "T.CLAY", "AWC_VALUE", "T.OC", "T.PH.H2O", "T.ECE", "T.CEC.SOIL", "T.CACO3"))

summary(gen.load[,c("env.var", "genus", "PC1", "PC2", "rank")])

write.csv(gen.load, "data/PCA_loadings.csv", row.names=F)



rsconnect::deployApp(forceUpdate = T, launch.browser = T)

