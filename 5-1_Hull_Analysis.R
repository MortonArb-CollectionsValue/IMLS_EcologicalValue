# Setting up a new script to play around with the calculation of hulls and do the stats

####################################################################################################
## 4-4_Figures_PCA_Clouds_by_genus_species.R
####################################################################################################
####################################################################################################
rm(list=ls())
### Load packages
my.packages <- c('ggplot2', 'plyr', 'readr', 'dplyr', 'sf', 'tidyverse', 'ks', 'vegan', 'ggbiplot')
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
gen.clean.pca$PC1.round <- round(gen.clean.pca$PC1, 2)
gen.clean.pca$PC2.round <- round(gen.clean.pca$PC2, 2)
dim(gen.clean.pca)
head(gen.clean.pca)

gen.simple.pca <- aggregate(UID ~ genus + PC1.round + PC2.round, data=gen.clean.pca, FUN=length)
gen.simple.pca <- rbind(gen.simple.pca, gen.clean.pca[gen.clean.pca$species=="MortonArb",names(gen.simple.pca)])
dim(gen.simple.pca)

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

summary(gen.clean.pca)
# Using Quercus alba as a test case
dat.test <- gen.clean.pca[gen.clean.pca$genus=="Quercus" & gen.clean.pca$species=="alba" & gen.clean.pca$absval=="in_gen_4", c("UID", "genus", "species", "decimalLatitude", "decimalLongitude", "PC1", "PC2")]

plot.base <- ggplot(data=dat.test) +
  geom_point(data=gen.simple.pca, aes(x=PC1.round, y=PC2.round), size=0.1, alpha=0.25, color="gray50") +
  geom_point(aes(x=PC1, y=PC2), size=1.5, color="dodgerblue2", alpha=0.5) +
  theme_bw()

# We need a test for multivariate outliers;
# we might be able to take somethign from spatial stats
# Resources:
#   - Cook's Distance; car package: http://r-statistics.co/Outlier-Treatment-With-R.html
#   - Mahalanobis: https://www.r-bloggers.com/2019/01/a-new-way-to-handle-multivariate-outliers/
#                  https://towardsdatascience.com/mahalonobis-distance-and-outlier-detection-in-r-cb9c37576d7d

# Workign off the Mahalanobis example from towardsdatascience
# 1. Get the mean and covariance matrix between these variables
pc.cent <- apply(dat.test[,c("PC1", "PC2")], 2, mean)
pc.cov  <- cov(dat.test[,c("PC1", "PC2")])
pc.cent
pc.cov

# 2. drawing an ellipse based on a chi-sq probability
rad.95 <- qchisq(p=0.95, df=2)
rad.90 <- qchisq(p=0.90, df=2)
rad.75 <- qchisq(p=0.75, df=2)

ellipse95 <- car::ellipse(center=pc.cent, shape=pc.cov, radius=rad.95, segments=100, draw=F)
ellipse90 <- car::ellipse(center=pc.cent, shape=pc.cov, radius=rad.90, segments=100, draw=F)
ellipse75 <- car::ellipse(center=pc.cent, shape=pc.cov, radius=rad.75, segments=100, draw=F)

ellipse95.df <- as.data.frame(ellipse95); colnames(ellipse95.df) <- c("PC1", "PC2")
ellipse90.df <- as.data.frame(ellipse90); colnames(ellipse90.df) <- c("PC1", "PC2")
ellipse75.df <- as.data.frame(ellipse75); colnames(ellipse75.df) <- c("PC1", "PC2")


plot.base + 
  geom_polygon(data=ellipse90,df, aes(x=PC1, y=PC2, fill="90% ellipse", color="90% ellipse"), alpha=0.25) +
  geom_polygon(data=ellipse75.df, aes(x=PC1, y=PC2, fill="75% ellipse", color="75% ellipse"), alpha=0.25) +
  # geom_density_2d(aes(x=PC1, y=PC2, color="95% ellipse"), color="orange2") +
  # geom_polygon_2d(aes(x=PC1, y=PC2, fill="75% ellipse", color="75% ellipse"), alpha=0.25) +
  scale_fill_manual(values=c("95% ellipse" = "dodgerblue1", "75% ellipse" = "orange2")) +
  scale_color_manual(values=c("95% ellipse" = "dodgerblue1", "75% ellipse" = "orange2"))

# Trying poitns in polygon
library(sp)
xy2SP <- function(xy, ID=NULL) {
  if(is.null(ID)) ID <- sample(1e12, size=1)
  SpatialPolygons(list(Polygons(list(Polygon(xy)), ID=ID)))
}

ell90sp <- xy2SP(ellipse90)
testsp <- SpatialPointsDataFrame(dat.test[,c("PC1", "PC2")], dat.test)
test.poly <- over(testsp, ell90sp)
summary(test.poly)

dat.test$ell90outlier <- ifelse(is.na(test.poly), T, F)

dat.test2 <- dat.test[!dat.test$ell90outlier,]
pc.hulls <- chull(dat.test2[,c("PC1", "PC2")])
# hull.coords <- c(pc.hulls, pc.hulls[1])
hull.coords <- dat.test2[c(pc.hulls, pc.hulls[1]),]
hull.sp <- xy2SP(hull.coords[,c("PC1", "PC2")])

ggplot(data=dat.test) +
  # geom_point(data=gen.simple.pca, aes(x=PC1.round, y=PC2.round), size=0.1, alpha=0.25, color="gray50") +
  geom_polygon(data=ellipse90.df, aes(x=PC1, y=PC2), fill="dodgerblue2", color="dodgerblue2", alpha=0.25) +
  # geom_point(aes(x=PC1, y=PC2, color=ell90outlier), size=1.5, alpha=0.5) +
  geom_polygon(data=hull.coords, aes(x=PC1, y=PC2), fill="red2", color="red2", alpha=0.25) +
  scale_fill_manual(values=c("FALSE" = "dodgerblue2", "TRUE" = "orange2")) +
  scale_color_manual(values=c("FALSE" = "dodgerblue2", "TRUE" = "orange2")) +
  theme_bw()

plot(dat.test$PC1, dat.test$PC2)
lines(hull.coords[order(hull.coords$PC2, order(hull.coords$PC1)),c("PC1", "PC2")], col="blue")
lines(hull.coords[order(hull.coords$PC2, order(hull.coords$PC1)),c("PC1", "PC2")], col="blue")

summary(pc.hulls)

# # Trying looking at the pairwise distance matrix --> this isn't going to work when we have tens of thousands of points#
# dist.test <- dist(dat.test[,c("PC1", "PC2")])
# dist.mat <- as.matrix(dist.test)
# dim(dist.mat)

####################################################################################################
