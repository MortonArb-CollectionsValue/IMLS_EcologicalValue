## 4-2_Figure_by_species.R

# figure for each species
# examples for each genus

# Convex hull of 

# Table in/out or distance to
# % species suitable/not suitable


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
  # path to the shared Google Drive folder
  path.dat <- "/Volumes/GoogleDrive/Shared drives/IMLS MFA/Environmental Niche Value"

  # path for the folder for figure output
  path.figs <- file.path(path.dat, "figures")

  # path on the local drive for accessing the data
  path.local <- "/Users/aesculus/Box/Research/Active_Projects/IMLS_MortonArb/local_data/PCA_Data"

####################################################################################################
####################################################################################################
## bring in data
path.clim   <- file.path(path.dat, "Analysis/Total_PostReductions2")
path.soils  <- file.path(path.dat, "Analysis/Total_PostSoilReductions")
path.pcas   <- file.path(path.dat, "Analysis/PCAs")

load(file.path(path.dat, "Analysis", "PCA_data.RData"))

####################################################################################################
####################################################################################################
####################################################################################################
## get list of species and genera
  # spp.ls <- 
  gen.ls <- c('Malus', 'Quercus', 'Tilia', 'Ulmus')

    df.in <- xxx
    # df.in <- all.clean

  i.ge <- 'Malus'
## set genus iteration
  for(i.ge in gen.ls){
    i.ge
    pc.i <- df.in %>% filter(genus==i.ge)
    
  png(paste0("Ordination_", i.ge, ".png"), height=8, width=8, units="in", res=320)
    ggplot(data=dat.all, aes(x=PC1, y=PC2)) +
    geom_point(aes(x=PC1, y=PC2), size=0.25, color="gray50")  +
    geom_polygon(data=pc.hulls[pc.hulls$genus %in% i.ge,], aes(fill=i.ge), alpha=0.25) +
    geom_point(data=dat.all[dat.all$genus %in% i.ge,], aes(color=i.ge)) +
    geom_point(data=dat.all[dat.all$genus=="MortonArb",], aes(color=i.ge), size=8) +
    scale_color_manual(name="Genus", values=c("MortonArb"="green4", i.ge="blue2")) +
    scale_fill_manual(name="Genus", values=c("MortonArb"="green4", i.ge="blue2"))
      dev.off()
  }

####################################################################################################
####################################################################################################
#     in.pca <- prcomp(pc.i[,important.traits], center = FALSE, scale. = FALSE)
# 
#     pca.plot <- ggbiplot(in.pca) #basic plot
#     # malus.pca.plot2
#     dev.copy(png, file.path(path.pcas, paste0(i.ge, "_pca.png")))
#     dev.off()
# pc.r <- as.data.frame(in.pca$rotation)
# set.seed(1234)
# d <- pc.r[,1:2]
# kd <- ks::kde(d, compute.cont=TRUE)
# 
# contour_95 <- with(kd, contourLines(x=eval.points[[1]], y=eval.points[[2]],
#                                     z=estimate, levels=cont["5%"])[[1]])
# contour_95 <- data.frame(contour_95)
# 
# 
# ggplot(data=d, aes(PC1, PC2)) +
#   geom_point() +
#   geom_path(aes(x, y), data=contour_95) +
#   theme_bw()
# 
# ## set species iteration
#   for(i.sp in spp.ls){
#     
#    ggbiplot(quercus.pca) #basic plot
#  
#   }
# 
  
### define species to be included
  spp.a <- "Quercus alba"
  spp.b <- "Quercus boyntonii"
  spp.c <- "Quercus brantii"

### filter by species
pc.a <- pca.df %>% filter(species_name_acc==spp.a) #%>% select(PC1, PC2)
pc.b <- pca.df %>% filter(species_name_acc==spp.b) #%>% select(PC1, PC2)
pc.c <- pca.df %>% filter(species_name_acc==spp.c) #%>% select(PC1, PC2)

### detect outliers for each species
pc.out.a <- OutlierDetection(pc.a[,2:3], distance=TRUE)
pc.out.b <- OutlierDetection(pc.b[,2:3], distance=TRUE)
pc.out.c <- OutlierDetection(pc.c[,2:3], distance=TRUE)

### detect outliers for each PCA for each species
pc.out.a2 <- PCOutlierDetection(pc.a[,2:3], distance=TRUE)
pc.out.b2 <- PCOutlierDetection(pc.b[,2:3], distance=TRUE)
pc.out.c2 <- PCOutlierDetection(pc.c[,2:3], distance=TRUE)

pc.out.c; pc.out.c2

pc.out.a3 <- spatial.outlier(pc.a[,2:3])
pc.out.b3 <- spatial.outlier(pc.b[,2:3])
pc.out.c3 <- spatial.outlier(pc.c[,2:3])

####################################################################################################
####################################################################################################
### compute contours

set.seed(1234)
d <- pc.a[,2:3]
# d <- data.frame(x=rnorm(1000),y=rnorm(1000))
kd <- ks::kde(d, compute.cont=TRUE)
# kd <- ks::kde(d, compute.cont=TRUE)

contour_95 <- with(kd, contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                    z=estimate, levels=cont["5%"])[[1]])
contour_95 <- data.frame(contour_95)

ggplot(data=d, aes(PC1, PC2)) +
  geom_point() +
  geom_path(aes(x, y), data=contour_95) +
  theme_bw()

####################################################################################################
####################################################################################################
### 

####################################################################################################
####################################################################################################
### 

####################################################################################################
####################################################################################################
# #Malus PCA
# malus.pca <- prcomp(malus.clean[,important.traits], center = FALSE, scale. = FALSE)
#   summary(malus.pca)
# malus.pca.plot2 <- ggbiplot(malus.pca2) #basic plot
# malus.pca.plot2
# dev.copy(png, file.path(path.pcas, "malus_pca2.png"))
# dev.off()
# 
# #Quercus PCA
# quercus.pca2 <- prcomp(quercus.clean[,important.traits], center = FALSE, scale. = FALSE)
# summary(quercus.pca2)
# write.csv(as.data.frame.matrix(quercus.pca$rotation), "D:/Data_IMLS_Ecological_Value/PCAs/Quercus_pca2.csv", row.names = TRUE)
# quercus.pca.plot <- ggbiplot(quercus.pca) #basic plot
# quercus.pca.plot
# dev.copy(png, file.path(path.pcas, "quercus_pca.png"))
# dev.off()
# 
# #Tilia PCA
# tilia.pca <- prcomp(tilia.clean[,important.traits], center = FALSE, scale. = FALSE)
# summary(tilia.pca2)
# write.csv(as.data.frame.matrix(tilia.pca$rotation), "D:/Data_IMLS_Ecological_Value/PCAs/Tilia_pca.csv", row.names = TRUE)
# tilia.pca.plot <- ggbiplot(tilia.pca) #basic plot
# tilia.pca.plot
# dev.copy(png, file.path(path.pcas, "tilia_pca.png"))
# dev.off()
# 
# #Ulmus PCA
# ulmus.pca <- prcomp(ulmus.clean[,important.traits], center = FALSE, scale. = FALSE)
# summary(ulmus.pca)
# write.csv(as.data.frame.matrix(ulmus.pca$rotation), "D:/Data_IMLS_Ecological_Value/PCAs/Ulmus_pca.csv", row.names = TRUE)
# ulmus.pca.plot <- ggbiplot(ulmus.pca) #basic plot
# ulmus.pca.plot
# dev.copy(png, file.path(path.pcas, "ulmus_pca.png"))
# dev.off()
####################################################################################################
####################################################################################################
### extra code and snippets
# y=abs(Value), color=ifelse(Value<0, “Neg”, “Pos”)
# total_pca2$Trait <- factor(total_pca2$Trait, levels=c([order you want things in])

# png(“PCA_Loadings_3PCs.png”, height=6, width=6, units=“in”, res=320)
# dev.off()



