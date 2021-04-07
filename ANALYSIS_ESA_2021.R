# Doing a script of some quick PCA/Permanova Analyses for an ESA abstract;
library(dplyr); library(ggplot2); library(vegan)
library(ggbiplot)
path.out <- "/Volumes/GoogleDrive/Shared drives/IMLS MFA/Environmental Niche Value/Analysis/PrelimPCA/figures"

# -----------------------
# Load the data
# -----------------------
# Testing just on the Quercus collection
path.dat <- "/Volumes/GoogleDrive/Shared drives/IMLS MFA/Environmental Niche Value/Extracted Data/"
path.soils <- file.path(path.dat, "Soil_Extract")
path.clim <- file.path(path.dat, "Climate_Extract")
cols.meta <- c("UID", "species_name_acc")
predictor.soil <- c("T.SILT", "T.CLAY", "T.OC", "T.PH.H2O", "T.ECE", "T.CEC.SOIL")
clim.suff <- c("ann.mean", "ann.sd", "max.max", "min.min")
clim.vars <- c("ppt", "soil", "srad", "tmax", "tmin", "vpd")

# Load the Quercus data; but also start with the Arb,s o we can root it in place
morton.soil <- read.csv(file.path(path.soils,"0_MortonArb.csv"))

#Quercus Collection with Morton Arb Data Point
soil_files <- list.files(path = path.soils,
                           pattern = "Quercus", full.names = TRUE)
soilcols <- names(read.csv(soil_files[1]))
col.char <- which(soilcols %in% c("nativeDatabaseID", "MU.SOURCE1"))
coltype <- rep(NA, length(soilcols))
coltype[col.char] <- "character"
quercus_soil <-  lapply(soil_files, read.csv, colClasses=coltype) %>% bind_rows()
quercus_soil <- rbind.fill(quercus_soil[,c(cols.meta, predictor.soil)], morton.soil[,c(cols.meta, predictor.soil)])
head(quercus_soil)
tail(quercus_soil)

# Now loop through our various climate variables and throw them in too
for(CLIM in clim.vars){
  morton.clim <- read.csv(file.path(path.clim, CLIM,"0_MortonArb.csv"))
  
  #Quercus Collection with Morton Arb Data Point
  quercus_files <- list.files(path = file.path(path.clim, CLIM),
                             pattern = "Quercus", full.names = TRUE)
  climcols <- names(read.csv(quercus_files[1]))
  col.char <- which(climcols %in% c("nativeDatabaseID", "MU.SOURCE1"))
  coltype <- rep(NA, length(climcols))
  coltype[col.char] <- "character"
  quercus_clim.temp <-  lapply(quercus_files, read.csv, colClasses=coltype) %>% bind_rows()
  quercus_clim.temp <- rbind(quercus_clim.temp[,c(cols.meta, paste(CLIM, clim.suff, sep="."))], morton.clim[,c(cols.meta, paste(CLIM, clim.suff, sep="."))])
  
  if(CLIM==clim.vars[1]) quercus_clim <- quercus_clim.temp[,cols.meta]
  quercus_clim <- cbind(quercus_clim, quercus_clim.temp[,paste(CLIM, clim.suff, sep=".")])
}
summary(quercus_clim)

quercus_all <- merge(quercus_soil, quercus_clim)
summary(quercus_all)

quercus_all <- quercus_all[complete.cases(quercus_all),]
summary(quercus_all)
row.arb <- which(quercus_all$UID=="MORTONARB")

# quercus.scale <- cbind[quescale(quercus_all)]
# -----------------------

# -----------------------
# PCA 1: kitchen sink approach -- throw it all in
# -----------------------
vars.pca <- names(quercus_all)[!names(quercus_all) %in% cols.meta]

quercus.pca1 <- prcomp(quercus_all[,vars.pca], center = TRUE,scale. = TRUE) 

# -----------------------

# -----------------------
# PCA 2: variable reduction based on corr
# -----------------------
var.cor <- cor(quercus_all[,vars.pca])
tail(var.cor)
varsums <- colSums(var.cor^2)
varMeans <- colMeans(var.cor^2)
var.cor[,grep("ppt", colnames(var.cor))]
var.cor[,grep("tmax", colnames(var.cor))]
var.cor[,grep("tmin", colnames(var.cor))]
var.cor[,grep("vpd", colnames(var.cor))]
var.cor[,grep("soil", colnames(var.cor))] # Doesn't correlate super high with precip; corr with mostly itself

# This is a bit ad hoc, but we'll check it out Reducing climate variables --> ditch VPD, SRAD
predictor.clim <- c("ppt.ann.mean", "ppt.max.max", "ppt.min.min", "tmax.ann.mean", "tmax.max.max", "tmin.min.min", "soil.ann.mean")

vars.pca2 <- c(predictor.soil, predictor.clim)
quercus.pca2 <- prcomp(quercus_all[,vars.pca2], center = TRUE,scale. = TRUE) 

summary(quercus.pca2)
quercus.pca2$rotation[,1:3]
summary(quercus.pca2$x[,1:3])
# -----------------------

# -----------------------
# Identify euclidean outliers from the first 2 PCs; based on 4 SDs
# - In future, could go and do this by each species
# -----------------------
# test <- scale(quercus_all[,3:])
quercus.scale <- cbind(quercus_all[,1:2], scale(quercus_all[,3:ncol(quercus_all)]))
summary(quercus.scale)

# Remove variables that are supreme outliers in any one variable
# NOTE: Because we have centered and scaled the data, it'll be normally distributed except for outliers!
rows.remove <- which(quercus.scale[,3:ncol(quercus.scale)]>6)
summary(rows.remove)

# Being fairly stringent with the outlier number for our sanity
rows.keep <- apply(quercus.scale[,3:ncol(quercus.scale)], 1, FUN=function(x){all(x<=4)})

quercus.clean <- quercus.scale[rows.keep,]
summary(quercus.clean)
# -----------------------

# -----------------------
# PCA3: reduced variables, removing weirdo outliers
# -----------------------
predictor.clim <- c("ppt.ann.mean", "ppt.max.max", "ppt.min.min", "tmax.ann.mean", "tmax.max.max", "tmin.min.min", "soil.ann.mean")

vars.pca2 <- c(predictor.soil, predictor.clim)

quercus.pca3 <- prcomp(quercus.clean[,vars.pca2]) 
summary(quercus.pca3)

quercus.pca3$rotation[,1:3]
summary(quercus.pca3$x[,1:3])

quercus.pca3b <- princomp(quercus.clean[,vars.pca2])
summary(quercus.pca3b)
quercus.pca3b$loadings

# Quick and dirty ggbiplot of the data
# ggbiplot(quercus.pca3b) + 
#   # geom_segment(data = PCAloadings, aes(x = 0, y = 0, xend = (PC1*5),yend = (PC2*5)), arrow = arrow(length = unit(1/2, "picas")),color = "black")
#   geom_point(x=quercus.pca3b$scores[grep("MORTONARB", quercus.clean$UID),1], y=quercus.pca3b$scores[grep("MORTONARB", quercus.clean$UID),1], color="green3", size=8) #basic plot

quercus.scores <- data.frame(quercus.pca3$x)
quercus.loads <- data.frame(quercus.pca3$rotation)
quercus.loads$labx <- quercus.loads$PC1*(max(quercus.scores$PC1)+0.25)
quercus.loads$laby <- quercus.loads$PC2*(max(quercus.scores$PC2)+0.25)
quercus.loads$var <- row.names(quercus.loads)

# png("../")
SPP="Quercus rubra"
for(SPP in unique(quercus.clean$species_name_acc)){
  png(file.path(path.out, paste0(SPP, " PrelimPCA.png")), height=8, width=8, units="in", res=180)
  print(
    ggplot(data=quercus.scores[,]) +
      ggtitle(SPP) +
      geom_point(aes(x=PC1, y=PC2), size=0.1, alpha=0.1, color="gray75") +
      geom_point(data=quercus.scores[quercus.clean$species_name_acc==SPP,], aes(x=PC1, y=PC2), size=0.1, alpha=0.2, color="orange3") +
      stat_ellipse(data=quercus.scores[quercus.clean$species_name_acc==SPP,], aes(x=PC1, y=PC2, fill="Quercus rubra"), alpha=0.5, geom="polygon") +
      geom_point(data=quercus.scores[quercus.clean$UID=="MORTONARB",], aes(x=PC1, y=PC2), color="green3", size=5) +
      geom_text(data=quercus.loads, aes(x=labx, y=laby, label=var), color="blue2", size=3) +
      geom_segment(data=quercus.loads, aes(x=0, y=0, xend=PC1*(max(quercus.scores$PC1)-0.5), yend=PC2*(max(quercus.scores$PC2)-0.5)), arrow=arrow(length=unit(1/2, "picas")), color="blue2") +
      guides(fill=F) +
      scale_fill_manual(values="orange2") +
      theme_bw() +
      theme(title = element_text(face="italic"))
  )  
  dev.off()
}
# Doing a quick permanova to see if there are clear species groupings
# adonis

# Note: I'm getting an error about vecotr memory on my mac and had to go here: https://stackoverflow.com/questions/51295402/r-on-macos-error-vector-memory-exhausted-limit-reached
# quercus.pca <- quercus.scale[,c(cols.meta, vars.pca2)]
check.sample <- sample(1:nrow(quercus.clean),1e4)
perm1 <- adonis(quercus.clean[check.sample,vars.pca2] ~ species_name_acc, data=quercus.clean[check.sample,], method="euclidean")
perm1
summary(perm1)
# -----------------------

#analysis of PCA Plots

# quercus.pca1 <- 
