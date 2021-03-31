library(ggplot2)
library(plyr); library(readr); library(dplyr)
# Step 1: Load the data for a single species
path.dat <- "/Volumes/GoogleDrive/Shared drives/IMLS MFA/Environmental Niche Value/Extracted Data/Soil_Extract"
path.figs <- file.path(path.dat, "..")

fsoils <- dir(path.dat, "Quercus")
dat.arb <- read.csv(file.path(path.dat, "0_MortonArb.csv"))

cols.soils <- c("T.GRAVEL", "T.SAND", "T.SILT", "T.CLAY", "T.OC", "T.PH.H2O", "T.TEB", "T.ECE", "AWC_VALUE", "T.REF.BULK.DENSITY")

# Setting up some column stuff to make it work when things get weird
soilcols <- names(dat.arb)
col.char <- which(soilcols %in% c("nativeDatabaseID", "MU.SOURCE1"))
coltype <- rep(NA, length(soilcols))
coltype[col.char] <- "character"


f.all <- list.files(path = path.dat,
                           pattern = "Quercus", full.names = TRUE)
dat.all <-  lapply(f.all, read.csv, colClasses=coltype) %>% bind_rows()

# Add the Arb in here
dat.all <- rbind(dat.arb, dat.all)

# Cut down to just the variables we care about
dat.long <- stack(dat.all[,cols.soils])
dat.long$UID <- dat.long$UID
dat.long$Species <- dat.long$species_name_acc

dim(dat.long)

# T.ECE is looking really weird
# dat.long[dat.long$ind=="T.ECE" & dat.long$values>1 & !is.na(dat.long$values), "values"] <- NA

pdf(file.path("Quercus_VariableSummary.pdf"), height=11, width=8)
for(SPP in unique(dat.long$Species[dat.long$UID!="MORTONARB"])){
  print(
    ggplot(data=dat.long[dat.long$UID!="MORTONARB",], aes(x=values)) +
      facet_wrap(~ind, scales="free") +
      geom_density(aes(fill="All Genus"), alpha=0.5) +
      geom_density(data=dat.long[dat.long$Species==SPP,], aes(fill=Species), alpha=0.5) +
      geom_vline(data=dat.long[dat.long$UID=="MORTONARB",], aes(xintercept=values),color="green4", size=1.5) +
      scale_fill_manual(values=c("All Genus"="black", "blue")) +
      scale_y_continuous(expand=c(0,0)) +
      ggtitle(SPP) +
      theme(legend.position="bottom",
            legend.title=element_blank())
    )
}
dev.off()



# Trying an ordination 
# randomly subset from our data frame to see if that works
dat.all <- dat.all[complete.cases(dat.all[, cols.soils]),]

# pts.samp <- sample(1:nrow(dat.all), 1e6, replace = F) 
pc.t1 <- prcomp(dat.all[, cols.soils])
summary(pc.t1)
pc.t1

soil.cor <- cor(dat.all[, cols.soils])
soil.cor
plot(dat.all[, cols.soils])


soil.cv <- cov(dat.all[, cols.soils])
soil.cv

pc.t2 <- prcomp(dat.all[, cols.soils[!cols.soils %in% c("T.SAND")]])
summary(pc.t2)
pc.t2

pc.t2 <- princomp(dat.all[, cols.soils[!cols.soils %in% c("AWC_VALUE", "T.SAND")]], scores=T)
summary(pc.t2)
summary(pc.t2$scores)
pc.t2$loadings

dat.all$PC1 <- pc.t2$scores[,1]
dat.all$PC2 <- pc.t2$scores[,2]

# Calculating convex hulls for Q. alba, Q. velutina, Q. boyntonii
library(tidyverse)
pc.hulls <- dat.all[,c("species_name_acc", "PC1", "PC2")] %>% group_by(species_name_acc) %>% slice(chull(PC1, PC2))
summary(pc.hulls)

png("TEST_Ordination_Quercus.png", height=8, width=8, units="in", res=320)
ggplot(data=dat.all, aes(x=PC1, y=PC2)) +
  geom_point(aes(x=PC1, y=PC2), size=0.25, color="gray50")  +
  geom_polygon(data=pc.hulls[pc.hulls$species_name_acc %in% c("Quercus alba"),], aes(fill=species_name_acc), alpha=0.25) +
  geom_polygon(data=pc.hulls[pc.hulls$species_name_acc %in% c("Quercus boyntonii"),], aes(fill=species_name_acc), alpha=0.5) +
  geom_point(data=dat.all[dat.all$species_name_acc %in% c("Quercus alba"),], aes(color=species_name_acc)) +
  geom_point(data=dat.all[dat.all$species_name_acc=="Quercus boyntonii",], aes(color=species_name_acc), size=2) +
  geom_point(data=dat.all[dat.all$species_name_acc=="MortonArb",], aes(color=species_name_acc), size=8) +
  scale_color_manual(name="Species", values=c("MortonArb"="green4", "Quercus alba"="blue2", "Quercus boyntonii"="red2", "Quercus velutina"="cadetblue3")) +
  scale_fill_manual(name="Species", values=c("MortonArb"="green4", "Quercus alba"="blue2", "Quercus boyntonii"="red2", "Quercus velutina"="cadetblue3"))
dev.off()
