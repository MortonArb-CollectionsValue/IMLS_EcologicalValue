library(ggplot2)
library(plyr); library(readr); library(dplyr)
# Step 1: Load the data for a single species
path.dat <- "/Volumes/GoogleDrive/Shared drives/IMLS MFA/Environmental Niche Value/Extracted Data/Soil_Extract/"
path.figs <- file.path(path.dat, "..")

fsoils <- dir(path.dat, "Quercus")
dat.arb <- read.csv(file.path(path.dat, "0_MortonArb.csv"))

cols.soils <- c("T.GRAVEL", "T.SAND", "T.SILT", "T.CLAY", "T.OC", "T.PH.H2O", "T.TEB", "T.ECE", "AWC_VALUE", "T.REF.BULK.DENSITY")

# Setting up some column stuff to amek it work when things get weird
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
dat.long <- stack(quercus_all[,cols.soils])
dat.long$UID <- quercus_all$UID
dat.long$Species <- quercus_all$species_name_acc

dim(dat.long)

# T.ECE is looking really weird
dat.long[dat.long$ind=="T.ECE" & dat.long$values>1 & !is.na(dat.long$values), "values"] <- NA

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
pts.samp <- sample(1:nrow(dat.all), 1e6, replace = F) 
pc.t1 <- prcomp(dat.all[, cols.soils])
pc.t1
