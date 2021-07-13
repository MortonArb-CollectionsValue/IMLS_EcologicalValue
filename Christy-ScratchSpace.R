# Christy playing around with PCA output to see how we can synthesize
library(ggplot2)

## path to the shared Google Drive folder
path.dat <- "/Volumes/GoogleDrive/Shared drives/IMLS MFA/Environmental Niche Value" ## path for Shannon
# path.dat <- "D:/Data_IMLS_Ecological_Value"   ## path to data for Shiven D drive
path.out <- file.path(path.dat, "Analysis/PrelimPCA/figures")

# path for the folder for figure output
path.figs <- file.path(path.dat, "figures")


# # Load in the PCA RData file
load(file.path(path.dat, "Extracted Data", "PCA_output.RData"))


# Exploring the data
# summary(gen.clean.pca)


# Extracting Quercus as a test case and doing some binning so we can do some density plot stuff
dat.quercus <- gen.clean.pca[grepl("Quercus", gen.clean.pca$genus) & !is.na(gen.clean.pca$PC1),]
# summary(dat.quercus)
dat.quercus$PC1.cut <- cut(dat.quercus$PC1, 50)
dat.quercus$PC1.mid <- sapply(strsplit(gsub("^\\W|\\W$", "", dat.quercus$PC1.cut), ","),
                              function(x)sum(as.numeric(x))/2)
dat.quercus$PC2.cut <- cut(dat.quercus$PC2, 50)
dat.quercus$PC2.mid <- sapply(strsplit(gsub("^\\W|\\W$", "", dat.quercus$PC2.cut), ","),
                              function(x)sum(as.numeric(x))/2)
dat.quercus$PC3.cut <- cut(dat.quercus$PC3, 50)
dat.quercus$PC3.mid <- sapply(strsplit(gsub("^\\W|\\W$", "", dat.quercus$PC3.cut), ","),
                              function(x)sum(as.numeric(x))/2)
dim(dat.quercus)

quercus.agg.spp <- aggregate(UID~species_name_acc + genus + species + PC1.mid + PC2.mid,data=dat.quercus, FUN=length)
names(quercus.agg.spp)[ncol(quercus.agg.spp)] <- c("UID.n") 
summary(quercus.agg.spp)

# Plotting the observation density for a handful of species as test cases
ggplot(data=quercus.agg.spp[quercus.agg.spp$species %in% c("alba", "lyrata", "petraea", "arizonica"),]) +
  facet_wrap(~species) +
  geom_tile(aes(x=PC1.mid, y=PC2.mid, fill=UID.n)) +
  geom_point(x=quercus.agg.spp$PC1[quercus.agg.spp$genus=="MortonArb_Quercus"], y=quercus.agg.spp$PC2[quercus.agg.spp$genus=="MortonArb_Quercus"], color="green3", size=5)

quercus.agg.gen <- aggregate(UID.n~genus + PC1.mid + PC2.mid, data=quercus.agg.spp, FUN=length)
names(quercus.agg.gen)[ncol(quercus.agg.gen)] <- c("Spp.n") 
summary(quercus.agg.gen)

ggplot(data=quercus.agg.gen) +
  geom_tile(aes(x=PC1.mid, y=PC2.mid, fill=Spp.n)) +
  geom_point(x=dat.quercus$PC1[dat.quercus$genus=="MortonArb_Quercus"], y=dat.quercus$PC2[dat.quercus$genus=="MortonArb_Quercus"], color="green3", size=5)

names(gen.clean.pca)
gen.clean.pca[grep("Morton", gen.clean.pca$genus),1:10]
