# 1. Organize the data -- selected cols + 1 row for each obs; all obs for genus + The Arb together 
#      -  Maybe start with a single species -- ideally ~100 data points
# 2.Reformat the data for an NMS -- nonmetric multidimensional scaling (similar to PCA = principle components analysis)
#    - when fitting any sort of ordination or AI, generally 3x as many rows as you have columns
#    - Variables Chosen by Shiven:
#          # T_GRAVEL (Gravel Content): composition of soil is very important
#          # T_SAND (Sand Fraction): composition of soil is very important
#          # T_SILT (Silt Fraction): composition of soil is very important
#          # T_CLAY (Clay Fraction): composition of soil is very important
#          # T_USDA_TEX_CLASS: combination of clay, silt, sand in soil
#          # T_OC: says on the technical report that with the pH it is the best indicator of the health status of the soil
#          # T_PH_H₂O: very important to know the pH of the soil
#          # T_TEB: total exchangeable bases, important because it measures the nutrients the soils provide
#          # T_ECE: electrical conductivity, important as it detects salt in soils
#          # AWC (Avg. Water Storage Capacity): how much rain it can handle, need something that has similar water capacity to Arb
#          # T_REF_BULK_DENSITY: amount of space the particles occupy (how compacted)

# 3. Parts to look at with the NMS:
#     - How much variance is explained by each axis?
#     - What variables load (correlate) with each axis?
#     - Are species in separate clumps or really mixed together?
#     - Where does the Arboretum lie in the messy clump?
#   
library(vegan); library(ggplot2)
# Step 1: Load the data for a single species
path.dat <- "/Volumes/GoogleDrive/Shared drives/IMLS MFA/Environmental Niche Value/Extracted Data/Soil_Extract/"

fsoils <- dir(path.dat, "Quercus")

cols.soils <- c("T.GRAVEL", "T.SAND", "T.SILT", "T.CLAY", "T.OC", "T.PH.H2O", "T.TEB", "T.ECE", "AWC_VALUE", "T.REF.BULK.DENSITY")

dat.arb <- read.csv(file.path(path.dat, "0_MortonArb.csv"))
dat.spp1 <- read.csv(file.path(path.dat, "Quercus_boyntonii.csv"))
dat.spp2 <- read.csv(file.path(path.dat, "Quercus_virginiana.csv"))
# dat.spp3 <- read.csv(file.path(path.dat, "Quercus_georgiana.csv"))

dat.comb <- rbind(dat.arb[,c("UID", "species_name_acc", cols.soils)],
                  dat.spp1[,c("UID", "species_name_acc", cols.soils)],
                  dat.spp2[,c("UID", "species_name_acc", cols.soils)])
#dat.spp3[,c("UID", "species_name_acc", cols.soils)])

# The NMS requires no missing values, so get rid of things with missing values
dat.comb <- dat.comb[complete.cases(dat.comb),]
dim(dat.comb) # we have X points to play with; that works for us!

# Step 2: Do an ordination! Usign the Vegan package
# ordi.spp <- metaMDS(dat.comb[,cols.soils], trymax=50)
# ordi.spp
# ordi.spp$model
# ordi.spp$stress
# ordi.spp$species
# # ordi.spp$points
# 
# # extracting info so we can look at this in ggplot
# ordi.xy <- data.frame(dat.comb[,c("UID", "species_name_acc")], ordi.spp$points)
# summary(ordi.spp)
# library(labdsv)

# Trying an alternate matrix
pca.spp <- princomp(dat.comb[,cols.soils], scores=T)


ggplot(data=ordi.xy) +
  geom_point(aes(x=MDS1, y=MDS2, color=species_name_acc)) +
  geom_point(data=ordi.xy[ordi.xy$species_name_acc=="MortonArb",], aes(x=MDS1, y=MDS2, color=species_name_acc), size=2)


# Visualizing the relationship among things
ordiplot(ordi.spp, type="none", cex.axis=1.5)
# points(tree.plot, "sites", pch=19, cex=1.3)
points(ordi.spp, "sites", pch=19, cex=1.3)
points(ordi.spp, "sites", pch=19, cex=1.3)
ordihull(ordi.spp, group=c("Arb", rep("MAAS", nrow(dat.comb)-1)), show=c("Arb", "MAAS"), col=c("orange", "green4"), lwd=3)

