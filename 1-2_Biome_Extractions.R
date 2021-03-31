#CURRENTLY NOT USING THIS SCRIPT FOR ANALYSIS

#changed entire script (file paths) so that it uses the occurrence points without the issues
#columns were: taxonIdentificationNotes, localityDescription, county, stateProvince
# Biome/Ecoregion Dataset from WWF: 
#   https://www.worldwildlife.org/publications/terrestrial-ecoregions-of-the-world
#   Metadata is in the html file with that download
#   Actual ecoregion codes are in Table 1 of this publication: https://c402277.ssl.cf1.rackcdn.com/publications/356/files/original/The_Global_200_Priority_Ecoregions_for_Global_Conservation.pdf?1345735162
# Dataset Citation: Olson, D. M., Dinerstein, E., Wikramanayake, E. D., Burgess, N. D., Powell, G. V. N., Underwood, E. C., D'Amico, J. A., Itoua, I., Strand, H. E., Morrison, J. C., Loucks, C. J., Allnutt, T. F., Ricketts, T. H., Kura, Y., Lamoreux, J. F., Wettengel, W. W., Hedao, P., Kassem, K. R. 2001. Terrestrial ecoregions of the world: a new map of life on Earth. Bioscience 51(11):933-938.

library(ggplot2)
library(rgdal); library(sp); library(raster)
library(maps)

path.google <- "/Volumes/GoogleDrive/Shared drives/IMLS MFA/"

# Path to occurrence points; Shiven is D:; Christy can work directly with Google
# path.occ <- "D:/spp_raw_points/spp_raw_points2/"
path.occ <- file.path(path.google, "occurrence_points/outputs/spp_edited_points/")

# path.out <- "D:/Data_IMLS_Ecological_Value/Biome_Extracts2/"
path.out <- file.path(path.google, "Environmental Niche Value/Extracted Data/Biome_Extract/")

# Quick plot of where these things are
map.world <- map_data("world")

# Reading in the ecoregion file
ecos <- readOGR("data_raw/ecoregions_wwf/official/wwf_terr_ecos.shp")
# ecos <- readOGR("D:/official/wwf_terr_ecos.shp")
ecos$BIOME_NAME <- car::recode(ecos$BIOME, 
                               "'1'='Tropical & Subtropical Moist Broadleaf Forests'; 
                               '2'='Tropical & Subtropical Dry Broadleaf Forests';
                               '3'='Tropical & Subtropical Coniferous Forests';
                               '4'='Temperate Broadleaf & Mixed Forests';
                               '5'='Temperate Conifer Forests';
                               '6'='Boreal Forests/Taiga';
                               '7'='Tropical & Subtropical Grasslands, Savannas & Shrublands';
                               '8'='Temperate Grasslands, Savannas & Shrublands';
                               '9'='Flooded Grasslands & Savannas';
                               '10'='Montane Grasslands & Shrublands';
                               '11'='Tundra';
                               '12'='Mediterranean Forests, Woodlands & Scrub';
                               '13'='Deserts & Xeric Shrublands';
                               '14'='Mangroves'")
summary(ecos)
names(ecos)
class(ecos)
head(ecos)

spp.species <- dir(path.occ)
spp.species
length(spp.species)

test.spp.1 <- read.csv(file.path(path.occ, spp.species[1]), stringsAsFactors = T)
colnames(test.spp.1)
colnames(ecos)

cols.keep <- c("UID", "species_name_acc", "database", "all_source_databases", "year", "basisOfRecord", "establishmentMeans", "decimalLatitude", "decimalLongitude", "coordinateUncertaintyInMeters", "nativeDatabaseID", "country.name", "country.continent")
ecos.keep <- c("ECO_NAME", "REALM", "BIOME", "BIOME_NAME", "ECO_NUM", "ECO_ID", "ECO_SYM", "G200_REGIO", "G200_NUM", "G200_BIOME", "eco_code")

pb <- txtProgressBar(min=0, max=length(spp.species), style=3)

for (i in 1:length(spp.species)) {
  setTxtProgressBar(pb, i)
  test.spp <- read.csv(file.path(path.occ, spp.species[i]), stringsAsFactors = T)
  test.spp <- test.spp[!is.na(test.spp$UID),cols.keep]
  spp.sp <- SpatialPointsDataFrame(coords=test.spp[,c("decimalLongitude", "decimalLatitude")], data=test.spp, proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs"))
  
  test.extract <- over(spp.sp, ecos)
  test.spp[,ecos.keep] <- test.extract[,ecos.keep]
  
  write.csv(test.spp, file.path(path.out, spp.species[i]), row.names = FALSE)
}

# #downloaded individual file to computer instead of using whole folder on hard drive
# #didn't work when I tried to download from hard drive
# test.spp <- read.csv("~/Quercus_lyrata.csv")
# summary(test.spp)
# head(test.spp)
# 
# 
# # Convert test.spp to a spatial poitns data frame
# spp.sp <- SpatialPointsDataFrame(coords=test.spp[,c("decimalLongitude", "decimalLatitude")], data=test.spp, proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs"))
# summary(spp.sp)
# class(spp.sp)
# 
# #works up until here
# test.spp.package <- spTransform(ecos, CRS=CRS(spp.sp))
# #This bonks because we're trying to do too much; read into it and ask questions
# test.extract <- extract(ecos, spp.sp, method="simple")
# 
# #Change df and FileName to actual things
# write.csv(df, "FileName.csv", row.names = FALSE)
# 
# ggplot(data=ecos[ecos$BIOME==14,]) +
#    coord_equal() +
#    geom_path(data=map.world, aes(x=long, y=lat, group=group)) +
#    geom_polygon(aes(x=long, y=lat))