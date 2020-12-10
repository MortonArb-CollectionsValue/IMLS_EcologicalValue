#changed entire script (file paths) so that it uses the occurrence points without the issues
#columns were: taxonIdentificationNotes, localityDescription, county, stateProvince
# Reading in the rasterized HWSD downloaded from here: http://www.fao.org/soils-portal/soil-survey/soil-maps-and-databases/harmonized-world-soil-database-v12/en/ 

library(ggplot2)
library(rgdal); library(sp); library(raster)

path.google <- "/Volumes/GoogleDrive/Shared drives/IMLS MFA/"

# Path to occurrence points; Shiven is D:; Christy can work directly with Google
path.occ <- "D:/spp_raw_points/spp_edited_points_Extracted2"
#path.occ <- file.path(path.google, "occurrence_points/outputs/spp_edited_points/")

# This folder has both the HWSD_RASTER folder and the .mdb (or .csv files)
path.hwsd <- file.path("data_raw/hwsd/")

spp.species <- dir(path.occ)

#original hwsd raster file path was not working
#hwsd <- raster(file.path(path.hwsd, "HWSD_RASTER/hwsd.bil"))
hwsd <- raster ("/Users/tardi/Downloads/HWSD_RASTER/hwsd.bil")
projection(hwsd) <- CRS("+proj=longlat +datum=WGS84 +no_defs")
hwsd

#Loading in each unique MU_GLOBAL majority SHARE values
HWSD_DATA_MAJORITIES <- read.csv("D:/HWSD/HWSD_DATA_MAJORITIES.csv")
head(HWSD_DATA_MAJORITIES)

# Extract the HWSD soil code from the master raster
spp.sp$hwsd_code <- extract(hwsd, spp.sp)
summary(spp.sp)
head(spp.sp)
spp.sp <- merge(spp.sp, HWSD_DATA_MAJORITIES, by.x="hwsd_code", by.y="MU_GLOBAL", all.x=TRUE)
spp.sp$SU_SYM74 <-  as.factor(spp.sp$SU_SYM74)
spp.sp$SU_SYM85 <-  as.factor(spp.sp$SU_SYM85)
spp.sp$SU_SYM90 <-  as.factor(spp.sp$SU_SYM90)
spp.sp$ROOTS <- as.factor(spp.sp$ROOTS)
spp.sp$AWC_CLASS <- as.factor(spp.sp$AWC_CLASS)
spp.sp <- merge(spp.sp, D_AWC, by.x="AWC_CLASS", by.y="?..CODE", all.x=TRUE)
names(spp.sp)[names(spp.sp)=="VALUE"] <- "AWC_VALUE"

summary(spp.sp)

spp.species <- dir(path.occ)

# Columns to keep from the occurrence point file
cols.keep <- c("UID", "species_name_acc", "database", "all_source_databases", "year", "basisOfRecord", "establishmentMeans", "decimalLatitude", 
               "decimalLongitude", "coordinateUncertaintyInMeters", "nativeDatabaseID", "country.name", "country.continent")
colnames(spp.sp)

pb <- txtProgressBar(min=0, max=length(spp.species), style=3)

for (i in 1:length(spp.species)) {
  setTxtProgressBar(pb, i)
  
  test.spp <- read.csv(file.path(path.occ, spp.species[i]))
  spp.sp <- SpatialPointsDataFrame(coords=test.spp[,c("decimalLongitude", "decimalLatitude")], data=test.spp, proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs"))
  spp.sp$hwsd_code <- extract(hwsd, spp.sp)
  summary(spp.sp)
  head(spp.sp)
  spp.sp <- merge(spp.sp, HWSD_DATA_MAJORITIES, by.x="hwsd_code", by.y="MU_GLOBAL", all.x=TRUE)
  spp.sp$SU_SYM74 <-  as.factor(spp.sp$SU_SYM74)
  spp.sp$SU_SYM85 <-  as.factor(spp.sp$SU_SYM85)
  spp.sp$SU_SYM90 <-  as.factor(spp.sp$SU_SYM90)
  spp.sp$ROOTS <- as.factor(spp.sp$ROOTS)
  spp.sp$AWC_CLASS <- as.factor(spp.sp$AWC_CLASS)
  spp.sp <- merge(spp.sp, D_AWC, by.x="AWC_CLASS", by.y="ï..CODE", all.x=TRUE)
  names(spp.sp)[names(spp.sp)=="VALUE"] <- "AWC_VALUE"
  write.csv(spp.sp, file.path("D:/Data_IMLS_Ecological_Value/Soil_Extracts2/", spp.species[i]), row.names = FALSE)
}
