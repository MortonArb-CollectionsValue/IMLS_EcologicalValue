#changed entire script (file paths) so that it uses the occurrence points without the issues
#columns were: taxonIdentificationNotes, localityDescription, county, stateProvince
# Reading in the rasterized HWSD downloaded from here: http://www.fao.org/soils-portal/soil-survey/soil-maps-and-databases/harmonized-world-soil-database-v12/en/ 

library(ggplot2)
library(rgdal); library(sp); library(raster)
library(Hmisc)

path.google <- "/Volumes/GoogleDrive/Shared drives/IMLS MFA/"

# Path to occurrence points; Shiven is D:; Christy can work directly with Google
path.occ <- "D:/spp_raw_points/spp_raw_points2/"
#path.occ <- file.path(path.google, "occurrence_points/outputs/spp_edited_points/")

path.out <- "D:/Data_IMLS_Ecological_Value/Soil_Extracts2/"
#path.out <- file.path(path.google, "Environmental Niche Value/Extracted Data/Soil_Extract/")

# This folder has both the HWSD_RASTER folder and the .mdb (or .csv files)
path.hwsd <- file.path("data_raw/hwsd/")
spp.species <- dir(path.occ)

#changed the raster file path because original was not extracted
hwsd <- raster(file.path("D:/HWSD_RASTER_Extracted/hwsd.bil"))
projection(hwsd) <- CRS("+proj=longlat +datum=WGS84 +no_defs")
hwsd

#Loading in each unique MU_GLOBAL majority SHARE values
dat.hwsd <- mdb.get(file.path("D:/HWSD/HWSD.mdb"))
hwsd.maj <- data.frame(MU.GLOBAL=unique(dat.hwsd$HWSD_DATA$MU.GLOBAL))
cols.pull <- names(dat.hwsd$HWSD_DATA)[3:ncol(dat.hwsd$HWSD_DATA)]

pb.hwsd <- txtProgressBar(min=0, max=nrow(hwsd.maj), style=3)
for(i in 1:nrow(hwsd.maj)){
  setTxtProgressBar(pb.hwsd, i)
  
  dat.temp <- dat.hwsd$HWSD_DATA[dat.hwsd$HWSD_DATA$MU.GLOBAL==hwsd.maj$MU.GLOBAL[i],]
  row.keep <- which(dat.temp$SHARE==max(dat.temp$SHARE))
  hwsd.maj[i,cols.pull] <- dat.temp[row.keep[1],cols.pull]
}

spp.species <- dir(path.occ)

# Columns to keep from the occurrence point file
cols.keep <- c("UID", "species_name_acc", "database", "all_source_databases", "year", "basisOfRecord", "establishmentMeans", "decimalLatitude", "decimalLongitude", "coordinateUncertaintyInMeters", "nativeDatabaseID", "country.name", "country.continent")

pb <- txtProgressBar(min=0, max=length(spp.species), style=3)
for (i in 1:length(spp.species)) {
  setTxtProgressBar(pb, i)
  
  test.spp <- read.csv(file.path(path.occ, spp.species[i]), stringsAsFactors = T)
  test.spp <- test.spp[!is.na(test.spp$UID),cols.keep]
  spp.sp <- SpatialPointsDataFrame(coords=test.spp[,c("decimalLongitude", "decimalLatitude")], data=test.spp, proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs"))
  
  spp.sp$hwsd_code <- extract(hwsd, spp.sp)
  # summary(spp.sp)
  # head(spp.sp)
  spp.sp <- merge(spp.sp, hwsd.maj, by.x="hwsd_code", by.y="MU.GLOBAL", all.x=TRUE)
  # spp.sp$AWC.CLASS <- as.factor(spp.sp$AWC.CLASS)
  spp.sp <- merge(spp.sp, dat.hwsd$D_AWC, by.x="AWC.CLASS", by.y="CODE", all.x=TRUE)
  names(spp.sp)[names(spp.sp)=="VALUE"] <- "AWC_VALUE"
  
  write.csv(spp.sp, file.path(path.out, spp.species[i]), row.names = FALSE)
}
