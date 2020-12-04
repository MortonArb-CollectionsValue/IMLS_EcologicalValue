#changed entire script (file paths) so that it uses the occurrence points without the issues
#columns were: taxonIdentificationNotes, localityDescription, county, stateProvince

library(ggplot2)
library(rgdal); library(sp); library(raster)

spp.species <- dir("/Volumes/GoogleDrive/Shared drives/IMLS MFA/occurrence_points/outputs/spp_raw_points/")

# for(i in 1:length(spp.species)){
#   spp.now <- read.csv(file.path("", spp.species[i]))
# }

#cannot create test.spp for some reason
test.spp <- read.csv("D:/spp_raw_points/spp_raw_points2/Quercus_lyrata.csv")
summary(test.spp)
# Convert test.spp to a spatial poitns data frame
spp.sp <- SpatialPointsDataFrame(coords=test.spp[,c("decimalLongitude", "decimalLatitude")], data=test.spp, proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs"))
summary(spp.sp)
class(spp.sp)

# Quick plot of where these things are
map.world <- map_data("world")

ggplot(data=test.spp) +
  coord_equal() +
  geom_path(data=map.world, aes(x=long, y=lat, group=group)) +
  geom_point(aes(x=decimalLongitude, y=decimalLatitude), color="red")


# Reading in the rasterized HWSD downloaded from here: http://www.fao.org/soils-portal/soil-survey/soil-maps-and-databases/harmonized-world-soil-database-v12/en/ 
# Good news: it reads in; bad news: I have no idea what it means
# I THINK these are soil codes that crosswalk with values in the database (.mdb) file that can be downloaded in the same place as the raster
# NOTE that for all fo these things, we'll need to assing a coordinate reference system (CRS; aka projection)
hwsd <- raster("/Users/tardi/Downloads/HWSD_RASTER/hwsd.bil")
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
spp.sp <- merge(spp.sp, D_AWC, by.x="AWC_CLASS", by.y="ï..CODE", all.x=TRUE)
names(spp.sp)[names(spp.sp)=="VALUE"] <- "AWC_VALUE"
summary(spp.sp)

spp.species <- dir("D:/spp_raw_points/spp_raw_points2/")

for (i in 1:length(spp.species)) {
  test.spp <- read.csv(file.path("D:/spp_raw_points/spp_raw_points2/", spp.species[i]))
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
