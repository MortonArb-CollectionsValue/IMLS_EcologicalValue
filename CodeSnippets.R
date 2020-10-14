library(ggplot2)
library(rgdal); library(sp); library(raster)

spp.species <- dir("/Volumes/GoogleDrive/Shared drives/IMLS MFA/occurrence_points/outputs/spp_raw_points/")

# for(i in 1:length(spp.species)){
#   spp.now <- read.csv(file.path("", spp.species[i]))
# }

test.spp <- read.csv("/Volumes/GoogleDrive/Shared drives/IMLS MFA/occurrence_points/outputs/spp_raw_points/Quercus_lyrata.csv")
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
hwsd <- raster("~/Downloads/HWSD_RASTER/hwsd.bil")
projection(hwsd) <- CRS("+proj=longlat +datum=WGS84 +no_defs")
hwsd

# Extract the HWSD soil code from the master raster
spp.sp$hwsd_code <- extract(hwsd, spp.sp)
summary(spp.sp)

# Now we'll need to wade through the many-leveled mess that is a database.
# I *THINK* the hwsd_code I extracted above is the "ID" OR "MU.GLOBAL" value in the HWSD_SMU data table.  These will probably get crosswalked with SU.SYMBOL and SU.CODE to get important data.  We shoudl start by trhing to get AWC (available water capactiy); ROOTS (rooting depth?) and TEXTURE values out

# Trying to read the access database
# https://stackoverflow.com/questions/37912560/programmatically-read-access-mdb-files-into-r-for-both-windows-and-mac
# https://stackoverflow.com/questions/23568899/access-data-base-import-to-r-installation-of-mdb-tools-on-mac
library("Hmisc")
df <- mdb.get("~/Downloads/HWSD.mdb")
summary(df)
summary(df$HWSD_SMU)
df$D_TEXTURE
head(df$HWSD_DATA)
df$D_AWC
df$D_SYMBOL
df$D_SYMBOL90

# Now we'll use the code we extracted to crosswalk things
#NOTE NOTE NOTE: I COULD HAVE THE COLUMN CROSSWALK WRONG
soil.spp1 <- df$HWSD_DATA[df$HWSD_DATA$ID %in% unique(spp.sp$hwsd_code),]
soil.spp <- merge(spp.sp[,c("decimalLongitude", "decimalLatitude", "database", "UID", "hwsd_code")], soil.spp1, by.x="hwsd_code", by.y="ID", all.x=T)
dim(soil.spp)
summary(soil.spp)

# db <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
#                         DBQ=~/Downloads/HWSD.mdb")
# data <- as_tibble(sqlFetch(db, "", rownames=T))

# 
for200 <- raster("~/Desktop/FOR_2000.asc")
for200


# ----------------------------------
# OLD STUFF
# ----------------------------------
# Readign in the ecoregion file
ecos <- readOGR("~/Downloads/official/wwf_terr_ecos.shp")
summary(ecos)
names(ecos)
class(ecos)

# ggplot(data=ecos[ecos$BIOME==14,]) +
#   coord_equal() +
#   geom_path(data=map.world, aes(x=long, y=lat, group=group)) +
#   geom_polygon(aes(x=long, y=lat))

# This bonks because we're trying to do too much; read into it and ask questions
test.extract <- over(spp.sp, ecos[,c("ECO_NAME", "BIOME", "ECO_NUM", "ECO_ID", "ECO_SYM", "G200_REGIO", "G200_NUM", "G200_BIOME")])
test.extract[,c("UID", "decimalLatitude", "decimalLongitude", "coordinateUncertaintyInMeters", "basisOfRecord", "database")] <- data.frame(spp.sp)[,c("UID", "decimalLatitude", "decimalLongitude", "coordinateUncertaintyInMeters", "basisOfRecord", "database")]
summary(test.extract)
head(test.extract)

summary(spp.sp)
