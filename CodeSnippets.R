library(ggplot2)
library(rgdal); library(sp); library(raster)

test.spp <- read.csv("/Volumes/GoogleDrive/Shared drives/IMLS MFA/occurrence_points/outputs/spp_raw_points/Quercus_alba.csv")
summary(test.spp)

# Quick plot of where these things are
map.world <- map_data("world")

ggplot(data=test.spp) +
  coord_equal() +
  geom_path(data=map.world, aes(x=long, y=lat, group=group)) +
  geom_point(aes(x=decimalLongitude, y=decimalLatitude), color="red")


# Convert test.spp to a spatial poitns data frame
spp.sp <- SpatialPointsDataFrame(coords=test.spp[,c("decimalLongitude", "decimalLatitude")], data=test.spp, proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs"))
summary(spp.sp)
class(spp.sp)

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
test.extract <- extract(ecos, spp.sp, method="simple")
