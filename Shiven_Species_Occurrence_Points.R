library(ggplot2)
library(rgdal); library(sp); library(raster)
library(maps)

#downloaded individual file to computer instead of using whole folder on hard drive
#didn't work when I tried to download from hard drive
test.spp <- read.csv("~/Quercus_lyrata.csv")
summary(test.spp)
head(test.spp)

# Quick plot of where these things are
map.world <- map_data("world")


# Convert test.spp to a spatial poitns data frame
spp.sp <- SpatialPointsDataFrame(coords=test.spp[,c("decimalLongitude", "decimalLatitude")], data=test.spp, proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs"))
summary(spp.sp)
class(spp.sp)

# # Readign in the ecoregion file
ecos <- readOGR("D:/official/wwf_terr_ecos.shp")
summary(ecos)
names(ecos)
class(ecos)
head(ecos)

#works up until here
test.spp.package <- spTransform(ecos, CRS=CRS(spp.sp))
#This bonks because we're trying to do too much; read into it and ask questions
test.extract <- extract(ecos, spp.sp, method="simple")

#Change df and FileName to actual things
write.csv(df, "FileName.csv", row.names = FALSE)

ggplot(data=ecos[ecos$BIOME==14,]) +
   coord_equal() +
   geom_path(data=map.world, aes(x=long, y=lat, group=group)) +
   geom_polygon(aes(x=long, y=lat))