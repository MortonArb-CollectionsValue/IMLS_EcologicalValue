library(ggplot2)
library(rgdal); library(sp); library(raster)

# test.spp <- read.csv("Desktop")
# summary(test.spp)
# 
# # Quick plot of where these things are
# map.world <- map_data("world")
# 
# # Convert test.spp to a spatial poitns data frame
# spp.sp <- SpatialPointsDataFrame(coords=test.spp[,c("decimalLongitude", "decimalLatitude")], data=test.spp, proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs"))
# summary(spp.sp)
# class(spp.sp)
# 
# # Readign in the ecoregion file
# ecos <- readOGR("~/Downloads/official/wwf_terr_ecos.shp")
# summary(ecos)
# names(ecos)
# class(ecos)
# 
# # This bonks because we're trying to do too much; read into it and ask questions
# test.extract <- extract(ecos, spp.sp, method="simple")