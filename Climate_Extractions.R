# Script to extract climate data from TerraClimate product
# http://www.climatologylab.org/terraclimate.html

path.google <- "/Volumes/GoogleDrive/Shared drives/IMLS MFA/"

# Path to occurrence points; Shiven is D:; Christy can work directly with Google
# path.occ <- "D:/spp_raw_points/spp_raw_points2/"
path.occ <- file.path(path.google, "occurrence_points/outputs/spp_edited_points/")

# path.out <- "D:/Data_IMLS_Ecological_Value/Soil_Extracts2/"
path.out <- file.path(path.google, "Environmental Niche Value/Extracted Data/Climate_Extract/")


# Arb test coords: 36.75414,	138.26602

# TerraClimate Data paths:
# full data catalog: http://thredds.northwestknowledge.net:8080/thredds/catalog/TERRACLIMATE_ALL/data/catalog.html
# Available variables (monthly): 
# documentation: http://www.climatologylab.org/terraclimate-variables.html
#  - Primary: max temp, min temp, vapor pressure, precipitaiton accumulation, shortwave radiation, wind speed
#  - Derived (Calculated): reference evapotrans., runoff, actual evapotrans, climate water deficit, soil moisture, snow water equivalent, PDSI, VPD

# OPENDAP example: http://thredds.northwestknowledge.net:8080/thredds/dodsC/TERRACLIMATE_ALL/data/TerraClimate_tmax_2019.nc.html
# HTTP example: http://thredds.northwestknowledge.net:8080/thredds/fileServer/TERRACLIMATE_ALL/data/TerraClimate_tmax_2019.nc
# NCSubset example: http://thredds.northwestknowledge.net:8080/thredds/ncss/grid/TERRACLIMATE_ALL/data/TerraClimate_tmax_2019.nc/dataset.html
yrs.avail <- 1958:(lubridate::year(Sys.Date())-2)

dat.vars <- data.frame(code=c("ws", "vpd", "vap", "tmin", "tmax", "swe", "srad", "soil", "g", "ppt", "pet", "def", "aet", "PDSI"), CF=c(NA), type=c(NA))

path.dat <- ""


# Example query URL:
# http://thredds.northwestknowledge.net:8080/thredds/ncss/TERRACLIMATE_ALL/data/TerraClimate_tmin_2018.nc?var=tmin&north=36.755&west=-93.267&east=-93.266&south=37.754&disableProjSubset=on&horizStride=1&time_start=2018-01-01T00%3A00%3A00Z&time_end=2018-12-01T00%3A00%3A00Z&timeStride=1&addLatLon=true&accept=ascii

dat <- ncdf4::nc_open("")

# dat <- RCurl::getURL("http://thredds.northwestknowledge.net:8080/thredds/ncss/TERRACLIMATE_ALL/data/TerraClimate_tmin_2018.nc?var=tmin&north=36.755&west=-93.267&east=-93.266&south=37.754&disableProjSubset=on&horizStride=1&time_start=2018-01-01T00%3A00%3A00Z&time_end=2018-12-01T00%3A00%3A00Z&timeStride=1&addLatLon=true&accept=netcdf")
dat

# /thredds/fileServer/TERRACLIMATE_ALL/data/TerraClimate_tmax_2019.nc
test <- ncdf4::nc_open("http://thredds.northwestknowledge.net:8080/thredds/dodsC/TERRACLIMATE_ALL/data/TerraClimate_tmax_2019.nc")


test.loc <- ncdf4::nc_open("~/Downloads/TerraClimate_tmax_2019.nc")
# test.dat <- ncdf4::ncvar_get(test.loc, "tmax")
summary(test.loc$var$tmax)
ncdf4::nc_close(test.loc)
# Aggregated data (climatic norms?): http://thredds.northwestknowledge.net:8080/thredds/terraclimate_aggregated.html
# path.clim <- 


###############################
# Example from http://www.climatologylab.org/uploads/2/2/1/3/22133936/read_terraclimate.r
###############################
# enter in longitude, latitude here
x<-c(-77.71, -1.59)

# enter in variable you want to download see: http://thredds.northwestknowledge.net:8080/thredds/terraclimate_aggregated.html
var="aet"

# install.packages("ncdf4")
library(ncdf4)


baseurlagg <- paste0(paste0("http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_terraclimate_",var),"_1958_CurrentYear_GLOBE.nc")

nc <- nc_open(baseurlagg)
lon <- ncvar_get(nc, "lon")
lat <- ncvar_get(nc, "lat")
flat = match(abs(lat - x[2]) < 1/48, 1)
latindex = which(flat %in% 1)
flon = match(abs(lon - x[1]) < 1/48, 1)
lonindex = which(flon %in% 1)
start <- c(lonindex, latindex, 1)
count <- c(1, 1, -1)


# read in the full period of record using aggregated files

data <- as.numeric(ncvar_get(nc, varid = var,start = start, count))
###############################
