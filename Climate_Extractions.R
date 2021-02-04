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
vars.code <- c("ws", "vpd", "vap", "tmin", "tmax", "swe", "srad", "soil", "g", "ppt", "pet", "def", "aet", "PDSI")
files.all <- dir(path.occ)


# Note: Web quercy wasn't working (grrrr!), so I've downloaded 1980-2019 locally
path.dat <- "/Volumes/Celtis/Meteorology/TERRACLIMATE/"


# Pseudo-code for figuring out how to do this.... 
# Challenges: 
#   1. can't read/store all met data -- just too big!
#   2. can't work with all occurrence data -- just too big
#   3. need all years/months at once to get the values I acutally want
# Order of Ops:
#  1. Load an occurrence record
#     1.1  Aggregate/round coords to match res of met product
#     1.2  create lat/lon index that corresponds
#     2.   Loop through vars
#          2.1 set up array for storing data: dim=c(unique points, months, years)
#          3.  Loop through Years
#              - open connection to file
#              - loop through each unique point and extract data; store in array
#          2.2 aggregate data to get vars (means, extremes, sd); create data frame
#     1.3  merge met data back w/ occurence points & save
# # End loop

spp.now <- read.csv(file.path(path.occ, files.all[1]))


VAR="tmax"
YR = 2019

test.loc <- ncdf4::nc_open(file.path(path.dat, paste0("TerraClimate_", VAR, "_", YR, ".nc")))
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
