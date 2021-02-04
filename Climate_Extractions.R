# Script to extract climate data from TerraClimate product
# http://www.climatologylab.org/terraclimate.html

# Variables used by Tree Atlas: 
# Temperature of Warmest Month
# Temperature of Coldest Month
# Ariditiy Index
# May - Sep Temp
# Mean Annual Temp
# Mean Annual Precip
# May - Sep precip


# Note: Additional:
# Annual Daylength Cooef of variation -- use abs(lat) as proxy?


path.google <- "/Volumes/GoogleDrive/Shared drives/IMLS MFA/"

# Path to occurrence points; Shiven is D:; Christy can work directly with Google
# path.occ <- "D:/spp_raw_points/spp_raw_points2/"
path.occ <- file.path(path.google, "occurrence_points/outputs/spp_edited_points/")

# path.out <- "D:/Data_IMLS_Ecological_Value/Soil_Extracts2/"
path.out <- file.path(path.google, "Environmental Niche Value/Extracted Data/Climate_Extract/")


# TerraClimate Data paths:
# full data catalog: http://thredds.northwestknowledge.net:8080/thredds/catalog/TERRACLIMATE_ALL/data/catalog.html
# Available variables (monthly): 
# documentation: http://www.climatologylab.org/terraclimate-variables.html
#  - Primary: max temp, min temp, vapor pressure, precipitaiton accumulation, shortwave radiation, wind speed
#  - Derived (Calculated): reference evapotrans., runoff, actual evapotrans, climate water deficit, soil moisture, snow water equivalent, PDSI, VPD
vars.code <- c("ws", "vpd", "vap", "tmin", "tmax", "swe", "srad", "soil", "g", "ppt", "pet", "def", "aet", "PDSI")
vars.use <- c("tmax", "tmin", "ppt", "soil", "vpd", "srad")
yrs.use <- 1990:2019

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


# Extracting lat & longitude vectors from the data
test.loc <- ncdf4::nc_open(file.path(path.dat, paste0("TerraClimate_tmax_2010.nc")))
lat.vec <- ncdf4::ncvar_get(test.loc, "lat")
lon.vec <- ncdf4::ncvar_get(test.loc, "lon")
ncdf4::nc_close(test.loc)

lat.diff <- diff(lat.vec)
lon.diff <- diff(lon.vec)

lat.upr <- lat.vec-c(lat.diff[1]/2, lat.diff/2)
lat.lwr <- lat.vec+c(lat.diff[1]/2, lat.diff/2)
lon.upr <- lon.vec-c(lon.diff[1]/2, lon.diff/2)
lon.lwr <- lon.vec+c(lon.diff[1]/2, lon.diff/2)

for(i in 1:length(files.all)){
  print("")
  print(files.all[i])
  
  spp.now <- read.csv(file.path(path.occ, files.all[i]))
  
  spp.dat <- spp.now[,c("UID", "decimalLatitude", "decimalLongitude")]
  
  # Loop through the points and attach a lat/lon ind to each point
  # Note: This will be SLOW for our large files, but hopefully it will help
  for(LAT in unique(spp.dat$decimalLatitude)){
    lat.ind <- which(lat.upr>LAT & lat.lwr<LAT)
    
    if(length(lat.ind)!=1) lat.ind <- which(lat.upr<LAT+1e-6 & lat.lwr>LAT+1e-6)
    
    spp.dat[spp.dat$decimalLatitude==LAT, "lat.ind"] <- lat.ind
  }
  for(LON in unique(spp.dat$decimalLongitude)){
    lon.ind <- which(lon.upr<LON & lon.lwr>LON)
    
    # Get a case where it's right on the break, ad a tiny bit to the point to fudge it
    if(length(lon.ind)!=1) lon.ind <- which(lon.upr<LON+1e-6 & lon.lwr>LON+1e-6)
    spp.dat[spp.dat$decimalLongitude==LON, "lon.ind"] <- lon.ind
  }
  
  # # If this gets too hard, may need to condense array to unique locations
  # spp.met <- aggregate(UID ~ lat.ind + lon.ind, data=spp.dat, FUN=length)
  
  # VAR="tmax"
  for(VAR in vars.use){
    tictoc::tic()
    print("")
    print(paste0("   ",VAR))
    dat.arr <- array(dim=c(nrow(spp.dat), 12, length(yrs.use)))
    # dat.arr <- array(dim=c(nrow(spp.met), 12, length(yrs.use)))
    dimnames(dat.arr)[[1]] <- spp.dat$UID
    dimnames(dat.arr)[[2]] <- 1:12
    dimnames(dat.arr)[[3]] <- yrs.use
    # YR = 2019
    pb <- txtProgressBar(min=min(yrs.use), max=max(yrs.use), style=3)
    for(YR in yrs.use){
      setTxtProgressBar(pb, YR)
      met.now <- ncdf4::nc_open(file.path(path.dat, paste0("TerraClimate_", VAR, "_", YR, ".nc")))
      
      # Loop through the points
      for(LAT in unique(spp.dat$lat.ind)){
        for(LON in unique(spp.dat$lon.ind[spp.dat$lat.ind==LAT])){
          row.ind <- which(spp.dat$lat.ind==LAT & spp.dat$lon.ind==LON)
          # Add the start/end indices before running
          dat.arr[row.ind,,paste(YR)] <- ncdf4::ncvar_get(met.now, VAR, start=c(LON, LAT, 1), count=c(1,1,12))
        } # End LON loop
      } # End LAT loop; all points extracted
      
      # # An alternate way of doing this that was *maybe* going to be faster, but isn't
      # pb2 <- txtProgressBar(min=0, max=nrow(spp.met), style=3)
      # for(j in 1:nrow(spp.met)){
      #   dat.arr[j,,paste(YR)] <- ncdf4::ncvar_get(met.now, VAR, start=c(spp.met$lon.ind[i], spp.met$lat.ind[i], 1), count=c(1,1,12)) 
      #   setTxtProgressBar(pb2, j)
      # } # End j loop
      
      ncdf4::nc_close(met.now)    
      
    } # End YR loop -- all data extracted
    
    # --------------
    # Aggregate & Store Output -- NOTE: using the same array name to save memory
    # --------------
    dat.agg <- apply(dat.arr, c(1,3), FUN=mean) 
    spp.dat[,paste0(VAR, ".ann.mean")] <- apply(dat.agg, 1, FUN=mean)
    spp.dat[,paste0(VAR, ".ann.sd")] <- apply(dat.agg, 1, FUN=sd)
    spp.dat[,paste0(VAR, ".ann.max")] <- apply(dat.agg, 1, FUN=max)
    spp.dat[,paste0(VAR, ".ann.min")] <- apply(dat.agg, 1, FUN=min)
    
    dat.agg <- apply(dat.arr, c(1,3), FUN=max)
    spp.dat[,paste0(VAR, ".max.mean")] <- apply(dat.agg, 1, FUN=mean)
    spp.dat[,paste0(VAR, ".max.sd")] <- apply(dat.agg, 1, FUN=sd)
    spp.dat[,paste0(VAR, ".max.max")] <- apply(dat.agg, 1, FUN=max)
    spp.dat[,paste0(VAR, ".max.min")] <- apply(dat.agg, 1, FUN=min)
    
    dat.agg <- apply(dat.arr, c(1,3), FUN=min)
    spp.dat[,paste0(VAR, ".min.mean")] <- apply(dat.agg, 1, FUN=mean)
    spp.dat[,paste0(VAR, ".min.sd")] <- apply(dat.agg, 1, FUN=sd)
    spp.dat[,paste0(VAR, ".min.max")] <- apply(dat.agg, 1, FUN=max)
    spp.dat[,paste0(VAR, ".min.min")] <- apply(dat.agg, 1, FUN=min)
    # --------------
    
    rm(dat.arr, dat.agg)
    tictoc::toc()
  } # End variable loop

  # Save file
  write.csv(spp.dat, file.path(path.out, files.all[i]), row.names=F)
} # end i file loop


