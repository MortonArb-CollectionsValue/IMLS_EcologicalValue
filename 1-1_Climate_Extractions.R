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
# path.occ <- "data/occurrence/spp_edited_points"

# path.out <- "D:/Data_IMLS_Ecological_Value/Soil_Extracts2/"
# path.out <- file.path(path.google, "Environmental Niche Value/Extracted Data/Climate_Extract/")
path.out <- "data/Climate_Extract"
if(!dir.exists(path.out)) dir.create(path.out, recursive = T)

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
# path.dat <- "/Volumes/Celtis/Meteorology/TERRACLIMATE/"
# path.dat <- "/home/data/TERRACLIMATE"
path.dat <- "~/Desktop/met_products/TERRACLIMATE"

# Pseudo-code for figuring out how to do this.... 
# Challenges: 
#   1. can't read/store all met data -- just too big!
#   2. can't work with all occurrence data -- just too big
#   3. need all years/months at once to get the values I acutally want


# Extracting lat & longitude vectors from the data
test.loc <- ncdf4::nc_open(file.path(path.dat, paste0("TerraClimate_tmax_2010.nc")))
lat.vec <- ncdf4::ncvar_get(test.loc, "lat")
lon.vec <- ncdf4::ncvar_get(test.loc, "lon")
ncdf4::nc_close(test.loc)
rm(test.loc)

lat.diff <- diff(lat.vec)
lon.diff <- diff(lon.vec)

lat.upr <- lat.vec-c(lat.diff[1]/2, lat.diff/2)
lat.lwr <- lat.vec+c(lat.diff[1]/2, lat.diff/2)
lon.upr <- lon.vec-c(lon.diff[1]/2, lon.diff/2)
lon.lwr <- lon.vec+c(lon.diff[1]/2, lon.diff/2)

n.chunk=200 # The number of points to chunk into each file

# for(VAR in vars.use){
  # VAR="tmax"
  # tictoc::tic()
  print("")
  print(paste0("   ",VAR))
  if(!dir.exists(file.path(path.out, VAR))) dir.create(file.path(path.out, VAR), recursive = T)
  
  # Set up a list of the connection info
  met.list <- list()
  for(YR in yrs.use){
    met.list[[paste(YR)]] <- ncdf4::nc_open(file.path(path.dat, paste0("TerraClimate_", VAR, "_", YR, ".nc")))
  }
  
  
  # Loop through the species files
  for(i in 1:length(files.all)){
    print("")
    print(files.all[i])
    
    spp.name <- stringr::str_split(files.all[i], "[.]")[[1]][1]
    
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
    # This takes a minute, but might be worth it to help us track
    pts.all <- aggregate(UID ~ lat.ind + lon.ind, data=spp.dat, FUN=length)
    names(pts.all)[names(pts.all)=="UID"] <- "coordID"
    pts.all$coordID <- 1:nrow(pts.all)
    
    # Merging the coordIDs back into the master dataframe
    spp.dat <- merge(spp.dat, pts.all, all=T)
    write.csv(spp.dat, file.path(path.out, paste0("CoordCodes_", files.all[i])), row.names=F)
    summary(spp.dat)
    
    # Chunk into n.chunk UID points at once for processing
    pts.chunk <- seq(1, nrow(pts.all), by=n.chunk)
    
    for(k in 1:length(pts.chunk)){
      # Subset our dataframes to just what we want to work with  
      dat.now <- spp.dat[spp.dat$coordID>=pts.chunk[k] & spp.dat$coordID<=pts.chunk[k]+n.chunk-1,]
      spp.met <- pts.all[pts.all$coordID>=pts.chunk[k] & pts.all$coordID<=pts.chunk[k]+n.chunk-1,]
      
      # dat.arr <- array(dim=c(nrow(spp.dat), 12, length(yrs.use)))
      # dimnames(dat.arr)[[1]] <- spp.dat$UID
      dat.arr <- array(dim=c(nrow(spp.met), 12, length(yrs.use)))
      dimnames(dat.arr)[[2]] <- 1:12
      dimnames(dat.arr)[[3]] <- yrs.use
  
      # Loop through the points
      # tictoc::tic()
      pb <- txtProgressBar(min=0, max=nrow(spp.met), style=3)
      # pb.ind=1
      # for(LAT in unique(spp.dat$lat.ind)){
      #   for(LON in unique(spp.dat$lon.ind[spp.dat$lat.ind==LAT])){
      #     
      #     # Get our indices for our array
      #     row.ind <- which(spp.dat$lat.ind==LAT & spp.dat$lon.ind==LON)
      #     
      #     # Loop through the years now that we have all the connections
      #     for(YR in yrs.use){
      #       dat.arr[row.ind,,paste(YR)] <- ncdf4::ncvar_get(met.list[[paste(YR)]], VAR, start=c(LON, LAT, 1), count=c(1,1,12))
      #     }
      #     
      #     setTxtProgressBar(pb, pb.ind)
      #     pb.ind=pb.ind+1
      #     
      #   } # End LON loop
      # } # End LAT loop; all points extracted
        
      for(j in 1:nrow(spp.met)){
        # Loop through the years now that we have all the connections
        LON=spp.met$lon.ind[j]
        LAT=spp.met$lat.ind[j]
        
        met.out <- lapply(met.list, FUN=function(x){ ncdf4::ncvar_get(x, VAR, start=c(LON, LAT, 1), count=c(1,1,12))})
        dat.arr[j,,] <- array(unlist(met.out), dim=c(12, length(met.out)))
        
        setTxtProgressBar(pb, j)
      } # End spp.met loop
      # tictoc::toc()
      
      
      # --------------
      # Aggregate & Store Output -- NOTE: using the same array name to save memory, but it's slower :-/
      # --------------
      dat.agg <- apply(dat.arr, c(1,3), FUN=mean)
      for(j in 1:nrow(spp.met)){
        row.ind <- which(dat.now$lat.ind==spp.met$lat.ind[j] & dat.now$lon.ind==spp.met$lon.ind[j])
        dat.now[row.ind, paste0(VAR, ".ann.mean")] <- mean(dat.agg[j,])
        dat.now[row.ind, paste0(VAR, ".ann.sd")] <- sd(dat.agg[j,])
        dat.now[row.ind, paste0(VAR, ".ann.max")] <- max(dat.agg[j,])
        dat.now[row.ind, paste0(VAR, ".ann.min")] <- min(dat.agg[j,])
      }
      
      dat.agg <- apply(dat.arr, c(1,3), FUN=max)
      for(j in 1:nrow(spp.met)){
        row.ind <- which(dat.now$lat.ind==spp.met$lat.ind[j] & dat.now$lon.ind==spp.met$lon.ind[j])
        dat.now[row.ind, paste0(VAR, ".max.mean")] <- mean(dat.agg[j,])
        dat.now[row.ind, paste0(VAR, ".max.sd")] <- sd(dat.agg[j,])
        dat.now[row.ind, paste0(VAR, ".max.max")] <- max(dat.agg[j,])
        dat.now[row.ind, paste0(VAR, ".max.min")] <- min(dat.agg[j,])
      }
      
      dat.agg <- apply(dat.arr, c(1,3), FUN=min)
      for(j in 1:nrow(spp.met)){
        row.ind <- which(dat.now$lat.ind==spp.met$lat.ind[j] & dat.now$lon.ind==spp.met$lon.ind[j])
        dat.now[row.ind, paste0(VAR, ".min.mean")] <- mean(dat.agg[j,])
        dat.now[row.ind, paste0(VAR, ".min.sd")] <- sd(dat.agg[j,])
        dat.now[row.ind, paste0(VAR, ".min.max")] <- max(dat.agg[j,])
        dat.now[row.ind, paste0(VAR, ".min.min")] <- min(dat.agg[j,])
      }
      # --------------
      
      # tictoc::toc()
      # Save file
      write.csv(dat.now, file.path(path.out, VAR, paste0(spp.name, "_", k, ".csv")), row.names=F)
      
      rm(dat.arr, dat.agg, dat.now)
    }  # End k chunk loop
  } # End i File loop

  # ncdf4::nc_close(met.now)    
  for(YR in yrs.use){
    ncdf4::nc_close(met.list[[paste(YR)]]) 
  }
  rm(met.list)
# } # end Variable Loop


