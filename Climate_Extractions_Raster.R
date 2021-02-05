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
library(raster)

# Note: Additional:
# Annual Daylength Cooef of variation -- use abs(lat) as proxy?

path.google <- "/Volumes/GoogleDrive/Shared drives/IMLS MFA/"


# Path to occurrence points; Shiven is D:; Christy can work directly with Google
# path.occ <- "D:/spp_raw_points/spp_raw_points2/"
# path.occ <- file.path(path.google, "occurrence_points/outputs/spp_edited_points/")
path.occ <- "data/occurrence/spp_edited_points"

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
path.dat <- "/home/data/TERRACLIMATE"
# path.dat <- "~/Desktop/met_products/TERRACLIMATE"


cols.keep <- c("UID", "species_name_acc", "decimalLatitude", "decimalLongitude")


# A funciton for chunking the points for speed
pts.chunk <- function(xmin, xmax, ymin, ymax, spp.pts, met.in, met.array){
  row.crop <- which(spp.pts$decimalLongitude>=xmin & spp.pts$decimalLongitude<xmax & 
                      spp.pts$decimalLatitude>=ymin & spp.pts$decimalLatitude<ymax)
  if(length(row.crop)>0){
    ext.crop <- extent(spp.pts[row.crop,])+c(-res(met.in)[1], res(met.in)[1], -res(met.in)[2], res(met.in)[2]) # Set the extent with a buffer
    met.crop <- crop(met.in, ext.crop) # Crop to the extent
    pt.ext <- extract(met.crop, spp.pts[row.crop,]) # Extract the data
    
    met.array[row.crop,,] <- as.vector(pt.ext) # store the data
    
    message(paste0("Data Extracted! ", length(row.crop), " points"))
    
    return(met.array)
    rm(ext.crop, met.crop)
  } else {
    message("No points in the chunk")
    return(met.array)
  }
}



# for(VAR in vars.use){
for(VAR in rev(vars.use)){
    # VAR="tmax"
  # tictoc::tic()
  print("")
  print(paste0("   ",VAR))
  if(!dir.exists(file.path(path.out, VAR))) dir.create(file.path(path.out, VAR), recursive = T)
  
  # ---------------
  # Get the list of files for what we want
  # ---------------
  # list all available files
  fvar <- dir(path.dat, VAR) 
  
  # Getting the years from string-splitting the names
  fyrs <- unlist(lapply(strsplit(fvar, "_"), FUN=function(x){x[3]}))
  fyrs <- which(unlist(lapply(strsplit(fyrs, "[.]"), FUN=function(x){x[1]})) %in% paste(yrs.use))
  
  # Subset the file list down to the years we want
  fvar <- fvar[fyrs]
  # ---------------
  
  
  # Doing everything at once takes a little longer, but gets us about a net 30% speed bump in extraction
  # Note: there are warnings, but it will be okay
  # tictoc::tic()
  met.stack <- raster::stack(file.path(path.dat, fvar))
  # tictoc::toc()

  # Loop through the species files
  for(i in 1:length(files.all)){
    print("")
    print(files.all[i])
    
    spp.name <- stringr::str_split(files.all[i], "[.]")[[1]][1]
    
    spp.now <- read.csv(file.path(path.occ, files.all[i]), stringsAsFactors = T)
    spp.now <- spp.now[!is.na(spp.now$UID),cols.keep]
    
    # Convert to a spatial points data frame
    spp.sp <- SpatialPointsDataFrame(coords=spp.now[,c("decimalLongitude", "decimalLatitude")], data=spp.now, proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs"))
    
    # Set up our met array place holder
    met.arr <- array(dim=c(nrow(spp.sp), 12, length(yrs.use)))
    dimnames(met.arr) <- list(UID=spp.sp$UID, month=1:12, year=yrs.use)
    
    
    # Extracting the data in chunks
    met.arr <- pts.chunk(xmin=-10, xmax=90, ymin=0, ymax=Inf, spp.pts=spp.sp, met.in=met.stack, met.array=met.arr)
    met.arr <- pts.chunk(xmin=90, xmax=Inf, ymin=0, ymax=Inf, spp.pts=spp.sp, met.in=met.stack, met.array=met.arr)
    met.arr <- pts.chunk(xmin=-Inf, xmax=-10, ymin=10, ymax=Inf, spp.pts=spp.sp, met.in=met.stack, met.array=met.arr)
    met.arr <- pts.chunk(xmin=-Inf, xmax=-10, ymin=-Inf, ymax=10, spp.pts=spp.sp, met.in=met.stack, met.array=met.arr)
    met.arr <- pts.chunk(xmin=-10, xmax=90, ymin=-Inf, ymax=0, spp.pts=spp.sp, met.in=met.stack, met.array=met.arr)
    met.arr <- pts.chunk(xmin=90, xmax=Inf, ymin=-Inf, ymax=0, spp.pts=spp.sp, met.in=met.stack, met.array=met.arr)
    

    # --------------
    # Aggregate & Store Output -- NOTE: using the same array name to save memory, but it's slower :-/
    # --------------
    dat.agg <- apply(met.arr, c(1,3), FUN=mean)
    spp.now[, paste0(VAR, ".ann.mean")] <- apply(dat.agg, 1, mean)
    spp.now[, paste0(VAR, ".ann.sd")  ] <- apply(dat.agg, 1, sd)
    spp.now[, paste0(VAR, ".ann.max") ] <- apply(dat.agg, 1, max)
    spp.now[, paste0(VAR, ".ann.min") ] <- apply(dat.agg, 1, min)
    
    dat.agg <- apply(met.arr, c(1,3), FUN=max)
    spp.now[, paste0(VAR, ".max.mean")] <- apply(dat.agg, 1, mean)
    spp.now[, paste0(VAR, ".max.sd")  ] <- apply(dat.agg, 1, sd)
    spp.now[, paste0(VAR, ".max.max") ] <- apply(dat.agg, 1, max)
    spp.now[, paste0(VAR, ".max.min") ] <- apply(dat.agg, 1, min)
    
    dat.agg <- apply(met.arr, c(1,3), FUN=min)
    spp.now[, paste0(VAR, ".min.mean")] <- apply(dat.agg, 1, mean)
    spp.now[, paste0(VAR, ".min.sd")  ] <- apply(dat.agg, 1, sd)
    spp.now[, paste0(VAR, ".min.max") ] <- apply(dat.agg, 1, max)
    spp.now[, paste0(VAR, ".min.min") ] <- apply(dat.agg, 1, min)
    # --------------
    
    # tictoc::toc()
    # Save file
    write.csv(spp.now, file.path(path.out, VAR, files.all[i]), row.names=F)
    
    rm(dat.arr, dat.agg, spp.now)
  } # End i File loop
  
  rm(met.stack)
} # end Variable Loop


