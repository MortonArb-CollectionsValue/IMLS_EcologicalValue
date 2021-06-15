# Calcuating variables from MACA v2 downscaled CMIP5 future climate ensembles
# Note: what we have from before was designed for ecosystem model drivers and doesn't have some of the derived variables (like VPD) that we want for other applications... so we'll have to home-brew a script and extract it ourselves... womp womp.
# Good news is this is the same people that made at least one of our other datasets!

# Data catalogs here: https://climate.northwestknowledge.net/MACA/data_catalogs.php

# Here's code to look at what we already have
path.maca <- "/Volumes/GoogleDrive/My Drive/Arboretum Met Data/GCM_future_Projections/MACAv2_raw/"

maca.ann <- read.csv(file.path(path.maca, "MACA.v2_summary_annual_gcms.csv"))
summary(maca.ann)

maca.day <- read.csv(file.path(path.maca, "MACA.v2_daily_gcms.csv"))
maca.day$model <- as.factor(maca.day$model)
maca.day$scenario <- as.factor(maca.day$scenario)
maca.day$DATE <- as.Date(maca.day$DATE)
summary(maca.day)



test.nc <- ncdf4::nc_open("/Volumes/GoogleDrive/My Drive/Arboretum Met Data/GCM_future_Projections/MACAv2_raw/bcc-csm1-1/rcp45/MACA.bcc-csm1-1.rcp45.r1i1p1.2006.nc")
summary(test.nc$var)
