library(ggplot2)
library(rgdal); library(sp); library(raster)

spp.species <- dir("/Volumes/GoogleDrive/Shared drives/IMLS MFA/occurrence_points/outputs/spp_raw_points/")

# for(i in 1:length(spp.species)){
#   spp.now <- read.csv(file.path("", spp.species[i]))
# }

#cannot create test.spp for some reason
test.spp <- read.csv("D:/spp_raw_points/spp_raw_points/Quercus_lyrata.csv")
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
hwsd <- raster("/Users/tardi/Downloads/HWSD_RASTER/hwsd.bil")
projection(hwsd) <- CRS("+proj=longlat +datum=WGS84 +no_defs")
hwsd

# Extract the HWSD soil code from the master raster
spp.sp$hwsd_code <- extract(hwsd, spp.sp)
summary(spp.sp)
head(spp.sp)

# Now we'll need to wade through the many-leveled mess that is a database.
# I *THINK* the hwsd_code I extracted above is the "ID" OR "MU.GLOBAL" value in the HWSD_SMU data table.  These will probably get crosswalked with SU.SYMBOL and SU.CODE to get important data.  We shoudl start by trhing to get AWC (available water capactiy); ROOTS (rooting depth?) and TEXTURE values out

# Trying to read the access database
# https://stackoverflow.com/questions/37912560/programmatically-read-access-mdb-files-into-r-for-both-windows-and-mac
# https://stackoverflow.com/questions/23568899/access-data-base-import-to-r-installation-of-mdb-tools-on-mac
library("Hmisc")
#df <- mdb.get("D:/HWSD/HWSD.mdb") #what is supposed to be inside the quotes: not working, can't get the downloads one to work either
df1 <- read.csv("D:/HWSD/D_ADD_PROP.csv")
head(df1)
# summary(df)
# summary(df$HWSD_SMU)
# df$D_TEXTURE
# head(df$HWSD_DATA)
# df$D_AWC
# df$D_SYMBOL
# df$D_SYMBOL90

# Now we'll use the code we extracted to crosswalk things
#NOTE NOTE NOTE: I COULD HAVE THE COLUMN CROSSWALK WRONG
soil.spp1 <- df$HWSD_DATA[df$HWSD_DATA$ID %in% unique(spp.sp$hwsd_code),] #example of merging (ignore df$)
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

#Loading in CSV Files into R from Access: Did I choose right CSV? (CSV UTR 8) & Are they supposed to be combined?
D_ADD_PROP <- read.csv("D:/HWSD/D_ADD_PROP.csv")
D_ADD_PROP
D_AWC <- read.csv("D:/HWSD/D_AWC.csv") #Focus on this one
D_AWC
D_COVERAGE <- read.csv("D:/HWSD/D_COVERAGE.csv")
D_COVERAGE
D_DRAINAGE <- read.csv("D:/HWSD/D_DRAINAGE.csv")
D_DRAINAGE
D_IL <- read.csv("D:/HWSD/D_IL.csv")
D_IL
D_ISSOIL <- read.csv("D:/HWSD/D_ISSOIL.csv")
D_ISSOIL
D_PHASE <- read.csv("D:/HWSD/D_PHASE.csv")
D_PHASE
D_ROOTS <- read.csv("D:/HWSD/D_ROOTS.csv")
D_ROOTS
D_SWR <- read.csv("D:/HWSD/D_SWR.csv")
D_SWR
D_SYMBOL <- read.csv("D:/HWSD/D_SYMBOL.csv")
D_SYMBOL
D_SYMBOL74 <- read.csv("D:/HWSD/D_SYMBOL74.csv")
D_SYMBOL74
D_SYMBOL85 <- read.csv("D:/HWSD/D_SYMBOL85.csv")
D_SYMBOL85
D_SYMBOL90 <- read.csv("D:/HWSD/D_SYMBOL90.csv")
D_SYMBOL90
D_TEXTURE <- read.csv("D:/HWSD/D_TEXTURE.csv")
D_TEXTURE
D_USDA_TEX_CLASS <- read.csv("D:/HWSD/D_USDA_TEX_CLASS.csv")
D_USDA_TEX_CLASS
HWSD_DATA <- read.csv("D:/HWSD/HWSD_DATA.csv")
head(HWSD_DATA)
HWSD_SMU <- read.csv("D:/HWSD/HWSD_SMU.csv")
head(HWSD_SMU)
colnames(HWSD_DATA)
unique(HWSD_DATA$MU_GLOBAL)
nrow(HWSD_DATA)

#Goal: Create For Loop that takes largest value of T's and makes that new column like "T-Actual 1"


HWSD_DATA$T_SAND
HWSD_DATA$T_CLAY
HWSD_DATA$T_SILT

# #Haven't tried this out yet but will to get column names of T_SILT, T_SAND, & T_CLAY: Whichever has more
# sum(is.na(HWSD_DATA$T_SAND))
# sum(is.na(HWSD_DATA$T_CLAY))
# sum(is.na(HWSD_DATA$T_SILT))
# #Not working with NA values
# unique(HWSD_DATA$MU_GLOBAL)
#HWSD_DATA$T_MAJORITY <- colnames(HWSD_DATA)[max.col(HWSD_DATA,ties.method="first")]
HWSD_DATA_MAJORITIES <- data.frame(matrix(ncol = length(colnames(HWSD_DATA))), nrow = 0)
colnames(HWSD_DATA_MAJORITIES) <- colnames(HWSD_DATA)

#For loop to create new df with only rows of unique MU_GLOBALS with highest share values but does not work
#for (i in 1:length(unique(HWSD_DATA$MU_GLOBAL))) {
  #HWSD_DATA_SHARE_MAJORITIES[i] <- max(HWSD_DATA$SHARE[HWSD_DATA$MU_GLOBAL==(unique(HWSD_DATA$MU_GLOBAL))[i]])
  HWSD_DATA_MAJORITIES[i, ] <- HWSD_DATA[HWSD_DATA$SHARE== max(HWSD_DATA$SHARE[HWSD_DATA$MU_GLOBAL==(unique(HWSD_DATA$MU_GLOBAL))[i]]) 
                                       & HWSD_DATA$MU_GLOBAL==(unique(HWSD_DATA$MU_GLOBAL))[i], ]
}
head(HWSD_DATA_MAJORITIES)
write.csv(HWSD_DATA_MAJORITIES, "D:/HWSD/HWSD_DATA_MAJORITIES.csv", row.names= FALSE)

#work with singular values before applying to whole df
# #Should Work because it works for a single unique MU_GLOBAL value but does not work in giant for loop: works now
# (unique(HWSD_DATA$MU_GLOBAL))[23]
# HWSD_DATA[HWSD_DATA$MU_GLOBAL==(unique(HWSD_DATA$MU_GLOBAL))[23], ]
# A <- HWSD_DATA[HWSD_DATA$SHARE== max(HWSD_DATA$SHARE[HWSD_DATA$MU_GLOBAL==(unique(HWSD_DATA$MU_GLOBAL))[23]]) 
#           & HWSD_DATA$MU_GLOBAL==(unique(HWSD_DATA$MU_GLOBAL))[23], ]
# (unique(HWSD_DATA$MU_GLOBAL))[29]
# HWSD_DATA[HWSD_DATA$MU_GLOBAL==(unique(HWSD_DATA$MU_GLOBAL))[29], ]
# B <- HWSD_DATA[HWSD_DATA$SHARE== max(HWSD_DATA$SHARE[HWSD_DATA$MU_GLOBAL==(unique(HWSD_DATA$MU_GLOBAL))[23]]) 
#           & HWSD_DATA$MU_GLOBAL==(unique(HWSD_DATA$MU_GLOBAL))[29], ]
# C <- data.frame(matrix(ncol = length(colnames(HWSD_DATA))), nrow = 0)
# colnames(C) <- colnames(HWSD_DATA)
# #Problem occurs here because there is not enough columns in C to fit A: fixed it
# C[1, ] <- A
# C[2, ] <- B
# head(C)

HWSD_DATA_MAJORITIES[23,]
head(HWSD_DATA_MAJORITIES)
HWSD_DATA$SHARE[HWSD_DATA$MU_GLOBAL==(unique(HWSD_DATA$MU_GLOBAL))[721]]

length(HWSD_DATA_SHARE_MAJORITIES) == length(unique(HWSD_DATA$MU_GLOBAL))
max(HWSD_DATA$SHARE[HWSD_DATA$MU_GLOBAL[1]])
HWSD_DATA$SHARE[HWSD_DATA$MU_GLOBAL==7760]

# #Example of how line 147 (HWSD_DATA$T_MAJORITY assignment) should work
# DF <- matrix(sample(1:9,9),ncol=3,nrow=3)
# DF <- as.data.frame.matrix(DF)
# DF
# colnames(DF)[max.col(DF,ties.method="first")]

# HWSD_DATA$T_SUM <- NA
# for (i in 1:nrow(HWSD_DATA)){
#   HWSD_DATA$T_SUM[i] <- HWSD_DATA$T_SAND[i] + HWSD_DATA$T_CLAY[i] + HWSD_DATA$T_SILT[i]
# }
# 
# unique(HWSD_DATA$T_SUM)
# table(HWSD_DATA$T_SUM)
# HWSD_DATA[(HWSD_DATA$T_SUM==100), ]
# 
# DF <- matrix(sample(1:9,9),ncol=3,nrow=3)
# DF <- as.data.frame.matrix(DF)
# DF
# colnames(DF)[max.col(DF,ties.method="first")]


#trying to remake the occurence points without the specific column values
Data.Tilia_platyphyllos <- read.csv("D:/spp_raw_points/spp_raw_points/Tilia_platyphyllos.csv")
colnames(Data.Tilia_platyphyllos)
unique(Data.Malus_asiatica$taxonIdentificationNotes)
Data.Malus_asiatica <- subset(Data.Malus_asiatica, select = -c(localityDescription, taxonIdentificationNotes,
                                                               county, stateProvince))
(Data.Malus_asiatica)
tail(spp.species)
spp.species[1]

spp.species <- spp.species[!spp.species %in% "Morton_Arb.csv"]
spp.species

#loop to do this for each species
for (i in 1:length(spp.species)) {
  species_raw <- read.csv(file.path("D:/spp_raw_points/spp_edited_points_Extracted/spp_edited_points/", spp.species[i]))
  species_raw <- subset(species_raw, select = -c(localityDescription, taxonIdentificationNotes,
                                                 county, stateProvince))
  write.csv(species_raw, file.path("D:/spp_raw_points/spp_edited_points_Extracted2/", spp.species[i]), row.names = FALSE)
}
