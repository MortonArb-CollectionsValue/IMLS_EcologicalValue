#Loading in correct packages to extract
# library(devtools)
# install_github("vqv/ggbiplot") #just used blank line when asked question
library(ggbiplot)
library(dplyr); library(plyr); library(readr)
library(ggplot2)
# library(rgdal); library(sp); library(raster)
library(Hmisc)
library(data.table)

Genera <- c("Malus", "Quercus", "Tilia", "Ulmus")
path.dat <- "D:/Data_IMLS_Ecological_Value/Soil_Extract_Drive/Soil_Extract/"
path.out <- "D:/Data_IMLS_Ecological_Value/PreloadedSoil_Data/"

# Google Drive File paths on a Mac
# path.dat <- "/Volumes/GoogleDrive/Shared drives/IMLS MFA/Environmental Niche Value/Extracted Data/Soil_Extract/"
# path.out <- "/Volumes/GoogleDrive/Shared drives/IMLS MFA/Environmental Niche Value/Extracted Data/PreloadedSoil_Data/"

if(!dir.exists(path.out)) dir.create(path.out, recursive = T)

#Sort out reduced columns for combining all together
soil.predictors <- c("UID", "T.GRAVEL", "T.SILT", "T.CLAY", "T.OC", "T.PH.H2O", "T.TEB", "T.ECE", "AWC_VALUE", 
                      "ROOTS", "T.CEC.CLAY",	"T.CEC.SOIL",	"T.CACO3",	"T.CASO4",	"T.ESP")

for (j in 1:length(Genera)) {
  MortonArb_Data_path <-  read.csv(file.path(path.dat,"0_MortonArb.csv")) #morton arb values in diff. folders
  #made each data frame end with original to indicate before NA values were removed
  #Data Extraction
  initial_extraction <- list.files(path = path.dat, pattern = Genera[j], full.names=TRUE)
  extractioncols <- names(read.csv(initial_extraction[1]))
  coltype <- rep(NA, length(extractioncols))
  col.char <- which(extractioncols %in% c("nativeDatabaseID", "MU.SOURCE1"))
  coltype[col.char] <- "character"
  intermediate_extraction <- lapply(initial_extraction, read.csv, colClasses=coltype) %>% bind_rows()
  intermediate_extraction <- tidyr::separate(intermediate_extraction, col = "species_name_acc", into=c("genus", "species"))
  
  #getting rid of the (genus) NA values
  final_extraction <- intermediate_extraction[complete.cases(intermediate_extraction[,colnames(intermediate_extraction[,soil.predictors])]),]
  final_extraction <- rbind.fill(final_extraction, MortonArb_Data_path) 
    #did this after so arb data would not be filtered out since UID is one of the important traits & Arb does not have UID
  final_extraction <- final_extraction[,soil.predictors]
  write.csv(final_extraction, file.path(path.out, paste0(Genera[j], ".csv")), row.names=FALSE)
}
