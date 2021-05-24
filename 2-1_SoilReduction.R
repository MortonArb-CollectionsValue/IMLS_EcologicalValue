#Loading in correct packages to extract
library(devtools)
# install_github("vqv/ggbiplot") #just used blank line when asked question
library(ggbiplot)
library("dplyr"); library("plyr"); library("readr")
library(ggplot2)
library(rgdal); library(sp); library(raster)
library(Hmisc)
library(data.table)

Genera <- c("Malus", "Quercus", "Tilia", "Ulmus")
path.dat <- "D:/Data_IMLS_Ecological_Value/Soil_Extract_Drive/Soil_Extract/"

#Sort out reduced columns for combining all together
soil.predictors <- c("T.GRAVEL", "T.SILT", "T.CLAY", "T.OC", "T.PH.H2O", "T.TEB", "T.ECE", "AWC_VALUE", 
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
  intermediate_extraction <- rbind.fill(intermediate_extraction, MortonArb_Data_path)
  intermediate_extraction <- tidyr::separate(intermediate_extraction, col = "species_name_acc", into=c("genus", "species"))
  
  #getting rid of the (genus) NA values
  final_extraction <- intermediate_extraction[complete.cases(intermediate_extraction[,colnames(intermediate_extraction[,6:17])]),]
  #creating loop so that reduced data (1 variable per ann, max, min) is saved as csv so it can be extracted easier
  final_extraction2 <- final_extraction[,soil.predictors]
  write.csv(final_extraction2, paste0("D:/Data_IMLS_Ecological_Value/PreloadedSoil_Data/", Genera[j], ".csv"), row.names=TRUE)
}