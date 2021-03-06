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
# path.dat <- "D:/Data_IMLS_Ecological_Value/PreloadedSoil_Data"
# path.dat2 <- "D:/Data_IMLS_Ecological_Value/Soil_Extract_Drive/Soil_Extract/"
# path.out <- "D:/Data_IMLS_Ecological_Value/Total_PostSoilReductions/"

# Paths for Google Drive on a Mac
path.dat <- "/Volumes/GoogleDrive/Shared drives/IMLS MFA/Environmental Niche Value/Extracted Data/PreloadedSoil_Data/"
path.dat2 <- "/Volumes/GoogleDrive/Shared drives/IMLS MFA/Environmental Niche Value/Extracted Data/Soil_Extract/"
path.out <- "/Volumes/GoogleDrive/Shared drives/IMLS MFA/Environmental Niche Value/Analysis/Total_PostSoilReductions/"

important.traits <- c("UID", "T.GRAVEL", "T.SILT", "T.CLAY", "T.OC", "T.PH.H2O", "T.TEB", "T.ECE", "AWC_VALUE",
                      "ROOTS", "T.CEC.CLAY", "T.CEC.SOIL", "T.CACO3", "T.CASO4",	"T.ESP")
important.traits.CR <- c("ppt.ann.mean", "ppt.min.min", "soil.ann.max", "soil.max.sd", "srad.ann.max", "srad.ann.sd", "tmax.ann.max", "tmax.max.sd", "tmin.ann.min", "tmin.min.sd", "vpd.ann.max", "vpd.max.sd", "T.SILT", "T.CLAY", "T.OC", "T.PH.H2O", "T.ECE", "AWC_VALUE", "T.CEC.SOIL", "T.CACO3")

#Loading in Data
Soil_Malus <- read.csv(file.path(path.dat, "Soil_Malus.csv"))
Soil_Quercus <- read.csv(file.path(path.dat, "Soil_Quercus.csv"))
Soil_Tilia <- read.csv(file.path(path.dat, "Soil_Tilia.csv"))
Soil_Ulmus <- read.csv(file.path(path.dat, "Soil_Ulmus.csv"))

#Steps in order to put categorical data in dataframe with final reductions
important_malus <- data.frame(UID=character(), species_name_acc=character(), decimalLatitude=double(), decimalLongitude=double(), stringsAsFactors=FALSE)
important_quercus <- data.frame(UID=character(), species_name_acc=character(), decimalLatitude=double(), decimalLongitude=double(), stringsAsFactors=FALSE)
important_tilia <- data.frame(UID=character(), species_name_acc=character(), decimalLatitude=double(), decimalLongitude=double(), stringsAsFactors=FALSE)
important_ulmus <- data.frame(UID=character(), species_name_acc=character(), decimalLatitude=double(), decimalLongitude=double(), stringsAsFactors=FALSE)

for (j in 1:length(Genera)) {
    MortonArb_Data_path <-  read.csv(file.path(path.dat2,"0_MortonArb.csv")) #morton arb values in diff. folders
    #made each data frame end with original to indicate before NA values were removed
    #Data Extraction
    initial_extraction <- list.files(path = path.dat2, pattern = Genera[j], full.names=TRUE)
    extractioncols <- names(read.csv(initial_extraction[1]))
    coltype <- rep(NA, length(extractioncols))
    col.char <- which(extractioncols %in% c("nativeDatabaseID", "MU.SOURCE1"))
    coltype[col.char] <- "character"
    intermediate_extraction <- lapply(initial_extraction, read.csv, colClasses=coltype) %>% bind_rows()
    intermediate_extraction <- rbind.fill(intermediate_extraction)
    intermediate_extraction <- tidyr::separate(intermediate_extraction, col = "species_name_acc", into=c("genus", "species"))
    final_extraction <- intermediate_extraction[complete.cases(intermediate_extraction[,colnames(intermediate_extraction[,c(important.traits)])]),]
    final_extraction <- rbind.fill(final_extraction, MortonArb_Data_path)
    final_extraction <- final_extraction[,c("UID", "genus", "species", "decimalLatitude", "decimalLongitude")]
    if (j==1) {
      important_malus <- rbind(important_malus, final_extraction)
      important_malus <- unique(important_malus)
    } else { if (j==2) {
      important_quercus <- rbind(important_quercus, final_extraction)
      important_quercus <- unique(important_quercus)
    } else { if (j==3) {
      important_tilia <- rbind(important_tilia, final_extraction)
      important_tilia <- unique(important_tilia)
    } else {
      important_ulmus <- rbind(important_ulmus, final_extraction)
      important_ulmus <- unique(important_ulmus)
      }
    }
  }
}


#SHOULD I GET RID OF THE UID BEFORE SO THEN I DON'T HAVE TO HARD CODE

#Adding in Categorical Variables & Saving
#Soil_Malus <- Soil_Malus[complete.cases(Soil_Malus), ]
#important_malus <- important_malus[complete.cases(important_malus), ]
Soil_Malus$UID <- NULL
Malus_Soil_Final <- cbind(important_malus, Soil_Malus)
#Malus_Soil_Final[, 6] <- NULL
write.csv(Malus_Soil_Final, file.path(path.out, "Malus_Soil_Final.csv"), row.names = FALSE)


#Adding in Categorical Variables & Saving
#Soil_Quercus <- Soil_Quercus[complete.cases(Soil_Quercus), ]
#important_quercus <- important_quercus[complete.cases(important_quercus), ]
Soil_Quercus$UID <- NULL
Quercus_Soil_Final <- cbind(important_quercus, Soil_Quercus)
#Quercus_Soil_Final[, 6] <- NULL
write.csv(Quercus_Soil_Final, file.path(path.out, "Quercus_Soil_Final.csv"), row.names = FALSE)


#Adding in Categorical Variables & Saving
#Soil_Tilia <- Soil_Tilia[complete.cases(Soil_Tilia), ]
#important_tilia <- important_tilia[complete.cases(important_tilia), ]
Soil_Tilia$UID <- NULL
Tilia_Soil_Final <- cbind(important_tilia, Soil_Tilia)
#Tilia_Soil_Final[, 6] <- NULL
write.csv(Tilia_Soil_Final, file.path(path.out, "Tilia_Soil_Final.csv"), row.names = FALSE)


#Adding in Categorical Variables & Saving
#Soil_Ulmus <- Soil_Ulmus[complete.cases(Soil_Ulmus), ]
#important_ulmus <- important_ulmus[complete.cases(important_ulmus), ]
Soil_Ulmus$UID <- NULL
Ulmus_Soil_Final <- cbind(important_ulmus, Soil_Ulmus)
#Ulmus_Soil_Final[, 6] <- NULL
write.csv(Ulmus_Soil_Final, file.path(path.out, "Ulmus_Soil_Final.csv"), row.names = FALSE)
