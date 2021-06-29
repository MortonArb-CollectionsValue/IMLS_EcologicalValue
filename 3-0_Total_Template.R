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
predictor <- c("ppt", "soil", "srad", "tmax", "tmin", "vpd")
path.dat <- "D:/Data_IMLS_Ecological_Value/Preloaded_Data2"
path.clim <- "D:/Data_IMLS_Ecological_Value/Climate_Extract_Drive"

# Mac Google Drive
# path.dat <- "/Volumes/GoogleDrive/Shared drives/IMLS MFA/Environmental Niche Value/Extracted Data/Preloaded_Data2/"
# path.clim <- "/Volumes/GoogleDrive/Shared drives/IMLS MFA/Environmental Niche Value/Extracted Data/Climate_Extract/"

# Where the data is getting saved
pre.out  <- "D:/Data_IMLS_Ecological_Value/Total_PreReductions2/"
post.out <- "D:/Data_IMLS_Ecological_Value/Total_PostReductions2/"

if(!dir.exists(pre.out)) dir.create(pre.out, recursive = T)
if(!dir.exists(post.out)) dir.create(post.out, recursive = T)

# Mac Google Drive
# pre.out  <- "/Volumes/GoogleDrive/Shared drives/IMLS MFA/Environmental Niche Value/Analysis/Toal_PreReductions2/"
# post.out <- "/Volumes/GoogleDrive/Shared drives/IMLS MFA/Environmental Niche Value/Analysis/Total_PostReductions2/"


for (j in 1:length(predictor)) {
  for (i in 1:length(Genera)) {
    a <- read.csv(file.path(path.dat, predictor[j], paste0(predictor[j], "_", Genera[i], ".csv")))
    a$X <- NULL
    assign(paste(predictor[j], "_", Genera[i], sep=''), a)
  }
}

#Steps in order to put categorical data in dataframe with final reductions
important_malus <- data.frame(UID=character(), species_name_acc=character(), decimalLatitude=double(), decimalLongitude=double(), stringsAsFactors=FALSE)
important_quercus <- data.frame(UID=character(), species_name_acc=character(), decimalLatitude=double(), decimalLongitude=double(), stringsAsFactors=FALSE)
important_tilia <- data.frame(UID=character(), species_name_acc=character(), decimalLatitude=double(), decimalLongitude=double(), stringsAsFactors=FALSE)
important_ulmus <- data.frame(UID=character(), species_name_acc=character(), decimalLatitude=double(), decimalLongitude=double(), stringsAsFactors=FALSE)

for (j in 1:length(Genera)) {
  for(i in 1:length(predictor)) {
    path.dat.specific <- file.path(path.clim, predictor[i]) #file paths for different folders
    MortonArb_Data_path <-  read.csv(file.path(path.dat.specific,"0_MortonArb.csv")) #morton arb values in diff. folders
    #made each data frame end with original to indicate before NA values were removed
    #Data Extraction
    initial_extraction <- list.files(path = path.dat.specific, pattern = Genera[j], full.names=TRUE)
    #assign(paste(Genera[j], "_", predictor[i]), sep = ''), list.files(path = file.path(path.dat, predictor[i]), pattern = Genera[i], full.names=TRUE))
    #assign(paste(predictor[1], "cols", sep = ''), names(read.csv()))
    extractioncols <- names(read.csv(initial_extraction[1]))
    coltype <- rep(NA, length(extractioncols))
    col.char <- which(extractioncols %in% c("nativeDatabaseID", "MU.SOURCE1"))
    coltype[col.char] <- "character"
    intermediate_extraction <- lapply(initial_extraction, read.csv, colClasses=coltype) %>% bind_rows()
    intermediate_extraction <- rbind.fill(intermediate_extraction, MortonArb_Data_path)
    intermediate_extraction <- tidyr::separate(intermediate_extraction, col = "species_name_acc", into=c("genus", "species"))
    final_extraction <- intermediate_extraction[complete.cases(intermediate_extraction[,colnames(intermediate_extraction[,6:17])]),]
    final_extraction <- final_extraction[1:5]
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
}



Malus_climate_total <- cbind(ppt_Malus, soil_Malus, srad_Malus, tmax_Malus, tmin_Malus, vpd_Malus)
#Malus_climate_total <- Malus_climate_total[complete.cases(Malus_climate_total[,6:37]),]
MalusTotal_Reduction <- cor(Malus_climate_total)
write.csv(MalusTotal_Reduction, file.path(pre.out, "MalusTotal_Reduction.csv"), row.names=FALSE)
# # write.csv(MalusTotal_Reduction1, paste0("D:/Data_IMLS_Ecological_Value/Total_PreReductions/MalusTotal_Reduction1.csv"), row.names=TRUE)
# # Malus_Climate_Final <- Malus_climate_total[,c(3,5,8,12,14,18)]
# #   #Malus Reduced Variables: ppt.min.min, soil.max.sd, srad.max.sd, tmax.min.sd, tmin.max.sd, vpd.min.min


#Adding in Categorical Variables & Saving
Malus_Climate_Final <- cbind(important_malus, Malus_climate_total)
write.csv(Malus_Climate_Final, file.path(post.out, "Malus_Climate_Final.csv"), row.names=FALSE)


Quercus_climate_total <- cbind(ppt_Quercus, soil_Quercus, srad_Quercus, tmax_Quercus, tmin_Quercus, vpd_Quercus)
#Quercus_climate_total <- Quercus_climate_total[complete.cases(Quercus_climate_total[,6:37]),]
QuercusTotal_Reduction <- cor(Quercus_climate_total)
write.csv(QuercusTotal_Reduction, file.path(pre.out, "QuercusTotal_Reduction.csv"), row.names=FALSE)
#write.csv(QuercusTotal_Reduction1, paste0("D:/Data_IMLS_Ecological_Value/Total_PreReductions/QuercusTotal_Reduction1.csv"), row.names=TRUE)
#Quercus_Climate_Final <- Quercus_climate_total[,c(3,5,7,10,13,17)]
  #Quercus Reduced Variables: ppt.min.min, soil.max.sd, srad.ann.sd, tmax.ann.sd, tmin.ann.sd, vpd.max.sd

#Adding in Categorical Variables & Saving
Quercus_Climate_Final <- cbind(important_quercus, Quercus_climate_total)
write.csv(Quercus_Climate_Final, file.path(post.out, "Quercus_Climate_Final.csv"), row.names=FALSE)


Tilia_climate_total <- cbind(ppt_Tilia, soil_Tilia, srad_Tilia, tmax_Tilia, tmin_Tilia, vpd_Tilia)
#Tilia_climate_total <- Tilia_climate_total[complete.cases(Tilia_climate_total[,6:37]),]
TiliaTotal_Reduction <- cor(Tilia_climate_total)
write.csv(TiliaTotal_Reduction, file.path(pre.out, "TiliaTotal_Reduction.csv"), row.names=FALSE)
#write.csv(TiliaTotal_Reduction1, paste0("D:/Data_IMLS_Ecological_Value/Total_PreReductions/TiliaTotal_Reduction1.csv"), row.names=TRUE)
#Tilia_Climate_Final <- Tilia_climate_total[,c(3,5,8,10,14,17)]
  #Tilia Reduced Variables: ppt.min.min, soil.max.sd, srad.max.sd, tmax.ann.sd, tmin.max.sd, vpd.max.sd

#Adding in Categorical Variables & Saving
Tilia_Climate_Final <- cbind(important_tilia, Tilia_climate_total)
write.csv(Tilia_Climate_Final, file.path(post.out, "Tilia_Climate_Final.csv"), row.names=FALSE)


Ulmus_climate_total <- cbind(ppt_Ulmus, soil_Ulmus, srad_Ulmus, tmax_Ulmus, tmin_Ulmus, vpd_Ulmus)
#Ulmus_climate_total <- Ulmus_climate_total[complete.cases(Ulmus_climate_total[,6:37]),]
UlmusTotal_Reduction <- cor(Ulmus_climate_total)
write.csv(UlmusTotal_Reduction, file.path(pre.out, "UlmusTotal_Reduction.csv"), row.names=FALSE)
#write.csv(UlmusTotal_Reduction1, paste0("D:/Data_IMLS_Ecological_Value/Total_PreReductions/UlmusTotal_Reduction1.csv"), row.names=TRUE)
#Ulmus_Climate_Final <- Ulmus_climate_total[,c(3,6,8,12,14,17)]
  #Ulmus Reduced Variables: ppt.min.min, soil.min.sd, srad.max.sd, tmax.min.sd, tmin.max.sd, vpd.max.sd

#Adding in Categorical Variables & Saving
Ulmus_Climate_Final <- cbind(important_ulmus, Ulmus_climate_total)
write.csv(Ulmus_Climate_Final, file.path(post.out, "Ulmus_Climate_Final.csv"), row.names = FALSE)


# for (i in 1:length(Genera)) {
#   # genus <- paste(Genera[i])
#   # #NEED TO CREATE ORIGINAL WITH NA'S REMOVED AND ALL COMBINED: TO HAVE THE NON-NUMERIC COL's
#   # genus_climate_total <- cbind(
#   #   #ppt_original[,1:5], 
#   #   ppt_genus, soil_genus, srad_genus, tmax_genus, tmin_genus, vpd_genus)
#   
#   #getting rid of the genus NA values
#   genus_climate_total <- genus_climate_total[complete.cases(genus_climate_total[,6:37]),]
#   #looking at similarity between diff. columns
#   genusTotal_Reduction1 <- cor(genus_climate_total[,6:37])
#   #saving these pre-reduction variable correlation charts for analysis into folder
#   write.csv(genusTotal_Reduction1, paste0("D:/Data_IMLS_Ecological_Value/Total_PreReductions/", genus, "Total.csv"), row.names=TRUE)
# }

#genus PCA 1
genus.pca <- prcomp(genus_climate_total, center = TRUE,scale. = TRUE) 
summary(genus.pca)
genus.pca$rotation
#analysis of PCA Plots
ggbiplot(genus.pca) #basic plot


##Reduced Variables: not anymore
#Malus: ppt.min.min, soil.max.sd, srad.max.sd, tmax.min.sd, tmin.max.sd, vpd.min.min
#Quercus: ppt.min.min, soil.max.sd, srad.ann.sd, tmax.ann.sd, tmin.ann.sd, vpd.max.sd
#Tilia: ppt.min.min, soil.max.sd, srad.max.sd, tmax.ann.sd, tmin.max.sd, vpd.max.sd
#Ulmus: ppt.min.min, soil.min.sd, srad.max.sd, tmax.min.sd, tmin.max.sd, vpd.max.sd