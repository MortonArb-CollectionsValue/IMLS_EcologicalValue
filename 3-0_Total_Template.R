Genera <- c("Malus", "Quercus", "Tilia", "Ulmus")
predictor <- c("ppt", "soil", "srad", "tmax", "tmin", "vpd")
path.dat <- "D:/Data_IMLS_Ecological_Value/Preloaded_Data"

for (j in 1:length(predictor)) {
  for (i in 1:length(Genera)) {
    a <- read.csv(file.path(path.dat, predictor[j], paste0(predictor[j], "_", Genera[i], ".csv")))
    a$X <- NULL
    assign(paste(predictor[j], "_", Genera[i], sep=''), a)
  }
}

Malus_climate_total <- cbind(ppt_Malus, soil_Malus, srad_Malus, tmax_Malus, tmin_Malus, vpd_Malus)
#Malus_climate_total <- Malus_climate_total[complete.cases(Malus_climate_total[,6:37]),]
MalusTotal_Reduction1 <- cor(Malus_climate_total)
#write.csv(MalusTotal_Reduction1, paste0("D:/Data_IMLS_Ecological_Value/Total_PreReductions/MalusTotal_Reduction1.csv"), row.names=TRUE)
Malus_Climate_Final <- Malus_climate_total[,c(3,5,8,12,14,18)]
  #Malus Reduced Variables: ppt.min.min, soil.max.sd, srad.max.sd, tmax.min.sd, tmin.max.sd, vpd.min.min

Quercus_climate_total <- cbind(ppt_Quercus, soil_Quercus, srad_Quercus, tmax_Quercus, tmin_Quercus, vpd_Quercus)
#Quercus_climate_total <- Quercus_climate_total[complete.cases(Quercus_climate_total[,6:37]),]
QuercusTotal_Reduction1 <- cor(Quercus_climate_total)
#write.csv(QuercusTotal_Reduction1, paste0("D:/Data_IMLS_Ecological_Value/Total_PreReductions/QuercusTotal_Reduction1.csv"), row.names=TRUE)
Quercus_Climate_Final <- Quercus_climate_total[,c(3,5,7,10,13,17)]
  #Quercus Reduced Variables: ppt.min.min, soil.max.sd, srad.ann.sd, tmax.ann.sd, tmin.ann.sd, vpd.max.sd

Tilia_climate_total <- cbind(ppt_Tilia, soil_Tilia, srad_Tilia, tmax_Tilia, tmin_Tilia, vpd_Tilia)
#Tilia_climate_total <- Tilia_climate_total[complete.cases(Tilia_climate_total[,6:37]),]
TiliaTotal_Reduction1 <- cor(Tilia_climate_total)
#write.csv(TiliaTotal_Reduction1, paste0("D:/Data_IMLS_Ecological_Value/Total_PreReductions/TiliaTotal_Reduction1.csv"), row.names=TRUE)
Tilia_Climate_Final <- Tilia_climate_total[,c(3,5,8,10,14,17)]
  #Tilia Reduced Variables: ppt.min.min, soil.max.sd, srad.max.sd, tmax.ann.sd, tmin.max.sd, vpd.max.sd

Ulmus_climate_total <- cbind(ppt_Ulmus, soil_Ulmus, srad_Ulmus, tmax_Ulmus, tmin_Ulmus, vpd_Ulmus)
#Ulmus_climate_total <- Ulmus_climate_total[complete.cases(Ulmus_climate_total[,6:37]),]
UlmusTotal_Reduction1 <- cor(Ulmus_climate_total)
#write.csv(UlmusTotal_Reduction1, paste0("D:/Data_IMLS_Ecological_Value/Total_PreReductions/UlmusTotal_Reduction1.csv"), row.names=TRUE)
Ulmus_Climate_Final <- Ulmus_climate_total[,c(3,6,8,12,14,17)]
  #Ulmus Reduced Variables: ppt.min.min, soil.min.sd, srad.max.sd, tmax.min.sd, tmin.max.sd, vpd.max.sd

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


##Reduced Variables
#Malus: ppt.min.min, soil.max.sd, srad.max.sd, tmax.min.sd, tmin.max.sd, vpd.min.min
#Quercus: ppt.min.min, soil.max.sd, srad.ann.sd, tmax.ann.sd, tmin.ann.sd, vpd.max.sd
#Tilia: ppt.min.min, soil.max.sd, srad.max.sd, tmax.ann.sd, tmin.max.sd, vpd.max.sd
#Ulmus: ppt.min.min, soil.min.sd, srad.max.sd, tmax.min.sd