Genera <- c("Malus", "Quercus", "Tilia", "Ulmus")
trait <- c("ppt", "soil", "srad", "tmax", "tmin", "vpd")
path.dat <- "D:/Data_IMLS_Ecological_Value/Preloaded_Data"

for (j in 1:length(trait)) {
  for (i in 1:length(Genera)) {
    a <- read.csv(file.path(path.dat, trait[j], paste0(trait[j], "_", Genera[i], ".csv")))
    a$X <- NULL
    assign(paste(trait[j], "_", Genera[i], sep=''), a)
  }
}


for (i in 1:length(Genera)) {
  genus <- paste(Genera[i])
  #NEED TO CREATE ORIGINAL WITH NA'S REMOVED AND ALL COMBINED: TO HAVE THE NON-NUMERIC COL's
  genus_climate_total <- cbind(
    #ppt_original[,1:5], 
    ppt_genus, soil_genus, srad_genus, tmax_genus, tmin_genus, vpd_genus)
  
  #getting rid of the genus NA values
  genus_climate_total <- genus_climate_total[complete.cases(genus_climate_total[,6:37]),]
  #looking at similarity between diff. columns
  genusTotal_Reduction1 <- cor(genus_climate_total
                               #[,6:37]
                               )
  #saving these pre-reduction variable correlation charts for analysis into folder
  write.csv(genusTotal_Reduction1, paste0("D:/Data_IMLS_Ecological_Value/Total_PreReductions/", genus, "Total.csv"), row.names=TRUE)
}

#genus PCA 1
genus.pca <- prcomp(genus_climate_total[,6:37], center = TRUE,scale. = TRUE) 
summary(genus.pca)
genus.pca$rotation
#analysis of PCA Plots
ggbiplot(genus.pca) #basic plot