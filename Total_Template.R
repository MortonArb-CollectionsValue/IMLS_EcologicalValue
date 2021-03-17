for (i in 1:length(Genera)) {
  genus <- paste(Genera[i])
  genus_climate_total <- cbind(genus_climate_ppt_original[,1:5], genus_climate_ppt_original[important_traits_ppt2], genus_climate_soil_original[important_traits_soil2],
                                   genus_climate_srad_original[important_traits_srad2], genus_climate_tmax_original[important_traits_tmax2], 
                                   genus_climate_tmin_original[important_traits_tmin2], genus_climate_vpd_original[important_traits_vpd2], genus_all[important_traits2])
  
  #getting rid of the genus NA values
  genus_climate_total <- genus_climate_total[complete.cases(genus_climate_total[,6:37]),]
  #looking at similarity between diff. columns
  genusTotal_Reduction1 <- cor(genus_climate_total[,6:37])
  #write.csv(genusTotal_Reduction1, "D:/Data_IMLS_Ecological_Value/genus_Reduction1.csv", row.names=TRUE)
  #genus PCA 1
  genus.pca <- prcomp(genus_climate_total[,6:37], center = TRUE,scale. = TRUE) 
  summary(genus.pca)
  genus.pca$rotation
  #analysis of PCA Plots
  ggbiplot(genus.pca) #basic plot
}
