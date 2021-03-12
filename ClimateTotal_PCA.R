#Loaded in packages from other dataframes
#Loaded in dataframes of data as well, converted all columns to numerics for PCA usage, got rid of NA values
  #Did this using ClimatePPT_PCA script, ClimateSoils_PCA script, etc.
#Loaded in important_traits vectors with reduced columns

#Different Number of Rows in ppt with others because ppt does not include Morton Arb Data points
  #Should I keep it?: decided to keep it for now

#Combining different Climate Traits of Malus
malus_climate_total <- cbind(malus_climate_ppt[,1:5], malus_climate_ppt[important_traits_ppt2], malus_climate_soil[important_traits_soil2],
                             malus_climate_srad[important_traits_srad2], malus_climate_tmax[important_traits_tmax2], 
                             malus_climate_tmin[important_traits_tmin2], malus_climate_vpd[important_traits_vpd2])
#looking at similarity between diff. columns
MalusTotal_Reduction1 <- cor(malus_climate_total[,6:23])
#write.csv(MalusTotal_Reduction1, "D:/Data_IMLS_Ecological_Value/Climate_Extract_Drive/MalusTotal_Reduction1.csv", row.names=TRUE)
#Malus PCA 1
malus.pca <- prcomp(malus_climate_total[,6:23], center = TRUE,scale. = TRUE) 
summary(malus.pca)
malus.pca$rotation
#analysis of PCA Plots
ggbiplot(malus.pca) #basic plot


#Combining different Climate Traits of Quercus
quercus_climate_total <- cbind(quercus_climate_ppt[,1:5], quercus_climate_ppt[important_traits_ppt2], quercus_climate_soil[important_traits_soil2], 
                               quercus_climate_srad[important_traits_srad2], quercus_climate_tmax[important_traits_tmax2], 
                               quercus_climate_tmin[important_traits_tmin2], quercus_climate_vpd[important_traits_vpd2])
#looking at similarity between diff. columns
QuercusTotal_Reduction1 <- cor(quercus_climate_total[,6:23])
#write.csv(QuercusTotal_Reduction1, "D:/Data_IMLS_Ecological_Value/Climate_Extract_Drive/QuercusTotal_Reduction1.csv", row.names=TRUE)
#Quercus PCA 1
quercus.pca <- prcomp(quercus_climate_total[,6:23], center = TRUE,scale. = TRUE) 
summary(quercus.pca)
quercus.pca$rotation
#analysis of PCA Plots
ggbiplot(quercus.pca) #basic plot


#Combining different Climate Traits of Tilia
tilia_climate_total <- cbind(tilia_climate_ppt[,1:5], tilia_climate_ppt[important_traits_ppt2], tilia_climate_soil[important_traits_soil2], 
                             tilia_climate_srad[important_traits_srad2], tilia_climate_tmax[important_traits_tmax2], 
                             tilia_climate_tmin[important_traits_tmin2], tilia_climate_vpd[important_traits_vpd2])
#looking at similarity between diff. columns
TiliaTotal_Reduction1 <- cor(tilia_climate_total[,6:23])
#write.csv(TiliaTotal_Reduction1, "D:/Data_IMLS_Ecological_Value/Climate_Extract_Drive/TiliaTotal_Reduction1.csv", row.names=TRUE)
#Tilia PCA 1
tilia.pca <- prcomp(tilia_climate_total[,6:23], center = TRUE,scale. = TRUE) 
summary(tilia.pca)
tilia.pca$rotation
#analysis of PCA Plots
ggbiplot(tilia.pca) #basic plot


#Combining different Climate Traits of Ulmus
ulmus_climate_total <- cbind(ulmus_climate_ppt[,1:5], ulmus_climate_ppt[important_traits_ppt2], ulmus_climate_soil[important_traits_soil2], 
                             ulmus_climate_srad[important_traits_srad2], ulmus_climate_tmax[important_traits_tmax2], 
                             ulmus_climate_tmin[important_traits_tmin2], ulmus_climate_vpd[important_traits_vpd2])
#looking at similarity between diff. columns
UlmusTotal_Reduction1 <- cor(ulmus_climate_total[,6:23])
#write.csv(UlmusTotal_Reduction1, "D:/Data_IMLS_Ecological_Value/Climate_Extract_Drive/UlmusTotal_Reduction1.csv", row.names=TRUE)
#Ulmus PCA 1
ulmus.pca <- prcomp(ulmus_climate_total[,6:23], center = TRUE,scale. = TRUE) 
summary(ulmus.pca)
ulmus.pca$rotation
#analysis of PCA Plots
ggbiplot(ulmus.pca) #basic plot


#Combining Climate w/ Soil Data: need to load data from Soil_PCA_Analysis
  #Different Number of Rows: Doesn't work
malus_total <- cbind(malus_climate_total, malus_all[important_traits2])
#Malus PCA 1
malus.pca <- prcomp(malus_total[,6:23], center = TRUE,scale. = TRUE) 
summary(malus.pca)
malus.pca$rotation
#analysis of PCA Plots
ggbiplot(malus.pca) #basic plot

