library(ggplot2); library(vegan)
library(ggbiplot); library(tidyverse)


#reading in PCA data
#Changing columns to include trait & genera
#stacking all the PCAs and Genera into one df
PCs <- c("PC1", "PC2", "PC3")
PC_dfs <- c("a", "b", "c")
Genera <- c("Malus", "Quercus", "Tilia", "Ulmus")
path.dat <- "D:/Data_IMLS_Ecological_Value/PCAs"
total_pca2 <- data.frame(Trait = character(), Value = numeric(), 
                         PC = character(), Genus = character())

for (i in 1:length(Genera)) {
  initial_extraction <- list.files(path.dat, full.names = TRUE)
  ab <- read.csv(initial_extraction[i])
  names(ab)[names(ab) == "X"] <- "Trait"
  for (j in 1:length(PCs)) {
    k <- select(ab, Trait, PCs[j])
    k$PC <- PCs[j]
    names(k)[names(k) == PCs[j]] <- "Value"
    k$Genus <- Genera[i]
    total_pca2 <- rbind(total_pca2, k)
  }
}

total_pca2
total_pca2$Trait
#Creating Graph: saving image
  #Changes to Make: ALL climate on top, ALL Soil on bottom
  #Could also make climate one color & soil one color

#trying to make it so that soil is on top and climate on bottom
total_pca2$Trait <- factor(total_pca2$Trait, 
                           levels=c("ppt.ann.mean", "ppt.min.min", "soil.ann.max", "soil.max.sd", "srad.ann.max", "srad.ann.sd", 
                                     "tmax.ann.min", "tmax.min.sd",  "tmin.ann.min", "tmin.ann.sd", "vpd.ann.max",  "vpd.max.sd",  
                                     "T.GRAVEL", "T.SILT", "T.CLAY", "T.OC", "T.PH.H2O", "T.TEB", "T.ECE", "AWC_VALUE", "T.CEC.CLAY", 
                                     "T.CEC.SOIL", "T.CACO3", "T.CASO4", "T.ESP"))

png("PCA_Loadings_3PCs.png", height=6, width=6, units="in", res=320)

ggplot(total_pca2, aes(x = Trait, y=abs(Value), fill=ifelse(Value<0, "Neg", "Pos"))) +
  geom_bar(stat = "identity", position = position_stack()) +
  coord_flip() + 
  facet_grid(PC~Genus, scales="free", space="free_x")

dev.off()
