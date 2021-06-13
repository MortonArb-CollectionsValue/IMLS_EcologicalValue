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

#Creating Graph
ggplot(total_pca2, aes(x = Trait, y = Value)) +
  geom_bar(stat = "identity", position = position_stack()) +
  coord_flip()
