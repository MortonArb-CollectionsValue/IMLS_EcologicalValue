#Structure
  #Load in Packages
  #File Paths & Loading in Plots
  #PCA Plots
    #Important Traits
    #Morton Arb Genera Dropdown
    #Convert to Numerics & Get rid of NAs
    #have to create individual loops to sort out only reduced variables
    #Reduction: don't think I need this
    #PCA Plot: don't think I need this either, maybe not in for loop but commented out in script


#Loading in correct packages to extract
library(devtools)
# install_github("vqv/ggbiplot") #just used blank line when asked question
library(ggbiplot)
library("dplyr"); library("plyr"); library("readr")
library(ggplot2)
library(rgdal); library(sp); library(raster)
library(Hmisc)
library(data.table)


#creating real for loop
Genera <- c("Malus", "Quercus", "Tilia", "Ulmus")
trait <- c("ppt", "soil", "srad", "tmax", "tmin", "vpd")
path.dat <- "D:/Data_IMLS_Ecological_Value/Climate_Extract_Drive"


for(i in 1:length(trait)) {
  path.dat.specific <- file.path(path.dat, trait[i]) #file paths for different folders
  MortonArb_Data_path <-  read.csv(file.path(path.dat.specific,"0_MortonArb.csv")) #morton arb values in diff. folders
  for (j in 1:length(Genera)) {
    #made each data frame end with original to indicate before NA values were removed
    #Data Extraction
    initial_extraction <- list.files(path = path.dat.specific, pattern = Genera[j], full.names=TRUE)
    #assign(paste(Genera[j], "_", trait[i]), sep = ''), list.files(path = file.path(path.dat, trait[i]), pattern = Genera[i], full.names=TRUE))
    #assign(paste(trait[1], "cols", sep = ''), names(read.csv()))
    extractioncols <- names(read.csv(initial_extraction[1]))
    coltype <- rep(NA, length(extractioncols))
    col.char <- which(extractioncols %in% c("nativeDatabaseID", "MU.SOURCE1"))
    coltype[col.char] <- "character"
    intermediate_extraction <- lapply(initial_extraction, read.csv, colClasses=coltype) %>% bind_rows()
    intermediate_extraction <- rbind.fill(intermediate_extraction, MortonArb_Data_path)
    intermediate_extraction <- tidyr::separate(intermediate_extraction, col = "species_name_acc", into=c("genus", "species"))
    
    #choosing only the important traits: no categorical, cols 6;17
    assign(paste("important_traits_", trait[i], sep = ''), c(colnames(intermediate_extraction[6:17])))
    #converting them to numerics: don't need this anymore
    # for (g in 6:17) {
    #   intermediate_extraction[,g] <- as.numeric(intermediate_extraction[,g])
    # }
    #getting rid of the (genus) NA values
    final_extraction <- intermediate_extraction[complete.cases(intermediate_extraction[,colnames(intermediate_extraction[,6:17])]),]
    assign(paste(Genera[j], "_climate_", trait[i], sep = ''), final_extraction) #could save this as R Data file or csv, save to preloaded data folder
    ##save(final_extraction, "paste0(Genera[j], [.], ".RData") #How to save as a R Data file
  }
}

# #Testing to see if PCA works: works
# #Tilia PCA 1
# Tilia.pca <- prcomp(Tilia_climate_vpd[,important_traits_vpd], center = TRUE,scale. = TRUE)
# summary(Tilia.pca)
# Tilia.pca$rotation
# #analysis of PCA Plots
# ggbiplot(Tilia.pca) #basic plot

#Sort out reduced columns for combining all together
important_traits_ppt2 <- c("ppt.ann.sd","ppt.max.sd","ppt.min.min")
important_traits_soil2 <- c("soil.ann.sd","soil.max.sd", "soil.min.sd")
important_traits_srad2 <- c("srad.ann.sd","srad.max.sd", "srad.min.min")
important_traits_tmax2 <- c("tmax.ann.sd","tmax.max.sd", "tmax.min.sd")
important_traits_tmin2 <- c("tmin.ann.sd","tmin.max.sd", "tmin.min.sd")
important_traits_vpd2 <- c("vpd.ann.sd","vpd.max.sd", "vpd.min.min")

#PCA PLOTS

# #Reduction of (genus) Variables: saved them to hard drive
# (genus)(trait)_Reduction1 <- cor((genus)_climate_(trait)[,important_traits_(trait)])
# (genus)(trait)_Reduction2 <- cor((genus)_climate_(trait)[,important_traits_(trait)2])
# #write.csv((genus)(trait)_Reduction1, "D:/Data_IMLS_Ecological_Value/Climate_Extract_Drive/(trait)_Reductions/(genus)(trait)_Reduction1.csv", row.names=TRUE)
# #write.csv((genus)(trait)_Reduction2, "D:/Data_IMLS_Ecological_Value/Climate_Extract_Drive/(trait)_Reductions/(genus)(trait)_Reduction2.csv", row.names=TRUE)
# #(genus) PCA 1
# (genus).pca <- prcomp((genus)_climate_(trait)[,important_traits_(trait)], center = TRUE,scale. = TRUE) 
# summary((genus).pca)
# (genus).pca$rotation
# #analysis of PCA Plots
# ggbiplot((genus).pca) #basic plot
# #(genus) PCA 2
# (genus).pca <- prcomp((genus)_climate_(trait)[,important_traits_(trait)2], center = TRUE,scale. = TRUE) 
# summary((genus).pca)
# (genus).pca$rotation
# #analysis of PCA Plots
# ggbiplot((genus).pca) #basic plot