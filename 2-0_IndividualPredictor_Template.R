#Structure
  #Load in Packages
  #File Paths & Loading in Plots
  #PCA Plots
    #Important Predictors
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
predictor <- c("ppt", "soil", "srad", "tmax", "tmin", "vpd")
path.dat <- "D:/Data_IMLS_Ecological_Value/Climate_Extract_Drive"

#Sort out reduced columns for combining all together
# important_predictors2_ppt <- c("ppt.ann.sd","ppt.max.sd","ppt.min.min")
# important_predictors2_soil <- c("soil.ann.sd","soil.max.sd", "soil.min.sd")
# important_predictors2_srad <- c("srad.ann.sd","srad.max.sd", "srad.min.min")
# important_predictors2_tmax <- c("tmax.ann.sd","tmax.max.sd", "tmax.min.sd")
# important_predictors2_tmin <- c("tmin.ann.sd","tmin.max.sd", "tmin.min.sd")
# important_predictors2_vpd <- c("vpd.ann.sd","vpd.max.sd", "vpd.min.min")
important_predictors_ppt <- c("UID", "ppt.ann.mean", "ppt.min.min")
important_predictors_soil <- c("UID", "soil.ann.max", "soil.max.sd")
important_predictors_srad <- c("UID", "srad.ann.max", "srad.ann.sd")
important_predictors_tmax <- c("UID", "tmax.ann.min", "tmax.min.sd")
important_predictors_tmin <- c("UID", "tmin.ann.min", "tmin.ann.sd")
important_predictors_vpd <- c("UID", "vpd.ann.max", "vpd.max.sd")


for(i in 1:length(predictor)) {
  path.dat.specific <- file.path(path.dat, predictor[i]) #file paths for different folders
  MortonArb_Data_path <-  read.csv(file.path(path.dat.specific,"0_MortonArb.csv")) #morton arb values in diff. folders
  for (j in 1:length(Genera)) {
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
    
    # #choosing only the important predictors: no categorical, cols 6;17
    # assign(paste("important_predictors_", predictor[i], sep = ''), c(colnames(intermediate_extraction[6:17])))
    #getting rid of the (genus) NA values
    final_extraction <- intermediate_extraction[complete.cases(intermediate_extraction[,colnames(intermediate_extraction[,6:17])]),]
    #assign(paste(Genera[j], "_climate_", predictor[i], sep = ''), final_extraction) #could save this as R Data file or csv, save to preloaded data folder
    #creating loop so that reduced data (1 variable per ann, max, min) is saved as csv so it can be extracted easier
    if (i==1) {
      final_extraction2 <- final_extraction[,important_predictors_ppt]
      write.csv(final_extraction2, paste0("D:/Data_IMLS_Ecological_Value/Preloaded_Data2/ppt/ppt_", Genera[j], ".csv"), row.names=TRUE)
    } else { if (i==2) {
      final_extraction2 <- final_extraction[,important_predictors_soil]
      write.csv(final_extraction2, paste0("D:/Data_IMLS_Ecological_Value/Preloaded_Data2/soil/soil_", Genera[j], ".csv"), row.names=TRUE)
      } else { if (i==3) {
        final_extraction2 <- final_extraction[,important_predictors_srad]
        write.csv(final_extraction2, paste0("D:/Data_IMLS_Ecological_Value/Preloaded_Data2/srad/srad_", Genera[j], ".csv"), row.names=TRUE)
        } else { if (i==4) {
          final_extraction2 <- final_extraction[,important_predictors_tmax]
          write.csv(final_extraction2, paste0("D:/Data_IMLS_Ecological_Value/Preloaded_Data2/tmax/tmax_", Genera[j], ".csv"), row.names=TRUE)
          } else { if (i==5) {
            final_extraction2 <- final_extraction[,important_predictors_tmin]
            write.csv(final_extraction2, paste0("D:/Data_IMLS_Ecological_Value/Preloaded_Data2/tmin/tmin_", Genera[j], ".csv"), row.names=TRUE)
            } else {
              final_extraction2 <- final_extraction[,important_predictors_vpd]
              write.csv(final_extraction2, paste0("D:/Data_IMLS_Ecological_Value/Preloaded_Data2/vpd/vpd_", Genera[j], ".csv"), row.names=TRUE)
            }
          }
        }
      }
    }
    ##save(final_extraction, "paste0(Genera[j], [.], ".RData") #How to save as a R Data file
  }
}

final_extraction2

#PCA PLOTS

# #Reduction of (genus) Variables: saved them to hard drive
# (genus)(predictor)_Reduction1 <- cor((genus)_climate_(predictor)[,important_predictors_(predictor)])
# (genus)(predictor)_Reduction2 <- cor((genus)_climate_(predictor)[,important_predictors_(predictor)2])
# #write.csv((genus)(predictor)_Reduction1, "D:/Data_IMLS_Ecological_Value/Climate_Extract_Drive/(predictor)_Reductions/(genus)(predictor)_Reduction1.csv", row.names=TRUE)
# #write.csv((genus)(predictor)_Reduction2, "D:/Data_IMLS_Ecological_Value/Climate_Extract_Drive/(predictor)_Reductions/(genus)(predictor)_Reduction2.csv", row.names=TRUE)
# #(genus) PCA 1
# (genus).pca <- prcomp((genus)_climate_(predictor)[,important_predictors_(predictor)], center = TRUE,scale. = TRUE) 
# summary((genus).pca)
# (genus).pca$rotation
# #analysis of PCA Plots
# ggbiplot((genus).pca) #basic plot
# #(genus) PCA 2
# (genus).pca <- prcomp((genus)_climate_(predictor)[,important_predictors_(predictor)2], center = TRUE,scale. = TRUE) 
# summary((genus).pca)
# (genus).pca$rotation
# #analysis of PCA Plots
# ggbiplot((genus).pca) #basic plot