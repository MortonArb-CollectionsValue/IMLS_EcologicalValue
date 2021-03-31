#Loading in correct packages to extract
library(devtools)
# install_github("vqv/ggbiplot") #just used blank line when asked question
library(ggbiplot)
library("dplyr"); library("plyr"); library("readr")
library(ggplot2)
library(rgdal); library(sp); library(raster)
library(Hmisc)
library(data.table)

#File Paths for PPT
path.dat.ppt <- "D:/Data_IMLS_Ecological_Value/Climate_Extract_Drive/ppt"  

#Morton Arb Values in PPT
MortonArb_Data_ppt <- read.csv(file.path(path.dat.ppt,"0_MortonArb.csv"))

#PPT Plots: 
  #made each data frame end with original to indicate before NA values were removed
  #used to combine climate and soil data
#Malus PPT Data
malus_ppt <- list.files(path = path.dat.ppt,
                         pattern = "Malus", full.names = TRUE)
pptcols <- names(read.csv(malus_ppt[1]))
#col.char <- which(pptcols %in% c("nativeDatabaseID", "MU.SOURCE1"))
coltype <- rep(NA, length(pptcols))
coltype <- "character"
malus_climate_ppt_original <-  lapply(malus_ppt, read.csv, colClasses=coltype) %>% bind_rows()
malus_climate_ppt_original <- rbind.fill(malus_climate_ppt_original, MortonArb_Data_ppt)
malus_climate_ppt_original <- tidyr::separate(malus_climate_ppt_original, col = "species_name_acc", into=c("genus", "species"))
head(malus_climate_ppt_original)
tail(malus_climate_ppt_original)

#Quercus PPT Data
quercus_ppt <- list.files(path = path.dat.ppt,
                        pattern = "Quercus", full.names = TRUE)
climatecols <- names(read.csv(quercus_ppt[1]))
# col.char <- which(pptcols %in% c("nativeDatabaseID", "MU.SOURCE1"))
coltype <- rep(NA, length(climatecols))
coltype <- "character"
quercus_climate_ppt_original <-  lapply(quercus_ppt, read.csv, colClasses=coltype) %>% bind_rows()
quercus_climate_ppt_original <- rbind.fill(quercus_climate_ppt_original, MortonArb_Data_ppt)
quercus_climate_ppt_original <- tidyr::separate(quercus_climate_ppt_original, col = "species_name_acc", into=c("genus", "species"))
head(quercus_climate_ppt_original)
tail(quercus_climate_ppt_original)

#Tilia PPT Data
tilia_ppt <- list.files(path = path.dat.ppt,
                          pattern = "Tilia", full.names = TRUE)
pptcols <- names(read.csv(tilia_ppt[1]))
# col.char <- which(pptcols %in% c("nativeDatabaseID", "MU.SOURCE1"))
coltype <- rep(NA, length(pptcols))
coltype <- "character"
tilia_climate_ppt_original <-  lapply(tilia_ppt, read.csv, colClasses=coltype) %>% bind_rows()
tilia_climate_ppt_original <- rbind.fill(tilia_climate_ppt_original, MortonArb_Data_ppt)
tilia_climate_ppt_original <- tidyr::separate(tilia_climate_ppt_original, col = "species_name_acc", into=c("genus", "species"))
head(tilia_climate_ppt_original)
tail(tilia_climate_ppt_original)

#Ulmus PPT Data
ulmus_ppt <- list.files(path = path.dat.ppt,
                        pattern = "Ulmus", full.names = TRUE)
pptcols <- names(read.csv(ulmus_ppt[1]))
# col.char <- which(pptcols %in% c("nativeDatabaseID", "MU.SOURCE1"))
coltype <- rep(NA, length(pptcols))
coltype <- "character"
ulmus_climate_ppt_original <-  lapply(ulmus_ppt, read.csv, colClasses=coltype) %>% bind_rows()
ulmus_climate_ppt_original <- rbind.fill(ulmus_climate_ppt_original, MortonArb_Data_ppt)
ulmus_climate_ppt_original <- tidyr::separate(ulmus_climate_ppt_original, col = "species_name_acc", into=c("genus", "species"))
head(ulmus_climate_ppt_original)
tail(ulmus_climate_ppt_original)

#PPT Boxplots to show data distribution
#package for shiny
library(shiny); library(shinydashboard); library(shinyWidgets); library(plyr)

#combining data frames of different genus to allow for dropdown chooser in shiny
#Made rbind.fills to add Morton Arb to each genera's df comments so it is not counted 2x in total_ppt
library(data.table)
total_ppt <- rbind.fill(malus_climate_ppt_original, quercus_climate_ppt_original, tilia_climate_ppt_original,  ulmus_climate_ppt_original, MortonArb_Data_ppt)
head(total_ppt)
tail(total_ppt)

#choosing only the important traits: no categorical
important_traits_ppt <- c("ppt.ann.mean","ppt.ann.sd","ppt.ann.max","ppt.ann.min",
                          "ppt.max.mean","ppt.max.sd","ppt.max.max","ppt.max.min",
                          "ppt.min.mean","ppt.min.sd","ppt.min.max","ppt.min.min")

#have to make sure MortonArb points is MortonArb so it doesn't show up in genera dropdown
total_ppt$genus[total_ppt$UID=="MORTONARB"] <- "MortonArb"

# #boxplot/violinplot that displays dropdown for genus & variable
# #horizontal redline displaying Arb's value
# shinyApp(
#   ui = fluidPage(
#     selectInput("Genus", "Choose a Genus:", list(Genus=as.list(unique(total_ppt$genus[total_ppt$genus!="MortonArb"])))),
#     selectInput("Variable", "Variable:", list(Variable=names(total_ppt[important_traits_ppt]))),
#     plotOutput("data")
#   ),
#   server = function(input, output) {
#     output$data <- renderPlot({
#       total_ppt$VAR.GRAPH <- total_ppt[,input$Variable]
#       # what original looked like
#       ggplot(total_ppt[total_ppt$genus==input$Genus, ]) +
#         geom_boxplot(data=total_ppt[total_ppt$genus==input$Genus, ], aes(x=species, y=VAR.GRAPH), scale = "width") + #can be boxplots or violin plot
#         geom_hline(data=total_ppt[total_ppt$UID==MortonArb_Data_ppt,], aes(yintercept=VAR.GRAPH), color="red") +
#         theme(axis.text.x = element_text(angle = 90, hjust = 1))
#       
#     })
#   }
# )
# 
# shinyApp(ui, server)


#PCA PLOTS
#for reduction looked at sums of each trait and then took least sum from each category (ann, max, min)
important_traits_ppt2 <- c("ppt.ann.sd","ppt.max.sd","ppt.min.min")

#Malus PPT PCA
#converting all malus columns to numerics
sapply(malus_climate_ppt_original, mode)
malus_climate_ppt_original$ppt.ann.mean <- as.numeric(malus_climate_ppt_original$ppt.ann.mean)
malus_climate_ppt_original$ppt.ann.sd <- as.numeric(malus_climate_ppt_original$ppt.ann.sd)
malus_climate_ppt_original$ppt.ann.max <- as.numeric(malus_climate_ppt_original$ppt.ann.max)
malus_climate_ppt_original$ppt.ann.min <- as.numeric(malus_climate_ppt_original$ppt.ann.min)
malus_climate_ppt_original$ppt.max.mean <- as.numeric(malus_climate_ppt_original$ppt.max.mean)
malus_climate_ppt_original$ppt.max.sd <- as.numeric(malus_climate_ppt_original$ppt.max.sd)
malus_climate_ppt_original$ppt.max.max <- as.numeric(malus_climate_ppt_original$ppt.max.max)
malus_climate_ppt_original$ppt.max.min <- as.numeric(malus_climate_ppt_original$ppt.max.min)
malus_climate_ppt_original$ppt.min.mean <- as.numeric(malus_climate_ppt_original$ppt.min.mean)
malus_climate_ppt_original$ppt.min.sd <- as.numeric(malus_climate_ppt_original$ppt.min.sd)
malus_climate_ppt_original$ppt.min.max <- as.numeric(malus_climate_ppt_original$ppt.min.max)
malus_climate_ppt_original$ppt.min.min <- as.numeric(malus_climate_ppt_original$ppt.min.min)
sapply(malus_climate_ppt_original, mode)

#getting rid of the Malus NA values
malus_climate_ppt <- malus_climate_ppt_original[complete.cases(malus_climate_ppt_original[,important_traits_ppt]),]
#Reduction of Malus Variables: saved them to hard drive
MalusPPT_Reduction1 <- cor(malus_climate_ppt[,important_traits_ppt])
MalusPPT_Reduction2 <- cor(malus_climate_ppt[,important_traits_ppt2])
#write.csv(MalusPPT_Reduction1, "D:/Data_IMLS_Ecological_Value/Climate_Extract_Drive/ppt_Reductions/MalusPPT_Reduction1.csv", row.names=TRUE)
#write.csv(MalusPPT_Reduction2, "D:/Data_IMLS_Ecological_Value/Climate_Extract_Drive/ppt_Reductions/MalusPPT_Reduction2.csv", row.names=TRUE)
#Malus PCA 1
malus.pca <- prcomp(malus_climate_ppt[,important_traits_ppt], center = TRUE,scale. = TRUE) 
summary(malus.pca)
malus.pca$rotation
#analysis of PCA Plots
ggbiplot(malus.pca) #basic plot
#Malus PCA 2
malus.pca <- prcomp(malus_climate_ppt[,important_traits_ppt2], center = TRUE,scale. = TRUE) 
summary(malus.pca)
malus.pca$rotation
#analysis of PCA Plots
ggbiplot(malus.pca) #basic plot


#Quercus PPT PCA
#converting all quercus columns to numerics
sapply(quercus_climate_ppt_original, mode)
quercus_climate_ppt_original$ppt.ann.mean <- as.numeric(quercus_climate_ppt_original$ppt.ann.mean)
quercus_climate_ppt_original$ppt.ann.sd <- as.numeric(quercus_climate_ppt_original$ppt.ann.sd)
quercus_climate_ppt_original$ppt.ann.max <- as.numeric(quercus_climate_ppt_original$ppt.ann.max)
quercus_climate_ppt_original$ppt.ann.min <- as.numeric(quercus_climate_ppt_original$ppt.ann.min)
quercus_climate_ppt_original$ppt.max.mean <- as.numeric(quercus_climate_ppt_original$ppt.max.mean)
quercus_climate_ppt_original$ppt.max.sd <- as.numeric(quercus_climate_ppt_original$ppt.max.sd)
quercus_climate_ppt_original$ppt.max.max <- as.numeric(quercus_climate_ppt_original$ppt.max.max)
quercus_climate_ppt_original$ppt.max.min <- as.numeric(quercus_climate_ppt_original$ppt.max.min)
quercus_climate_ppt_original$ppt.min.mean <- as.numeric(quercus_climate_ppt_original$ppt.min.mean)
quercus_climate_ppt_original$ppt.min.sd <- as.numeric(quercus_climate_ppt_original$ppt.min.sd)
quercus_climate_ppt_original$ppt.min.max <- as.numeric(quercus_climate_ppt_original$ppt.min.max)
quercus_climate_ppt_original$ppt.min.min <- as.numeric(quercus_climate_ppt_original$ppt.min.min)
sapply(quercus_climate_ppt_original, mode)

#apply to all scripts to condense code
for(IMP in important_traits_ppt){ quercus_climate_ppt_original[,IMP] <- as.numeric(quercus_climate_ppt_original[,IMP])}

#getting rid of the Quercus NA values
quercus_climate_ppt <- quercus_climate_ppt_original[complete.cases(quercus_climate_ppt_original[,important_traits_ppt]),]
#Reduction of Quercus Variables: saved them to hard drive
QuercusPPT_Reduction1 <- cor(quercus_climate_ppt[,important_traits_ppt])
QuercusPPT_Reduction2 <- cor(quercus_climate_ppt[,important_traits_ppt2])
#write.csv(QuercusPPT_Reduction1, "D:/Data_IMLS_Ecological_Value/Climate_Extract_Drive/ppt_Reductions/QuercusPPT_Reduction1.csv", row.names=TRUE)
#write.csv(QuercusPPT_Reduction2, "D:/Data_IMLS_Ecological_Value/Climate_Extract_Drive/ppt_Reductions/QuercusPPT_Reduction2.csv", row.names=TRUE)
#Quercus PCA 1
quercus.pca <- prcomp(quercus_climate_ppt[,important_traits_ppt], center = TRUE,scale. = TRUE) 
summary(quercus.pca)
quercus.pca$rotation
#analysis of PCA Plots
ggbiplot(quercus.pca) #basic plot
#Quercus PCA 2
quercus.pca <- prcomp(quercus_climate_ppt[,important_traits_ppt2], center = TRUE,scale. = TRUE) 
summary(quercus.pca)
quercus.pca$rotation
#analysis of PCA Plots
ggbiplot(quercus.pca) #basic plot


#Tilia PPT PCA
#converting all tilia columns to numerics
sapply(tilia_climate_ppt_original, mode)
tilia_climate_ppt_original$ppt.ann.mean <- as.numeric(tilia_climate_ppt_original$ppt.ann.mean)
tilia_climate_ppt_original$ppt.ann.sd <- as.numeric(tilia_climate_ppt_original$ppt.ann.sd)
tilia_climate_ppt_original$ppt.ann.max <- as.numeric(tilia_climate_ppt_original$ppt.ann.max)
tilia_climate_ppt_original$ppt.ann.min <- as.numeric(tilia_climate_ppt_original$ppt.ann.min)
tilia_climate_ppt_original$ppt.max.mean <- as.numeric(tilia_climate_ppt_original$ppt.max.mean)
tilia_climate_ppt_original$ppt.max.sd <- as.numeric(tilia_climate_ppt_original$ppt.max.sd)
tilia_climate_ppt_original$ppt.max.max <- as.numeric(tilia_climate_ppt_original$ppt.max.max)
tilia_climate_ppt_original$ppt.max.min <- as.numeric(tilia_climate_ppt_original$ppt.max.min)
tilia_climate_ppt_original$ppt.min.mean <- as.numeric(tilia_climate_ppt_original$ppt.min.mean)
tilia_climate_ppt_original$ppt.min.sd <- as.numeric(tilia_climate_ppt_original$ppt.min.sd)
tilia_climate_ppt_original$ppt.min.max <- as.numeric(tilia_climate_ppt_original$ppt.min.max)
tilia_climate_ppt_original$ppt.min.min <- as.numeric(tilia_climate_ppt_original$ppt.min.min)
sapply(tilia_climate_ppt_original, mode)

#getting rid of the Tilia NA values
tilia_climate_ppt <- tilia_climate_ppt_original[complete.cases(tilia_climate_ppt_original[,important_traits_ppt]),]
#Reduction of tilia Variables: saved them to hard drive
TiliaPPT_Reduction1 <- cor(tilia_climate_ppt[,important_traits_ppt])
TiliaPPT_Reduction2 <- cor(tilia_climate_ppt[,important_traits_ppt2])
#write.csv(TiliaPPT_Reduction1, "D:/Data_IMLS_Ecological_Value/Climate_Extract_Drive/ppt_Reductions/TiliaPPT_Reduction1.csv", row.names=TRUE)
#write.csv(TiliaPPT_Reduction2, "D:/Data_IMLS_Ecological_Value/Climate_Extract_Drive/ppt_Reductions/TiliaPPT_Reduction2.csv", row.names=TRUE)
#tilia PCA 1
tilia.pca <- prcomp(tilia_climate_ppt[,important_traits_ppt], center = TRUE,scale. = TRUE) 
summary(tilia.pca)
tilia.pca$rotation
#analysis of PCA Plots
ggbiplot(tilia.pca) #basic plot
#tilia PCA 2
tilia.pca <- prcomp(tilia_climate_ppt[,important_traits_ppt2], center = TRUE,scale. = TRUE) 
summary(tilia.pca)
tilia.pca$rotation
#analysis of PCA Plots
ggbiplot(tilia.pca) #basic plot


#Ulmus PPT PCA
#converting all ulmus columns to numerics
sapply(ulmus_climate_ppt_original, mode)
ulmus_climate_ppt_original$ppt.ann.mean <- as.numeric(ulmus_climate_ppt_original$ppt.ann.mean)
ulmus_climate_ppt_original$ppt.ann.sd <- as.numeric(ulmus_climate_ppt_original$ppt.ann.sd)
ulmus_climate_ppt_original$ppt.ann.max <- as.numeric(ulmus_climate_ppt_original$ppt.ann.max)
ulmus_climate_ppt_original$ppt.ann.min <- as.numeric(ulmus_climate_ppt_original$ppt.ann.min)
ulmus_climate_ppt_original$ppt.max.mean <- as.numeric(ulmus_climate_ppt_original$ppt.max.mean)
ulmus_climate_ppt_original$ppt.max.sd <- as.numeric(ulmus_climate_ppt_original$ppt.max.sd)
ulmus_climate_ppt_original$ppt.max.max <- as.numeric(ulmus_climate_ppt_original$ppt.max.max)
ulmus_climate_ppt_original$ppt.max.min <- as.numeric(ulmus_climate_ppt_original$ppt.max.min)
ulmus_climate_ppt_original$ppt.min.mean <- as.numeric(ulmus_climate_ppt_original$ppt.min.mean)
ulmus_climate_ppt_original$ppt.min.sd <- as.numeric(ulmus_climate_ppt_original$ppt.min.sd)
ulmus_climate_ppt_original$ppt.min.max <- as.numeric(ulmus_climate_ppt_original$ppt.min.max)
ulmus_climate_ppt_original$ppt.min.min <- as.numeric(ulmus_climate_ppt_original$ppt.min.min)
sapply(ulmus_climate_ppt_original, mode)

#getting rid of the Ulmus NA values
ulmus_climate_ppt <- ulmus_climate_ppt_original[complete.cases(ulmus_climate_ppt_original[,important_traits_ppt]),]
#Reduction of Ulmus Variables: saved them to hard drive
UlmusPPT_Reduction1 <- cor(ulmus_climate_ppt[,important_traits_ppt])
UlmusPPT_Reduction2 <- cor(ulmus_climate_ppt[,important_traits_ppt2])
#write.csv(UlmusPPT_Reduction1, "D:/Data_IMLS_Ecological_Value/Climate_Extract_Drive/ppt_Reductions/UlmusPPT_Reduction1.csv", row.names=TRUE)
#write.csv(UlmusPPT_Reduction2, "D:/Data_IMLS_Ecological_Value/Climate_Extract_Drive/ppt_Reductions/UlmusPPT_Reduction2.csv", row.names=TRUE)
#Ulmus PCA 1
ulmus.pca <- prcomp(ulmus_climate_ppt[,important_traits_ppt], center = TRUE,scale. = TRUE) 
summary(ulmus.pca)
ulmus.pca$rotation
#analysis of PCA Plots
ggbiplot(ulmus.pca) #basic plot
#Ulmus PCA 2
ulmus.pca <- prcomp(ulmus_climate_ppt[,important_traits_ppt2], center = TRUE,scale. = TRUE) 
summary(ulmus.pca)
ulmus.pca$rotation
#analysis of PCA Plots
ggbiplot(ulmus.pca) #basic plot
