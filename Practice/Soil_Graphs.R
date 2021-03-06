#loading in necessary packages
library("dplyr"); library(plyr); library("readr")
library(ggplot2)
library(rgdal); library(sp); library(raster)
library(Hmisc)
library(data.table)

#Creating dfs that combine every species in each collectio
#Malus Soil Data
path.dat <- "D:/Data_IMLS_Ecological_Value/Soil_Extract_Drive/Soil_Extract"
#path.dat <- "/Volumes/GoogleDrive/Shared drives/IMLS MFA/Environmental Niche Value/Extracted Data/Soil_Extract/"
malus_soil <- list.files(path = path.dat,
                        pattern = "Malus", full.names = TRUE)
soilcols <- names(read.csv(malus_soil[1]))
col.char <- which(soilcols %in% c("nativeDatabaseID", "MU.SOURCE1"))
coltype <- rep(NA, length(soilcols))
coltype[col.char] <- "character"
malus_all <-  lapply(malus_soil, read.csv, colClasses=coltype) %>% bind_rows()
head(malus_all)
tail(malus_all)

#Quercus Collection
quercus_soil <- list.files(path = path.dat,
                         pattern = "Quercus", full.names = TRUE)
soilcols <- names(read.csv(quercus_soil[1]))
col.char <- which(soilcols %in% c("nativeDatabaseID", "MU.SOURCE1"))
coltype <- rep(NA, length(soilcols))
coltype[col.char] <- "character"
quercus_all <-  lapply(quercus_soil, read.csv, colClasses=coltype) %>% bind_rows()
head(quercus_all)
tail(quercus_all)

#Tilia Collection
tilia_soil <- list.files(path = path.dat,
                           pattern = "Tilia", full.names = TRUE)
soilcols <- names(read.csv(tilia_soil[1]))
col.char <- which(soilcols %in% c("nativeDatabaseID", "MU.SOURCE1"))
coltype <- rep(NA, length(soilcols))
coltype[col.char] <- "character"
tilia_all <-  lapply(tilia_soil, read.csv, colClasses=coltype) %>% bind_rows()
head(tilia_all)
tail(tilia_all)

#Ulmus Collection
ulmus_soil <- list.files(path = path.dat,
                           pattern = "Ulmus", full.names = TRUE)
soilcols <- names(read.csv(ulmus_soil[1]))
col.char <- which(soilcols %in% c("nativeDatabaseID", "MU.SOURCE1"))
coltype <- rep(NA, length(soilcols))
coltype[col.char] <- "character"
ulmus_all <-  lapply(ulmus_soil, read.csv, colClasses=coltype) %>% bind_rows()
head(ulmus_all)
tail(ulmus_all)

#loading in the Morton Arb Data for horizontal line in shiny app graph
MortonArb_Data <- read.csv(file.path(path.dat,"0_MortonArb.csv"))
MortonArb_Data

#package for shiny
library(shiny); library(shinydashboard); library(shinyWidgets)

#combining data frames of different genus to allow for dropdown chooser in shiny
library(data.table)
total <- rbind(malus_all, quercus_all, tilia_all,  ulmus_all, MortonArb_Data)
# total <- rbind(malus_all, quercus_all, tilia_all, ulmus_all, MortonArb_Data)
head(total)
tail(total)

#Lucien's code that vectorizes species_name_acc into genus & species:
total <- tidyr::separate(total, col = "species_name_acc", into=c("genus", "species"))
head(total)
tail(total)
colnames(total)

#filtering out only important soil columns: used only Top Soil, numeric variables
#Took out PHASE1,PHASE2,ROOTS,IL,SWR because there were NA in head & tail
important_soil_traits <- c("T.TEXTURE","DRAINAGE","REF.DEPTH","ADD.PROP","T.GRAVEL","T.SAND","T.SILT","T.CLAY","T.USDA.TEX.CLASS",
                           "T.REF.BULK.DENSITY","T.OC","T.PH.H2O","T.CEC.CLAY","T.CEC.SOIL","T.BS","T.TEB","T.CACO3","T.CASO4",
                           "T.ESP","T.ECE","T.BULK.DENSITY","AWC_VALUE","decimalLongitude.1","decimalLatitude.1")

#boxplot/violinplot that displays dropdown for genus & variable
#horizontal redline displaying Arb's value
shinyApp(
  ui = fluidPage(
    selectInput("Genus", "Choose a Genus:", list(Genus=as.list(unique(total$genus[total$genus!="MortonArb"])))),
    selectInput("Variable", "Variable:", list(Variable=names(total[important_soil_traits]))),
    plotOutput("data")
  ),
  server = function(input, output) {
    output$data <- renderPlot({
      total$VAR.GRAPH <- total[,input$Variable]
      # what original looked like
      ggplot(total[total$genus==input$Genus, ]) +
        geom_violin(data=total[total$genus==input$Genus, ], aes(x=species, y=VAR.GRAPH), scale = "width") + #can be boxplots or violin plot
        geom_hline(data=total[total$UID=="MORTONARB",], aes(yintercept=VAR.GRAPH), color="red") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))

    })
  }
)

   shinyApp(ui, server)
