#loading in necessary packages
library("dplyr"); library("plyr"); library("readr")
library(ggplot2)
library(rgdal); library(sp); library(raster)
library(Hmisc)
library(data.table)

#Creating dfs that combine every species in each collection
#Malus Collection
malus_soil <- list.files(path = "D:/Data_IMLS_Ecological_Value/Soil_Extract_Drive/Soil_Extract", 
                        pattern = "Malus", full.names = TRUE)
soilcols <- names(read.csv(malus_soil[1]))
col.char <- which(soilcols %in% c("nativeDatabaseID", "MU.SOURCE1"))
coltype <- rep(NA, length(soilcols))
coltype[col.char] <- "character"
malus_all <-  lapply(malus_soil, read.csv, colClasses=coltype) %>% bind_rows()
head(malus_all)
tail(malus_all)

#Quercus Collection
quercus_soil <- list.files(path = "D:/Data_IMLS_Ecological_Value/Soil_Extract_Drive/Soil_Extract", 
                         pattern = "Quercus", full.names = TRUE)
soilcols <- names(read.csv(quercus_soil[1]))
col.char <- which(soilcols %in% c("nativeDatabaseID", "MU.SOURCE1"))
coltype <- rep(NA, length(soilcols))
coltype[col.char] <- "character"
quercus_all <-  lapply(quercus_soil, read.csv, colClasses=coltype) %>% bind_rows()
head(quercus_all)
tail(quercus_all)

#Tilia Collection
tilia_soil <- list.files(path = "D:/Data_IMLS_Ecological_Value/Soil_Extract_Drive/Soil_Extract", 
                           pattern = "Tilia", full.names = TRUE)
soilcols <- names(read.csv(tilia_soil[1]))
col.char <- which(soilcols %in% c("nativeDatabaseID", "MU.SOURCE1"))
coltype <- rep(NA, length(soilcols))
coltype[col.char] <- "character"
tilia_all <-  lapply(tilia_soil, read.csv, colClasses=coltype) %>% bind_rows()
head(tilia_all)
tail(tilia_all)

#Ulmus Collection
ulmus_soil <- list.files(path = "D:/Data_IMLS_Ecological_Value/Soil_Extract_Drive/Soil_Extract", 
                           pattern = "Ulmus", full.names = TRUE)
soilcols <- names(read.csv(ulmus_soil[1]))
col.char <- which(soilcols %in% c("nativeDatabaseID", "MU.SOURCE1"))
coltype <- rep(NA, length(soilcols))
coltype[col.char] <- "character"
ulmus_all <-  lapply(ulmus_soil, read.csv, colClasses=coltype) %>% bind_rows()
head(ulmus_all)
tail(ulmus_all)

#package for shiny
library(shiny); library(shinydashboard); library(shinyWidgets)

#combining data frames of different genus to allow for dropdown chooser in shiny
library(data.table)
total <- rbind(malus_all, quercus_all, tilia_all, ulmus_all)
head(total)
tail(total)

#Lucien's code that vectorizes species_name_acc into genus & species:
total <- tidyr::separate(total, col = "species_name_acc", into=c("genus", "species"))
tail(total)

MortonArb_Data <- read.csv("D:/Data_IMLS_Ecological_Value/Soil_Extract_Drive/Soil_Extract/0_MortonArb.csv")

Unique_genus <- unique(total$genus)

#boxplot that displays dropdown for genus & variable
shinyApp(
  ui = fluidPage(
    selectInput("Genus", "Choose a Genus:", list(Genus=as.list(Unique_genus))),
    varSelectInput("Variable", "Variable:", total[27:70]),
    plotOutput("data")
  ),
  server = function(input, output) {
    output$data <- renderPlot({
      ggplot(total[total$genus==input$Genus, ], aes(species, !!input$Variable)) + geom_boxplot() + 
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
    })
  }
)

shinyApp(ui, server)
