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

# #tried to combine all different species into 1 df: not sure if this actually worked, would need to do it for each genus
# read.csv("D:/Data_IMLS_Ecological_Value/Soil_Extract_Drive/Soil_Extract/")
# 
# data_malus <- list.files(path = "D:/Data_IMLS_Ecological_Value/Soil_Extract_Drive/Soil_Extract",     # Identify all csv files in folder
#                          pattern = "Malus", full.names = TRUE) %>%
#   lapply(read_csv) %>%                                            # Store all files in list
#   bind_rows                                                       # Combine data sets into one data set
# data_malus
# as.data.frame(data_malus)
# 
# data_quercus <- list.files(path = "D:/Data_IMLS_Ecological_Value/Soil_Extract_Drive/Soil_Extract",     # Identify all csv files in folder
#                        pattern = "Quercus", full.names = TRUE) %>%
#   lapply(read_csv) %>%                                            # Store all files in list
#   bind_rows                                                       # Combine data sets into one data set
# data_quercus
# as.data.frame(data_quercus)
# 
# data_tilia <- list.files(path = "D:/Data_IMLS_Ecological_Value/Soil_Extract_Drive/Soil_Extract",     # Identify all csv files in folder
#                          pattern = "Tilia", full.names = TRUE) %>%
#   lapply(read_csv) %>%                                            # Store all files in list
#   bind_rows                                                       # Combine data sets into one data set
# data_tilia
# as.data.frame(data_tilia)
# 
# data_ulmus <- list.files(path = "D:/Data_IMLS_Ecological_Value/Soil_Extract_Drive/Soil_Extract",     # Identify all csv files in folder
#                          pattern = "Ulmus", full.names = TRUE) %>%
#   lapply(read_csv) %>%                                            # Store all files in list
#   bind_rows                                                       # Combine data sets into one data set
# data_ulmus
# as.data.frame(data_ulmus)

##example code from Lucien
# bud.files <- list.files(path = "../data_processed/model_output/", pattern = "OldTT_model_budburst.csv", full.names = T)
# bud.stats <- as.data.frame(sapply(bud.files, read.csv, simplify=FALSE) %>% bind_rows(.id = "id"))
# 
# #original testing with individual species to make graphs
# Soil_Malus_asiatica <- read.csv("D:/Data_IMLS_Ecological_Value/Soil_Extract_Test/Malus_asiatica.csv")
# Soil_Malus_angustifolia <- read.csv("D:/Data_IMLS_Ecological_Value/Soil_Extract_Test/Malus_angustifolia.csv")
# class(Soil_Malus_angustifolia$nativeDatabaseID)
# Soil_Malus_asiatica
# class(Soil_Malus_asiatica$nativeDatabaseID)

# #tried to combine 2 different species into 1 df
# data_all <- list.files(path = "D:/Data_IMLS_Ecological_Value/Soil_Extract_Test",     # Identify all csv files in folder
#                        pattern = "*.csv", full.names = TRUE) %>%
#   lapply(read_csv) %>%                                            # Store all files in list
#   bind_rows                                                       # Combine data sets into one data set
# data_all
# as.data.frame(data_all)

#Example of what boxplot looks like without 
ggplot(Soil_Malus_asiatica) + geom_boxplot(aes(x=species_name_acc, y=T.SAND))

# Path to occurrence points; Shiven is D:; Christy can work directly with Google
path.occ <- "D:/spp_raw_points/spp_raw_points2/"
#path.occ <- file.path(path.google, "occurrence_points/outputs/spp_edited_points/")
spp.species <- dir(path.occ)
spp.species
spp.traits <- c(colnames(Soil_Malus_asiatica)[27:70])
spp.traits

## Original Method of creating dropdowns didn't work correctly
# ui <- fluidPage(
#   # Some custom CSS for a smaller font for preformatted text
#   tags$head(
#     tags$style(HTML("
#       pre, table.table {
#         font-size: smaller;
#       }
#     "))),
#   
#   #selectInput("Species", "Choose a Species:", list(Species=as.list(paste(spp.species)))), 
#   #selectInput("Trait", "Choose a Trait:", list(Traits=as.list(paste(spp.traits)))), 
#   varSelectInput("variable", "Variable", Soil_Malus_asiatica),
#   verbatimTextOutput("hover_info"),			    
#   mainPanel(plotOutput("plot1", width = 850, height = 750),
#             # hover=hoverOpts(id="plot_hover")
#   ))
# 
# 
# 
# #goal: try to get a geom_bin2d graph to work in shiny
# #so far almost everything of the graph is working: Date Observed is now normal but it has to be repeated twice for it to work
# #Weirds that on scale_x_date I have to have the range as quercus
# #Not working with Obs.List as x
# 
# server <- function(input, output) {
#   output$plot1 <- renderPlot({
#     #ggplot(read.csv(file.path("D:/Data_IMLS_Ecological_Value/Soil_Extract_Drive/", input$spp.species))) + 
#     ggplot(data = Soil_Malus_asiatica) +
#         geom_boxplot(aes(x=species_name_acc, y=input$variable))
#   })
# }
# 
# shinyApp(ui, server)

#didn't know which packages I needed for shiny so I just loaded them all
library(googlesheets4)
library(rgeos) # spatial analysis packages
library(lubridate)
library(car)
library(ggmap)
library(xts)
library(tigris)
library(shiny); library(shinydashboard); library(shinyWidgets)
library(stringr)

#combining data frames of different genus to allow for dropdown chooser in shiny
#Should I change the name of the data frame from total?
library(data.table)
head(malus_all)
head(quercus_all)
head(tilia_all)
head(ulmus_all)
total <- rbind(malus_all, quercus_all, tilia_all, ulmus_all)
head(total)
tail(total)

# #Experimenting with word function: should work
# word(total$species_name_acc[1], 1) #should be malus
# total$species_name_acc[1]
# word(total$species_name_acc[100000], 1) #should be quercus
# total$species_name_acc[100000]
# word(total$species_name_acc[1100000], 1) #should be tilia
# total$species_name_acc[1100000]
# word(total$species_name_acc[1400000], 1) #should be ulmus
# total$species_name_acc[1400000]
# 
# 
# #creating a column for the genus by extracting genus from species_name_acc
# total$Genus <- 0
# for (i in 1:nrow(total)) {             #don't understand why this isn't working
#   total$Genus[i] <- word(total$species_name_acc[i], 1)
# }
# head(total)

# #testing to see where the error is/when it stopped
# total$Genus[1] #should be malus
# sum(word(total$species_name_acc, 1)=="Malus") #number of malus rows
# total$Genus[51525] #should be quercus
# sum(word(total$species_name_acc, 1)=="Quercus") #number of quercus rows
# total$Genus[1089426]  #did not reach the tilia yet
# sum(word(total$species_name_acc, 1)=="Tilia") #number of quercus rows
# total$Genus[1089426]  #did not reach the tilia yet
# tail(total)
#Haven't actually seen the df when the for loop ends but I am making a csv because that might be faster than needing 
  #to run it each time I use script 

#Lucien's code that vectorizes species_name_acc into genus & species:
total <- tidyr::separate(total, col = "species_name_acc", into=c("genus", "species"))
tail(total)

Unique_genus <- unique(total$genus)

# single selection of column: only works for 1 species so far
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
