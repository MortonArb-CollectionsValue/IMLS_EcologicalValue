#loading in necessary packages
library("dplyr"); library("plyr"); library("readr")
library(ggplot2)
library(rgdal); library(sp); library(raster)
library(Hmisc)
library(data.table)

#Creating dfs that combine every species in each collection
#Malus Collection
path.dat <- "D:/Data_IMLS_Ecological_Value/Soil_Extract_Drive/Soil_Extract"
path.dat <- "/Volumes/GoogleDrive/Shared drives/IMLS MFA/Environmental Niche Value/Extracted Data/Soil_Extract/"
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
total <- rbind(malus_all, MortonArb_Data)
# total <- rbind(malus_all, quercus_all, tilia_all, ulmus_all, MortonArb_Data)
head(total)
tail(total)

#Lucien's code that vectorizes species_name_acc into genus & species:
total <- tidyr::separate(total, col = "species_name_acc", into=c("genus", "species"))
tail(total)

#example of hor. line working with non-ggplot
chickwts
boxplot(chickwts$weight ~ chickwts$feed)
abline(h=chickwts$weight[1], col = "Red")

#example of hor.lin working with ggplot when using 2 different sets of data
library(dslabs)
murders
ggplot(data=murders) + geom_boxplot(data=murders[murders$state!="Texas", ], aes(murders$region[murders$state!="Texas"], murders$total[murders$state!="Texas"])) +
  geom_hline(yintercept=murders$total[44], color= "red")

#trouble shooting: possible with ggplot, possible to make horizontal line
#boxplot that displays dropdown for genus & variable
shinyApp(
  ui = fluidPage(
    selectInput("Genus", "Choose a Genus:", list(Genus=as.list(unique(total$genus[total$genus!="MortonArb"])))),
    selectInput("Variable", "Variable:", list(Variable=names(total)[27:70])), 
    plotOutput("data")
  ),
  server = function(input, output) {
    output$data <- renderPlot({
      # ggplot(total) +
      #   geom_boxplot(data=total[total$genus==input$Genus, ], aes(total$species[total$genus==input$Genus], !!input$Variable[total$genus==input$Genus])) + 
      #   geom_hline(yintercept=input$Variable[1467381], color="red") +
      #   theme(axis.text.x = element_text(angle = 90, hjust = 1))
      # HLINE <- as.numeric(MortonArb_Data[,input$Variable])
      
      total$VAR.GRAPH <- total[,input$Variable]
     # what original looked like
       ggplot(total[total$genus==input$Genus,]) +
       # geom_boxplot(data=total[total$genus==input$Genus,], aes(x=species, y=VAR.GRAPH)) +
         geom_violin(data=total[total$genus==input$Genus,], aes(x=species, y=VAR.GRAPH), scale="width") +
         
         geom_hline(data=total[total$UID=="MORTONARB",], aes(yintercept=VAR.GRAPH), color="red") +
         theme(axis.text.x = element_text(angle = 90, hjust = 1))
    })
  }
)

   shinyApp(ui, server)

   