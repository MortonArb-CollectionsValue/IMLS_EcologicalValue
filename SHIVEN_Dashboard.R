# Christy playing around with PCA output to see how we can synthesize
library(ggplot2); library(ggrepel)
library(sp); library(plotly)

#Put a ton of packages just to make sure everything works in case
# library(dplyr)
library(rgdal)
library(ggmap)
library(raster)
library(rgeos)
library(xts)
library(tigris)
library(shiny)
library(shinydashboard)
# library(htmltools)
# library(stringr)
# library(car)
library(googlesheets4)
library(maps)
library(shinyWidgets)
# library(lubridate)


## path to the shared Google Drive folder
# path.dat <- "/Volumes/GoogleDrive/Shared drives/IMLS MFA/Environmental Niche Value" ## path for Shannon
path.dat <- "D:/Data_IMLS_Ecological_Value"   ## path to data for Shiven D drive
path.out <- file.path(path.dat, "Analysis/PrelimPCA/figures")

# path for the folder for figure output
path.figs <- file.path(path.dat, "figures")

# # Load in the PCA RData file
load(file.path(path.dat, "Extracted Data", "PCA_output.RData"))

#### Cleaning up the data
names(gen.clean.pca)
# To Find Morton, UID = MORTONARB; Species = MortonArb
gen.clean.pca[grep("Morton", gen.clean.pca$genus),"genus"] <- unlist(lapply(strsplit(gen.clean.pca$genus[grep("Morton", gen.clean.pca$genus)], "_"), function(x) x[2]))
# gen.clean.pca[]

# Getting rid of NAs just for sanity
gen.clean.pca <- gen.clean.pca[!is.na(gen.clean.pca$PC1),]



# Creating a map of two example species for the poster
### --------------
map.world <- map_data("world")

oak.examples <- gen.clean.pca[gen.clean.pca$species_name_acc %in% c("Quercus pontica","MortonArb") & gen.clean.pca$genus=="Quercus",] #changed to only Quercus pontica

oaks.use <- c("Quercus pontica") #changed it so only has Quercus Pontica

oak.examples$species_name_acc <- factor(oak.examples$species_name_acc, levels=c(oaks.use, "MortonArb"))



### Creating midpoints to bin occurrence in PCA space
gen.clean.pca$PC1.round <- round(gen.clean.pca$PC1, 3)
# gen.clean.pca$PC1.cut <- cut(gen.clean.pca$PC1, 50)
# gen.clean.pca$PC1.mid <- sapply(strsplit(gsub("^\\W|\\W$", "", gen.clean.pca$PC1.cut), ","),
#                                 function(x)sum(as.numeric(x))/2)
gen.clean.pca$PC2.round <- round(gen.clean.pca$PC2, 3)
# gen.clean.pca$PC2.cut <- cut(gen.clean.pca$PC2, 50)
# gen.clean.pca$PC2.mid <- sapply(strsplit(gsub("^\\W|\\W$", "", gen.clean.pca$PC2.cut), ","),
#                                 function(x)sum(as.numeric(x))/2)
gen.clean.pca$PC3.round <- round(gen.clean.pca$PC3, 3)
# gen.clean.pca$PC3.cut <- cut(gen.clean.pca$PC3, 50)
# gen.clean.pca$PC3.mid <- sapply(strsplit(gsub("^\\W|\\W$", "", gen.clean.pca$PC3.cut), ","),
#                                 function(x)sum(as.numeric(x))/2)
dim(gen.clean.pca)

head(gen.clean.pca)

gen.simple.pca <- aggregate(UID ~ genus + species + PC1.round + PC2.round, data=gen.clean.pca, FUN=length)
dim(gen.simple.pca)

### Showing our example species in PCA space
oak.hulls <- pc.hulls_PC1_PC2[pc.hulls_PC1_PC2$species_name_acc %in% oak.examples$species_name_acc,]
oak.hulls$species_name_acc <- factor(oak.hulls$species_name_acc, levels=c(oaks.use, "MortonArb"))


# png(file.path(path.figs, "Fig3_PCA_ExampleOaks_PC1-PC2.png"), 
#     height=8, width=8.1, units="in", res=320)
# a <- ggplot(oak.examples[oak.examples$UID!="MORTONARB",], aes(x=PC1, y=PC2)) +
#       facet_wrap(~species_name_acc) +
#       stat_unique(data=gen.clean.pca[gen.clean.pca$genus=="Quercus" & !gen.clean.pca$UID=="MORTONARB",c("PC1", "PC2")], size=0.1, color="gray80", alpha=0.2) + #gray points in background
#       #geom_point(size=0.5, color="dodgerblue2") + #blue points
#       geom_polygon(data=oak.hulls, aes(x=PC1, y=PC2), color="dodgerblue2", fill="dodgerblue2", alpha=0.25) + #blue figure
#       geom_point(data=oak.examples[oak.examples$UID=="MORTONARB",c("PC1", "PC2")], color="orange2", size=2.5) + #morton arb orange point
#       theme(panel.background=element_rect(fill=NA),
#             panel.grid = element_blank(),
#             strip.background = element_blank(),
#             strip.text=element_text(size=rel(1.5), face="bold.italic"), 
#             axis.title=element_text(size=rel(1.25), face="bold"),
#             legend.key = element_blank())

## implement shiny on how to change the species
   ## Only doing Quercus for now
   ## using gen.clean.pca: might have some species that have no points after cleaning to "gen.simple.pca
unique.genus <- c("Malus", "Quercus", "Tilia", "Ulmus")
# unique.species.quercus <- unique(gen.clean.pca$species[gen.clean.pca$genus=="Quercus"])
# unique.species.malus <- unique(gen.simple.pca$species[gen.simple.pca$genus=="Malus"])
# unique.species.tilia <- unique(gen.simple.pca$species[gen.simple.pca$genus=="Tilia"])
# unique.species.ulmus <- unique(gen.simple.pca$species[gen.simple.pca$genus=="Ulmus"])


ui <- fluidPage(
   # Some custom CSS for a smaller font for preformatted text
   tags$head(
      tags$style(HTML("
      pre, table.table {
        font-size: smaller;
      }
    "))),
   
   #selectInput("genus", "Choose a genus:", list(genus=as.list(paste(sort(unique.genus))))), 
   #selectInput("Species", "Choose a Species:", list(Phenos=as.list(paste(unique(gen.clean.pca$species[gen.clean.pca$genus==input$genus]))))), 
      #will likely be an error here
   verbatimTextOutput("hover_info"),			    
   mainPanel(plotlyOutput("plot1", width = 850, height = 750),
   ))

#Need to change the hulls based on the species I choose: put the longer version of making the hulls above
   #NOT LOADING
server <- function(input, output) {
   
   # oak.examples <- gen.clean.pca[gen.clean.pca$species_name_acc %in% c(input$Species,"MortonArb") & gen.clean.pca$genus=="Quercus",] #changed to where you can select any species of Quercus
   # 
   # oaks.use <- c(input$Species) #changes the species based on the 
   # 
   # oak.examples$species_name_acc <- factor(oak.examples$species_name_acc, levels=c(oaks.use, "MortonArb"))
   # 
   # ### Creating midpoints to bin occurrence in PCA space
   # gen.clean.pca$PC1.round <- round(gen.clean.pca$PC1, 3)
   # gen.clean.pca$PC2.round <- round(gen.clean.pca$PC2, 3)
   # gen.clean.pca$PC3.round <- round(gen.clean.pca$PC3, 3)
   # 
   # gen.simple.pca <- aggregate(UID ~ genus + species + PC1.round + PC2.round, data=gen.clean.pca, FUN=length)
   # dim(gen.simple.pca)
   # 
   # ### Showing our example species in PCA space
   # oak.hulls <- pc.hulls_PC1_PC2[pc.hulls_PC1_PC2$species_name_acc %in% oak.examples$species_name_acc,]
   # oak.hulls$species_name_acc <- factor(oak.hulls$species_name_acc, levels=c(oaks.use, "MortonArb"))
      
   
   output$plot1 <- renderPlot({
      ggplot(oak.examples[oak.examples$UID!="MORTONARB",], aes(x=PC1, y=PC2)) +
         facet_wrap(~species_name_acc) +
         # stat_unique(data=gen.clean.pca[gen.clean.pca$genus=="Quercus" & !gen.clean.pca$UID=="MORTONARB",c("PC1", "PC2")], size=0.1, color="gray80", alpha=0.2) + #gray points in background
         #geom_point(size=0.5, color="dodgerblue2") + #blue points
         #geom_polygon(data=oak.hulls, aes(x=PC1, y=PC2), color="dodgerblue2", fill="dodgerblue2", alpha=0.25) + #blue figure
         geom_point(data=oak.examples[oak.examples$UID=="MORTONARB",c("PC1", "PC2")], color="orange2", size=2.5) + #morton arb orange point
         theme(panel.background=element_rect(fill=NA),
               panel.grid = element_blank(),
               strip.background = element_blank(),
               strip.text=element_text(size=rel(1.5), face="bold.italic"), 
               axis.title=element_text(size=rel(1.25), face="bold"),
               legend.key = element_blank())
   })
}

shinyApp(ui, server)



#Basic Graph Example

gen.clean.pca

ui <- fluidPage(
   titlePanel("Data"),
   #selectInput("gear", "Select gear", choices = mtcars$gear, selected=4, multiple=TRUE),
   # Slider inputs work with numeric data, not categorical data
   #selectInput("age", "Select Age", choices = exampledata$Ageband, selected="35 to 39", multiple=TRUE),
   plotOutput("plot")
)

server <- function(input, output, session) {
   # data <- reactive({
   #    mtcars %>%
   #       filter(
   #          gear %in% input$gear,
   #          #Ageband %in% input$age
   #       )
   # })
   
   output$plot <- renderPlot({
      #req(input$gear)
      
      #data() %>%
      ggplot(
         #oak.examples[oak.examples[oak.examples$UID!="MORTONARB",], #having problems when data() part added, similar to other ggplot
                aes(x=PC1, y=PC2)
                ) + 
         #facet_wrap(~species_name_acc) +
         geom_point(data=oak.examples[oak.examples$UID=="MORTONARB"], color="orange2", size=2.5) +
         theme(panel.background=element_rect(fill=NA),
               panel.grid = element_blank(),
               strip.background = element_blank(),
               strip.text=element_text(size=rel(1.5), face="bold.italic"), 
               axis.title=element_text(size=rel(1.25), face="bold"),
               legend.key = element_blank())
      
      # data() %>% 
      #    ggplot(aes(fill=City, y=count, x=Ageband)) + 
      #    geom_bar(position="dodge", stat="identity")
   })
}

shinyApp(ui = ui, server = server)





# aa <- ggplotly(a, tooltip = "none") #takes twice as long
# aa %>% add_markers(x = c(oak.examples$PC1[oak.examples$UID!="MORTONARB"]), y = c(oak.examples$PC2[oak.examples$UID!="MORTONARB"]), color = I("dodgerblue2"), text = c(1:12), symbol = I('circle'), marker = list(size = c(1:12)))
#  
# plot_ly(data=greenh, x=~Row, y= ~Plant) %>% 
#   add_markers(showlegend = FALSE, color = I("green"), hoverinfo = "none") %>% 
#   add_markers(data=df, x=~row, y= ~plant, showlegend = TRUE, color = ~ Wtot, size = ~ Wtot)
 
dev.off()

