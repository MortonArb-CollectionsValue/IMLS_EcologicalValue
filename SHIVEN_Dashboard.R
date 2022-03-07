# Christy playing around with PCA output to see how we can synthesize
library(ggplot2)
library(dplyr)
library(shiny)
library(shinydashboard)
library(shinyWidgets)

# library(plotly); library(dplyr)
# library(ggrepel); library(ggmap)
# library(rgdal); library(raster)
# library(rgeos); library(xts)
# library(tigris); library(car)
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

# Getting rid of NAs just for sanity
gen.clean.pca <- gen.clean.pca[!is.na(gen.clean.pca$PC1),]

### Creating midpoints to bin occurrence in PCA space
gen.clean.pca$PC1.round <- round(gen.clean.pca$PC1, 3)
# gen.clean.pca$PC1.cut <- cut(gen.clean.pca$PC1, 50)
# gen.clean.pca$PC1.mid <- sapply(strsplit(gsub("^\\W|\\W$", "", gen.clean.pca$PC1.cut), ","),
#                                 function(x)sum(as.numeric(x))/2)
gen.clean.pca$PC2.round <- round(gen.clean.pca$PC2, 3)
gen.clean.pca$PC3.round <- round(gen.clean.pca$PC3, 3)
dim(gen.clean.pca)
head(gen.clean.pca)

gen.simple.pca <- aggregate(UID ~ genus + species + species_name_acc + PC1.round + PC2.round, data=gen.clean.pca, FUN=length)
dim(gen.simple.pca)

# Creating a map of two example species for the poster
### --------------
map.world <- map_data("world")


##Trying to add Eigenvectors to Graph
#Sample Eigenvectors
gen.pcas$Quercus$rotation

#Scaling them
gen.load <- data.frame()
pc.expvar <- data.frame()
for(i in 1:length(gen.pcas)){
   df.tmp <- data.frame(genus=names(gen.pcas)[i], 
                        env.var=row.names(gen.pcas[[i]]$rotation), 
                        gen.pcas[[i]]$rotation[,1:2])
   df.tmp$labx <- df.tmp$PC1*(max(gen.pcas[[i]]$x[,1])+0.5)
   df.tmp$laby <- df.tmp$PC2*(max(gen.pcas[[i]]$x[,2])+0.5)
   df.tmp$xend <- df.tmp$PC1*(max(gen.pcas[[i]]$x[,1])-0.5)
   df.tmp$yend <- df.tmp$PC2*(max(gen.pcas[[i]]$x[,2]))
   
   # Getting the top predictors
   pc.sum <- summary(gen.pcas[[i]])$importance
   
   sum.tmp <- data.frame(genus=names(gen.pcas)[i], PC1=pc.sum[2,1], PC2=pc.sum[2,2], Pcum1.2=pc.sum[3,2])
   df.tmp$dist <- sqrt((df.tmp$PC1*pc.sum[2,1]/pc.sum[3,2])^2 + (df.tmp$PC2*pc.sum[2,2]/pc.sum[3,2])^2)# How long the combined arrow is
   df.tmp$rank <- order(df.tmp$dist)
   
   # Formatting the summary
   
   # Put it together
   gen.load <- rbind(gen.load, df.tmp)
   pc.expvar <- rbind(pc.expvar, sum.tmp)
}
summary(gen.load)
pc.expvar

# Adding some grouping classification to env vars
vars.soil <- c("T.SILT", "T.CLAY", "AWC_VALUE", "T.OC", "T.PH.H2O", "T.ECE", "T.CEC.SOIL", "T.CACO3")
gen.load$var.type <- ifelse(gen.load$env.var %in% c(vars.soil), "Soil", "Climate")
# gen.load

# gen.load$env.var[!gen.load$env.var %in% c("tmax.ann.amx", "tmax.max.sd", "tmin.ann.min", "tmin.min.sd", "ppt.ann.mean", "ppt.min.min", "vpd.ann.max", "vpd.max.sd", "srad.ann.max", "srad.ann.sd", "soil.ann.max", "soil.max.sd", "T.SILT", "T.CLAY", "AWC_VALUE", "T.OC", "T.PH.H2O", "T.ECE", "T.CEC.SOIL", "T.CACO3")]
gen.load$env.var <- factor(gen.load$env.var, levels=c("tmax.ann.max", "tmax.max.sd", "tmin.ann.min", "tmin.min.sd", "ppt.ann.mean", "ppt.min.min", "vpd.ann.max", "vpd.max.sd", "srad.ann.max", "srad.ann.sd", "soil.ann.max", "soil.max.sd", "T.SILT", "T.CLAY", "AWC_VALUE", "T.OC", "T.PH.H2O", "T.ECE", "T.CEC.SOIL", "T.CACO3"))

summary(gen.load[,c("env.var", "genus", "PC1", "PC2", "rank")])

#to choose envir.vars
envir.vars <- c("decimalLatitude","decimalLongitude","ppt.ann.mean","ppt.min.min","soil.ann.max","soil.max.sd","srad.ann.max",
                "srad.ann.sd","tmax.ann.max","tmax.max.sd","tmin.ann.min","tmin.min.sd",
                "vpd.ann.max","vpd.max.sd","T.SILT","T.CLAY","T.OC","T.PH.H2O","T.ECE","T.CACO3")

#Filters both genus & Species at the same time instead of individually
ui <- shinyUI(fluidPage(
   titlePanel("PC Values Across Species"),
   sidebarPanel(
   selectInput("genus", "Select a genus:", choices=c(unique(gen.simple.pca$genus)))),
   selectInput("envir.vars", "Select envir.vars:", choices=envir.vars, multiple = T),
   uiOutput("select_Species"),
   mainPanel(plotOutput("scatterPlot")),
   verbatimTextOutput("info")
)
)

server <- shinyServer(function(input, output) {
   output$select_Species <- renderUI({
      spp.avail <- unique(paste(gen.simple.pca$species[gen.simple.pca$genus==input$genus]))
      pickerInput('species','Choose a species: ', choices = c(sort(spp.avail)), options = list(`actions-box` = TRUE, 'live-search' = TRUE), multiple = T)
      
   })
   
   output$scatterPlot <- renderPlot({
      tree.hulls <- pc.hulls_PC1_PC2[pc.hulls_PC1_PC2$species %in% 
                                        gen.simple.pca$species[gen.simple.pca$species %in% c(input$species, "MortonArb") & gen.simple.pca$genus==input$genus],]
      
      ggplot() +
         stat_unique(data=gen.simple.pca[gen.simple.pca$genus==input$genus & !gen.simple.pca$UID=="MORTONARB",], aes(x=PC1.round, y=PC2.round), size=0.1, color="#888888", alpha=0.2) + #gray points in background
         geom_point(data= gen.simple.pca[gen.simple.pca$genus==input$genus & gen.simple.pca$species==input$species & !gen.simple.pca$UID=="MORTONARB", ], aes(x=PC1.round, y=PC2.round), size=1, color="#88CCEE") +  #blue points
         geom_polygon(data=tree.hulls, aes(x=PC1, y=PC2, group=species), color="#88CCEE", fill="#88CCEE", alpha=0.25) + #blue figure
            #Idea: make polygons interactive
         #geom_segment(data=gen.load[gen.load$genus==input$genus & gen.load$rank<=3 & gen.load$var.type=="Soil",], aes(x=0, y=0, xend=2*xend, yend=2*yend), arrow=arrow(length=unit(1/2, "picas")), color="#882255") + #Soil eigenvectors
         #geom_text(data=gen.load[gen.load$genus==input$genus & gen.load$rank<=3 & gen.load$var.type=="Soil",], aes(x=labx*2, y=laby*2, label=env.var), color="#882255", size=3, fontface="bold") +
         geom_segment(data=gen.load[gen.load$genus==input$genus & gen.load$rank<=3,], aes(x=0, y=0, xend=2*xend, yend=2*yend), arrow=arrow(length=unit(1/2, "picas")), color="#117733") + #eigenvectors
         geom_text(data=gen.load[gen.load$genus==input$genus & gen.load$rank<=3,], aes(x=labx*2, y=laby*2, label=env.var), color="#117733", size=3, fontface="bold") +
         geom_point(data=gen.simple.pca[gen.simple.pca$genus==input$genus & gen.simple.pca$species=="MortonArb",], aes(x=PC1.round, y=PC2.round), color="#CC6677", size=2.5) + #morton arb orange point
         labs(x="PC 1 Values", y="PC 2 Values") +
         theme(panel.background=element_rect(fill=NA),
               panel.grid = element_blank(),
               strip.background = element_blank(),
               strip.text=element_text(size=rel(1.5), face="bold.italic"),
               axis.title=element_text(size=rel(1.25), face="bold"),
               legend.title = element_blank())
   })
   
   envir.range.titles <- paste(colnames(gen.clean.pca)[7:18], "Range", sep = " ") #creating names for row names of textbox regarding environmental variables
      #should this be inside renderPrint statement?
   
   
   output$info <- renderPrint({
      #dat.subs <- dat.pheno$Date.Observed>=min(input$DateRange) & dat.pheno$Date.Observed<=max(input$DateRange) & dat.pheno$collection==input$Collection & dat.pheno$pheno.label==input$Phenophase & !is.na(dat.pheno$status)
      input.gen.clean.pca <- gen.clean.pca[gen.clean.pca$genus==input$genus & 
                                              gen.clean.pca$species %in% input$species & 
                                              !gen.clean.pca$UID=="MORTONARB",]
      # total.pts <- nrow(input.gen.clean.pca)
      # decimalLat.range <- range(input.gen.clean.pca$decimalLatitude)
      # decimalLong.range <- range(input.gen.clean.pca$decimalLongitude)
      # 
      # txthere <- data.frame(total.pts,
      #                       decimalLat.range,
      #                       decimalLong.range
      #                      )
      
      # stats.spp <- aggregate(input.gen.clean.pca[,envir.vars], by=input.gen.clean.pca[,c("genus", "species")], FUN=min)
      # stats.spp <- data.frame(stat="min", aggregate(input.gen.clean.pca[,envir.vars], by=input.gen.clean.pca[,c("genus", "species")], FUN=min))
      # stats.spp <- rbind(stats.spp, data.frame(stat="max", aggregate(input.gen.clean.pca[,envir.vars], by=input.gen.clean.pca[,c("genus", "species")], FUN=max)))
      # stat.spp <- stats.spp[order(stats.spp$species),]
      
      #stats.spp <- data.frame(stat="n points", aggregate(input.gen.clean.pca[,envir.vars], by=input.gen.clean.pca[,c("genus", "species")], FUN=length))
      #stats.spp <- data.frame()
      stats.spp <- data.frame(stat="min", aggregate(round(input.gen.clean.pca[,input$envir.vars], digits = 3), by=input.gen.clean.pca[,c("genus", "species")], FUN=min))
      stats.spp <- rbind(stats.spp, data.frame(stat="max", aggregate(round(input.gen.clean.pca[,input$envir.vars], digits = 3), by=input.gen.clean.pca[,c("genus", "species")], FUN=max)))
      names(stats.spp)[names(stats.spp) == "x"] <- input$envir.vars #changing name of column from x to envi.var
      #stats.spp$n.points <- aggregate(input.gen.clean.pca[,envir.vars], by=input.gen.clean.pca[,c("genus", "species")], FUN=length)
      #stats.spp <- cbind(n.points = nrow(input.gen.clean.pca), stats.spp)
      stats.spp <- cbind(aggregate(input.gen.clean.pca$species, by = list(input.gen.clean.pca$species), FUN = length), stats.spp)
      stats.spp$Group.1 <- NULL #getting rid of repeat naming of species that comes with aggregate function
      names(stats.spp)[names(stats.spp) == "x"] <- "n.points" #changing name of column from x to n.points
      stats.spp <- stats.spp[order(stats.spp$species),]
      
      # for (i in 1:length(envir.vars)) {
      #    txthere$new_col <- range(input.gen.clean.pca[,envir.vars[i]])
      #    names(txthere)[3+i] <- paste0(envir.vars[i], ".range")
      # }
         #nearPoints(gen.clean.pca[gen.clean.pca$genus==input$genus & gen.clean.pca$species==input$species, colnames(gen.clean.pca)[5:18]], 
                            #input$plot_click, 
                            #threshold =10, maxpoints=5)
      #txthere <- t(txthere) #flips rows and columns
      #row.names(txthere) <- c("Number of Points", "Latitude Range", "Longitude Range", envir.range.titles)
      stats.spp
      # names(txthere) <- "observation"
   })
   
})

shinyApp(ui, server)

#Pre-shiny dfs

#oak.examples <- gen.simple.pca[gen.simple.pca$species %in% c("angustifolia", "asiatica","MortonArb") & gen.simple.pca$genus=="Malus",] #changed to only Quercus pontica
#oaks.use <- c("angustifolia", "asiatica") #changed it so only has Malus angustifolia, Malus asiatica
#oak.examples$species <- factor(oak.examples$species, levels=c(c("angustifolia", "asiatica"), "MortonArb"))

### Showing our example species in PCA space
#oak.hulls <- pc.hulls_PC1_PC2[pc.hulls_PC1_PC2$species %in% oak.examples$species,]
# oak.hulls <- pc.hulls_PC1_PC2[pc.hulls_PC1_PC2$species %in% 
#                                  gen.simple.pca$species[gen.simple.pca$species %in% c("angustifolia", "asiatica","MortonArb") & gen.simple.pca$genus=="Malus"],]
#oak.hulls$species <- factor(oak.hulls$species, levels=c(c("angustifolia", "asiatica"), "MortonArb"))


# aa <- ggplotly(a, tooltip = "none") #takes twice as long
# aa %>% add_markers(x = c(oak.examples$PC1[oak.examples$UID!="MORTONARB"]), y = c(oak.examples$PC2[oak.examples$UID!="MORTONARB"]), color = I("dodgerblue2"), text = c(1:12), symbol = I('circle'), marker = list(size = c(1:12)))
#  
# plot_ly(data=greenh, x=~Row, y= ~Plant) %>% 
#   add_markers(showlegend = FALSE, color = I("green"), hoverinfo = "none") %>% 
#   add_markers(data=df, x=~row, y= ~plant, showlegend = TRUE, color = ~ Wtot, size = ~ Wtot)