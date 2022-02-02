# https://shiny.rstudio.com/articles/shinyapps.html?_ga=2.159806585.535201199.1597241310-391903967.1597085012
library(shiny)
library(ggplot2)
library(stringr)
library(shinyWidgets)

gen.simple.pca <- read.csv("data/PCA_Points.csv")
pc.hulls_PC1_PC2 <- read.csv("data/PCA_Hulls.csv")

function(input, output) {
  output$select_Species <- renderUI({
    spp.avail <- unique(paste(gen.simple.pca$species[gen.simple.pca$genus==input$genus]))
    pickerInput('species','Choose a species: ', choices = c(sort(spp.avail)), options = list(`actions-box` = TRUE, 'live-search' = TRUE), multiple = T)
    
  })
  
  output$scatterPlot <- renderPlot({
    tree.hulls <- pc.hulls_PC1_PC2[pc.hulls_PC1_PC2$species %in% 
                                     gen.simple.pca$species[gen.simple.pca$species %in% c(input$species, "MortonArb") & gen.simple.pca$genus==input$genus],]
    
    ggplot() +
      stat_unique(data=gen.simple.pca[gen.simple.pca$genus==input$genus & !gen.simple.pca$UID=="MORTONARB",], aes(x=PC1.round, y=PC2.round), size=0.1, color="gray80", alpha=0.2) + #gray points in background
      geom_point(data= gen.simple.pca[gen.simple.pca$genus==input$genus & gen.simple.pca$species==input$species & !gen.simple.pca$UID=="MORTONARB", ], aes(x=PC1.round, y=PC2.round), alpha=0.5, size=1.5, color="dodgerblue2") +  #blue points
      geom_polygon(data=tree.hulls, aes(x=PC1, y=PC2, group=species), color="dodgerblue2", fill="dodgerblue2", alpha=0.25) + #blue figure
      #Idea: make polygons interactive
      geom_point(data=gen.simple.pca[gen.simple.pca$genus==input$genus & gen.simple.pca$species=="MortonArb",], aes(x=PC1.round, y=PC2.round), color="orange2", size=5) + #morton arb orange point
      labs(x="PC 1 Values", y="PC 2 Values") +
      theme(panel.background=element_rect(fill=NA),
            panel.grid = element_blank(),
            strip.background = element_blank(),
            strip.text=element_text(size=rel(3), face="bold.italic"),
            axis.title=element_text(size=rel(3), face="bold"),
            legend.key = element_blank())
  })
}