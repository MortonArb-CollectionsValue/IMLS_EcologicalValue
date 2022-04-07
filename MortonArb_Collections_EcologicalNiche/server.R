# https://shiny.rstudio.com/articles/shinyapps.html?_ga=2.159806585.535201199.1597241310-391903967.1597085012
library(shiny)
library(ggplot2)
library(stringr)
library(shinyWidgets)

gen.simple.pca <- read.csv("data/PCA_Points.csv")
pc.hulls_PC1_PC2 <- read.csv("data/PCA_Hulls.csv")
gen.load <- read.csv("data/PCA_loadings.csv")
gen.load$env.var <- as.factor(gen.load$env.var)
gen.load$env.var <- factor(gen.load$env.var, levels=c("tmax.ann.max", "tmax.max.sd", "tmin.ann.min", "tmin.min.sd", "ppt.ann.mean", "ppt.min.min", "vpd.ann.max", "vpd.max.sd", "srad.ann.max", "srad.ann.sd", "soil.ann.max", "soil.max.sd", "T.SILT", "T.CLAY", "AWC_VALUE", "T.OC", "T.PH.H2O", "T.ECE", "T.CEC.SOIL", "T.CACO3"))


function(input, output) {
  output$select_Species <- renderUI({
    spp.avail <- unique(paste(gen.simple.pca$species[gen.simple.pca$genus==input$genus]))
    pickerInput('species','Choose a species: ', choices = c(sort(spp.avail)), options = list(`actions-box` = TRUE, 'live-search' = TRUE), multiple = T)
    
  })
  
  output$scatterPlot <- renderPlot({
    tree.hulls <- pc.hulls_PC1_PC2[pc.hulls_PC1_PC2$species %in% 
                                     gen.simple.pca$species[gen.simple.pca$species %in% c(input$species, "MortonArb") & gen.simple.pca$genus==input$genus],]
    
    # loads.graph <- ggplot() + 
    
    plot.base <- ggplot() +
      stat_unique(data=gen.simple.pca[gen.simple.pca$genus==input$genus & !gen.simple.pca$UID=="MORTONARB",], aes(x=PC1.round, y=PC2.round), size=0.1, color="gray80", alpha=0.2) + #gray points in background
      geom_point(data= gen.simple.pca[gen.simple.pca$genus==input$genus & gen.simple.pca$species==input$species & !gen.simple.pca$UID=="MORTONARB", ], aes(x=PC1.round, y=PC2.round), alpha=0.5, size=1.5, color="dodgerblue2") +  #blue points
      geom_polygon(data=tree.hulls, aes(x=PC1, y=PC2, group=species), color="dodgerblue2", fill="dodgerblue2", alpha=0.25) + #blue figure
      #Idea: make polygons interactive
      geom_point(data=gen.simple.pca[gen.simple.pca$genus==input$genus & gen.simple.pca$species=="MortonArb",], aes(x=PC1.round, y=PC2.round), color="orange2", size=5) + #morton arb orange point
      # Adding the loadings
      labs(x="PC 1 Values", y="PC 2 Values") +
      theme(panel.background=element_rect(fill=NA),
            panel.grid = element_blank(),
            strip.background = element_blank(),
            strip.text=element_text(size=rel(3), face="bold.italic"),
            axis.title=element_text(size=rel(3), face="bold"),
            legend.key = element_blank())
    
    if(length(input$envars)>0){
      plot.base <- plot.base + geom_segment(data=gen.load[gen.load$genus==input$genus & gen.load$env.var %in% input$envars,], aes(x=0, y=0, xend=2*xend, yend=2*yend), arrow=arrow(length=unit(1/2, "picas")), color="#117733") + #eigenvectors
        geom_text(data=gen.load[gen.load$genus==input$genus & gen.load$env.var %in% input$envars,], aes(x=labx*2, y=laby*2, label=env.var), color="#117733", size=3, fontface="bold")
    }
    
    plot.base
    
  })
}