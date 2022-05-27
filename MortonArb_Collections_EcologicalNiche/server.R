# https://shiny.rstudio.com/articles/shinyapps.html?_ga=2.159806585.535201199.1597241310-391903967.1597085012
library(shiny)
library(ggplot2)
library(stringr)
library(shinyWidgets)

gen.simple.pca <- read.csv("data/PCA_Points.csv")
hulls.df <- read.csv("data/PCA_Hulls.csv")
# load("data/PCA_Hulls_Coords.RData")
gen.load <- read.csv("data/PCA_loadings.csv")
gen.load$env.var <- as.factor(gen.load$env.var)
gen.load$env.var <- factor(gen.load$env.var, levels=c("tmax.ann.max", "tmax.max.sd", "tmin.ann.min", "tmin.min.sd", "ppt.ann.mean", "ppt.min.min", "vpd.ann.max", "vpd.max.sd", "srad.ann.max", "srad.ann.sd", "soil.ann.max", "soil.max.sd", "T.SILT", "T.CLAY", "AWC_VALUE", "T.OC", "T.PH.H2O", "T.ECE", "T.CEC.SOIL", "T.CACO3"))
load("data/PCA_OverlapStats.RData")


function(input, output) {
  output$select_Species <- renderUI({
    spp.avail <- unique(paste(gen.simple.pca$species[gen.simple.pca$genus==input$genus & gen.simple.pca$species!="MortonArb"]))
    pickerInput('species','Choose a species: ', choices = c(sort(spp.avail)), options = list(`actions-box` = TRUE, 'live-search' = TRUE), multiple = T)
    
  })
  
  output$scatterPlot <- renderPlot({
    tree.hulls <- hulls.df[hulls.df$species %in% input$species & hulls.df$genus==input$genus,]
    
    # tree.hulls <- pca.hulls[[input$species]]
    # loads.graph <- ggplot() + 
    
    plot.base <- ggplot() +
      stat_unique(data=gen.simple.pca[gen.simple.pca$genus==input$genus & !gen.simple.pca$UID=="MORTONARB",], aes(x=PC1.round, y=PC2.round), size=0.1, color="gray80", alpha=0.2) + #gray points in background
      geom_point(data= gen.simple.pca[gen.simple.pca$genus==input$genus & gen.simple.pca$species==input$species & !gen.simple.pca$UID=="MORTONARB", ], aes(x=PC1.round, y=PC2.round), alpha=0.5, size=1.5, color="dodgerblue2") +  #blue points
      geom_polygon(data=tree.hulls, aes(x=PC1, y=PC2, group=species), color="dodgerblue2", fill="dodgerblue2", alpha=0.25) + #blue figure
      #Idea: make polygons interactive
      geom_point(data=gen.simple.pca[gen.simple.pca$genus==input$genus & gen.simple.pca$species=="MortonArb",], aes(x=PC1.round, y=PC2.round), color="orange2", size=5) + #morton arb orange point
      # Adding the loadings
      labs(title="Niche Space (Genus PCA)", x="PC 1 Values", y="PC 2 Values") +
      theme(panel.background=element_rect(fill=NA),
            panel.grid = element_blank(),
            strip.background = element_blank(),
            strip.text=element_text(size=rel(3), face="bold.italic"),
            axis.title=element_text(size=rel(3), face="bold"),
            plot.title=element_text(size=rel(3), face="bold"),
            legend.key = element_blank())
    
    if(length(input$envars)>0){
      plot.base <- plot.base + geom_segment(data=gen.load[gen.load$genus==input$genus & gen.load$env.var %in% input$envars,], aes(x=0, y=0, xend=2*xend, yend=2*yend), arrow=arrow(length=unit(1/2, "picas")), color="#117733") + #eigenvectors
        geom_text(data=gen.load[gen.load$genus==input$genus & gen.load$env.var %in% input$envars,], aes(x=labx*2, y=laby*2, label=env.var), color="#117733", size=3, fontface="bold")
    }
    
    plot.base
    
  })
  
  output$overlapPlot <- renderPlot({
    stats.gen <- gen.stats[[input$genus]]
    
    plot.over <- ggplot(data=stats.gen) +
      geom_histogram(aes(x=p.over.mean)) +
      geom_vline(data=stats.gen[stats.gen$species %in% paste(input$genus, input$species),], aes(xintercept=p.over.mean, color=hull.TMA), size=2.5) +
      scale_y_continuous(expand=c(0,0)) +
      # geom_text(x=min(stats.gen$p.over.mean, na.rm=T), y=0.5, label="More Unique", color="dodgerblue1", hjust=0) +
      scale_x_continuous(limits=c(0,1), breaks=seq(0, 1, by=0.25), labels=c("0\n(unique)", 0.25, 0.5, 0.75, ("1\n(not unique)"))) +
      scale_color_manual(name="TMA in Hull?", values=c("TRUE" = "dodgerblue3", "FALSE"="firebrick1")) +
      labs(title="Species Uniqueness", x="Mean Overlap", y="# Species") +
      theme(panel.background=element_rect(fill=NA),
            panel.grid = element_blank(),
            axis.line = element_line(color="black", size=0.5),
            axis.text = element_text(color="black", size=rel(2)),
            strip.background = element_blank(),
            strip.text=element_text(size=rel(3), face="bold.italic"),
            axis.title=element_text(size=rel(3), face="bold"),
            plot.title=element_text(size=rel(3), face="bold"),
            legend.key = element_blank(),
            plot.margin = unit(c(1,3,1,1), "lines"))
    
    
    plot.over
    
  })

  output$distancePlot <- renderPlot({
    stats.gen <- gen.stats[[input$genus]]
    
    plot.dist <- ggplot(data=stats.gen) +
      geom_histogram(aes(x=dist.TMA)) +
      geom_vline(data=stats.gen[stats.gen$species %in% paste(input$genus, input$species),], aes(xintercept=dist.TMA, color=hull.TMA), size=2) +
      scale_y_continuous(expand=c(0,0)) +
      # geom_text(x=min(stats.gen$p.over.mean, na.rm=T), y=0.5, label="More Unique", color="dodgerblue1", hjust=0) +
      # scale_x_continuous(limits=c(0,1), breaks=seq(0, 1, by=0.25), labels=c("0\n(unique)", 0.25, 0.5, 0.75, ("1\n(not unique)"))) +
      scale_color_manual(name="TMA in Hull?", values=c("TRUE" = "dodgerblue3", "FALSE"="firebrick2")) +
      labs(title="Distance to The Morton Arboretum", x="Centroid Distance", y="# Species") +
      theme(panel.background=element_rect(fill=NA),
            panel.grid = element_blank(),
            axis.line = element_line(color="black", size=0.5),
            axis.text = element_text(color="black", size=rel(2)),
            strip.background = element_blank(),
            strip.text=element_text(size=rel(3), face="bold.italic"),
            axis.title=element_text(size=rel(3), face="bold"),
            plot.title=element_text(size=rel(3), face="bold"),
            legend.key = element_blank(),
            plot.margin = unit(c(1,3,1,1), "lines"))
    
    
    plot.dist
    
  })    
  
  output$info <- renderPrint({
    stats.spp <- gen.stats[[input$genus]][gen.stats[[input$genus]]$species %in% paste(input$genus, input$species, sep=" "),c("species", "hull.TMA", "dist.TMA", "area", "p.over.mean", "p.over.max")]
    stats.spp[,c("area", "p.over.mean", "p.over.max")] <- round(stats.spp[,c("area", "p.over.mean", "p.over.max")], 2)
    names(stats.spp) <- c("Species", "TMA in Hull?", "Centroid Distance", "Hull Area", "Mean Overlap (Prop.)", "Max Overlap (prop.)")
    stats.spp
  })
}
