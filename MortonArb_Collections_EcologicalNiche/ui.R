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

fluidPage(
  titlePanel("IMLS Collections Value: Environmental/Niche Space"),
  sidebarPanel(
    selectInput("genus", "Select a genus:", choices=c(unique(gen.simple.pca$genus))),
  uiOutput("select_Species"),
  pickerInput("envars", "Select a environmental variables to graph:", choices=c(paste(unique(gen.load$env.var))), options = list(`actions-box` = TRUE, 'live-search' = TRUE), multiple = T)),
  mainPanel(
    plotOutput("scatterPlot"), 
    verbatimTextOutput("info"),
    plotOutput("overlapPlot"),
    plotOutput("distancePlot"))
)
