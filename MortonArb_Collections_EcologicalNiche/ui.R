# https://shiny.rstudio.com/articles/shinyapps.html?_ga=2.159806585.535201199.1597241310-391903967.1597085012
library(shiny)
library(ggplot2)
library(stringr)
library(shinyWidgets)

gen.simple.pca <- read.csv("data/PCA_Points.csv")
pc.hulls_PC1_PC2 <- read.csv("data/PCA_Hulls.csv")

fluidPage(
  titlePanel("Species distribution in genus environmental PCA space"),
  sidebarPanel(
    selectInput("genus", "Select a genus:", choices=c(unique(gen.simple.pca$genus)))),
  uiOutput("select_Species"),
  mainPanel(plotOutput("scatterPlot"))
)
