# Christy playing around with PCA output to see how we can synthesize
library(ggplot2); library(ggrepel)
library(sp); library(plotly)

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
gen.clean.pca$PC1.cut <- cut(gen.clean.pca$PC1, 50)
gen.clean.pca$PC1.mid <- sapply(strsplit(gsub("^\\W|\\W$", "", gen.clean.pca$PC1.cut), ","),
                                function(x)sum(as.numeric(x))/2)
gen.clean.pca$PC2.cut <- cut(gen.clean.pca$PC2, 50)
gen.clean.pca$PC2.mid <- sapply(strsplit(gsub("^\\W|\\W$", "", gen.clean.pca$PC2.cut), ","),
                                function(x)sum(as.numeric(x))/2)
gen.clean.pca$PC3.cut <- cut(gen.clean.pca$PC3, 50)
gen.clean.pca$PC3.mid <- sapply(strsplit(gsub("^\\W|\\W$", "", gen.clean.pca$PC3.cut), ","),
                                function(x)sum(as.numeric(x))/2)
dim(gen.clean.pca)


### Showing our example species in PCA space
oak.hulls <- pc.hulls_PC1_PC2[pc.hulls_PC1_PC2$species_name_acc %in% oak.examples$species_name_acc,]
oak.hulls$species_name_acc <- factor(oak.hulls$species_name_acc, levels=c(oaks.use, "MortonArb"))


# png(file.path(path.figs, "Fig3_PCA_ExampleOaks_PC1-PC2.png"), 
#     height=8, width=8.1, units="in", res=320)
a <- ggplot(oak.examples[oak.examples$UID!="MORTONARB",], aes(x=PC1, y=PC2)) +
      facet_wrap(~species_name_acc) +
      geom_point(data=gen.clean.pca[gen.clean.pca$genus=="Quercus" & !gen.clean.pca$UID=="MORTONARB",c("PC1", "PC2")], size=0.1, color="gray80", alpha=0.2) + #gray points in background
      #geom_point(size=0.5, color="dodgerblue2") + #blue points
      geom_polygon(data=oak.hulls, aes(x=PC1, y=PC2), color="dodgerblue2", fill="dodgerblue2", alpha=0.25) + #blue figure
      geom_point(data=oak.examples[oak.examples$UID=="MORTONARB",c("PC1", "PC2")], color="orange2", size=2.5) + #morton arb orange point
      theme(panel.background=element_rect(fill=NA),
            panel.grid = element_blank(),
            strip.background = element_blank(),
            strip.text=element_text(size=rel(1.5), face="bold.italic"), 
            axis.title=element_text(size=rel(1.25), face="bold"),
            legend.key = element_blank())
aa <- ggplotly(a, tooltip = "none") #takes twice as long
aa %>% add_markers(x = c(oak.examples$PC1[oak.examples$UID!="MORTONARB"]), y = c(oak.examples$PC2[oak.examples$UID!="MORTONARB"]), color = I("dodgerblue2"), text = c(1:12), symbol = I('circle'), marker = list(size = c(1:12)))
 
plot_ly(data=greenh, x=~Row, y= ~Plant) %>% 
  add_markers(showlegend = FALSE, color = I("green"), hoverinfo = "none") %>% 
  add_markers(data=df, x=~row, y= ~plant, showlegend = TRUE, color = ~ Wtot, size = ~ Wtot)
 
dev.off()

