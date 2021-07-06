####################################################################################################
## 4-4_Figures_PCA_Clouds_by_genus_species.R
####################################################################################################
####################################################################################################
rm(list=ls())
### Load packages
my.packages <- c('ggplot2', 'plyr', 'readr', 'dplyr', 'sf', 'tidyverse', 'ks', 'vegan', 'ggbiplot')
# install.packages (my.packages) #Turn on to install current versions
lapply(my.packages, require, character.only=TRUE)
  rm(my.packages)
####################################################################################################
####################################################################################################
### set paths/folders
  ## path to the shared Google Drive folder
  path.dat <- "/Volumes/GoogleDrive/Shared drives/IMLS MFA/Environmental Niche Value" ## path for Shannon
  # path.dat <- "D:/Data_IMLS_Ecological_Value"   ## path to data for Shiven D drive
  path.out <- file.path(path.dat, "Analysis/PrelimPCA/figures")
  # path for the folder for figure output
  path.figs <- file.path(path.dat, "figures")
####################################################################################################
####################################################################################################
# load PCA data for figures
load(file.path(path.dat, "Extracted Data", "PCA_output.RData"))
####################################################################################################
####################################################################################################
## load functions
source("0-X_Ecological_Value_functions.R")
####################################################################################################
####################################################################################################
## create PCA clouds for genera
  lapply(gen.ls, pcaCloudFigures, df.all=gen.clean[gen.clean.pca$absval=='in_gen_4',], 
            meta.traits=meta.traits, important.traits=important.traits, pc.incl1='PC1', pc.incl2='PC2', 
            pc.hulls=pc.hulls_PC1_PC2, exp.load=3, exp.score=-0.5, 
            calc.level='genus', spp.poly='convex hull')
## create PCA clouds for species
  lapply(gen.ls, pcaCloudFigures, df.all=gen.clean[gen.clean.pca$absval=='in_gen_4',], 
            meta.traits=meta.traits, important.traits=important.traits, pc.incl1='PC1', pc.incl2='PC2', 
            pc.hulls=pc.hulls_PC1_PC2, exp.load=3, exp.score=-0.5, 
            calc.level='species', spp.poly='both')
## create PCA clouds for species
  lapply(gen.ls, pcaCloudFigures, df.all=gen.clean[gen.clean.pca$absval=='in_gen_4',], 
            meta.traits=meta.traits, important.traits=important.traits, pc.incl1='PC1', pc.incl2='PC2', 
            pc.hulls=pc.hulls_PC1_PC2, exp.load=3, exp.score=-0.5, 
            calc.level='species', spp.poly='convex hull')
## create PCA clouds for species
  lapply(gen.ls, pcaCloudFigures, df.all=gen.clean[gen.clean.pca$absval=='in_gen_4',], 
            meta.traits=meta.traits, important.traits=important.traits, pc.incl1='PC1', pc.incl2='PC2', 
            pc.hulls=pc.hulls_PC1_PC2, exp.load=3, exp.score=-0.5, 
            calc.level='species', spp.poly='ellipse')

####################################################################################################
####################################################################################################
# ##  create convex hulls
# png("TEST_Ordination_Quercus.png", height=8, width=8, units="in", res=320)
# ggplot(data=dat.all, aes(x=PC1, y=PC2)) +
#   geom_point(aes(x=PC1, y=PC2), size=0.25, color="gray50")  +
#   geom_polygon(data=pc.hulls[pc.hulls$species_name_acc %in% c("Quercus alba"),], aes(fill=species_name_acc), alpha=0.25) +
#   geom_polygon(data=pc.hulls[pc.hulls$species_name_acc %in% c("Quercus boyntonii"),], aes(fill=species_name_acc), alpha=0.5) +
#   geom_point(data=dat.all[dat.all$species_name_acc %in% c("Quercus alba"),], aes(color=species_name_acc)) +
#   geom_point(data=dat.all[dat.all$species_name_acc=="Quercus boyntonii",], aes(color=species_name_acc), size=2) +
#   geom_point(data=dat.all[dat.all$species_name_acc=="MortonArb",], aes(color=species_name_acc), size=8) +
#   scale_color_manual(name="Species", values=c("MortonArb"="green4", "Quercus alba"="blue2", "Quercus boyntonii"="red2", "Quercus velutina"="cadetblue3")) +
#   scale_fill_manual(name="Species", values=c("MortonArb"="green4", "Quercus alba"="blue2", "Quercus boyntonii"="red2", "Quercus velutina"="cadetblue3"))
# dev.off()

####################################################################################################
####################################################################################################
# colorBlindBlack8  <- c("#000000", "#E69F00", "#56B4E9", "#009E73", 
#                        "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# pie(rep(1, 8), col = colorBlindBlack8)
####################################################################################################
####################################################################################################
# ## pcaCloudFigures: function to prep data and create PCA figures from a list of prcomp objects, 
# ##        each object named for the taxon that was examined
# ## define: 
#   ## t; genus/species/taxon level; taxon name to iterate through, usually provided as one element of a list of taxa
#   ## pca.ls; list of prcomp objects; each object should be the output from prcomp function
#   ## calc.level; genus/species; whether the PCA was done at genus or species level
#   ## exp.load; default=0, value to expand past maximum value of PCA loading for hulls in figures; 
#   ##          ex. 0.25 would be 1+0.25, or 1.25 of maximum
#   ## exp.score; default=0, value to expand past maximum value of PCA loading for hulls in figures; 
#   ##          ex. 0.5 would be 1+0.5, or 1.5 of maximum
#   ## pc.incl1; default='PC1'; first of which 2 PCs to include for plots
#   ## pc.incl2; default='PC2'; second of which 2 PCs to include for plots
# pcaCloudFigures <- function(t, pca.ls=gen.pcas, df.all=filter(gen.clean, absval=='in_gen_4'), 
#                             pc.incl1='PC1', pc.incl2='PC2', exp.load=0.25, exp.score=0.5, 
#                             calc.level='genus', ...){
#   
#         one.pca <- pca.ls[[t]]
#         df.one <- df.all %>% filter(genus %in% c(paste0('MortonArb_', t), t))
#         one.scores      <- data.frame(one.pca$x)
#         one.loads       <- data.frame(one.pca$rotation)
#         one.loads$labx  <- one.loads[, pc.incl1]*(max(one.scores[, pc.incl1])+exp.load)
#         one.loads$laby  <- one.loads[, pc.incl2]*(max(one.scores[, pc.incl2])+exp.load)
#         one.loads$var   <- row.names(one.loads)
#       ## set values for PC axes
#         lab1val <- summary(one.pca)$importance["Proportion of Variance", pc.incl1]
#         lab2val <- summary(one.pca)$importance["Proportion of Variance", pc.incl2]
#         cumvals <- round(lab1val + lab2val, digits=3)
#         lab1 <- paste0(pc.incl1, " (", round(lab1val, digits=3), " of variance)")
#         lab2 <- paste0(pc.incl2, " (", round(lab2val, digits=3), " of variance)")
#         
# ## begin making basic plot and add for genus or species specific info
#   ## main plot to display genus points
#     p <- ggplot(data=one.scores[,]) +
#           geom_point(aes(x=one.scores[, pc.incl1], 
#                          y=one.scores[, pc.incl2]), 
#                           size=0.2, alpha=0.1, color="gray75") +
#         ## add labels
#           labs(x=lab1, y=lab2) +
#           theme_bw() + 
#           theme(plot.title = element_text(size=16, face="bold.italic")) +
#           annotate("text", x=Inf, y=Inf, label=paste0("Cumulative variance: ", cumvals), hjust=1.1, vjust=1.2)
# 
# ## run through specific info for genus or species
#   if(calc.level=='genus'){
#     print(paste0("Creating PCA cloud figure for ", t, " for ", pc.incl1, " and ", pc.incl2,  "."))
#       p.title <- t
#     ## genus plot name
#       p2 <- p +
#             ggtitle(p.title) +
#           ## loadings text and arrows
#             geom_segment(data=one.loads, 
#                        aes(x=0, 
#                            y=0, 
#                         xend=one.loads[,pc.incl1]*(max(one.scores[, pc.incl1])+exp.load), 
#                         yend=one.loads[,pc.incl2]*(max(one.scores[, pc.incl2])+exp.load)), 
#                         arrow=arrow(length=unit(1/2, "picas")), color="#56B4E9") +
#             guides(fill=FALSE) +
#             geom_text(data=one.loads, aes(x=labx, y=laby, label=var), color="#0072B2", size=3) +
#             scale_fill_manual(values="#E69F00") +
# 
#           ## add MortonArb point
#             geom_point(data=one.scores[df.one$UID=="MORTONARB",], 
#                        aes(x=one.scores[df.one$UID=="MORTONARB", pc.incl1], 
#                            y=one.scores[df.one$UID=="MORTONARB", pc.incl2]), color="#009E73", size=5,
#                           shape=18) +
# 
#     ## write out genus figure from combined plot data
#       png(file.path(path.out, paste0(gsub(' ', '_', p.title), "_", pc.incl1, "_", pc.incl2, ".png")), 
#         height=8, width=8, units="in", res=180)
#             print(
#               p2
#               )
#       dev.off()
# 
#   } else if(calc.level=='species'){
#     
#     spp.ls <- df.one$species_name_acc[df.one$genus == t]
#     
#     for(s in spp.ls){
#         
#       print(paste0("Creating PCA cloud figure for ", s, " for ", pc.incl1, " and ", pc.incl2,  "."))
#         p.title <- s
#       ## extra to add to plot for species
#         p2 <- p + 
#           ggtitle(p.title) +
#            geom_point(data=one.scores[df.one$species_name_acc==s,], 
#                        aes(x=one.scores[df.one$species_name_acc==s, pc.incl1], 
#                            y=one.scores[df.one$species_name_acc==s, pc.incl2]), 
#                             size=0.2, alpha=0.2, color="#E69F00") +
#             stat_ellipse(data=one.scores[df.one$species_name_acc==s,], 
#                        aes(x=one.scores[df.one$species_name_acc==s, pc.incl1], 
#                            y=one.scores[df.one$species_name_acc==s, pc.incl2], fill=s), 
#                             alpha=0.25, geom="polygon") +
#           ## loadings text and arrows
#             geom_segment(data=one.loads, 
#                        aes(x=0, 
#                            y=0, 
#                         xend=one.loads[,pc.incl1]*(max(one.scores[, pc.incl1])+exp.load), 
#                         yend=one.loads[,pc.incl2]*(max(one.scores[, pc.incl2])+exp.load)), 
#                         arrow=arrow(length=unit(1/2, "picas")), color="#56B4E9") +
#             guides(fill=FALSE) +
#             geom_text(data=one.loads, aes(x=labx, y=laby, label=var), color="#0072B2", size=3) +
#             scale_fill_manual(values="#E69F00") +
# 
#           ## add MortonArb point
#             geom_point(data=one.scores[df.one$UID=="MORTONARB",], 
#                        aes(x=one.scores[df.one$UID=="MORTONARB", pc.incl1], 
#                            y=one.scores[df.one$UID=="MORTONARB", pc.incl2]), color="#009E73", size=5,
#                            shape=18) +
# 
#     ## write out figure from combined plot data
#       png(file.path(path.out, paste0(gsub(' ', '_', p.title), "_", pc.incl1, "_", pc.incl2, ".png")), 
#         height=8, width=8, units="in", res=180)
#             print(
#               p2
#               )
#       dev.off()
#         }
#     }
# }
####################################################################################################
####################################################################################################