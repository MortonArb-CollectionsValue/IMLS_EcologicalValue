# Christy playing around with PCA output to see how we can synthesize
library(ggplot2); library(ggrepel)

## path to the shared Google Drive folder
path.dat <- "/Volumes/GoogleDrive/Shared drives/IMLS MFA/Environmental Niche Value" ## path for Shannon
# path.dat <- "D:/Data_IMLS_Ecological_Value"   ## path to data for Shiven D drive
path.out <- file.path(path.dat, "Analysis/PrelimPCA/figures")

# path for the folder for figure output
path.figs <- file.path(path.dat, "figures")


# # Load in the PCA RData file
load(file.path(path.dat, "Extracted Data", "PCA_output.RData"))


# Exploring the data
# summary(gen.clean.pca)

# # # -----------------
# # Extracting Quercus as a test case and doing some binning so we can do some density plot stuff
# dat.quercus <- gen.clean.pca[grepl("Quercus", gen.clean.pca$genus) & !is.na(gen.clean.pca$PC1),]
# # summary(dat.quercus)
# dat.quercus$PC1.cut <- cut(dat.quercus$PC1, 50)
# dat.quercus$PC1.mid <- sapply(strsplit(gsub("^\\W|\\W$", "", dat.quercus$PC1.cut), ","),
#                               function(x)sum(as.numeric(x))/2)
# dat.quercus$PC2.cut <- cut(dat.quercus$PC2, 50)
# dat.quercus$PC2.mid <- sapply(strsplit(gsub("^\\W|\\W$", "", dat.quercus$PC2.cut), ","),
#                               function(x)sum(as.numeric(x))/2)
# dat.quercus$PC3.cut <- cut(dat.quercus$PC3, 50)
# dat.quercus$PC3.mid <- sapply(strsplit(gsub("^\\W|\\W$", "", dat.quercus$PC3.cut), ","),
#                               function(x)sum(as.numeric(x))/2)
# dim(dat.quercus)
# 
# quercus.agg.spp <- aggregate(UID~species_name_acc + genus + species + PC1.mid + PC2.mid,data=dat.quercus, FUN=length)
# names(quercus.agg.spp)[ncol(quercus.agg.spp)] <- c("UID.n") 
# summary(quercus.agg.spp)
# 
# # Plotting the observation density for a handful of species as test cases
# ggplot(data=quercus.agg.spp[quercus.agg.spp$species %in% c("alba", "lyrata", "petraea", "arizonica"),]) +
#   facet_wrap(~species) +
#   geom_tile(aes(x=PC1.mid, y=PC2.mid, fill=UID.n)) +
#   geom_point(x=quercus.agg.spp$PC1[quercus.agg.spp$genus=="MortonArb_Quercus"], y=quercus.agg.spp$PC2[quercus.agg.spp$genus=="MortonArb_Quercus"], color="green3", size=5)
# 
# quercus.agg.gen <- aggregate(UID.n~genus + PC1.mid + PC2.mid, data=quercus.agg.spp, FUN=length)
# names(quercus.agg.gen)[ncol(quercus.agg.gen)] <- c("Spp.n") 
# summary(quercus.agg.gen)
# 
# ggplot(data=quercus.agg.gen) +
#   geom_tile(aes(x=PC1.mid, y=PC2.mid, fill=Spp.n)) +
#   geom_point(x=dat.quercus$PC1[dat.quercus$genus=="MortonArb_Quercus"], y=dat.quercus$PC2[dat.quercus$genus=="MortonArb_Quercus"], color="green3", size=5)


#### ----------------------
#### Doing the aggregation on all genera at once
#### ----------------------
names(gen.clean.pca)
# To FInd Morton, UID = MORTONARB; Species = MortonArb
gen.clean.pca[grep("Morton", gen.clean.pca$genus),"genus"] <- unlist(lapply(strsplit(gen.clean.pca$genus[grep("Morton", gen.clean.pca$genus)], "_"), function(x) x[2]))
# gen.clean.pca[]

# Getting rid of NAs just for sanity
gen.clean.pca <- gen.clean.pca[!is.na(gen.clean.pca$PC1),]
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
 

clean.spp.agg <- aggregate(UID~species_name_acc + genus + species + PC1.mid + PC2.mid, data=gen.clean.pca, FUN=length)
names(clean.spp.agg)[ncol(clean.spp.agg)] <- c("UID.n")
summary(clean.spp.agg)

# # Plotting the observation density for a handful of species as test cases
ggplot(data=clean.spp.agg[clean.spp.agg$species_name_acc  %in% c("Quercus macrocarpa", "Tilia americana", "Ulmus rubra", "Malus angustifolia"),]) +
  facet_wrap(~genus) +
  geom_tile(aes(x=PC1.mid, y=PC2.mid, fill=UID.n)) +
  geom_point(data=gen.clean.pca[gen.clean.pca$species=="MortonArb",], aes(x=PC1, y=PC2), color="cadetblue2", size=5) +
  scale_fill_viridis_b()
# 
clean.gen.agg <- aggregate(UID.n~genus + PC1.mid + PC2.mid, data=clean.spp.agg[clean.spp.agg$UID!="MortonArb",], FUN=length)
names(clean.gen.agg)[ncol(clean.gen.agg)] <- c("Spp.n")
summary(clean.gen.agg)
# 

# ggplot(data=clean.gen.agg) +
#   facet_wrap(~genus, scales="free_x") +
#   geom_histogram(aes(x=Spp.n))
  
labs.list <- c(Quercus=paste0("Quercus (", length(unique(gen.clean.pca$species_name_acc[gen.clean.pca$species!="MortonArb" & gen.clean.pca$genus=="Quercus"])), " spp)"),
                  Malus=paste0("Malus (", length(unique(gen.clean.pca$species_name_acc[gen.clean.pca$species!="MortonArb" & gen.clean.pca$genus=="Malus"])), " spp)"),
                  Tilia=paste0("Tilia (", length(unique(gen.clean.pca$species_name_acc[gen.clean.pca$species!="MortonArb" & gen.clean.pca$genus=="Tilia"])), " spp)"),
                  Ulmus=paste0("Ulmus (", length(unique(gen.clean.pca$species_name_acc[gen.clean.pca$species!="MortonArb" & gen.clean.pca$genus=="Ulmus"])), " spp)"))


plot.pca.den <- ggplot(data=clean.gen.agg) +
  facet_wrap(~genus, labeller = as_labeller(labs.list)) +
  geom_raster(aes(x=PC1.mid, y=PC2.mid, fill=Spp.n)) +
  geom_point(data=gen.clean.pca[gen.clean.pca$species=="MortonArb",], aes(x=PC1, y=PC2), color="orange2", size=5) +
  scale_fill_viridis_b(name="Spp", breaks=c(1,2,5,10,20,30,50)) +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_line(color="black"))


png(file.path(path.figs, "PCA_SpeciesDensity_Genus.png"), 
    height=8, width=8, units="in", res=320)
print(plot.pca.den)
dev.off()


## Figuring out how to add the loadings:
summary(gen.pcas)
summary(gen.pcas$Quercus$rotation)
row.names(gen.pcas$Quercus$rotation)

gen.load <- data.frame()
for(i in 1:length(gen.pcas)){
  df.tmp <- data.frame(genus=names(gen.pcas)[i], 
                        env.var=row.names(gen.pcas[[i]]$rotation), 
                        gen.pcas[[i]]$rotation[,1:2])
  df.tmp$labx <- df.tmp$PC1*(max(gen.pcas[[i]]$x[,1])+0.25)
  df.tmp$laby <- df.tmp$PC2*(max(gen.pcas[[i]]$x[,2])+0.25)
  # df.tmp$labx <- ifelse(df.tmp$labx<0, df.tmp$labx-0.15, df.tmp$labx+0.15)
  # df.tmp$laby <- ifelse(df.tmp$laby<0, df.tmp$laby-0.15, df.tmp$laby+0.15)
  df.tmp$xend <- df.tmp$PC1*(max(gen.pcas[[i]]$x[,1])-0.5)
  df.tmp$yend <- df.tmp$PC2*(max(gen.pcas[[i]]$x[,2])-0.5)
  df.tmp$dist <- sqrt(df.tmp$PC1^2 + df.tmp$PC2^2)# How long the combiend arrow is
  df.tmp$rank <- order(df.tmp$dist)
  gen.load <- rbind(gen.load, df.tmp)
}
summary(gen.load)

gen.load$graph <- apply(gen.load[,c("PC1", "PC2")], 1, function(x) ifelse(max(abs(x))>0.3, "*", NA))
summary(gen.load[!is.na(gen.load$graph),])
# gen.load$graph <- ifelse(gen.load$dist>0.3, "*", NA)

summary(gen.load[,c("env.var", "genus", "PC1", "PC2", "graph")])

png(file.path(path.figs, "PCA_SpeciesDensity_Genus-LoadingsTop3.png"), 
    height=8, width=9, units="in", res=320)
ggplot(data=clean.gen.agg) +
  facet_wrap(~genus, labeller = as_labeller(labs.list)) +
  geom_raster(aes(x=PC1.mid, y=PC2.mid, fill=Spp.n)) +
  geom_point(data=gen.clean.pca[gen.clean.pca$species=="MortonArb",], aes(x=PC1, y=PC2, color="Morton Arb"), size=3) +
  geom_segment(data=gen.load[gen.load$rank<=3,], aes(x=0, y=0, xend=xend, yend=yend), arrow=arrow(length=unit(1/2, "picas")), color="gray75") +
  geom_text_repel(data=gen.load[gen.load$rank<=3,], aes(x=labx, y=laby, label=env.var), color="white", size=3, fontface="bold") +
  labs(x="PC1", y="PC2") +
  scale_fill_viridis_b(name="Spp", breaks=c(1,2,5,10,20,30,50)) +
  scale_color_manual(name="", values="orange2") +
  theme(panel.background = element_rect(fill="gray30"),
        panel.grid = element_blank(),
        axis.line = element_line(color="black"),
        strip.background = element_blank(),
        strip.text = element_text(size=rel(1.5), face="bold"),
        axis.title=element_text(size=rel(1.25), face="bold"),
        legend.key = element_blank())
dev.off()
