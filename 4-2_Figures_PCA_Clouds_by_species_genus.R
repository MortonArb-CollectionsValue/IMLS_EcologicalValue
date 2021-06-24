## 4-2_Figure_by_species.R

# figure for each species
# examples for each genus

# Convex hull of 

# Table in/out or distance to
# % species suitable/not suitable


####################################################################################################
####################################################################################################
### Load packages
my.packages <- c('ggplot2', 'plyr', 'readr', 'dplyr', 'sf', 'tidyverse', 'ks', 'vegan', 'ggbiplot')
# install.packages (my.packages) #Turn on to install current versions
lapply(my.packages, require, character.only=TRUE)
  rm(my.packages)

####################################################################################################
####################################################################################################
### Load functions
spatial.outlier<-function(data,x=data,threshold=0.05){
  spatial.depth=function(x,data){
    if(ncol(as.matrix(x))==1)
      x<-t(x)
    if(ncol(data)!=ncol(as.matrix(x))){
      sd<-"Dimensions do not match"
    }else{
      spd<-function(data,x)
      {
        v=array(0,ncol(data))
        for(j in 1:ncol(data))
        {
          for(i in 1:nrow(data))
          {
            if(sqrt(sum((x-data[i,])^2))!=0)
              v[j]=v[j]+((x[j]-data[i,j])/sqrt(sum((x-data[i,])^2)))
          }
          v[j]=v[j]/nrow(data)
        }
        sd=1-sqrt(sum(v^2))
      }
      sd<-apply(x,1,function(y){spd(data,y)})
    }
    sd
  }
  sd<-spatial.depth(x,data)
  if(length(dim(x))!=0){
    m<-list()
    m[[1]]<-which(sd<=threshold)
    m[[2]]<-x[m[[1]],]
    names(m)<-c("index","observation")
    if(length(m[[1]])==0)
      m<-"FALSE"
  }else{
    m<-ifelse(sd<=threshold,"TRUE","FALSE")
  }
  return(m)
}

####################################################################################################
####################################################################################################
### set paths/folders
  # path to the shared Google Drive folder
  path.dat1 <- "/Volumes/GoogleDrive/Shared drives/IMLS MFA/Environmental Niche Value"
  
  # path for the folder for figure output
  path.figs <- file.path(path.dat1, "figures")
  
  # path on the local drive for accessing the data
  path.local <- "/Users/aesculus/Box/Research/Active_Projects/IMLS_MortonArb/local_data/PCA_Data"

####################################################################################################
####################################################################################################
## get list of species and genera
  spp.ls <- 
  gen.ls <- c('Malus', 'Quercus', 'Tilia', 'Ulmus')

# ## set species iteration
#   for(i.sp in spp.ls){
#     
#    ggbiplot(quercus.pca) #basic plot
#  
#   }
# 
## set genus iteration
  for(i.ge in gen.ls){
    i.ge
   ggbiplot() #basic plot

  }

  
### define species to be included
  spp.a <- "Quercus alba"
  spp.b <- "Quercus boyntonii"
  spp.c <- "Quercus brantii"

### filter by species
pc.a <- pca.df %>% filter(species_name_acc==spp.a) #%>% select(PC1, PC2)
pc.b <- pca.df %>% filter(species_name_acc==spp.b) #%>% select(PC1, PC2)
pc.c <- pca.df %>% filter(species_name_acc==spp.c) #%>% select(PC1, PC2)

### detect outliers for each species
pc.out.a <- OutlierDetection(pc.a[,2:3], distance=TRUE)
pc.out.b <- OutlierDetection(pc.b[,2:3], distance=TRUE)
pc.out.c <- OutlierDetection(pc.c[,2:3], distance=TRUE)

### detect outliers for each PCA for each species
pc.out.a2 <- PCOutlierDetection(pc.a[,2:3], distance=TRUE)
pc.out.b2 <- PCOutlierDetection(pc.b[,2:3], distance=TRUE)
pc.out.c2 <- PCOutlierDetection(pc.c[,2:3], distance=TRUE)

pc.out.c; pc.out.c2

pc.out.a3 <- spatial.outlier(pc.a[,2:3])
pc.out.b3 <- spatial.outlier(pc.b[,2:3])
pc.out.c3 <- spatial.outlier(pc.c[,2:3])

####################################################################################################
####################################################################################################
### compute contours

set.seed(1234)
d <- pc.a[,2:3]
# d <- data.frame(x=rnorm(1000),y=rnorm(1000))
kd <- ks::kde(d, compute.cont=TRUE)
# kd <- ks::kde(d, compute.cont=TRUE)

contour_95 <- with(kd, contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                    z=estimate, levels=cont["5%"])[[1]])
contour_95 <- data.frame(contour_95)

ggplot(data=d, aes(PC1, PC2)) +
  geom_point() +
  geom_path(aes(x, y), data=contour_95) +
  theme_bw()

####################################################################################################
####################################################################################################
### 

####################################################################################################
####################################################################################################
### 

####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
### extra code and snippets
# y=abs(Value), color=ifelse(Value<0, “Neg”, “Pos”)
# total_pca2$Trait <- factor(total_pca2$Trait, levels=c([order you want things in])

# png(“PCA_Loadings_3PCs.png”, height=6, width=6, units=“in”, res=320)
# dev.off()

