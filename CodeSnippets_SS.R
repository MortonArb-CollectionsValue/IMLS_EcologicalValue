spp.a <- "Quercus alba"
spp.b <- "Quercus boyntonii"
spp.c <- "Quercus acutissima"

      poly.ch.a <- polys.ch %>% dplyr::filter(species_name_acc == spp.a)
      poly.ch.b <- polys.ch %>% dplyr::filter(species_name_acc == spp.b)
      poly.ch.c <- polys.ch %>% dplyr::filter(species_name_acc == spp.c)

  just <- expand.grid(hjust = c(-60, 0, 60), vjust = c(-60, 0, 60))
    just$label <- paste0(just$hjust, ", ", just$vjust)
   g <- 
    ggplot(just, aes(hjust, vjust)) #+
        geom_sf(fill = "antiquewhite1")# +
      geom_point(data = ma.pt, size = 4, 
        shape = 23, fill = "darkred", inherit.aes = FALSE) +
    coord_sf(xlim = c(-60, -60), ylim = c(-60, 60), expand = FALSE)
      
geom="polygon"
  
  g + geom_polygon(data=poly.ch.c, mapping=aes(st_geometry(poly.ch.c), fill=species_name_acc), inherit.aes = FALSE)




library("rnaturalearth")
library("rnaturalearthdata")

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)
  
(sites <- data.frame(longitude = c(-80.144005, -80.109), latitude = c(26.479005, 
    26.83)))


ggplot(data = world) +
    geom_sf() +
    geom_point(data = sites, aes(x = longitude, y = latitude), size = 4, 
        shape = 23, fill = "darkred") +
    coord_sf(xlim = c(-88, -78), ylim = c(24.5, 33), expand = FALSE)

(sites <- st_as_sf(sites, coords = c("longitude", "latitude"), 
    crs = 4326, agr = "constant"))

  ggplot(data = world) +
    geom_sf() +
    geom_sf(data = sites, size = 4, shape = 23, fill = "darkred") +
    coord_sf(xlim = c(-88, -78), ylim = c(24.5, 33), expand = FALSE)
  
counties <- st_as_sf(map("county", plot = FALSE, fill = TRUE))
counties <- subset(counties, grepl("florida", counties$ID))
counties$area <- as.numeric(st_area(counties))
head(counties)

ggplot(data = world) +
    geom_sf() +
    geom_sf(data = counties, fill = NA, color = gray(.5)) +
    coord_sf(xlim = c(-88, -78), ylim = c(24.5, 33), expand = FALSE)


counties <- st_as_sf(map("county", plot = FALSE, fill = TRUE))
counties <- subset(counties, grepl("florida", counties$ID))
counties$area <- as.numeric(st_area(counties))
head(counties)
  
  
  # ggplot(data = just) +
    g +
    geom_sf() +
    geom_sf(data = poly.ch.c, , fill = species, color = gray(.5), inherit.aes = FALSE) +
    coord_sf(xlim = c(-60, -60), ylim = c(-60, 60), expand = FALSE)
  

    just <- expand.grid(hjust = c(-60, 0, 60), vjust = c(-60, 0, 60))
    just$label <- paste0(just$hjust, ", ", just$vjust)
  g <- ggplot(just, aes(hjust, vjust))
  st_as_sf(poly.ch.c)
  g + geom_polygon(data=poly.ch.c, mapping=aes(st_geometry(poly.ch.c), fill=species_name_acc), inherit.aes = FALSE)
  
  
  ggplot(just, aes(hjust, vjust)) + geom_polygon(data=poly.ch.c, aes(x=PC1, vjust=vjust, fill=species_name_acc)) #+ 
  geom_text(aes(label = label, hjust = hjust, vjust = vjust))

  
  
  # g <- 
  ggplot() + 
    geom_sf(data=poly.ch.a, aes(fill=species_name_acc)) + 
    geom_sf(data=poly.ch.b, aes(fill=species_name_acc)) +
    geom_sf(data=poly.ch.c, aes(fill=species_name_acc)) +
    geom_sf(data=ma.pt) + 
        coord_sf(xlim = c(-60, 60), ylim = c(-60, 60), expand = FALSE)

  
  
  
  
  ggplot(data=dat.all, aes(x=PC1, y=PC2)) +
    geom_point(aes(x=PC1, y=PC2), size=0.25, color="gray50") + 
    geom_sf() +
    
    # geom_polygon(data=pc.hulls[pc.hulls$species_name_acc %in% c("Quercus alba"),], aes(fill=species_name_acc), alpha=0.25) +
    # geom_polygon(data=pc.hulls[pc.hulls$species_name_acc %in% c("Quercus boyntonii"),], aes(fill=species_name_acc), alpha=0.5) +
    # geom_point(data=dat.all[dat.all$species_name_acc %in% c("Quercus alba"),], aes(color=species_name_acc)) +
    # geom_point(data=dat.all[dat.all$species_name_acc=="Quercus boyntonii",], aes(color=species_name_acc), size=2) +
    # geom_point(data=dat.all[dat.all$species_name_acc=="MortonArb",], aes(color=species_name_acc), size=8) +
    # scale_color_manual(name="Species", values=c("MortonArb"="green4", "Quercus alba"="blue2", "Quercus boyntonii"="red2", "Quercus velutina"="cadetblue3")) +
    # scale_fill_manual(name="Species", values=c("MortonArb"="green4", "Quercus alba"="blue2", "Quercus boyntonii"="red2", "Quercus velutina"="cadetblue3"))
