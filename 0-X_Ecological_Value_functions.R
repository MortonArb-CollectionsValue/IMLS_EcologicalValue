####################################################################################################
####################################################################################################
## 0-X_Ecological_Value_functions.R
####################################################################################################
####################################################################################################

####################################################################################################
####################################################################################################
## center.scale.one
  ##  function to center and/or scale values by taxon
  ## define: 
    ## df.all; data set to use for the PCA analyses; should be complete cases or without missing values 
    ## meta.traits; traits that are metadata
    ## important.traits; traits that are used for PCA
    ## calc.level; genus/species; should the PCA be done at genus or species level
    ## center.scale; default="both"; values may be "center", "scale", "both"
  center.scale.one <- function(x, df.all=all.gen, meta.traits=meta.traits, important.traits=important.traits, 
                        calc.level='genus', center.scale='both', ...){
    
      # if(center.scale=='scale'){
      #     scaleTRUE  <- TRUE
      #     centerTRUE <- FALSE
      #   } else if(center.scale=='center'){
      #     scaleTRUE  <- FALSE
      #     centerTRUE <- TRUE
      #     } else if(center.scale=='both'){
      #     scaleTRUE  <- TRUE
      #     centerTRUE <- TRUE
      #       } else 
      #       scaleTRUE  <- FALSE
      #       centerTRUE <- FALSE

        if(calc.level=='genus'){
            one.scale <- cbind(df.all[df.all$genus %in% c('MortonArb', x), c("UID", meta.traits)], 
                               scale(df.all[df.all$genus %in% c('MortonArb', x), important.traits]))
            # one.scale <- cbind(df.all[df.all$genus %in% c('MortonArb', x), c("UID", meta.traits)], 
            #                    scale(df.all[df.all$genus %in% c('MortonArb', x), important.traits], 
            #                         scale=scaleTRUE, center=centerTRUE))
            one.scale$genus[one.scale$UID %in% 'MORTONARB'] <- paste0('MortonArb_', x)
        } else if(calc.level=='species'){
            one.scale <- cbind(df.all[df.all$species_name_acc %in% c('MortonArb', x), 
                                c("UID", meta.traits)], 
                               scale(df.all[df.all$species_name_acc %in% 
                                c('MortonArb', x), important.traits]))
            # one.scale <- cbind(df.all[df.all$species_name_acc %in% c('MortonArb', x), 
            #                     c("UID", meta.traits)], 
            #                    scale(df.all[df.all$species_name_acc %in% 
            #                     c('MortonArb', x), important.traits], 
            #                         scale=scaleTRUE, center=centerTRUE))
            one.scale$species_name_acc[one.scale$UID %in% 'MORTONARB'] <- paste0('MortonArb_', x)
        }

          if(nrow(one.scale) == 0L){} else return(one.scale)
        rm(one.scale)
    }
####################################################################################################
####################################################################################################
## abs_rows2keep
  ##  function to calculate the records that are within a distance of a multiple of absolute value 
  ##          and then set new field that defines whether the values are in or out of that distance
  ## define: 
    ## df.all; data set to use; should be complete cases or without missing values
    ## meta.traits; traits that are metadata
    ## important.traits; traits that are used for PCA
    ## calc.level; genus/species; should the PCA be done at genus or species level
    ## abs.val: value to define whether records is within or out of the total distance of a multiple 
    ##        of the absolute value for that field
  abs_rows2keep <- function(x, df.all=all.gen, abs.val=4, meta.traits=meta.traits, 
                            important.traits=important.traits, calc.level='genus', ...){
          if(calc.level=='genus'){
              one.val <- df.all %>% filter(genus %in% c(x, paste0('MortonArb_', x)))
              keepers <- apply(one.val[, important.traits], 1, 
                               FUN=function(x){all(abs(x)<=abs.val)})
              keep.UID <- one.val[keepers,"UID"]
              one.keep <- one.val %>% mutate(absval=if_else(one.val$UID %in% keep.UID, 
                                paste0("in_gen_", abs.val), paste0("out_gen_", abs.val)))
          } else if(calc.level=='species'){
              one.val <- df.all %>% filter(species_name_acc %in% c(x, paste0('MortonArb_', x)))
              keepers <- apply(one.val[, important.traits], 1, 
                               FUN=function(x){all(abs(x)<=abs.val)})
              keep.UID <- one.val[keepers,"UID"]
              one.keep <- one.val %>% mutate(absval=if_else(one.val$UID %in% keep.UID, 
                                paste0("in_spp_", abs.val), paste0("out_spp_", abs.val)))
          }
              if(nrow(one.keep) == 0L){} else return(one.keep)
            rm(one.keep)
    }
####################################################################################################
####################################################################################################
## prcomp_calcs
  ##  function to run PCA by taxon and then return PCA object
  ## define: 
    ## df.all; data set to use for the PCA analyses; should be complete cases or without missing values 
    ## meta.traits; traits that are metadata
    ## important.traits; traits that are used for PCA
    ## calc.level; genus/species; should the PCA be done at genus or species level
    ## center.scale; default="both"; values may be "center", "scale", "both"; 
    ##        should the PCA center and/or scale the data
prcomp_calcs <- function(x, df.all=gen.clean, meta.traits=meta.traits, important.traits=important.traits, 
                         center.scale="both", calc.level='genus', ...){
      
      if(center.scale=='scale'){
          scaleTRUE  <- TRUE
          centerTRUE <- FALSE
        } else if(center.scale=='center'){
          scaleTRUE  <- FALSE
          centerTRUE <- TRUE
          } else if(center.scale=='both'){
          scaleTRUE  <- TRUE
          centerTRUE <- TRUE
            } else 
            scaleTRUE  <- FALSE
            centerTRUE <- FALSE

          if(calc.level=='genus'){
              one.pca <- prcomp(df.all[df.all$genus %in% c(x, paste0('MortonArb_', x)), 
                                       important.traits], center=centerTRUE, scale.=scaleTRUE)
          } else if(calc.level=='species'){
              one.pca <- prcomp(df.all[df.all$species_name_acc %in% c(x, paste0('MortonArb_', x)), 
                                       important.traits], center=centerTRUE, scale.=scaleTRUE)
          }
              return(one.pca)
            rm(one.pca)
    }
  
####################################################################################################
####################################################################################################
## pcaCloudFigures: function to prep data and create PCA figures from a list of prcomp objects, 
  ##        each object named for the taxon that was examined
  ## define: 
    ## t; genus/species/taxon level; taxon name to iterate through, usually provided as one element of a list of taxa
    ## pca.ls; list of prcomp objects; each object should be the output from prcomp function
    ## calc.level; genus/species; whether the PCA was done at genus or species level
    ## exp.load; default=0, value to expand past maximum value of PCA loading for hulls in figures; 
    ##          ex. 0.25 would be 1+0.25, or 1.25 of maximum
    ## exp.score; default=0, value to expand past maximum value of PCA loading for hulls in figures; 
    ##          ex. 0.5 would be 1+0.5, or 1.5 of maximum
    ## pc.incl1; default='PC1'; first of which 2 PCs to include for plots
    ## pc.incl2; default='PC2'; second of which 2 PCs to include for plots
  pcaCloudFigures <- function(t, pca.ls=gen.pcas, df.all=filter(gen.clean, absval=='in_gen_4'), 
                              pc.incl1='PC1', pc.incl2='PC2', exp.load=0.25, exp.score=0.5, 
                              calc.level='genus', ...){
    
          one.pca <- pca.ls[[t]]
          df.one <- df.all %>% filter(genus %in% c(paste0('MortonArb_', t), t))
          one.scores      <- data.frame(one.pca$x)
          one.loads       <- data.frame(one.pca$rotation)
          one.loads$labx  <- one.loads[, pc.incl1]*(max(one.scores[, pc.incl1])+exp.load)
          one.loads$laby  <- one.loads[, pc.incl2]*(max(one.scores[, pc.incl2])+exp.load)
          one.loads$var   <- row.names(one.loads)
        ## set values for PC axes
          lab1val <- summary(one.pca)$importance["Proportion of Variance", pc.incl1]
          lab2val <- summary(one.pca)$importance["Proportion of Variance", pc.incl2]
          cumvals <- round(lab1val + lab2val, digits=3)
          lab1 <- paste0(pc.incl1, " (", round(lab1val, digits=3), " of variance)")
          lab2 <- paste0(pc.incl2, " (", round(lab2val, digits=3), " of variance)")
          
  ## begin making basic plot and add for genus or species specific info
    ## main plot to display genus points
      p <- ggplot(data=one.scores[,]) +
            geom_point(aes(x=one.scores[, pc.incl1], 
                           y=one.scores[, pc.incl2]), 
                            size=0.2, alpha=0.2, color="gray75") +
          ## add labels
            labs(x=lab1, y=lab2) +
            theme_bw() + 
            theme(plot.title = element_text(size=16, face="bold.italic")) +
            annotate("text", x=Inf, y=Inf, label=paste0("Cumulative variance: ", cumvals), hjust=1.1, vjust=1.2)

  ## run through specific info for genus or species
    if(calc.level=='genus'){
      print(paste0("Creating PCA cloud figure for ", t, " for ", pc.incl1, " and ", pc.incl2,  "."))
        p.title <- t
      ## genus plot name
        p2 <- p +
              ggtitle(p.title) +
            ## loadings text and arrows
              geom_segment(data=one.loads, 
                         aes(x=0, 
                             y=0, 
                          xend=one.loads[,pc.incl1]*(max(one.scores[, pc.incl1])+exp.load), 
                          yend=one.loads[,pc.incl2]*(max(one.scores[, pc.incl2])+exp.load)), 
                          arrow=arrow(length=unit(1/2, "picas")), color="#56B4E9") +
              guides(fill=FALSE) +
              geom_text(data=one.loads, aes(x=labx, y=laby, label=var), color="#0072B2", size=3) +
              scale_fill_manual(values="#E69F00") +

            ## add MortonArb point
              geom_point(data=one.scores[df.one$UID=="MORTONARB",], 
                         aes(x=one.scores[df.one$UID=="MORTONARB", pc.incl1], 
                             y=one.scores[df.one$UID=="MORTONARB", pc.incl2]), color="#009E73", size=5,
                            shape=18)

      ## write out genus figure from combined plot data
        png(file.path(path.out, paste0(gsub(' ', '_', p.title), "_", pc.incl1, "_", pc.incl2, ".png")), 
          height=8, width=8, units="in", res=180)
              print(
                p2
                )
        dev.off()

    } else if(calc.level=='species'){
      
      spp.ls <- unique(df.one$species_name_acc[df.one$genus == t])
      
      for(s in spp.ls){
          
        print(paste0("Creating PCA cloud figure for ", s, " for ", pc.incl1, " and ", pc.incl2,  "."))
          p.title <- s
        ## extra to add to plot for species
          p2 <- p + 
            ggtitle(p.title) +
             geom_point(data=one.scores[df.one$species_name_acc==s,], 
                         aes(x=one.scores[df.one$species_name_acc==s, pc.incl1], 
                             y=one.scores[df.one$species_name_acc==s, pc.incl2]), 
                              size=0.4, alpha=0.8, color="#E69F00") +
              stat_ellipse(data=one.scores[df.one$species_name_acc==s,], 
                         aes(x=one.scores[df.one$species_name_acc==s, pc.incl1], 
                             y=one.scores[df.one$species_name_acc==s, pc.incl2], fill=s), 
                              alpha=0.25, geom="polygon") +
            ## loadings text and arrows
              geom_segment(data=one.loads, 
                         aes(x=0, 
                             y=0, 
                          xend=one.loads[,pc.incl1]*(max(one.scores[, pc.incl1])+exp.load), 
                          yend=one.loads[,pc.incl2]*(max(one.scores[, pc.incl2])+exp.load)), 
                          arrow=arrow(length=unit(1/2, "picas")), color="#56B4E9") +
              guides(fill=FALSE) +
              geom_text(data=one.loads, aes(x=labx, y=laby, label=var), color="#0072B2", size=3) +
              scale_fill_manual(values="#E69F00") +

            ## add MortonArb point
              geom_point(data=one.scores[df.one$UID=="MORTONARB",], 
                         aes(x=one.scores[df.one$UID=="MORTONARB", pc.incl1], 
                             y=one.scores[df.one$UID=="MORTONARB", pc.incl2]), color="#009E73", size=5,
                             shape=18)

      ## write out figure from combined plot data
        png(file.path(path.out, paste0(gsub(' ', '_', p.title), "_", pc.incl1, "_", pc.incl2, ".png")), 
          height=8, width=8, units="in", res=180)
              print(
                p2
                )
        dev.off()
          }
      }
  }
####################################################################################################
####################################################################################################
