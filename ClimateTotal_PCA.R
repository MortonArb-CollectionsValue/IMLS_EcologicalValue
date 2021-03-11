#Different Number of Rows in ppt with others because ppt does not include Morton Arb Data points
  #Should I keep it?: decided to keep it for now

#Combining different Climate Traits of Malus
cbind(malus_climate_ppt, malus_climate_soil, malus_climate_srad, 
      malus_climate_tmax, malus_climate_tmin, malus_climate_vpd)

#Combining different Climate Traits of Quercus
cbind(quercus_climate_ppt, quercus_climate_soil, quercus_climate_srad,
      quercus_climate_tmax, quercus_climate_tmin, quercus_climate_vpd)

#Combining different Climate Traits of Tilia
cbind(tilia_climate_ppt, tilia_climate_soil, tilia_climate_srad, 
      tilia_climate_tmax, tilia_climate_tmin, tilia_climate_vpd)

#Combining different Climate Traits of Ulmus
cbind(ulmus_climate_ppt, ulmus_climate_soil, ulmus_climate_srad,
      ulmus_climate_tmax, ulmus_climate_tmin, ulmus_climate_vpd)