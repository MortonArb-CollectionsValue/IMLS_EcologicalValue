# IMLS_EcologicalValue
 Ecological Value Analyeses
## 1-0 Soil_Extractions
Used soil codes to find individual species soil predictor values

Generally what happens in each script: 
- Creates paths for extracting data and where to save the data
- Loading in each unique MU_GLOBAL majority SHARE values
- Extract the HWSD soil code from the master raster: take the dominant soil type for the data
- Filters columns to keep from the occurrence point file
- Extracts soil data and saves as csv’s in output file path

Inputs: 
- Soil Occurrence Points from Database (Locally or Google Drive)
  - Found on http://www.fao.org/soils-portal/soil-survey/soil-maps-and-databases/harmonized-world-soil-database-v12/en/ 
  - Spatial Resolution: 30 arc second (~1 km)
  - Spatial Extent: Global
  - Temporal Resolution: Snapshot (single time)
  - Temporal Extent: modern era (not paleo)
  - Stored in "D:/spp_raw_points/spp_edited_points_Extracted2/[SPECIES].csv”
  - Stored in "/Volumes/GoogleDrive/Shared drives/IMLS MFA/occurrence_points/outputs/spp_edited_points/[SPECIES].csv"

Outputs:
- Cleaned Soil Data separated by species w/ Morton Arb Data saved as CSVs
  - "/Volumes/GoogleDrive/Shared drives/IMLS MFA/Extracted Data/Soil_Extract/[SPECIES].csv"
 - Extracted Quantitative Variable Names:
   - AWC_VALUE: Available Water storage capacity, mm/m
   - ROOTS: Obstacles to Roots (ESDB/European Soil Database), code
   - S.BS: Subsoil Base Saturation, %
   - S.CACO3: Subsoil Calcium Carbonate, %wt
   - S.CASO4: Subsoil Gypsum, %wt
   - S.CEC.CLAY: Subsoil CEC (Cation Exchange Capacity) Clay, cmol/kg
   - S.CEC.SOIL: Subsoil CEC Soil, cmol/kg
   - S.CLAY: Subsoil Clay Fraction, %wt
   - S.ECE: Subsoil Salinity (Elco), dS/m
   - S.ESP: Subsoil Sodicity (ESP/Exchangeable Sodium Percentage), %
   - S.GRAVEL: Subsoil Gravel Content, %vol
   - S.OC: Subsoil Organic Carbon, %wt
   - S.PH.H20: Subsoil pH (H20), -log(H+)
   - S.REF.BULK.DENSITY: Subsoil Reference Bulk Density, kg/dm3
   - S.SAND: Subsoil Sand Fraction, %wt
   - S.SILT: Subsoil Silt Fraction, %wt
   - S.TEB: Subsoil TEB, cmol/kg
   - T.BS: Topsoil Base Saturation, %
   - T.CACO3: Topsoil Calcium Carbonate, %wt
   - T.CASO4: Topsoil Gypsum, %wt
   - T.CEC.CLAY: Topsoil CEC (Cation Exchange Capacity) Clay, cmol/kg
   - T.CEC.SOIL: Topsoil CEC Soil, cmol/kg
   - T.CLAY: Topsoil Clay Fraction, %wt
   - T.ECE: Topsoil Salinity (Elco), dS/m
   - T.ESP: Topsoil Sodicity (ESP/Exchangeable Sodium Percentage), %
   - T.GRAVEL: Topsoil Gravel Content, %vol
   - T.OC: Topsoil Organic Carbon, %wt
   - T.PH.H20: Topsoil pH (H20), -log(H+)
   - T.REF.BULK.DENSITY: Topsoil Reference Bulk Density, kg/dm3
   - T.SAND: Topsoil Sand Fraction, %wt
   - T.SILT: Topsoil Silt Fraction, %wt
   - T.TEB: Topsoil TEB, cmol/kg
   - T.TEXTURE: Topsoil Texture, code


## 1-0 Soil_Extractions_MDB: 
Uses very similar steps to Soil_Extractions but uses MDB file, which does not run so well on Windows.

Generally what happens in each script: 
- Similar to Soil Extractions Script (Christy explain changes)

Inputs:
- Soil Occurrence Points from Database
   - Found on http://www.fao.org/soils-portal/soil-survey/soil-maps-and-databases/harmonized-world-soil-database-v12/en/ 
   - Spatial Resolution: 30 arc second (~1 km)
   - Spatial Extent: Global
   - Temporal Resolution: Snapshot (single time)
   - Temporal Extent: modern era (not paleo)
   - Stored in "D:/spp_raw_points/spp_raw_points2/[SPECIES].csv"
   - Stored in "/Volumes/GoogleDrive/Shared drives/IMLS MFA/"occurrence_points/outputs/spp_edited_points/[SPECIES].csv"

Outputs:
- Cleaned Soil Data separated by species w/ Morton Arb Data saved as CSVs
  - "/Volumes/GoogleDrive/Shared drives/IMLS MFA/Extracted Data/Soil_Extract/[SPECIES].csv"
 - Extracted Quantitative Variable Names:
   - AWC_VALUE: Available Water storage capacity, mm/m
   - ROOTS: Obstacles to Roots (ESDB/European Soil Database), code
   - S.BS: Subsoil Base Saturation, %
   - S.CACO3: Subsoil Calcium Carbonate, %wt
   - S.CASO4: Subsoil Gypsum, %wt
   - S.CEC.CLAY: Subsoil CEC (Cation Exchange Capacity) Clay, cmol/kg
   - S.CEC.SOIL: Subsoil CEC Soil, cmol/kg
   - S.CLAY: Subsoil Clay Fraction, %wt
   - S.ECE: Subsoil Salinity (Elco), dS/m
   - S.ESP: Subsoil Sodicity (ESP/Exchangeable Sodium Percentage), %
   - S.GRAVEL: Subsoil Gravel Content, %vol
   - S.OC: Subsoil Organic Carbon, %wt
   - S.PH.H20: Subsoil pH (H20), -log(H+)
   - S.REF.BULK.DENSITY: Subsoil Reference Bulk Density, kg/dm3
   - S.SAND: Subsoil Sand Fraction, %wt
   - S.SILT: Subsoil Silt Fraction, %wt
   - S.TEB: Subsoil TEB, cmol/kg
   - T.BS: Topsoil Base Saturation, %
   - T.CACO3: Topsoil Calcium Carbonate, %wt
   - T.CASO4: Topsoil Gypsum, %wt
   - T.CEC.CLAY: Topsoil CEC (Cation Exchange Capacity) Clay, cmol/kg
   - T.CEC.SOIL: Topsoil CEC Soil, cmol/kg
   - T.CLAY: Topsoil Clay Fraction, %wt
   - T.ECE: Topsoil Salinity (Elco), dS/m
   - T.ESP: Topsoil Sodicity (ESP/Exchangeable Sodium Percentage), %
   - T.GRAVEL: Topsoil Gravel Content, %vol
   - T.OC: Topsoil Organic Carbon, %wt
   - T.PH.H20: Topsoil pH (H20), -log(H+)
   - T.REF.BULK.DENSITY: Topsoil Reference Bulk Density, kg/dm3
   - T.SAND: Topsoil Sand Fraction, %wt
   - T.SILT: Topsoil Silt Fraction, %wt
   - T.TEB: Topsoil TEB, cmol/kg
   - T.TEXTURE: Topsoil Texture, code

## 1-1 Climate_Extractions: WARNING- Takes >1 week to run (non-stop)
Takes climate data from database and aggregates data by year.  The data is split into folders by predictor with each individual species’ data in csv’s.

Generally what happens in each script:
- Creates paths for extracting data and where to save the data
- Extracting lat & longitude vectors from the data
- Aggregating data by ppt (precipitation), soil (soil moisture), srad (soil radiation), tmax (temp max), tmin (temp min), & vpd (vapor pressure)
- Split each of these environmental predictors into ann, min, max and split each of those by mean, min, max, and sd
- Saved each of the individual environmental predictors into their own folders, separated by species

Inputs:
- Climate Occurrence Points from Database
   - Found on http://www.climatologylab.org/terraclimate.html 
   - Spatial Resolution: ~4 km
   - Spatial Extent: Global
   - Temporal Resolution: Monthly
   - Temporal Extent: 1958 - 2018 (+ climatologies for future +2/+4 C)
   - Stored in "/Volumes/GoogleDrive/Shared drives/IMLS MFA/"occurrence_points/outputs/spp_edited_points/[SPECIES].csv"
 
Outputs:
- Cleaned Climate Data separated by species & by ppt, soil, srad, tmax, tmin, vpd w/ Morton Arb Data saved as CSVs
   - "D:/Data_IMLS_Ecological_Value/Soil_Extracts2/[SPECIES].csv”
   - "/Volumes/GoogleDrive/Shared drives/IMLS MFA/Environmental Niche Value/Extracted Data/Climate_Extract/[PREDICTOR]/[SPECIES].csv"

Extracted Variables:
- aet: Actual Evapotranspiration, monthly total; units- mm per month
- def: Climate Water Deficit, monthly total; units- mm per month
- pet: Potential evapotranspiration, monthly total; units- mm per month
- ppt: Precipitation, monthly total; units- mm per month
- q: Runoff, monthly total; units- mm per month
- soil: Soil Moisture, total column - at end of month; units-  mm
- srad: Downward surface shortwave radiation; units- Watts per square meter-  W/m2-  W m-2
- swe: Snow water equivalent - at end of month- liquid water content of surface snow at end of month; units- mm - 
- tmax: Max Temperature, average for month; units- deg. C
- tmin: Min Temperature, average for month; units- deg. C
- vap: Vapor pressure, average for month; units- kPa- kiloPascals
- ws: Wind speed, average for month; units- meters per second- m/s-  m s-1
- vpd: Vapor Pressure Deficit, average for month; units- kPa(kiloPascals)
- PDSI: Palmer Drought Severity Index, at end of month; unitless

## 1-1 Climate_Extractions_Raster: WARNING- Takes >1 week to run (non-stop)
Although similar to the normal Climate_Extractions script, this is an alternate, faster way of extracting climate data by using the raster package in R.

Generally what happens in each script: similar to Climate_Extractions (Christy provide summary on differences)

Inputs:
- Climate Occurrence Points from Database
  - Found on http://www.climatologylab.org/terraclimate.html 
  - Spatial Resolution: ~4 km
  - Spatial Extent: Global
  - Temporal Resolution: Monthly
  - Temporal Extent: 1958 - 2018 (+ climatologies for future +2/+4 C)
  - Stored in "/Volumes/GoogleDrive/Shared drives/IMLS MFA/occurrence_points/outputs/spp_edited_points/[SPECIES}.csv"

Outputs:
- Cleaned Climate Data separated by species & by  ppt (precipitation), soil (soil s), srad (soil radiation), tmax (temp max), tmin (temp min), & vpd (vapor pressure) w/ Morton Arb Data saved as CSVs
  - "/Volumes/GoogleDrive/Shared drives/IMLS MFA/Environmental Niche Value/Extracted Data/Climate_Extract/[PREDICTOR]/[SPECIES].csv"

Extracted Variables:
- aet: Actual Evapotranspiration, monthly total; units- mm per month
- def: Climate Water Deficit, monthly total; units- mm per month
- pet: Potential evapotranspiration, monthly total; units- mm per month
- ppt: Precipitation, monthly total; units- mm per month
- q: Runoff, monthly total; units- mm per month
- soil: Soil Moisture, total column - at end of month; units-  mm
- srad: Downward surface shortwave radiation; units- Watts per square meter-  W/m2-  W m-2
- swe: Snow water equivalent - at end of month- liquid water content of surface snow at end of month; units- mm
- tmax: Max Temperature, average for month; units- deg. C
- tmin: Min Temperature, average for month; units- deg. C
- vap: Vapor pressure, average for month; units- kPa- kiloPascals
- ws: Wind speed, average for month; units- meters per second- m/s-  m s-1
- vpd: Vapor Pressure Deficit, average for month; units- kPa(kiloPascals)
- PDSI: Palmer Drought Severity Index, at end of month; unitless

## 1-2 Biome_Extractions: NOTE- not currently using this data
Utilizes for loop to sort out important biome occurrence point columns and save extractions as csv’s.

Generally what happens in each script:
- Creates paths for extracting data and where to save the data
- Reading in the ecoregion file
- Sorted Columns: used cols.keep & ecos.keep to sort out important columns
- Saved sorted data frames as SpatialPointsDataFrames
- Saved as CSV’s split up by species

Inputs:
- Biome Data Occurence Points
  - Found on https://www.worldwildlife.org/publications/terrestrial-ecoregions-of-the-world 
  - Spatial Resolution: Polygons
  - Spatial Extent: Global
  - Temporal Resolution: Snapshot (single time)
  - Temporal Extent: modern era (not paleo)
  - "/Volumes/GoogleDrive/Shared drives/IMLS MFA/occurrence_points/outputs/spp_edited_points/[SPECIES].csv”

Outputs:
- Cleaned Biome Data separated by species w/ Morton Arb Data saved as CSVs
  - Stored in "/Volumes/GoogleDrive/Shared drives/IMLS MFA/Environmental Niche Value/Extracted Data/Biome_Extract/[SPECIES].csv"

## 2-0 IndividualPredictor_Template:
Combined different species into dataframes by genus in order to reduce the amount of dataframes for analysis.  Reduce the redundancy of similar characteristics that occur across different environmental predictor.

Generally what happens in each script:
- Loading in Individual Species for each predictor & saved them all into dataframes with same species for each climate predictor (ppt, soil, srad, tmax, tmin, vpd)
- Saving "nativeDatabaseID" & "MU.SOURCE1" Columns as Characters
- Split Up Names into Genus & Species Columns
- Got rid of NA Values
- Reductions: Filtered out only 3 important columns (1 per ann, max, min)
  - Reduced columns by adding all the PCA similarity values together for each column
  - Then, reduced variables by taking the minimum sum for each of the ann, max, & min 
- Saved as CSV’s: makes loading easier

Inputs:
- Extracted Climate Data (split up by ppt, soil, srad, tmax, tmin, vpd)
   - “D:/Data_IMLS_Ecological_Value/Climate_Extract_Drive/[PREDICTOR]/[SPECIES].csv”

Outputs:
- Edited Climate Data (still split up by ppt, soil, srad, tmax, tmin, vpd) 
   - “D:/Data_IMLS_Ecological_Value/Preloaded_Data/[PREDICTOR]/[PREDICTOR]_[GENUS].csv"

## 3-0 Total_Template:
Similar methodology to previous step except combines & reduces different environmental predictors into 4 dataframes by genus (malus, quercus, tilia, ulmus).

Generally what happens in each script:
- Different climate predictors combined into large data frame for each of 4 Genera
- Another round of reduction based on overlapping climate predictors among the different (likely 1 for each climate predictor)

Inputs:
- Edited Climate data csv’s (split up by ppt, soil, srad, tmax, tmin, vpd)
   - “D:/Data_IMLS_Ecological_Value/Preloaded_Data/[PREDICTOR]/[PREDICTOR]_[GENUS].csv"

Outputs:
- Combined & Reduced Data organized by genera
   - "D:/Data_IMLS_Ecological_Value/Total_PreReductions/[GENUS]/Total.csv"
