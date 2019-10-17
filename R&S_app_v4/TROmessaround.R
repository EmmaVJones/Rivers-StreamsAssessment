library(tidyverse)
library(sf)
library(mapview)


# read in TRO final 'riverine' stations
TRO <- read_csv('C:/HardDriveBackup/R/GitHub/Rivers-StreamsAssessment/preprocessingRegionalData/processedStationData/final2020data/RegionalResultsRiverine_TROFINAL.csv')

TRO_sf <- st_as_sf(TRO,
                   coords = c("Longitude", "Latitude"), 
                    remove = F, # don't remove these lat/lon cols from df
                    crs = 4326)  # add projection, needs to be geographic bc entering lat/lng
TRO_sf$STA_LV1_CODE <- as.factor(TRO_sf$STA_LV1_CODE)

# TRO riverine AUs
regionalAUs <- st_read('GIS/draft2018_spatialData/va_2018_aus_riverineTRO.shp') %>%
  st_transform( st_crs(4326)) # transform to WQS84 for spatial intersection


assessmentLayer <- st_read('GIS/AssessmentRegions_VA84_basins.shp') %>%
  st_transform( st_crs(4326)) # transform to WQS84 for spatial intersection
assessmentLayerSelection <- filter(assessmentLayer, ASSESS_REG == 'TRO')

estuary <- st_read('C:/HardDriveBackup/R/GitHub/Rivers-StreamsAssessment/preprocessingRegionalData/GIS/draft2018_spatialData/va_2018_aus_estuarine.shp') %>%
  st_transform( st_crs(4326)) 

estuaryTRO <- st_join(estuary,assessmentLayerSelection, join = st_intersects) %>%
  filter(!is.na(HUC12))

mapviewOptions(basemaps = c( "OpenStreetMap",'Esri.WorldImagery'),
               vector.palette = colorRampPalette(brewer.pal(3, "Set1")),
               na.color = "magenta",
               legend=FALSE)


mapview(estuaryTRO, 
        label = estuary$ID305B, color='black') +
mapview(regionalAUs,
        label= regionalAUs$ID305B) + 
  mapview(TRO_sf,
          label= TRO_sf$FDT_STA_ID, 
          zcol = 'STA_LV1_CODE')
