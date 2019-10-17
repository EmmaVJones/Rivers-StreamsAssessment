# how to clip spatial layers to region of interest

library(tidyverse)
library(sf)
library(mapview)

# first get statewide assessment polygon layer into R
assessmentLayer <- st_read('GIS/AssessmentRegions_VA84_basins.shp') %>%
  st_transform( st_crs(4326)) # transform to WQS84 for spatial intersection

#Below are the regional codes you need to choose from 
unique(assessmentLayer$ASSESS_REG)

# Choose your region
regionCode <- 'PRO'

assessmentLayerSelection <- filter(assessmentLayer, ASSESS_REG == regionCode)

# Now bring in statewide riverine AUs to be clipped
riverine <- st_read('GIS/draft2018_spatialData/va_2018_aus_riverine.shp') %>%
  st_transform( st_crs(4326)) # transform to WQS84 for spatial intersection


regionRiverine <- st_intersection(st_zm(riverine), assessmentLayerSelection)

# Inspect to make sure it looks right
mapview(regionRiverine)


# check column names before saving
names(regionRiverine)

# Save out as shapefile
st_write(regionRiverine, paste('GIS/draft2018_spatialData/va_2018_aus_riverine',regionCode,'.shp',sep=''))


# read back in to make sure no name changing funny business happened
regionalAUs <- st_read('GIS/draft2018_spatialData/va_2018_aus_riverinePRO.shp') %>%
  st_transform( st_crs(4326)) # transform to WQS84 for spatial intersection

names(regionalAUs)
names(regionalAUs) <- names(regionRiverine)
# Save out as shapefile
st_write(regionRiverine, paste('GIS/draft2018_spatialData/va_2018_aus_riverine',regionCode,'.shp',sep=''))
