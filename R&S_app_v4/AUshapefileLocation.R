# This code identifies the RELATIVE location of the shapefile you wish to call
# into the application to serve as your Regional Assessment Unit layer.

### NOTE: This RELATIVE location is called from the working directory (where
#         your application .Rproj is stored). Calling a file from outside this
#         relative location will require an ABSOLUTE file path 
#         (e.g. 'C:/directory/directory/fileName.shp')
### NOTE: The direction of the / is very important and is opposite of Windows
#         default path break notation.


# PRO #

#regionalAUs <- st_read('GIS/draft2018_spatialData/va_2018_aus_riverinePRO.shp') %>%
#  st_transform( st_crs(4326)) # transform to WQS84 for spatial intersection
# special step for PRO bc clipped in R and saving to shapefile changed column names
#names(regionalAUs) <- c("OBJECTID","ID305B", "MILES","CYCLE","WATER_NAME", "LOCATION",
#                        "CATEGORY", "AU_COMMENT", "IMP_CAUSE","SOURCE", "AQUA_LIFE",
#                        "DEEP_CHANN", "DEEP_WATER", "FISH_CONSU", "MIGRATORY", 
#                        "OPEN_WATER", "PWS", "RECREATION", "SHELLFISH", "SW_SAV", 
#                        "WILDLIFE", "Shape_Leng", "HUC12", "VAHU6", "Portion", "MAP",
#                        "ASSESS_REG", "OFFICE_NM" ,"States", "HUType", "HUMod", "ToHUC",
#                        "META_ID", "Location", "VaName","PC_Water", "Tidal" , "VAHUSB",
#                        "FedName", "HUC10" , "VAHU5", "Shape_Leng.1", "Shape_Area", "Basin", "geometry"  )




# BRRO #

regionalAUs <- st_read('GIS/draft2018_spatialData/va_2018_aus_riverineBRRO.shp') %>%
  st_transform( st_crs(4326)) # transform to WQS84 for spatial intersection




# SWRO #

#regionalAUs <- st_read('GIS/draft2018_spatialData/va_2018_aus_riverineSWRO.shp') %>%
#  st_transform( st_crs(4326)) # transform to WQS84 for spatial intersection


## TRO ##

#regionalAUs <- st_read('GIS/draft2018_spatialData/va_2018_aus_riverineTRO.shp') %>%
#  st_transform( st_crs(4326)) # transform to WQS84 for spatial intersection

