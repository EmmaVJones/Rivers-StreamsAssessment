# Run in R 3.5.1
source('global.R')

assessmentLayer <- st_read('GIS/AssessmentRegions_VA84_basins.shp') %>%
  st_transform( st_crs(4326)) %>% st_zm(regionalAUs)
#stationTable <- readRDS('data/BRROsites_ROA_sf.RDS')
#conventionals <- read_excel('data/CONVENTIONALS_20171010.xlsx') # need to change to read_CSV for final to make sure it runs faster
#conventionals$FDT_DATE_TIME2 <- as.POSIXct(conventionals$FDT_DATE_TIME, format="%m/%d/%y %H:%M")

# User Specific Data
regionalAUs <- st_read('GIS/AU_Roa_WGS84.shp') %>% st_zm(regionalAUs)



region_filter_ <- filter(assessmentLayer, ASSESS_REG=="BRRO")
basin_filter_ <- filter(region_filter_, Basin == 'Roanoke River Basin')
huc6_filter_ <- filter(basin_filter_, VAHU6 == 'RU25')

#4AGNF002.84, 4AGIL004.46

# Need to link huc6_filter to ID305B

identical(st_crs(assessmentLayer),st_crs(regionalAUs))

x <- st_intersection(st_zm(regionalAUs), st_zm(huc6_filter_))
mapview(x, zcol = "ID305B", legend=FALSE)
