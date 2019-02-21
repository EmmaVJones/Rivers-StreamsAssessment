source('global.R')
source('AUshapefileLocation.R')

assessmentLayer <- st_read('GIS/AssessmentRegions_VA84_basins.shp') %>%
  st_transform( st_crs(4326)) 
conventionals <- suppressWarnings(read_csv('data/CONVENTIONALS_20171010.csv'))
conventionals$FDT_DATE_TIME2 <- as.POSIXct(conventionals$FDT_DATE_TIME, format="%m/%d/%Y %H:%M")
##commentList <- readRDS('Comments/commentList.RDS')
monStationTemplate <- read_excel('data/tbl_ir_mon_stations_template.xlsx') # from X:\2018_Assessment\StationsDatabase\VRO
WCmetals <- read_excel('data/WATER_METALS_20170712.xlsx')
Smetals <- read_excel('data/SEDIMENT_20170712.xlsx')
## Bring in latest EDAS VSCI and (combined) VCPMI queries
VSCI <- read_excel('data/Family Metrics VSCI Calculation.xlsx')%>%
  filter(RepNum == 1 & Target_Count == 110 &
           CollDate >= assessmentPeriod[1] )
VCPMI <- read_excel('data/Family Metrics - CPMI Combined.xlsx')%>%
  filter(RepNum == 1 & Target_Count == 110 &
           CollDate >= assessmentPeriod[1] )

# User uploaded things
# snapped sites from other tool that is flat file for user to edit if they need to
stationTable <- read_csv('data/RegionalResults_AU_WQS.csv') %>%
  as_tibble() %>%
  dplyr::rename(`Point Unique Identifier` ="Point.Unique.Identifier", `Buffer Distance` = "Buffer.Distance")
# Their region shapefile of AUs that they can edit/split if they need to
regionalAUs <- st_read('GIS/AU_BRRO_2016_WGS84.shp')

# figure out how many AU's still have issues with no WQS or AU
stationTableProblems <- read_csv('data/RegionalResults_missing_AU_WQS.csv') 


# Query VAHUC6's By Selectize arguments
#the_data <- assessmentLayer
#unique(assessmentLayer$ASSESS_REG)
region_filter <- filter(assessmentLayer, ASSESS_REG %in% 'BRRO')
#unique(region_filter$Basin)
basin_filter <- filter(region_filter, Basin %in% 'Roanoke River Basin')
#unique(basin_filter$VAHU6)
huc6_filter <- filter(region_filter, VAHU6 %in% 'RD01')
AUs <- st_intersection(st_zm(regionalAUs), huc6_filter)



conventionals_HUC <- filter(conventionals, Huc6_Vahu6 %in% huc6_filter$VAHU6) %>%
    left_join(dplyr::select(stationTable, FDT_STA_ID, SEC, CLASS, SPSTDS, ID305B_1, ID305B_2, ID305B_3,
                            STATION_TYPE_1, STATION_TYPE_2, STATION_TYPE_3), by='FDT_STA_ID')
# AU selection drop down
unique(conventionals_HUC$ID305B_1) # if any NA then not correct stationTable info for AU
AUSelection <- unique(conventionals_HUC$ID305B_1)[1]
selectedAU <- filter(regionalAUs, ID305B %in% AUSelection) %>% st_set_geometry(NULL) %>% as.data.frame()
stationSelection_ <- filter(conventionals_HUC, ID305B_1 %in% AUSelection | 
                              ID305B_2 %in% AUSelection | 
                              ID305B_2 %in% AUSelection) %>%
    distinct(FDT_STA_ID)
stationSelection <- unique(stationSelection_$FDT_STA_ID)[1]    
AUData <- filter(conventionals_HUC, ID305B_1 %in% AUSelection | ID305B_2 %in% AUSelection |
                   ID305B_2 %in% AUSelection) %>% 
      left_join(WQSvalues, by = 'CLASS')
stationData <- filter(AUData, FDT_STA_ID %in% stationSelection) 


# Station Info Table
stationInfo <- filter(stationTable, FDT_STA_ID == stationSelection) %>% 
    select(FDT_STA_ID:STA_CBP_NAME, `Point Unique Identifier`:Shape_Leng ) %>%
    t() %>% as.data.frame() %>% rename(`Station and WQS Information` = 1)
