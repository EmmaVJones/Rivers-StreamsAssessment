b <- filter(conventionals_D_Region, FDT_STA_ID %in% unique(stationTableLake$STATION_ID))

notThere <- b$FDT_STA_ID[!(b$FDT_STA_ID %in% reservoirStationsWQS$FDT_STA_ID )]

addMe <- filter(conventionals_D_Region_AU, FDT_STA_ID %in% notThere)




# Reorder columns in reservoirStationsWQS to match RegionalResults to allow row binding
reservoirStationsWQS1 <- mutate(reservoirStationsWQS, Point.Unique.Identifier=NA, Buffer.Distance=NA, StreamType=NA) %>%
  select(FDT_STA_ID:COMMENTS, Point.Unique.Identifier, Buffer.Distance, OBJECTID:FCode, BASIN, WQS_COMMEN, 
         WATER_NAME, SEC, CLASS, SPSTDS, SECTION_DE, Basin_Code, PWS, Trout, Edit_Date, StreamType, 
         Tier_III, Backlog, Shape_Leng)%>%
  #st_set_geometry(NULL) %>%
  bind_rows(addMe %>% st_set_geometry(NULL))

reservoirStationsWQS1$FDT_STA_ID[duplicated(reservoirStationsWQS1$FDT_STA_ID)]
# fix pigg station
reservoirStationsWQS1 <- reservoirStationsWQS1[-65,]

# Find all stations that have lake AUs
#lakeStations <- filter(reservoirStationsWQS, STATION_TYPE_1 == "L" | 
#                         STATION_TYPE_2 == "L" | STATION_TYPE_3 == "L")
# Save both for app use
write.csv(reservoirStationsWQS1, paste('processedStationData/final2020data/RegionalResultsLake_',
                              unique(assessmentLayerSelection$ASSESS_REG),'.csv',sep=''), row.names=FALSE)
