source('testingDataset.R')
monStationTemplate <- read_excel('data/tbl_ir_mon_stations_template.xlsx') # from X:\2018_Assessment\StationsDatabase\VRO
conventionals <- read_csv('C:/HardDriveBackup/R/GitHub/Rivers-StreamsAssessment/data/draft2020data/CEDSWQM_2020_IR_DATA-CONVENTIONALS_20190213.csv') %>%
  filter(!is.na(Latitude)|!is.na(Longitude)) %>% # remove sites without coordinates
  rename('DO' = "DO_mg/L", "NITROGEN" = "NITROGEN_mg/L",  "AMMONIA" = "AMMONIA_mg/L" ,
         #"NH3_DISS" = , "NH3_TOTAL"  , 
         "PHOSPHORUS"= "PHOSPHORUS_mg/L" , "FECAL_COLI" = 'STORET_31616', "E.COLI" = 'ECOLI_CFU/100mL', 
         "ENTEROCOCCI" = 'STORET_31649', "CHLOROPHYLL" = 'STORET_32211', "SSC" = "STORET_SSC-TOTAL" , 
         "SSC_RMK" = "RMK_SSC-TOTAL" , "NITRATE" = "NITRATE_mg/L",  "CHLORIDE" = "CHLORIDE_mg/L" , 
         "SULFATE_TOTAL" = "SULFATE_mg/L",   "SULFATE_DISS" = 'STORET_00946')
conventionals$FDT_DATE_TIME2 <- as.POSIXct(conventionals$FDT_DATE_TIME, format="%m/%d/%Y %H:%M")




stationTable <- read_csv('data/draft2020data/RegionalResultsRiverine_BRRO.csv')
conventionals_HUC<-  #filter(conventionals, Huc6_Vahu6 %in% 'JM01') %>% #huc6_filter()$VAHU6) %>%
  left_join(conventionals, dplyr::select(stationTable,#stationTable(), 
                                         FDT_STA_ID, SEC, CLASS, SPSTDS, ID305B_1, ID305B_2, ID305B_3,
                                         STATION_TYPE_1, STATION_TYPE_2, STATION_TYPE_3, Basin
  ), by='FDT_STA_ID')

WCmetals <- read_csv('data/draft2020data/CEDSWQM_2020_IR_DATA-WATER_METALS_VALUES_20190207_EVJ.csv')
Smetals <- read_excel('data/draft2020data/CEDSWQM_2020_IR_DATA-CEDSWQM_SEDIMENT_20190213.xlsx') %>%
  dplyr::select(FDT_STA_ID:ZINC..70, COMMENT..89)
names(Smetals) <- gsub( "[..].*", "", names(Smetals)) # remove anything after .. in name


VSCI <- read_excel('data/Family Metrics VSCI Calculation.xlsx')%>%
  filter(RepNum == 1 & Target_Count == 110 &
           CollDate >= assessmentPeriod[1] )
VCPMI <- read_excel('data/Family Metrics - CPMI Combined.xlsx')%>%
  filter(RepNum == 1 & Target_Count == 110 &
           CollDate >= assessmentPeriod[1] )


x <-filter(conventionals_HUC, FDT_STA_ID %in% '2-JMS279.41') %>%#'2-BKL000.15') %>%##'2-JKS033.06')%>% 
  left_join(WQSvalues, by = 'CLASS') #'2-JMS279.41')#
x2 <-filter(conventionals_HUC, FDT_STA_ID %in% '2-JKS028.69') %>%#'2-BKL000.15') %>%##'2-JKS033.06')%>% 
  left_join(WQSvalues, by = 'CLASS') #'2-JMS279.41')#


PWSconcat <- cbind(data.frame(STATION_ID = unique(x$FDT_STA_ID)),
               nitratePWS(x),
               chloridePWS(x),
               TSulfatePWS(x)) %>%
  dplyr::select(-ends_with('exceedanceRate'))
               
datatable(PWSconcat, escape=F, rownames = F, editable = TRUE,
          options= list(scrollX = TRUE, pageLength = nrow(PWSconcat),
                        dom='t')) %>% 
  # format cell background color based on hidden column
  formatStyle(c("PWS_Acute_Nitrate_VIO","PWS_Acute_Nitrate_SAMP","PWS_Acute_Nitrate_STAT"), "PWS_Acute_Nitrate_STAT", backgroundColor = styleEqual(c('Review'), c('red'))) %>%
  formatStyle(c("PWS_Acute_Chloride_VIO","PWS_Acute_Chloride_SAMP","PWS_Acute_Chloride_STAT"), "PWS_Acute_Chloride_STAT", backgroundColor = styleEqual(c('Review'), c('red'))) %>%
  formatStyle(c("PWS_Acute_Total_Sulfate_VIO","PWS_Acute_Total_Sulfate_SAMP","PWS_Acute_Total_Sulfate_STAT"), "PWS_Acute_Total_Sulfate_STAT", backgroundColor = styleEqual(c('Review'), c('red'))) 
  