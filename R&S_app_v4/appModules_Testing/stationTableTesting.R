source('testingDataset.R')
monStationTemplate <- read_excel('data/tbl_ir_mon_stations_template.xlsx') # from X:\2018_Assessment\StationsDatabase\VRO
conventionals <- read_csv('data/final2020data/CEDSWQM_2020_IR_DATA-CONVENTIONALS_20190305.csv') %>%
  filter(!is.na(Latitude)|!is.na(Longitude)) %>% # remove sites without coordinates
  rename("FDT_TEMP_CELCIUS"  ="TEMPERATURE_00010_DEGREES CENTIGRADE",
         "FDT_TEMP_CELCIUS_RMK" = "FDT_TEMP_CELCIUS_RMK",  
         "FDT_FIELD_PH" = "pH_00400_STD_UNITS" ,          
         "FDT_FIELD_PH_RMK"  ="FDT_FIELD_PH_RMK", 
         "DO" =  "DO_mg/L",       
         "DO_RMK"  ="DO_RMK",    
         "FDT_SPECIFIC_CONDUCTANCE"="SPECIFIC_CONDUCTANCE_00095_UMHOS/CM @ 25C",    
         "FDT_SPECIFIC_CONDUCTANCE_RMK" ="FDT_SPECIFIC_CONDUCTANCE_RMK" ,
         "FDT_SALINITY" = "SALINITY_00480_PPT" ,            
         "FDT_SALINITY_RMK"  ="FDT_SALINITY_RMK",  
         "NITROGEN" = "NITROGEN_mg/L" ,                    
         "AMMONIA" ="AMMONIA_mg/L",
         "PHOSPHORUS" =  "PHOSPHORUS_mg/L",
         "FECAL_COLI" = "FECAL_COLIFORM_31616_NO/100mL" ,
         "E.COLI" = "ECOLI_CFU/100mL",                       
         "ENTEROCOCCI" =  "ENTEROCOCCI_31649_NO/100mL",
         "CHLOROPHYLL" ="CHLOROPHYLL_32211_ug/L",                
         "SSC" ="SSC-TOTAL_00530_mg/L" , 
         "NITRATE" ="NITRATE_mg/L", 
         "CHLORIDE" ="CHLORIDE_mg/L",
         "SULFATE_TOTAL" ="SULFATE_TOTAL_00945_mg/L",              
         "SULFATE_DISS" ="SULFATE_DISSOLVED_00946_mg/L")
conventionals$FDT_DATE_TIME2 <- as.POSIXct(conventionals$FDT_DATE_TIME, format="%m/%d/%Y %H:%M")



#stationTable <- read_csv('processedStationData/draft2020data/RegionalResultsRiverine_BRRO_NOMISSINGDATA.csv') #RegionalResultsRiverine_BRRO.csv')

stationTable <- read_csv('processedStationData/RegionalResultsRiverine_SWRO_updated.csv') %>%
  as_tibble() %>%
  dplyr::rename(`Point Unique Identifier` ="Point.Unique.Identifier", `Buffer Distance` = "Buffer.Distance") %>%
  mutate(VAHU6 = ifelse(is.na(VAHU6), Huc6_Vahu6, VAHU6)) 
  

conventionals_HUC<-  filter(conventionals, Huc6_Vahu6 %in% 'NE01') %>% #huc6_filter()$VAHU6) %>%
  left_join(dplyr::select(stationTable,#stationTable(), 
                          FDT_STA_ID, SEC, CLASS, SPSTDS, ID305B_1, ID305B_2, ID305B_3,
                          STATION_TYPE_1, STATION_TYPE_2, STATION_TYPE_3, Basin
  ), by='FDT_STA_ID')

WCmetals <- read_csv('data/final2020data/CEDSWQM_2020_IR_DATA-WATER_METALS_VALUES_20190207_EVJ.csv')
Smetals <- read_excel('data/final2020data/CEDSWQM_2020_IR_DATA-CEDSWQM_SEDIMENT_20190213.xlsx') %>%
  dplyr::select(FDT_STA_ID:ZINC..70, COMMENT..89)
names(Smetals) <- gsub( "[..].*", "", names(Smetals)) # remove anything after .. in name


VSCI <- read_excel('data/Family Metrics VSCI Calculation.xlsx')%>%
  filter(RepNum == 1 & Target_Count == 110 &
           CollDate >= assessmentPeriod[1] )
VCPMI <- read_excel('data/Family Metrics - CPMI Combined.xlsx')%>%
  filter(RepNum == 1 & Target_Count == 110 &
           CollDate >= assessmentPeriod[1] )


x <-filter(conventionals_HUC, FDT_STA_ID == '9-BHO017.70') %>%#'2-RED000.16') %>%#'2-HUO005.87')%>% #'2-OGL000.23')%>%#'2-JMS279.41') %>%#'2-BKL000.15') %>%##'2-JKS033.06')%>%  %in% '2-JKS028.69') %>%
  left_join(WQSvalues, by = 'CLASS') #'2-JMS279.41')#
x <- filter(conventionals_HUC, FDT_STA_ID %in% '2-DCK003.94')%>% 
  left_join(WQSvalues, by = 'CLASS')

x <-filter(conventionals_HUC, Huc6_Vahu6 =='JU17') %>%#'2-BKL000.15') %>%##'2-JKS033.06')%>%  %in% '2-JKS028.69') %>%
  left_join(WQSvalues, by = 'CLASS') #'2-JMS279.41')#

concatinateUnique <- function(stuff){
  if(length(stuff)==1){
    if(is.na(stuff)){return(NA)
  }else{
    return(paste(unique(stuff), collapse= ', ')) }
  } 
  if(length(stuff) > 1){return(paste(unique(stuff), collapse= ', '))}
}




StationTableResults <- cbind(#StationTableStartingData(x), 
  tempExceedances(x), DOExceedances_Min(x), pHExceedances(x),
  bacteriaExceedances_OLD(bacteria_Assessment_OLD(x, 'E.COLI', 126, 235),'E.COLI') %>% 
    dplyr::rename('ECOLI_VIO' = 'E.COLI_VIO', 'ECOLI_SAMP'='E.COLI_SAMP', 'ECOLI_STAT'='E.COLI_STAT'),
  # new bacteria exceedance stats
  bacteriaExceedances_NEW(x,'E.COLI', 10, 410, 126),
  bacteriaExceedances_OLD(bacteria_Assessment_OLD(x, 'ENTEROCOCCI', 35, 104),'ENTEROCOCCI') %>% 
    dplyr::rename('ENTER_VIO' = 'ENTEROCOCCI_VIO', 'ENTER_SAMP'='ENTEROCOCCI_SAMP', 'ENTER_STAT'='ENTEROCOCCI_STAT'),
  #new bacteria exceedance stats
  bacteriaExceedances_NEW(x,'ENTEROCOCCI',10, 35, 104),
                             
                             metalsExceedances(filter(WCmetals, FDT_STA_ID %in% x$FDT_STA_ID) %>% 
                                                 dplyr::select(`ANTIMONY HUMAN HEALTH PWS`:`ZINC ALL OTHER SURFACE WATERS`), 'WAT_MET'),
                             acuteNH3exceedance(x) %>% 
                               dplyr::select(AcuteAmmonia_VIO, AcuteAmmonia_STAT) %>% 
                               dplyr::rename('WAT_TOX_VIO' ='AcuteAmmonia_VIO','WAT_TOX_STAT' = 'AcuteAmmonia_STAT'),#data.frame(WAT_TOX_VIO='Not Analyzed by App', WAT_TOX_STAT='Not Analyzed by App'),# Placeholder for water toxics
                             
                            # Placeholder for water toxics
                             metalsExceedances(filter(Smetals, FDT_STA_ID %in% x$FDT_STA_ID) %>% 
                                                 dplyr::select(ARSENIC:ZINC), 'SED_MET'),
                             
                             data.frame(SED_TOX_VIO='Not Analyzed by App', SED_TOX_STAT='Not Analyzed by App'),# Placeholder for sediment toxics
                             data.frame(FISH_MET_VIO='Not Analyzed by App', FISH_MET_STAT='Not Analyzed by App'), # Placeholder for fish metals
                             data.frame(FISH_TOX_VIO='Not Analyzed by App', FISH_TOX_STAT='Not Analyzed by App'),# Placeholder for fish toxics
                             benthicAssessment(x,conventionals_sf,VSCI,VCPMI),
                             countTP(x),
                             countchla(x),
                             #data.frame(NUT_TP_VIO='Not Analyzed by App',NUT_TP_SAMP= 'Not Analyzed by App', NUT_TP_STAT='Not Analyzed by App'), # Placeholder bc only applies to Lakes or Cbay
                             #data.frame(NUT_CHLA_VIO='Not Analyzed by App', NUT_CHLA_SAMP='Not Analyzed by App', NUT_CHLA_STAT='Not Analyzed by App'),# Placeholder bc only applies to Lakes or Cbay
                             data.frame(COMMENTS= 'Not Analyzed by App') # Assessor Comments
                             )%>%
  dplyr::select(-ends_with('exceedanceRate'))

x1 <- filter(conventionals_sf, FDT_STA_ID %in% x$FDT_STA_ID)#'2-JKS033.06') #'2-JMS279.41')##
if(nrow(x1) >0){
  x2 <- benthicResultMetrics(x1,VSCI,VCPMI)$data
  if (!is.na(x2)){return(data.frame(BENTHIC_STAT='Review'))
  }else{return(data.frame(BENTHIC_STAT=NA))}
} else{return(data.frame(BENTHIC_STAT=NA))}
z <- cbind(data.frame(StationID = unique(x$FDT_STA_ID)), StationTableResults) 

z <- editData::editData(z)

datatable(z, extensions = 'Buttons', escape=F, rownames = F, editable = TRUE,
          options= list(scrollX = TRUE, pageLength = nrow(z),
                        # hide certain columns
                        columnDefs = list(list(targets = 6, visible = FALSE)),
                        dom='Bt')) %>%#, buttons=list('copy',
                                #               list(extend='csv',filename=paste('AssessmentResults_',paste(assessmentCycle,input$stationSelection, collapse = "_"),Sys.Date(),sep='')),
                                #               list(extend='excel',filename=paste('AssessmentResults_',paste(assessmentCycle,input$stationSelection, collapse = "_"),Sys.Date(),sep=''))))) %>% 
  # format cell background color based on hidden column
  formatStyle(c('TEMP_SAMP','TEMP_VIO','TEMP_STAT'), 'TEMP_STAT', backgroundColor = styleEqual(c('Review', '10.5% Exceedance'), c('yellow','red'))) %>%
  formatStyle(c('DO_SAMP','DO_VIO','DO_STAT'), 'DO_STAT', backgroundColor = styleEqual(c('Review','10.5% Exceedance'), c('yellow','red'))) %>%
  formatStyle(c('PH_SAMP','PH_VIO','PH_STAT'), 'PH_STAT', backgroundColor = styleEqual(c('Review','10.5% Exceedance'), c('yellow','red')))%>%
  formatStyle(c('ECOLI_SAMP','ECOLI_VIO','ECOLI_STAT'), 'ECOLI_STAT', backgroundColor = styleEqual(c('Review','10.5% Exceedance'), c('yellow','red'))) %>%
  formatStyle(c('ENTER_SAMP','ENTER_VIO','ENTER_STAT'), 'ENTER_STAT', backgroundColor = styleEqual(c('Review','10.5% Exceedance'), c('yellow','red'))) %>%
  formatStyle(c('WAT_MET_VIO','WAT_MET_STAT'), 'WAT_MET_STAT', backgroundColor = styleEqual(c('Review','10.5% Exceedance'), c('yellow','red'))) %>%
  formatStyle(c('SED_MET_VIO','SED_MET_STAT'), 'SED_MET_STAT', backgroundColor = styleEqual(c('Review','10.5% Exceedance'), c('yellow','red')))
