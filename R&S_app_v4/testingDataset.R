# Run in R 3.5.1

library(shiny)
library(shinyjs)
library(leaflet)
library(mapview)
library(tidyverse)
library(sf)
library(plotly)
#library(raster)
library(DT)
library(readxl)
library(RColorBrewer)
#library(FSA)
library(lubridate)
library(magrittr)

WQSvalues <- tibble(CLASS_BASIN = c('I',"II","II_7","III","IV","V","VI","VII"),
                    CLASS = c('I',"II","II","III","IV","V","VI","VII"),
                    `Description Of Waters` = c('Open Ocean', 'Tidal Waters in the Chowan Basin and the Atlantic Ocean Basin',
                                                'Tidal Waters in the Chesapeake Bay and its tidal tributaries',
                                                'Nontidal Waters (Coastal and Piedmont Zone)','Mountainous Zone Waters',
                                                'Stockable Trout Waters','Natural Trout Waters','Swamp Waters'),
                    `Dissolved Oxygen Min (mg/L)` = c(5,4,NA,4,4,5,6,NA),
                    `Dissolved Oxygen Daily Avg (mg/L)` = c(NA,5,NA,5,5,6,7,NA),
                    `pH Min` = c(6,6,6.0,6.0,6.0,6.0,6.0,3.7),
                    `pH Max` = c(9.0,9.0,9.0,9.0,9.0,9.0,9.0,8.0),
                    `Max Temperature (C)` = c(NA, NA, NA, 32, 31, 21, 20, NA))



#conventionals <- read_csv('data/final2020data/CEDSWQM_2020_IR_DATA-CONVENTIONALS_20190305.csv') %>%
#conventionals <- read_csv('data/final2020data/conventionals_final2020_citmon1.csv') %>%
conventionals <- read_csv('data/final2020data/conventionals_final2020_citmonNonAgency.csv') %>%
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
###### CHANGED 5/9/19
#conventionals$FDT_DATE_TIME2 <- as.POSIXct(conventionals$FDT_DATE_TIME, format="%m/%d/%Y %H:%M") # comment out if using with citmon data








stationTable <- 
  ## TRO
  #read_csv('processedStationData/RegionalResultsRiverine_TROFINAL.csv') %>%
  
  # BRRO WITH citmon
  #read_csv('processedStationData/RegionalResultsRiverine_BRROCitMon.csv') %>%
  
  # BRRO with citmon and nonAgency
  read_csv('processedStationData/RegionalResultsRiverine_BRROCitMonNonAgencyFINAL.csv') %>%
  
  ## BRRO no citmon
  #read_csv('processedStationData/RegionalResultsRiverine_BRROFINAL.csv') %>%
  
  # SWRO
  #read_csv('processedStationData/RegionalResultsRiverine_SWRO_updated.csv') %>%
  
  
  # fix periods in column names from excel
  as_tibble() %>%
  dplyr::rename(`Point Unique Identifier` ="Point.Unique.Identifier", `Buffer Distance` = "Buffer.Distance") %>%
  mutate(VAHU6 = ifelse(is.na(VAHU6), Huc6_Vahu6, VAHU6)) 



stationTable <- mutate(stationTable, CLASS_BASIN = paste(CLASS,substr(stationTable$BASIN, 1,1), sep="_")) %>%
  mutate(CLASS_BASIN = ifelse(CLASS_BASIN == 'II_7', "II_7", CLASS))




conventionals_HUC<- left_join(conventionals, dplyr::select(stationTable, FDT_STA_ID, SEC, CLASS_BASIN, CLASS, 
                                                           SPSTDS, ID305B_1, ID305B_2, ID305B_3,
                                                           STATION_TYPE_1, STATION_TYPE_2, STATION_TYPE_3, Basin), by='FDT_STA_ID') %>%
  left_join(WQSvalues, by = 'CLASS_BASIN') %>%
  dplyr::select(-c(CLASS.y,CLASS_BASIN)) %>%
  rename('CLASS' = 'CLASS.x')


#stationTable <- read_csv('data/BRRO_Sites_AU_WQS.csv')


#conventionals_HUC<- left_join(conventionals,dplyr::select(stationTable, FDT_STA_ID, SEC, CLASS, SPSTDS, ID305B_1, ID305B_2, ID305B_3), by='FDT_STA_ID')
# Better to leave this one out and run in build Script and then remove before shiny component
#AUData <- filter(conventionals_HUC, ID305B_1 %in% 'VAW-H01R_JMS04A00' | 
#                   ID305B_2 %in% 'VAW-H01R_JMS04A00' | 
#                   ID305B_2 %in% 'VAW-H01R_JMS04A00')%>% 
#  left_join(WQSvalues, by = 'CLASS')
#x <-filter(AUData, FDT_STA_ID %in% '2-JMS279.41') 



# Super Assessment function
assessmentDetermination <- function(parameterDF,parameterAssessmentDF,parameter,use){
  
  results <- data.frame(nSamples = nrow(parameterDF),nExceedance = nrow(parameterAssessmentDF))%>%
    mutate(exceedanceRate = (nExceedance/nSamples)*100)
  
  if(results$exceedanceRate > 10.5 & results$nSamples > 10){outcome <- paste('Water impaired for',parameter)}
  if(results$exceedanceRate < 10.5 & results$nSamples > 10){outcome <- paste('Water not impaired for',parameter)}
  if(results$nExceedance >= 2 & results$nSamples < 10){outcome <- paste('Water impaired for',parameter)}
  if(results$nExceedance < 2 & results$nSamples < 10){outcome <- paste('Water not impaired for',parameter)}
  
  results <- mutate(results,Assessment=outcome, Use= use)
  return(results)
}
#assessmentDetermination(temp,temp_Assess,"temperature","Aquatic Life")

