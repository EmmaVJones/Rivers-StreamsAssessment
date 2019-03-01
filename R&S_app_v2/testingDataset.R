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

WQSvalues <- tibble(CLASS = c('I',"II","II","III","IV","V","VI","VII"),
                    `Description Of Waters` = c('Open Ocean', 'Tidal Waters in the Chowan Basin and the Atlantic Ocean Basin',
                                                'Tidal Waters in the Chesapeake Bay and its tidal tributaries',
                                                'Nontidal Waters (Coastal and Piedmont Zone)','Mountainous Zone Waters',
                                                'Stockable Trout Waters','Natural Trout Waters','Swamp Waters'),
                    `Dissolved Oxygen Min (mg/L)` = c(5,4,NA,4,4,5,6,NA),
                    `Dissolved Oxygen Daily Avg (mg/L)` = c(NA,5,NA,5,5,6,7,NA),
                    `pH Min` = c(6,6,6.0,6.0,6.0,6.0,6.0,3.7),
                    `pH Max` = c(9.0,9.0,9.0,9.0,9.0,9.0,9.0,8.0),
                    `Max Temperature (C)` = c(NA, NA, NA, 32, 31, 21, 20, NA))



conventionals <- read_csv('C:/HardDriveBackup/R/GitHub/Rivers-StreamsAssessment/data/draft2020data/CEDSWQM_2020_IR_DATA-CONVENTIONALS_20190213.csv') %>%
  filter(!is.na(Latitude)|!is.na(Longitude)) %>% # remove sites without coordinates
  rename('DO' = "DO_mg/L", "NITROGEN" = "NITROGEN_mg/L",  "AMMONIA" = "AMMONIA_mg/L" ,
         #"NH3_DISS" = , "NH3_TOTAL"  , 
         "PHOSPHORUS"= "PHOSPHORUS_mg/L" , "FECAL_COLI" = 'STORET_31616', "E.COLI" = 'ECOLI_CFU/100mL', 
         "ENTEROCOCCI" = 'STORET_31649', "CHLOROPHYLL" = 'STORET_32211', "SSC" = "STORET_SSC-TOTAL" , 
         "SSC_RMK" = "RMK_SSC-TOTAL" , "NITRATE" = "NITRATE_mg/L",  "CHLORIDE" = "CHLORIDE_mg/L" , 
         "SULFATE_TOTAL" = "SULFATE_mg/L",   "SULFATE_DISS" = 'STORET_00946')
conventionals$FDT_DATE_TIME2 <- as.POSIXct(conventionals$FDT_DATE_TIME, format="%m/%d/%Y %H:%M")

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

