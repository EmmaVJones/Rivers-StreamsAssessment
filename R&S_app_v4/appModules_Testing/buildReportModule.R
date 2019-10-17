source('testingDataset.R')
monStationTemplate <- read_excel('data/tbl_ir_mon_stations_template.xlsx') # from X:\2018_Assessment\StationsDatabase\VRO

# Single station data ----------------------------------------------------------------------
AUData <- filter(conventionals_HUC, ID305B_1 %in% 'VAW-H01R_JMS04A00' | 
                   ID305B_2 %in% 'VAW-H01R_JMS04A00' | 
                   ID305B_2 %in% 'VAW-H01R_JMS04A00')%>% 
  left_join(WQSvalues, by = 'CLASS')

x <-filter(AUData, FDT_STA_ID %in% '2-JMS279.41') 
#------------------------------------------------------------------------------------------

#parameterDataset <- pH
#parameter <- 'PH'

quickStats <- function(parameterDataset, parameter){
  results <- data.frame(SAMP = nrow(parameterDataset),
                        VIO = nrow(filter(parameterDataset, exceeds == TRUE))) %>%
    mutate(exceedanceRate = (VIO/SAMP)*100)
  
  if(results$exceedanceRate > 10.5 & results$SAMP > 10){outcome <- 'Review'}
  if(results$exceedanceRate < 10.5 & results$SAMP > 10){outcome <- 'S'}
  if(results$VIO >= 2 & results$SAMP < 10){outcome <- 'Review'}
  if(results$VIO < 2 & results$SAMP < 10){outcome <- 'Review'}
  
  results <- mutate(results, STAT = outcome)
  names(results) <- c(paste(parameter,names(results)[1], sep = '_'),
                      paste(parameter,names(results)[2], sep = '_'),
                      paste(parameter,names(results)[3], sep = '_'),
                      paste(parameter,names(results)[4], sep = '_'))
  #rename based on parameter entered
  return(results)
}


#Max Temperature Exceedance Function
tempExceedances <- function(x){
  temp <- dplyr::select(x,FDT_DATE_TIME,FDT_TEMP_CELCIUS, `Max Temperature (C)`)%>% # Just get relevant columns, 
    filter(!is.na(FDT_TEMP_CELCIUS))%>% #get rid of NA's
    rename(parameter = !!names(.[2]), limit = !!names(.[3])) %>% # rename columns to make functions easier to apply
    mutate(exceeds = ifelse(parameter > limit, T, F)) # Identify where above max Temperature, 
    
  quickStats(temp, 'TEMP')
}
#tempExceedances(x)

pHExceedances <- function(x){
  pH <- dplyr::select(x,FDT_DATE_TIME,FDT_FIELD_PH,`pH Min`,`pH Max`)%>% # Just get relevant columns, 
    filter(!is.na(FDT_FIELD_PH))%>% #get rid of NA's
    rowwise() %>% mutate(interval=findInterval(FDT_FIELD_PH,c(`pH Min`,`pH Max`)))%>% # Identify where pH outside of assessment range
    ungroup()%>%
    mutate(exceeds=ifelse(interval == 1, F, T)) # Highlight where pH doesn't fall into assessment range
    
  quickStats(pH, 'PH')
}
#pHExceedances(x)


DOExceedances_Min <- function(x){
  DO <- dplyr::select(x,FDT_DATE_TIME2,DO,`Dissolved Oxygen Min (mg/L)`)%>% # Just get relevant columns, 
    filter(!is.na(DO)) %>% 
    rename(parameter = !!names(.[2]), limit = !!names(.[3])) %>% # rename columns to make functions easier to apply
    mutate(exceeds = ifelse(parameter < limit, T, F)) # Identify where below min DO 
   
  quickStats(DO, 'DO')
}
#DOExceedances_Min(x)
