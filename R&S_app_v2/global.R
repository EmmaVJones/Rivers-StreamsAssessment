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
library(FSA)
library(lubridate)
library(magrittr)

# Bring in modules
source('appModules/multipleDependentSelectizeArguments.R')
source('newBacteriaStandard_workingUpdatedRecSeason.R') # version with 2/3 samples in April-Oct
#source('newBacteriaStandard_working.R') # version with April-Oct +/- 30 days
#conventionals_sf <- readRDS('data/conventionals_sf.RDS')
conventionals_sf <- readRDS('data/conventionals_sf_draft2020.RDS')

monStationTemplate <- read_excel('data/tbl_ir_mon_stations_template.xlsx') # from X:\2018_Assessment\StationsDatabase\VRO

modulesToReadIn <- c('temperature','pH','DO','SpCond','Salinity','TN','Ecoli','chlA','Enteroccoci', 'TP','sulfate',
                     'Ammonia', 'Chloride', 'Nitrate','metals', 'fecalColiform','SSC','Benthics')
for (i in 1:length(modulesToReadIn)){
  source(paste('appModules/',modulesToReadIn[i],'Module.R',sep=''))
}


# Loading screen
load_data <- function() {
  Sys.sleep(2)
  shinyjs::hide("loading_page")
  shinyjs::show("main_content")
}

#####################################   UPDATE EACH NEW TOOL REBUILD #############################################
# Establish Assessment Period 
assessmentPeriod <- as.POSIXct(c("2013-01-01 00:00:00 UTC","2018-12-31 23:59:59 UTC"),tz='UTC')
assessmentCycle <- '2020'
##################################################################################################################


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



withinAssessmentPeriod <- function(x){
  if((range(unique(x$FDT_DATE_TIME2))[1] < assessmentPeriod[1]) | 
     (range(unique(x$FDT_DATE_TIME2))[2] > assessmentPeriod[2])){
    print('Data included that falls outside of assessment period. Review input data.')
  }else{print('All input data falls within the assessment period.')}
}

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


# Station Table building functions

concatinateUnique <- function(stuff){
  if(length(stuff)==1){
    if(is.na(stuff)){return(NA)
    }else{
      return(paste(unique(stuff), collapse= ', ')) }
  } 
  if(length(stuff) > 1){return(paste(unique(stuff), collapse= ', '))}
}

changeDEQRegionName <- function(stuff){
  # have to do this bc different places in conventionals report the assessment region over sample region
  if(length(stuff) == 1){
    if(stuff == "Valley"){return('VRO')}
    if(stuff == "Northern"){return('NRO')}
    if(stuff == "Piedmont"){return('PRO')}
    if(stuff == "Blue Ridge"){return('BRRO')}
    if(stuff == "Tidewater"){return('TRO')}
    if(stuff == "Southwest" ){return('SWRO')}
    if(is.na(stuff))return(NA)
  } else {return(concatinateUnique(stuff))}
}


StationTableStartingData <- function(x){
  data.frame(ID305B_1= concatinateUnique(x$ID305B_1), ID305B_2= concatinateUnique(x$ID305B_2), ID305B_3= concatinateUnique(x$ID305B_3),
             DEPTH = concatinateUnique(x$FDT_DEPTH_DESC), STATION_ID = concatinateUnique(x$FDT_STA_ID), REGION = changeDEQRegionName(concatinateUnique(x$Deq_Region)), 
             STATION_TYPE_1= concatinateUnique(x$STATION_TYPE_1), STATION_TYPE_2=concatinateUnique(x$STATION_TYPE_2), 
             STATION_TYPE_3= concatinateUnique(x$STATION_TYPE_3), STATION_LAT = concatinateUnique(x$Latitude), 
             STATION_LON = concatinateUnique(x$Longitude), WATERSHED_ID= concatinateUnique(x$ID305B_1),
             VAHU6 = concatinateUnique(x$Huc6_Vahu6) )
  # Should be this but issues with shiny application of function          
  #WATERSHED_ID= substr(strsplit(as.character(concatinateUnique(x$ID305B_1), '-'))[[1]][2], 1, 3), 
  
}

#dataFrame <-ammonia
#dateTimeColumn <- 'FDT_DATE_TIME2'
#nYears <- 3
#consecutive <- TRUE
lastXyears <- function(dataFrame, dateTimeColumn, nYears, consecutive){ # not bombproof but works for nYears=3
  uniqueYears <- dplyr::select(dataFrame, dateTimeColumn) %>% 
    mutate(sampleYear = lubridate::year(get(dateTimeColumn))) %>% 
    distinct(sampleYear) %>% 
    dplyr::pull()
  #uniqueYears <- c(2011, 2012, 2013, 2014, 2015, 2018)
  #uniqueYears <- c(2013, 2014, 2015, 2017)
  #uniqueYears <- c(2013, 2014, 2015, 2017, 2018)
  nYearsConversion <- nYears-1
  
  if(length(uniqueYears) > nYearsConversion){
    years <- sort(uniqueYears)[(length(uniqueYears)-nYearsConversion):length(uniqueYears)]
  } else { 
    years <- sort(uniqueYears)}
  
  if(consecutive == TRUE){
    recent <- years[which(years > max(years)-nYears)]
    return(recent)
    
  } else {return(years)}
}

#lastXyears(dataFrame, dateTimeColumn, 7, TRUE)





#### Temperature Assessment Functions ---------------------------------------------------------------------------------------------------

#Max Temperature Exceedance Function
temp_Assessment <- function(x){
  temp <- dplyr::select(x,FDT_DATE_TIME,FDT_TEMP_CELCIUS, `Max Temperature (C)`)%>% # Just get relevant columns, 
    filter(!is.na(FDT_TEMP_CELCIUS))%>% #get rid of NA's
    mutate(TemperatureExceedance=ifelse(FDT_TEMP_CELCIUS > `Max Temperature (C)`,T,F))%>% # Identify where above max Temperature, 
    filter(TemperatureExceedance==TRUE) # Only return temp measures above threshold
  return(temp)
}

# Exceedance Rate Temperature
exceedance_temp <- function(x){
  temp <- dplyr::select(x,FDT_DATE_TIME,FDT_TEMP_CELCIUS,`Max Temperature (C)`)%>% # Just get relevant columns, 
    filter(!is.na(FDT_TEMP_CELCIUS)) #get rid of NA's
  temp_Assess <- temp_Assessment(x)
  
  temp_results <- assessmentDetermination(temp,temp_Assess,"temperature","Aquatic Life")
  return(temp_results)
}


#### pH Assessment Functions ---------------------------------------------------------------------------------------------------

pH_rangeAssessment <- function(x){
  pH <- dplyr::select(x,FDT_STA_ID,FDT_DATE_TIME,FDT_DEPTH,FDT_FIELD_PH,`pH Min`,`pH Max`)%>% # Just get relevant columns, 
    filter(!is.na(FDT_FIELD_PH))%>% #get rid of NA's
    rowwise()%>% mutate(interval=findInterval(FDT_FIELD_PH,c(`pH Min`,`pH Max`)))%>% # Identify where pH outside of assessment range
    ungroup()%>%
    mutate(pHrange=ifelse(interval==1,T,F))%>% # Highlight where pH doesn't fall into assessment range
    filter(pHrange==FALSE)%>% # Only return pH measures outside of assessement range
    dplyr::select(-c(interval,pHrange)) # Don't show user interval column, could be confusing to them, T/F in pHrange column sufficient
  return(pH)
}


exceedance_pH <- function(x){
  pH <- dplyr::select(x,FDT_STA_ID,FDT_DATE_TIME,FDT_DEPTH,FDT_FIELD_PH,`pH Min`,`pH Max`)%>% # Just get relavent columns, 
    filter(!is.na(FDT_FIELD_PH)) #get rid of NA's
  pH_rangeAssess <- pH_rangeAssessment(x)
  pH_results <- assessmentDetermination(pH, pH_rangeAssess,"pH","Aquatic Life")
  return(pH_results)
}


#### DO Assessment Functions ---------------------------------------------------------------------------------------------------

# DO exceedance function
DO_Assessment_Min <- function(x){ 
  dplyr::select(x,FDT_STA_ID,FDT_DATE_TIME, FDT_DATE_TIME2,FDT_DEPTH,DO,`Dissolved Oxygen Min (mg/L)`,`Dissolved Oxygen Daily Avg (mg/L)`)%>% # Just get relevant columns, 
    filter(!is.na(DO)) %>% 
    mutate(DOExceedanceMin=ifelse(DO < `Dissolved Oxygen Min (mg/L)`,T,F))%>% # Identify where above max Temperature, 9VAC25-260-50 ClassIII= 32C
    filter(DOExceedanceMin==TRUE) %>% # Only return DO measures below threshold
    dplyr::select(-c(DOExceedanceMin,FDT_DATE_TIME2,`Dissolved Oxygen Daily Avg (mg/L)`)) # Don't show user column, could be confusing to them
}

# Daily Average exceedance function
DO_Assessment_DailyAvg <- function(x){ 
  dplyr::select(x,FDT_STA_ID,FDT_DATE_TIME, FDT_DATE_TIME2,FDT_DEPTH,DO,`Dissolved Oxygen Min (mg/L)`,`Dissolved Oxygen Daily Avg (mg/L)`)%>% # Just get relevant columns, 
    filter(!is.na(DO)) %>% #get rid of NA's
    mutate(date = as.Date(FDT_DATE_TIME2, format="%m/%d/%Y")) %>% 
    group_by(date) %>%
    mutate(n_Samples_Daily = n()) %>% # how many samples per day?
    filter(n_Samples_Daily > 1) %>%
    mutate(DO_DailyAverage = mean(DO), DOExceedanceDailyAvg=ifelse(DO_DailyAverage < `Dissolved Oxygen Daily Avg (mg/L)`,T,F)) %>% # Identify where above max Temperature, 9VAC25-260-50 ClassIII= 32C
    ungroup() %>% 
    filter( DOExceedanceDailyAvg==TRUE) %>% # Only return DO measures below threshold
    dplyr::select(-c(FDT_DATE_TIME2, `Dissolved Oxygen Min (mg/L)`,date))
}

# Exceedance Rate DO, for all samples
exceedance_DO <- function(x){
  DO <- dplyr::select(x,FDT_STA_ID,FDT_DATE_TIME, FDT_DATE_TIME2,FDT_DEPTH,DO,`Dissolved Oxygen Min (mg/L)`,`Dissolved Oxygen Daily Avg (mg/L)`)%>% # Just get relevant columns, 
    filter(!is.na(DO)) #get rid of NA's
  DO_Assess <- DO_Assessment_Min(x)
  DO_results <- assessmentDetermination(DO,DO_Assess,"Dissolved Oxygen","Aquatic Life")
  return(DO_results)
}

# Exceedance Rate DO, for daily average samples
exceedance_DO_DailyAvg <- function(x){
  DO <- dplyr::select(x,FDT_STA_ID,FDT_DATE_TIME, FDT_DATE_TIME2,FDT_DEPTH,DO,`Dissolved Oxygen Min (mg/L)`,`Dissolved Oxygen Daily Avg (mg/L)`)%>% # Just get relevant columns, 
    filter(!is.na(DO)) %>% #get rid of NA's
    mutate(date = as.Date(FDT_DATE_TIME2, format="%m/%d/%Y")) %>% 
    group_by(date) %>%
    mutate(n_Samples_Daily = n()) %>% # how many samples per day?
    filter(n_Samples_Daily > 1)  # only keep days with > 1 sample 
  DO_Assess <- suppressMessages(DO_Assessment_DailyAvg(x))
  DO_results <- assessmentDetermination(DO %>% distinct(date),DO_Assess,"Dissolved Oxygen Daily Average","Aquatic Life")
  return(DO_results)
}


#### TP semi Assessment Functions ---------------------------------------------------------------------------------------------------

countTP <- function(x){
  dplyr::select(x,FDT_STA_ID,FDT_DATE_TIME,PHOSPHORUS)%>% # Just get relevant columns
    filter(!is.na(PHOSPHORUS)) %>% #get rid of NA's
    summarize(NUT_TP_VIO= NA, NUT_TP_SAMP= n(), NUT_TP_STAT= NA)
}

TPexceed <- function(x){
  TP <- dplyr::select(x,FDT_STA_ID,FDT_DATE_TIME,PHOSPHORUS) %>%
    filter(!is.na(PHOSPHORUS)) %>% #get rid of NA's
    mutate(limit = 0.2) %>%
    rename(parameter = !!names(.[3])) %>% # rename columns to make functions easier to apply
    mutate(exceeds = ifelse(parameter > limit, T, F)) # Identify where above NH3 WQS limit
  return(quickStats(TP, 'NUT_TP')) 
}


#### Chl a semi Assessment Functions ---------------------------------------------------------------------------------------------------

countchla <- function(x){
  dplyr::select(x,FDT_STA_ID,FDT_DATE_TIME,PHOSPHORUS)%>% # Just get relevant columns
    filter(!is.na(PHOSPHORUS)) %>% #get rid of NA's
    summarize(NUT_CHLA_VIO= NA, NUT_CHLA_SAMP= n(), NUT_CHLA_STAT= NA)
}

#### E.coli OLD Assessment Functions ---------------------------------------------------------------------------------------------------

bacteria_ExceedancesGeomeanOLD <- function(x, bacteriaType, geomeanLimit){
  if(nrow(x) > 0){
    suppressWarnings(mutate(x, SampleDate = format(FDT_DATE_TIME2,"%m/%d/%y"), # Separate sampling events by day
                            previousSample=lag(SampleDate,1),previousSampleBacteria=lag(get(bacteriaType),1)) %>% # Line up previous sample with current sample line
                       rowwise() %>% 
                       mutate(sameSampleMonth= as.numeric(strsplit(SampleDate,'/')[[1]][1])  -  as.numeric(strsplit(previousSample,'/')[[1]][1])) %>% # See if sample months are the same, e.g. more than one sample per calendar month
                       filter(sameSampleMonth == 0 | is.na(sameSampleMonth)) %>% # keep only rows with multiple samples per calendar month  or no previous sample (NA) to then test for geomean
                       # USING CALENDAR MONTH BC THAT'S HOW WRITTEN IN GUIDANCE, rolling 4 wk windows would have been more appropriate
                       mutate(sampleMonthYear = paste(month(as.Date(SampleDate,"%m/%d/%y")),year(as.Date(SampleDate,"%m/%d/%y")),sep='/')) %>% # grab sample month and year to group_by() for next analysis
                       group_by(sampleMonthYear) %>%
                       mutate(geoMeanCalendarMonth = FSA::geomean(as.numeric(get(bacteriaType))), # Calculate geomean
                              limit = geomeanLimit, samplesPerMonth = n()))
  }
  
}


bacteria_ExceedancesSTV_OLD <- function(x, STVlimit){                                    
  x %>% rename(parameter = !!names(.[2])) %>% # rename columns to make functions easier to apply
    mutate(limit = STVlimit, exceeds = ifelse(parameter > limit, T, F)) # Single Sample Maximum 
}

# How bacteria is assessed
bacteria_Assessment_OLD <- function(x, bacteriaType, geomeanLimit, STVlimit){
  bacteria <- dplyr::select(x,FDT_DATE_TIME2,bacteriaType)%>% # Just get relavent columns, 
    filter(!is.na(get(bacteriaType))) #get rid of NA's
  # Geomean Analysis (if enough n)
  if(nrow(bacteria)>0){
    bacteriaGeomean <- bacteria_ExceedancesGeomeanOLD(bacteria, bacteriaType, geomeanLimit) %>%     
      distinct(sampleMonthYear, .keep_all = T) %>%
      filter(samplesPerMonth > 4, geoMeanCalendarMonth > limit) %>% # minimum sampling rule for geomean to apply
      mutate(exceeds = TRUE) %>%
      select(sampleMonthYear, geoMeanCalendarMonth, limit, exceeds, samplesPerMonth)
    geomeanResults <- quickStats(bacteriaGeomean, bacteriaType) %>%
      mutate(`Assessment Method` = 'Old Monthly Geomean')
    geomeanResults[,4] <- ifelse(is.na(geomeanResults[,4]),NA, dplyr::recode(geomeanResults[,4], 'Review' = paste('Review if ', bacteriaType,'_VIO > 1',sep='')))
    
    # Single Sample Maximum Analysis
    bacteriaSSM <- bacteria_ExceedancesSTV_OLD(bacteria, STVlimit) 
    SSMresults <- quickStats(bacteriaSSM, bacteriaType) %>% mutate(`Assessment Method` = 'Old Single Sample Maximum')
    return( rbind(geomeanResults, SSMresults) )
  }
  
}

conventionalsToBacteria <- function(x, bacteriaType){
  z <- dplyr::select(x, FDT_STA_ID, FDT_DATE_TIME2, bacteriaType) %>%
    rename(ID = FDT_STA_ID, `Date Time` = FDT_DATE_TIME2, Value = bacteriaType) %>%
    filter(!is.na(Value))
  z$`Date Time` <- as.Date(z$`Date Time`)
  z$Value <- as.numeric(z$Value)
  return(z)
}


#### Ammonia Assessment Functions ---------------------------------------------------------------------------------------------------

# Calculate limits and return dataframe with original data and limits
acuteNH3limit <- function(x){
  # Trout absent scenario, freshwater
  if(unique(x$CLASS) %in% c("III","IV")){
    return(dplyr::select(x, FDT_DATE_TIME2, FDT_DEPTH, FDT_FIELD_PH, AMMONIA) %>%
             mutate(NH3limit = (0.411/(1+10^(7.204-FDT_FIELD_PH)))+(58.4/(1+10^(FDT_FIELD_PH-7.204)))))  }
  # Trout present scenario, freshwater
  if(unique(x$CLASS) %in% c("V","VI")){
    return(dplyr::select(x, FDT_DATE_TIME2, FDT_DEPTH, FDT_FIELD_PH, AMMONIA) %>%
             mutate(NH3limit = (0.275/(1+10^(7.204-FDT_FIELD_PH)))+(39/(1+10^(FDT_FIELD_PH-7.204)))))  }
}

# Return one line summarizing samples and violation rate
acuteNH3exceedance <- function(x){
  # Trout absent scenario, freshwater
  if(unique(x$CLASS) %in% c("III","IV")){
    ammonia <- acuteNH3limit(x) %>%
      filter(!is.na(AMMONIA)) %>% #get rid of NA's
      mutate(sampleYear = lubridate::year(FDT_DATE_TIME2)) # add year to enable filtering by last 3 years with data
    if(nrow(ammonia) == 0){
      return(quickStats(ammonia,'AcuteAmmonia'))
    } else {
      last3years <- lastXyears(ammonia, 'FDT_DATE_TIME2', 3, TRUE)
      ammonia <- filter(ammonia, sampleYear %in% last3years) %>%
        select(-sampleYear) %>%
        rename(parameter = !!names(.[4]), limit = !!names(.[5])) %>% # rename columns to make functions easier to apply
        mutate(exceeds = ifelse(parameter > limit, T, F)) # Identify where above NH3 WQS limit
      
      return(quickStats(ammonia, 'AcuteAmmonia'))  }
  }
  
  # Trout present scenario, freshwater
  if(unique(x$CLASS) %in% c("V","VI")){
    ammonia <- acuteNH3limit(x) %>%
      filter(!is.na(AMMONIA)) %>% #get rid of NA's
      mutate(sampleYear = lubridate::year(FDT_DATE_TIME2)) # add year to enable filtering by last 3 years with data
    if(nrow(ammonia) == 0){
      return(quickStats(ammonia,'AcuteAmmonia'))
    } else {
      last3years <- lastXyears(ammonia, 'FDT_DATE_TIME2', 3, TRUE)
      ammonia <- filter(ammonia, sampleYear %in% last3years) %>%
        select(-sampleYear) %>%
        rename(parameter = !!names(.[4]), limit = !!names(.[5])) %>% # rename columns to make functions easier to apply
        mutate(exceeds = ifelse(parameter > limit, T, F)) # Identify where above NH3 WQS limit
      return(quickStats(ammonia, 'AcuteAmmonia'))
    }
    
  }
}



#### Chloride PWS Assessment Functions ---------------------------------------------------------------------------------------------------


chloridePWS <- function(x){
  if(grepl('PWS', unique(x$SPSTDS))){
    chloride <- dplyr::select(x, FDT_DATE_TIME, FDT_DEPTH, CHLORIDE) %>%
      filter(!is.na(CHLORIDE)) %>% #get rid of NA's
      mutate(limit = 250) %>%
      rename(parameter = !!names(.[3])) %>% # rename columns to make functions easier to apply
      mutate(exceeds = ifelse(parameter > limit, T, F)) # Identify where above NH3 WQS limit
    return(quickStats(chloride, 'PWS_Acute_Chloride'))  }  
}


#### Nitrate PWS Assessment Functions ---------------------------------------------------------------------------------------------------

nitratePWS <- function(x){
  if(grepl('PWS', unique(x$SPSTDS))){
    nitrate <- dplyr::select(x, FDT_DATE_TIME, FDT_DEPTH, NITRATE) %>%
      filter(!is.na(NITRATE)) %>% #get rid of NA's
      mutate(limit = 10) %>%
      rename(parameter = !!names(.[3])) %>% # rename columns to make functions easier to apply
      mutate(exceeds = ifelse(parameter > limit, T, F)) # Identify where above NH3 WQS limit
    return(quickStats(nitrate, 'PWS_Acute_Nitrate'))  }  
}


#### Total Sulfate PWS Assessment Functions ---------------------------------------------------------------------------------------------------

TSulfatePWS <- function(x){
  if(grepl('PWS', unique(x$SPSTDS))){
    tSulfate <- dplyr::select(x, FDT_DATE_TIME, FDT_DEPTH, SULFATE_TOTAL) %>%
      filter(!is.na(SULFATE_TOTAL)) %>% #get rid of NA's
      mutate(limit = 250) %>%
      rename(parameter = !!names(.[3])) %>% # rename columns to make functions easier to apply
      mutate(exceeds = ifelse(parameter > limit, T, F)) # Identify where above NH3 WQS limit
    return(quickStats(tSulfate, 'PWS_Acute_Total_Sulfate'))  }  
}




#### Benthics Sampling Metrics Functions ---------------------------------------------------------------------------------------------------

benthicResultMetrics <- function(x, VSCI, VCPMI){
  out <- list()
  # VCPMI Ecoregion 63 + Chowan
  if(unique(x$US_L3CODE) %in% 63 & 
     unique(x$Basin) %in% 'Chowan and Dismal Swamp River Basin'){
    z <- filter(VCPMI, StationID %in% x$FDT_STA_ID) %>% mutate(SCI = 'VCPMI')}
  if(unique(x$US_L3CODE) %in% 65  & 
     !(unique(x$Basin) %in% 'Chowan and Dismal Swamp River Basin' ) ){
    z <- filter(VCPMI, StationID %in% x$FDT_STA_ID) %>% mutate(SCI = 'VCPMI')}
  if(unique(x$US_L3CODE) %in% c(45, 64, 66, 67, 69)){
    z <- filter(VSCI, StationID %in% x$FDT_STA_ID) %>% mutate(SCI = 'VSCI') }
  
  if(nrow(z) > 0){
    z1 <- mutate(z, Year = lubridate::year(CollDate))
    out$data <- z1
    spring <- filter(z1, Season %in% 'Spring' )
    fall <- filter(z1, Season %in% 'Fall' )
    # output list with all metrics
    nSamples <- nrow(z1)
    averageSCI <- format(mean(z1$`Fam SCI`), digits = 3)
    nSpringSample <- nrow(spring)
    nFallSample <- nrow(fall)
    minFamSCI <- format(min(z1$`Fam SCI`), digits = 3)
    maxFamSCI <- format(max(z1$`Fam SCI`), digits = 3)
    springAverage <- as.numeric(summarise(spring, springAverage = format(mean(`Fam SCI`), digits = 3)))
    fallAverage <- as.numeric(summarise(fall, fallAverage = format(mean(`Fam SCI`), digits = 3)))
    out$roundup <- tibble(StationID = unique(z1$StationID),  
                          `n Samples`=nSamples, `n Spring Samples`= nSpringSample, `n Fall Samples` = nFallSample,
                          `Average SCI` =averageSCI, `Spring Average SCI`=springAverage, `Fall Average SCI`= fallAverage,
                          `Minimum SCI` = minFamSCI, `Maximum SCI`= maxFamSCI)
    
    out$yearlyAverage <- z1 %>%
      group_by(Year) %>%
      summarise(yearAverage = format(mean(`Fam SCI`), digits = 3)) 
    
    
  } else {
    out$data <- NA
    
    out$roundup <- tibble(StationID = NA,  
                          `n Samples`=NA, `n Spring Samples`= NA, `n Fall Samples` = NA,
                          `Average SCI` =NA, `Spring Average SCI`=NA, `Fall Average SCI`= NA,
                          `Minimum SCI` = NA, `Maximum SCI`= NA)
    out$yearlyAverage <- tibble(Year= NA, yearAverage=NA)
    }
  return(out)
  
}

benthicAssessment <- function(x,conventionals_sf,VSCI,VCPMI){
  x <- filter(conventionals_sf, FDT_STA_ID %in% x$FDT_STA_ID)#'2-JKS033.06') #'2-JMS279.41')##
  if(nrow(x) >0){
    x2 <- benthicResultMetrics(x,VSCI,VCPMI)$data
    if (!any(is.na(x2))){return(data.frame(BENTHIC_STAT='Review'))
    }else{return(data.frame(BENTHIC_STAT=NA))}
  } else{return(data.frame(BENTHIC_STAT=NA))}
}
#benthicAssessment(x,conventionals_sf,VSCI,VCPMI)
  









quickStats <- function(parameterDataset, parameter){
  if(nrow(parameterDataset) > 0 ){
    results <- data.frame(VIO = nrow(filter(parameterDataset, exceeds == TRUE)),
                          SAMP = nrow(parameterDataset)) %>%
      mutate(exceedanceRate = format((VIO/SAMP)*100,digits=3))
    
    #if(results$VIO >= 1 & results$exceedanceRate < 10.5){outcome <- 'Review'}
    if(results$exceedanceRate > 10.5 & results$VIO >= 1 & results$SAMP > 10){outcome <- 'Review'}
    if(results$exceedanceRate < 10.5 & results$SAMP > 10){outcome <- 'S'}
    if(results$VIO >= 1 & results$SAMP <= 10){outcome <- 'Review'}
    if(results$VIO < 1 & results$SAMP <= 10){outcome <- 'S'}
    
    
    results <- mutate(results, STAT = outcome)
    names(results) <- c(paste(parameter,names(results)[1], sep = '_'),
                        paste(parameter,names(results)[2], sep = '_'),
                        paste(parameter,names(results)[3], sep = '_'),
                        paste(parameter,names(results)[4], sep = '_'))
    #rename based on parameter entered
    return(results)
  } else {
    z <- data.frame(VIO = NA, SAMP=NA, exceedanceRate= NA, STAT=NA)
    names(z) <- paste(parameter,names(z), sep='_')
    return(z)
  }
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

bacteriaExceedances_OLD <- function(results, bacteriaType){
  # If no results to report, give nothing
  if(length(results)>0){
    # if geomean applied, use those results
    if(grepl('Review',results[1,4]) | is.na(results[1,4])){
      return(results[2,1:4])}
    else{return(results[1,1:4])}
  }else{
    z <- data.frame(SAMP=NA, VIO = NA, exceedanceRate= NA, STAT=NA)
    names(z) <- paste(bacteriaType,names(z), sep='_')
    return(z)
  }
}

#bacteriaExceedances_OLD(bacteria_Assessment_OLD(x, 'E.COLI', 126, 235),'E.COLI')
#bacteriaExceedances_OLD( bacteria_Assessment_OLD(filter(conventionals, FDT_STA_ID %in% '2-DCK003.94'), 'ENTEROCOCCI', 35, 104))
#bacteriaExceedances_OLD( bacteria_Assessment_OLD(filter(conventionals, FDT_STA_ID %in% '1AABR000.78'), 'ENTEROCOCCI', 35, 104),'ENTEROCOCCI')


metalsExceedances <- function(x, metalType){
  # if any data given to function
  if(nrow(x) > 0){ VIO <- length(which(x == 'NSP')) 
  }else {
    VIO <- NA  }
  
  x <- data.frame(VIO = VIO, STAT = ifelse(VIO > 0, 'Review', 'S'))
  names(x) <- paste(metalType,names(x), sep='_')
  return(x)
}