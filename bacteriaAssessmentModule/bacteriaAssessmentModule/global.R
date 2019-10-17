library(shiny)
library(shinyjs)
library(tidyverse)
library(plotly)
library(DT)
library(FSA)
library(lubridate)
library(magrittr)
library(readxl)


#####################################   UPDATE EACH NEW TOOL REBUILD #############################################
# Establish Assessment Period 
assessmentPeriod <- as.POSIXct(c("2013-01-01 00:00:00 UTC","2018-12-31 23:59:59 UTC"),tz='UTC')
assessmentCycle <- '2020'
##################################################################################################################

conventionals <- read_csv('data/CEDSWQM_2020_IR_DATA-CONVENTIONALS_20190305.csv') %>%
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




# Loading screen
load_data <- function() {
  Sys.sleep(2)
  shinyjs::hide("loading_page")
  shinyjs::show("main_content")
}



quickStats <- function(parameterDataset, parameter){
  if(nrow(parameterDataset) > 0 ){
    results <- data.frame(VIO = nrow(filter(parameterDataset, exceeds == TRUE)),
                          SAMP = nrow(parameterDataset)) %>%
      mutate(exceedanceRate = as.numeric(format((VIO/SAMP)*100,digits=3)))
    
    if(results$VIO >= 1){outcome <- 'Review'} # for Mary
    if(results$VIO >= 1 & results$exceedanceRate < 10.5){outcome <- 'Review'}
    if(results$exceedanceRate > 10.5 & results$VIO >= 2 & results$SAMP > 10){outcome <- '10.5% Exceedance'}
    if(results$VIO < 1 &results$exceedanceRate < 10.5 & results$SAMP > 10){outcome <- 'S'}
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
      dplyr::select(sampleMonthYear, geoMeanCalendarMonth, limit, exceeds, samplesPerMonth)
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



# New bacteria station table function

bacteriaExceedances_NEW <- function(stationData,bacteriaType, sampleRequirement, STV, geomeanCriteria){
  # get assessment
  z <- bacteriaAssessmentDecision(conventionalsToBacteria(stationData, bacteriaType), sampleRequirement, STV, geomeanCriteria)  %>%
    distinct(`Assessment Decision`)  # only grab 1 record
  
  if(is.na(z$`Assessment Decision`)){
    nSTVExceedances <- NA
    nGeomeanExceedances <- NA
    decision <- NA
  } else {
    # Get assessment
    if(str_detect( z$`Assessment Decision`, 'Insufficient Information')){decision <- 'IN'}
    if(str_detect( z$`Assessment Decision`, 'Impaired')){decision <- 'Review'}
    if(str_detect( z$`Assessment Decision`, 'Fully Supporting')) {decision <- 'FS'}
    if(str_detect( z$`Assessment Decision`, 'Observed effect')) {decision <- 'Review'} # put this last to capture any OE's
    
    # Samples taken in most recent 2 years
    y <- bacteriaExceedances_NewStd(conventionalsToBacteria(stationData, bacteriaType), sampleRequirement, STV, geomeanCriteria) 
    # what are the last two years sampled? They get a bit of priority
    last2years <- sort(unique(year(y$`Date Window Starts`)), TRUE)[1:2]
    x <- filter(y, (year(y$`Date Window Starts`) %in% last2years))
    nonOverlappingExceedanceResults <- nonOverlappingIntervals(x, TRUE, last2years)
    
    nSTVExceedances <- nrow(filter(nonOverlappingExceedanceResults, `Window Within Recreation Season` == TRUE & `STV Exceedance Rate` > 10.5 &
                                     `STV Exceedances In Window` > 1))
    nGeomeanExceedances <- nrow(filter(nonOverlappingExceedanceResults, `Window Within Recreation Season` == TRUE & 
                                         `Samples in 90 Day Window` >= sampleRequirement &
                                         `Geomean In Window` > geomeanCriteria))
    
  }
  
  s <- data.frame(STV_VIO = nSTVExceedances, GEOMEAN_VIO = nGeomeanExceedances, STAT_NEW = decision)
  names(s) <- if(bacteriaType == 'ENTEROCOCCI'){
    paste("ENTER", names(s), sep='_')} else {paste(bacteriaType, names(s), sep='_')}
  names(s) <- gsub('[.]','',names(s))
  
  return(s)
  
  
  
}



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
