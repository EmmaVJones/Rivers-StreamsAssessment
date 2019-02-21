#source('testingDataset.R')

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

# Tish's DEQ data spreadsheet
tish <- read_excel('exampleData/20172018_bacteria.xlsx', sheet = 'rpp170') 
tish1 <- select(tish, ID, `Date Time`, Value)
tish1$`Date Time` <- as.Date(tish1$`Date Time`,format = '%Y-%m-%d')



x <- tish1
sampleRequirement <- 10
STV <- 410
geomeanCriteria <- 126

ecoliExceedances_NewStd <- function(x, sampleRequirement, STV, geomeanCriteria){
  # Output tiblle to organize results, need list object to save associate data
  out <- tibble(`Date Window Starts` = as.Date(NA), `Date Window Ends` = as.Date(NA), 
                       `Samples in 90 Day Window` = as.numeric(NA), 
                       `Assessment Recommendation` = as.character(NA),
                       associatedData = list()) 
  
  # Loop through each row of input df to test 90 day windows against assessment criteria
  for( i in 1 : nrow(x)){
    time1 <- x$`Date Time`[i]
    timePlus90 <- ymd(x$`Date Time`[i]) + days(90)
    
    # Organize prerequisites to decision process
    z <- filter(x, `Date Time` >= time1 & `Date Time` <= timePlus90) %>% 
      mutate(nSamples = n(), # count number of samples in 90 day window
             STVhit = ifelse(Value > STV, TRUE, FALSE), # test values in window against STV
             E.COLI_geomean = ifelse(nSamples > 1, FSA::geomean(Value), NA), # calculate geomean of samples if nSamples>1
             geomeanCriteriaHit = ifelse(E.COLI_geomean > geomeanCriteria, TRUE, FALSE)) # test geomean against geomean Criteria
    
    # First level of testing: any STV hits in dataset? Want this information for all scenarios
    nSTVhitsInWindow <- nrow(filter(z, STVhit == TRUE))
    STVexceedanceRate <- ( nSTVhitsInWindow / unique(z$nSamples)) * 100 # STV exceedance rate calculation
    if(nSTVhitsInWindow == 1){
      STVresults <- nSTVhitsInWindow
      `Assessment Recommendation` <- 'Insufficient Information (Prioritize for follow up monitoring)'   }
    if(nSTVhitsInWindow >= 2){
      STVresults <- nSTVhitsInWindow
      `Assessment Recommendation` <- paste('Impaired: ', STVresults,' hits in the same 90-day period', sep= '')    }
    
    # Second level of testing: STV hits overrule geomean so only do this if no STV hits
    if(nSTVhitsInWindow == 0 | `Assessment Recommendation` == 'Insufficient Information (Prioritize for follow up monitoring)'){
      # minimum geomean sampling requirements met in 90 day period
      if(unique(z$nSamples) >= sampleRequirement){
        # Geomean Hit
        if(unique(z$geomeanCriteriaHit) == TRUE){
          `Assessment Recommendation` <- 'Impaired: geomean exceeds criteria in the 90-day period'  
        } else{
          `Assessment Recommendation` <- 'Geomean criteria met, hold assessment decision for further testing'} 
      } else { # minimum geomean sampling requirements NOT met in 90 day period
        `Assessment Recommendation` <- paste('Insufficient Information: ', nSTVhitsInWindow,
                                             ' hit(s) and geomean sampling criteria not met (Prioritize for follow-up Monitoring)',sep='')
      }
    }
    
    out[i,] <-  tibble(`Date Window Starts` = time1, `Date Window Ends` = timePlus90, 
                        `Samples in 90 Day Window` = unique(z$nSamples), 
                        `Assessment Recommendation` = `Assessment Recommendation`,
                        associatedData = list(z)) 
  } #end for loop
  
  return(out) 

}

z <- ecoliExceedances_NewStd(tish1, 10, 410, 126)   

# Function to summarize results into single decision
bacteriaAssessmentDecision <- function(x, sampleRequirement, STV, geomeanCriteria){
  # Recreation Season, first filter out samples that do not comply with recreation season requirements
  x <- filter(x, `Date Time` >= '2018-04-01' & `Date Time` <= '2018-10-31')
  
  # Run assessment function
  z <- ecoliExceedances_NewStd(x,  sampleRequirement, STV, geomeanCriteria)   
  
  # Then make sure at least 2 independent 90 day windows present in dataset before testing for other things
  

  
  if(STVresults >= 2 & unique(z$nSamples) < 10){outcome <- paste('Water impaired for',parameter)}
  if(results$nExceedance < 2 & results$nSamples < 10){outcome <- paste('Water not impaired for',parameter)}
  
  
  if(results$exceedanceRate > 10.5 & results$nSamples > 10){outcome <- paste('Water impaired for',parameter)}
  if(results$exceedanceRate < 10.5 & results$nSamples > 10){outcome <- paste('Water not impaired for',parameter)}
  
    
    # only add to output if sampleRequirement met in window
    if(unique(z$nSamples) >= sampleRequirement){
      out[[i]] <-  tibble(`Date Window Starts` = time1, `Date Window Ends` = timePlus90, 
                          `Samples in 90 Day Window` = unique(z$nSamples), 
                          `Window Geomean` = unique(z$E.COLI_geomean), associatedData = list(z)) }
  }
  return(out)
}
test <- ecoliExceedances_NewStd(tish1, 4)
