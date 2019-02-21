# R 3.5.1

library(tidyverse)
library(readxl)
library(FSA)
library(lubridate)
library(magrittr)



# For Testing ---------------------------
## Tish's DEQ data spreadsheet
#tish <- read_excel('exampleData/20172018_bacteria.xlsx', sheet = 'rpp170') 
#tish1 <- select(tish, ID, `Date Time`, Value)
#tish1$`Date Time` <- as.Date(tish1$`Date Time`,format = '%Y-%m-%d')

#x <- tish1
#sampleRequirement <- 10
#STV <- 410
#geomeanCriteria <- 126
# --------------------------------------

# New standard logic, can substitute in E.coli or Enter. limits and play with sampling
# requirements
bacteriaExceedances_NewStd <- function(x, # input dataframe with bacteria data
                                       sampleRequirement, # minimum n samples in 90 day window needed to apply geomean
                                       STV, # unique for ecoli/enter
                                       geomeanCriteria# unique for ecoli/enter
){
  # Output tiblle to organize results, need list object to save associate data
  out <- tibble(`StationID` = as.character(NA),
                `Window Within Recreation Season` = as.logical(NA),
                `Date Window Starts` = as.Date(NA), `Date Window Ends` = as.Date(NA), 
                `Samples in 90 Day Window` = as.numeric(NA), 
                `STV Exceedances In Window` = as.numeric(NA), 
                `STV Exceedance Rate` = as.numeric(NA), 
                `STV Assessment` = as.character(NA),
                `Geomean In Window` = as.numeric(NA),
                `Geomean Assessment` = as.character(NA),
                associatedData = list()) 
  
  # Loop through each row of input df to test 90 day windows against assessment criteria
  for( i in 1 : nrow(x)){
    time1 <- x$`Date Time`[i]
    timePlus90 <- ymd(x$`Date Time`[i]) + days(90)
    
    # Recreation Season, first flag samples that do not comply with recreation season requirements
    recSeason <- ifelse(month(time1) >= 4 & month(time1) < 11 & 
                          month(timePlus90) >= 4 & month(timePlus90) < 11, TRUE, FALSE)
    
    
    # Organize prerequisites to decision process
    z <- filter(x, `Date Time` >= time1 & `Date Time` <= timePlus90) %>% 
      mutate(nSamples = n(), # count number of samples in 90 day window
             STVhit = ifelse(Value > STV, TRUE, FALSE), # test values in window against STV
             E.COLI_geomean = ifelse(nSamples > 1, FSA::geomean(Value), NA), # calculate geomean of samples if nSamples>1
             geomeanCriteriaHit = ifelse(E.COLI_geomean > geomeanCriteria, TRUE, FALSE)) # test geomean against geomean Criteria
    
    # First level of testing: any STV hits in dataset? Want this information for all scenarios
    nSTVhitsInWindow <- nrow(filter(z, STVhit == TRUE))
    STVexceedanceRate <- ( nSTVhitsInWindow / unique(z$nSamples)) * 100 # STV exceedance rate calculation
    if(nSTVhitsInWindow == 0){
      `STV Assessment` <- 'No STV violations within 90 day window' } 
    if(nSTVhitsInWindow == 1){
      `STV Assessment` <- paste(nSTVhitsInWindow, ' STV violation(s) with ', format(STVexceedanceRate, digits = 3), 
                                '% exceedance rate in 90 day window | Insufficient Information (Prioritize for follow up monitoring)',sep='')}
    if(nSTVhitsInWindow >= 2){
      `STV Assessment` <- paste(nSTVhitsInWindow, ' STV violation(s) with ', format(STVexceedanceRate, digits = 3), 
                                '% exceedance rate in 90 day window | Impaired: ', nSTVhitsInWindow,' hits in the same 90-day period',sep='') }
    
    # Second level of testing: only if minimum geomean sampling requirements met in 90 day period
    if(unique(z$nSamples) >= sampleRequirement){
      # Geomean Hit
      if(unique(z$geomeanCriteriaHit) == TRUE){
        `Geomean Assessment` <- paste('Geomean: ', format(unique(z$E.COLI_geomean), digits = 3), 
                                      ' | Impaired: geomean exceeds criteria in the 90-day period', sep='')  
      } else{
        `Geomean Assessment` <-  paste('Geomean: ', format(unique(z$E.COLI_geomean), digits = 3), 
                                       ' | Geomean criteria met, hold assessment decision for further testing', sep= '')} 
    } else { # minimum geomean sampling requirements NOT met in 90 day period
      `Geomean Assessment` <- 'Insufficient Information: geomean sampling criteria not met'  }
    
    out[i,] <-  tibble(`StationID` = unique(x$ID),
                       `Window Within Recreation Season` = recSeason,
                       `Date Window Starts` = time1, `Date Window Ends` = timePlus90, 
                       `Samples in 90 Day Window` = unique(z$nSamples), 
                       `STV Exceedances In Window` = nSTVhitsInWindow, 
                       `STV Exceedance Rate` = STVexceedanceRate,
                       `STV Assessment` = `STV Assessment`,
                       `Geomean In Window` = ifelse(unique(z$nSamples) >= sampleRequirement, unique(z$E.COLI_geomean), NA), # avoid excitement, only give geomean result if 10+ samples
                       `Geomean Assessment` = `Geomean Assessment`,
                       associatedData = list(z)) 
  } #end for loop
  return(out) 
}
# really just a building block, one probably wouldn't run this function independently
#results <- ecoliExceedances_NewStd(tish1, 10, 410, 126)   

# Function to identify non overlapping windows in a given dataset.
# this ONLY returns the first non overlapping datasets and gives NO PRIORITY
# to datasets that may exceed more/less
# I'm not 100% happy with this function so far. Needs more testing to see utility
#last2years = 2016

nonOverlappingIntervals <- function(results, onlyWithinRec, last2years){
  if(onlyWithinRec == T){ results <- filter(results, `Window Within Recreation Season` == onlyWithinRec) }
  if(!is.na(last2years)){ results <- mutate(results, yr= year(`Date Window Starts`)) %>%
    filter(yr > last2years) %>% select(-yr)}
  
  
  if(nrow(results) > 0){
    results$newInt <- NA
    x <- results$`Date Window Ends`[1]
    
    for(i in 1:nrow(results)){
      if(results$`Date Window Starts`[i] < x){results$newInt[i] <- as.character(x)
      }else{
        x <- results$`Date Window Ends`[i]
        results$newInt[i] <- as.character(x)}  }
    
    uniqueInt <- unique(results$newInt)
    finalResults <- filter(results, `Date Window Ends` %in% as.Date(unique(results$newInt))) %>%
      select(-newInt)
  } else {
    finalResults <- results
  }
  
  
  suppressWarnings(return(finalResults))
}



# Function to summarize results into decisions
# This function returns all potential issues with priorty on geomean results IF there
# are enough samples to run geomean
# NO filtering of recreation season, so in theory this could run the citizen data, too.
# Recreation season is noted as boolean with April 1 - October 31 compliance
bacteriaAssessmentDecision <- function(x, sampleRequirement, STV, geomeanCriteria, last2years){
  # Run assessment function
  z <- bacteriaExceedances_NewStd(x,  sampleRequirement, STV, geomeanCriteria)   
  
  # Then make sure at least 2 independent 90 day windows present in dataset before testing for other things
  nonOverlappingExceedanceResults <- nonOverlappingIntervals(z, FALSE, last2years)
  if(nrow(nonOverlappingExceedanceResults) < 2){
    return(mutate(z,`Assessment Decision` = 'Dataset lacks two independent 90 day windows: Insufficient Information (Prioritize for follow-up Monitoring) '))
  } else {
    # See if geomean method applies
    geomeanApplies <- filter(z, `Samples in 90 Day Window` >= sampleRequirement)
    
    if(nrow(geomeanApplies) > 1){
      geomeanExceedances <- filter(geomeanApplies, `Geomean In Window` > geomeanCriteria)
      
      # If any geomean exceedances then return all windows where geomean exceeds
      if(nrow(geomeanExceedances) > 0){
        return(mutate(geomeanExceedances,`Assessment Decision` = 
                        'Impaired: geometric means calculated for the 90-day periods represented by 10+ samples do not meet the GM criterion'))
      } else{ # If no geomean exceedances then dig into STV exceedances
        
        # Any STV exceedance rates > 10.5% in a 90 day window with 10+ samples?
        STVexceedanceAfterPassingGeomean <- filter(z, `STV Exceedance Rate` >= 10.5)
        if(nrow(STVexceedanceAfterPassingGeomean) > 0){
          
          # If any STV exceedance rate > 10.5% from 90 day windows with 10+ samples then impaired
          STVexceedanceAnd10Samples <- filter(STVexceedanceAfterPassingGeomean, `Samples in 90 Day Window` >= 10)
          if(nrow(STVexceedanceAnd10Samples) > 0 ){
            return(mutate(STVexceedanceAnd10Samples, 
                          `Assessment Decision` = 'Impaired: at least one 90 day window with 10+ samples and STV exceedance rate > 10.5%'))  }
          
          # If any STV exceedance rate > 10.5% from 90 day windows with <10 samples then test more
          
          # If 2+ STV exceedances in any size window
          morethan1STVexceedanceInAnyWindow <- filter(STVexceedanceAfterPassingGeomean, `STV Exceedances In Window` >= 2)
          if(nrow(morethan1STVexceedanceInAnyWindow) > 0){
            return(mutate(morethan1STVexceedanceInAnyWindow, `Assessment Decision` = paste('Impaired: 2 or more STV exceedances in a 90 day window')))  
          } else {
            # Only other option is only 1 STV exceedance in any size window
            nonOverlappingIntervalsPassingGeomeanButHave1STV <- nonOverlappingIntervals(STVexceedanceAfterPassingGeomean, TRUE, last2years)
            if(nrow(nonOverlappingIntervalsPassingGeomeanButHave1STV) >=2){
              return(mutate(nonOverlappingIntervalsPassingGeomeanButHave1STV, 
                            `Assessment Decision` = 'Fully Supporting: no geomean exceedances in 2+ non overlapping 90 day windows within the Recreation Season; window(s) have 1 STV Exceedance'))  
            } else {
              return(mutate(z, `Assessment Decision` = 
                              paste('Insufficient Information: meets geomean criteria but 1 STV exceedance in 90 day window with < 10 samples (Prioritize for follow up monitoring)'))) }
          }
        } else { 
          # If no geomean exceedances and no STV exceedances in 90 day window 
          
          # First see if two independent 90 day windows passing geomean can render Fully Supporting
          nonOverlappingIntervalsPassingGeomean <- nonOverlappingIntervals(geomeanApplies, TRUE, last2years)
          if(nrow(nonOverlappingIntervalsPassingGeomean) >= 2 ){
            # Means two or more nonoverlapping windows that fall in rec season
            return(mutate(nonOverlappingIntervalsPassingGeomean, 
                          `Assessment Decision` = 'Fully Supporting: no geomean exceedances in 2+ non overlapping 90 day windows within the Recreation Season'))    
          } else { # If no independent windows meeting geomean and STV needs additional sampling
            return(mutate(z, `Assessment Decision` = 'Insufficient Information: not enough nonoverlapping 90 day windows meeting geomean and STV criteria' ))    }
        }
      }
    } else { # These scenarios have no geomean criteria bc sampling < sampleRequirements
      exceedsSTVrate <- filter(z, `STV Exceedance Rate` >= 10.5)
      # First deal with no STV exceedance rate issues AND not enough data to assess geomean
      if(nrow(exceedsSTVrate) == 0 ){
        return(mutate(z,`Assessment Decision` = 
                        'Insufficient Information: no 90 day windows with > 10.5% exceedance of STV but not enough samples in any 90 day window to assess geomean (Prioritize for follow up monitoring)'))   
      } else { # These have STV exceedance rate issues AND not enough data to assess geomean
        moreThan2STVhitsInWindow <- filter(exceedsSTVrate, `STV Exceedances In Window` >= 2)
        return(mutate(moreThan2STVhitsInWindow,`Assessment Decision` = 
                        paste('Impaired: ',nrow(moreThan2STVhitsInWindow), '90 day window(s) with 2+ exceedances of STV but not enough samples in any 90 day window to assess geomean')))   }
    }
  }
}
          
          

####### ------------------------------------------------------------------------------------------------------
# Now working with Tish's simulated data

# Example 1: Assessment = FULLY SUPPORTING
# Two Independent 90-Day Periods, no GM exceedences and no 2+ hits in any 90 day period
simData1 <- read_excel('exampleData/simulated_Ecoli_bacteria_EVJ.xlsx', sheet='Example 1 DATA') %>%
  mutate(`Date Time` = DateTime) %>% select(-DateTime)

simData1 <- read_excel('exampleData/simulated_Ecoli_bacteria.xlsx', sheet='Example 1 DATA') %>%
  mutate(`Date Time` = DateTime) %>% select(-DateTime)

# this result will highlight all potential windows with issues to assessor
simData1.2 <- bacteriaAssessmentDecision(simData1, sampleRequirement = 10, STV = 410, geomeanCriteria = 126, last2years = 2016) 
unique(simData1.2$`Assessment Decision`)
# this result will highlight any non overlapping windows with issues to assessor
#simData1.3 <- nonOverlappingIntervals(simData1.2)

# Example 1: Emma gets same as Tish
#--------------------------------------
# Example 2: Assessment = Insufficient Information
#All the 90-day periods w/10+ samples are overlapping and there are no 90-day periods with 2+ STV hits

simData2 <- read_excel('exampleData/simulated_Ecoli_bacteria.xlsx', sheet='Example 2 DATA') %>%
  mutate(`Date Time` = DateTime) %>% select(-DateTime)

# this result will highlight all potential windows with issues to assessor
simData2.2 <- bacteriaAssessmentDecision(simData2, sampleRequirement = 10, STV = 410, geomeanCriteria = 126, last2years = 2016) 
unique(simData2.2$`Assessment Decision`)
# this result will highlight any non overlapping windows with issues to assessor
#simData2.3 <- nonOverlappingIntervals(simData2.2)

# Example 2: Emma FS, doesnt match tish. Emma needs rule about 2 nonoverlapping windows to get same answer
#--------------------------------------
# Example 3: Assessment = Assessment = Insufficient Information
#The two independent 90-day periods with 10+ samples don't  both fall within a recreation season and there are no GM exceedences or 2+ STV hits

simData3 <- read_excel('exampleData/simulated_Ecoli_bacteria.xlsx', sheet='Example 3 DATA') %>%
  mutate(`Date Time` = DateTime) %>% select(-DateTime)

# this result will highlight all potential windows with issues to assessor
simData3.2 <- bacteriaAssessmentDecision(simData3, sampleRequirement = 10, STV = 410, geomeanCriteria = 126, last2years = 2016) 
unique(simData3.2$`Assessment Decision`)
# this result will highlight any non overlapping windows with issues to assessor
#simData3.3 <- nonOverlappingIntervals(simData3.2)

# Example 3: Emma FS, doesnt match tish. Emma needs to firm up rule about recreation season bc the second record says
# it is T for recSeason but is really false

#--------------------------------------
# Example 4: Assessment = Assessment=Impaired
#At least one 90-day period in the dataset has 2+ STV hits.  

simData4 <- read_excel('exampleData/simulated_Ecoli_bacteria.xlsx', sheet='Example 4 DATA') %>%
  mutate(`Date Time` = DateTime) %>% select(-DateTime)

# this result will highlight all potential windows with issues to assessor
simData4.2 <- bacteriaAssessmentDecision(simData4, sampleRequirement = 10, STV = 410, geomeanCriteria = 126, last2years = 2016) 
unique(simData4.2$`Assessment Decision`)
# this result will highlight any non overlapping windows with issues to assessor
#simData4.3 <- nonOverlappingIntervals(simData4.2)

# Example 4: Emma Impaired. Matches Tish's but need to dig in more to see why only one row returned in simData4.2
# will need additional rules on nonoverlapping windows, sample season, and years.
#--------------------------------------
# Example 5: Assessment=Fully Supporting
#The two most recent years have no 90-day period with 2+ STV hits (>410) and there are no GM exceedences (>126) 
#in two independent recreational season 90-day periods

#There is a 90-day period with 2+ STV hits, but it is not in the two most recent years.


simData5 <- read_excel('exampleData/additional_simulated_Ecoli_bacteria.xlsx', sheet='Example 5') %>%
  mutate(`Date Time` = DateTime) %>% select(-DateTime)

# this result will highlight all potential windows with issues to assessor
simData5.2 <- bacteriaAssessmentDecision(simData5, sampleRequirement = 10, STV = 410, geomeanCriteria = 126, last2years = 2016) 
unique(simData5.2$`Assessment Decision`)

# this result will highlight any non overlapping windows with issues to assessor
#simData5.3 <- nonOverlappingIntervals(simData5.2)

# Example 5: Emma FS,matches but needs more work on time rules
#--------------------------------------
# Example 6: Assessment=Impaired
#There is a 90-day period in the two most recent years that has 2+ STV hits (>410). But there are no GM exceedences
#in two independent recreational season 90-day periods


simData6 <- read_excel('exampleData/additional_simulated_Ecoli_bacteria.xlsx', sheet='Example 6') %>%
  mutate(`Date Time` = DateTime) %>% select(-DateTime)

# this result will highlight all potential windows with issues to assessor
simData6.2 <- bacteriaAssessmentDecision(simData6, sampleRequirement = 10, STV = 410, geomeanCriteria = 126, last2years = 2016) 
unique(simData6.2$`Assessment Decision`)

# this result will highlight any non overlapping windows with issues to assessor
simData6.3 <- nonOverlappingIntervals(simData4.2)

# Example 6: Emma FS, doesnt match. need more work






### Function Testing ----------------------------------------------------------------------------

VDH <- read_excel('exampleData/VDH beach data and assessment.xlsx', sheet = 'data') %>%
  mutate(ID = Beach, `Date Time` = Date, Value = Enterococcus) %>%
  select(-c(Beach, Date, Enterococcus))
VDH$`Date Time` <- as.Date(VDH$`Date Time`)


unique(VDH$ID)
## Station: "10TH VIEW, BEHIND QUALITY INN,1010 W OCEAN VIEW AVE"
x1 <- filter(VDH, ID == "10TH VIEW, BEHIND QUALITY INN,1010 W OCEAN VIEW AVE")

# this result will highlight all potential windows with issues to assessor
results2 <- bacteriaAssessmentDecision(x1, sampleRequirement = 10, STV = 130, geomeanCriteria = 35, last2years = 2016) 

# this result will highlight any non overlapping windows with issues to assessor
results3 <- nonOverlappingIntervals(results2, T, 2016)


#### Rinse and repeat
z <- VDH %>% split(.$ID) %>%
  map_df( . , bacteriaAssessmentDecision, sampleRequirement = 10, STV = 130, geomeanCriteria = 35, last2years = 2016)
View(z %>% distinct(`StationID`, .keep_all = T) %>% select(StationID, `Assessment Decision`, `STV Assessment`,`Geomean Assessment`))
# Notes:
# 13th view has two records bc missing space in ID for some samples, decisions still agree
# Sara constant park east end decisions dont agree


## Digging in to Sara Constant Park
x1 <- filter(VDH, ID == "SARA CONSTANT PARK, EAST END")

# this result will highlight all potential windows with issues to assessor
results2 <- bacteriaAssessmentDecision(x1, sampleRequirement = 10, STV = 130, geomeanCriteria = 35, last2years = 2016) 

# this result will highlight any non overlapping windows with issues to assessor
results3 <- nonOverlappingIntervals(results2)
# I stand by my decision: Impaired: at least one 90 day window with 10+ samples and STV exceedance rate > 10.5%
# Looks like Tish should have same decision based on data but Fully supporting is just a mistype


# really neat statistic just for fun:
startTime <- Sys.time()
z <- VDH %>% split(.$ID) %>%
  map_df( . , bacteriaAssessmentDecision, sampleRequirement = 10, STV = 130, geomeanCriteria = 35, last2years = 2016)
totTime <- Sys.time()- startTime
print(paste('It takes', format(totTime, digits=3), 'seconds to analyze 26 stations and make assessment decisions with new bacteria criteria'))


# digging in to "13TH VIEW, NORTH END" , "13TH VIEW,\r\nNORTH END", NORTH COMMUNITY BEACH,   OCEAN VIEW PARK, EAST SIDE OF PARKING LOT
# First 13th view record
results <- bacteriaAssessmentDecision( filter(VDH, ID == "13TH VIEW, NORTH END"),
                                        sampleRequirement = 10, STV = 130, geomeanCriteria = 35, last2years = 2016) 
unique(results$`Assessment Decision`)
# We both agree that 1 STV exceedance and meets geomean, but spreadsheet has FS and I flag as insufficient. What is correct answer and 
# is comment about 1 STV exceedance in 90 day window with <10 samples appropriate?





# second 13th view record
results <- bacteriaAssessmentDecision( filter(VDH, ID == "13TH VIEW,\r\nNORTH END"),
                                        sampleRequirement = 10, STV = 130, geomeanCriteria = 35, last2years = 2016) 
unique(results$`Assessment Decision`)
# My catch for 2 non overlapping assessment windows within rec season and last 2 years stops from being fully supporting





# NORTH COMMUNITY BEACH why two outputs????
unique(VDH$ID)
# apparently 2 north community beach ID's that got condensed into single result in spreadsheet. Let's see if they 
# match if I smash them together
north <- filter(VDH, ID == "NORTH COMMUNITY BEACH" | ID == "NORTH\r\nCOMMUNITY BEACH") %>%
  mutate(ID = dplyr::recode(ID,  "NORTH\r\nCOMMUNITY BEACH" ="NORTH COMMUNITY BEACH" ))
results <- bacteriaAssessmentDecision( north, sampleRequirement = 10, STV = 130, geomeanCriteria = 35, last2years = 2016) 
unique(results$`Assessment Decision`)
# We both agree





# OCEAN VIEW PARK, EAST SIDE OF PARKING LOT
results <- bacteriaAssessmentDecision( filter(VDH, ID == "OCEAN VIEW PARK, EAST SIDE OF PARKING LOT"),
                                       sampleRequirement = 10, STV = 130, geomeanCriteria = 35, last2years = 2016) 
unique(results$`Assessment Decision`)
# Again issue with not enough independent sample windows to meet FS rule. Should I drop my flag?

