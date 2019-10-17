library(tidyverse)
library(readxl)


# Tish's spreadsheet
tish <- saveRDS(tish1,'exampleData/EcoliNewStandard/tish1.RDS')

ecoliExceedances_NewStd <- function(x, sampleRequirement){
  out <- list()
  for( i in 1 : nrow(x)){
    time1 <- x$`Date Time`[i]
    timePlus90 <- ymd(x$`Date Time`[i]) + days(90)
    z <- filter(x, `Date Time` >= time1 & `Date Time` <= timePlus90) %>% 
      mutate(nSamples = n(), E.COLI_geomean = FSA::geomean(Value)) 
    # only add to output if sampleRequirement met in window
    if(unique(z$nSamples) >= sampleRequirement){
      out[[i]] <-  tibble(`Date Window Starts` = time1, `Date Window Ends` = timePlus90, 
                          `Samples in 90 Day Window` = unique(z$nSamples), 
                          `Window Geomean` = unique(z$E.COLI_geomean), associatedData = list(z)) }
  }
  return(out)
}
test <- ecoliExceedances_NewStd(tish1, 4)

# Extract info from list
results <- map_dfr(test, extract, c('Date Window Starts', 'Date Window Ends', 'Samples in 90 Day Window', 'Window Geomean')) %>% # exctract necessary Columns
  filter(`Window Geomean` > 126) %>% # Just pull exceeding geomeans to start
  mutate(newInt = NA)


nonOverlappingIntervals <- function(results){
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
  
  return(finalResults)
}


nonOverlappingExceedanceResults <- nonOverlappingIntervals(results)

