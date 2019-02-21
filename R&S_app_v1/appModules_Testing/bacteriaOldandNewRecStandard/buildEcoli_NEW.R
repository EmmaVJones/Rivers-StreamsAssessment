source('testingDataset.R')
monStationTemplate <- read_excel('data/tbl_ir_mon_stations_template.xlsx') # from X:\2018_Assessment\StationsDatabase\VRO

# Single station data ----------------------------------------------------------------------
AUData <- filter(conventionals_HUC, ID305B_1 %in% 'VAW-H01R_JMS04A00' | 
                   ID305B_2 %in% 'VAW-H01R_JMS04A00' | 
                   ID305B_2 %in% 'VAW-H01R_JMS04A00')%>% 
  left_join(WQSvalues, by = 'CLASS')

x <-filter(AUData, FDT_STA_ID %in% '2-JMS279.41') 
#------------------------------------------------------------------------------------------


#https://cran.rstudio.com/web/packages/tibbletime/vignettes/TT-03-rollify-for-rolling-analysis.html
library(tibbletime)

data(FB)

# Only a few columns
FB <- select(FB, symbol, date, open, close, adjusted)
rolling_mean <- rollify(mean, window = 5)
mutate(FB, mean_5 = rolling_mean(adjusted))

# Our data frame summary
summary_df <- function(x) {
  data.frame(  
    rolled_summary_type = c("mean", "sd",  "min",  "max",  "median"),
    rolled_summary_val  = c(mean(x), sd(x), min(x), max(x), median(x))
  )
}

# A rolling version, with unlist = FALSE
rolling_summary <- rollify(~summary_df(.x), window = 5, 
                           unlist = FALSE)

FB_summarised <- mutate(FB, summary_list_col = rolling_summary(adjusted))
View(FB_summarised)
# unnest
View(FB_summarised %>% 
       filter(!is.na(summary_list_col)) %>%
       unnest())


#Flexible calendar periods are tough!
#https://cran.r-project.org/web/packages/tsibble/vignettes/window.html
library(tsibble)

pedestrian_full <- pedestrian %>% 
  fill_gaps(.full = TRUE)
pedestrian_full

#What if the time period we’d like to slide over happens not to be a fixed window size, 
# for example sliding over three months. The preprocessing step is to wrap observations 
# into monthly subsets (a list of tsibbles) using nest().
pedestrian_mth1 <- pedestrian_full %>% 
  mutate(YrMth = yearmonth(Date_Time)) %>% 
  nest(-Sensor, -YrMth)
pedestrian_mth

pedestrian_mth %>% 
  group_by(Sensor) %>% 
  # (1)
  # mutate(Monthly_MA = slide_dbl(data, 
  #   ~ mean(bind_rows(.)$Count, na.rm = TRUE), .size = 3, .align = "center"
  # ))
  # (2) equivalent to (1)
  mutate(Monthly_MA = slide_dbl(data, 
                                ~ mean(.$Count, na.rm = TRUE), .size = 3, .align = "center", .bind = TRUE
  ))

# Tish's spreadsheet
tish <- read_excel('exampleData/20172018_bacteria.xlsx', sheet = 'Cub Run') 
tish1 <- select(tish, ID, `Date Time`, Value) %>% 
  filter(!is.na(Value)) %>%
  mutate(DateTime = `Date Time`) %>%
  group_by(`Date Time`) %>% 
  mutate(analysisWindow = slide_dbl(data, ~ mean(.$Value), .size = 3, align = 'center'))#, .bind = TRUE))


# Tish's spreadsheet
tish <- read_excel('exampleData/20172018_bacteria.xlsx', sheet = 'Cub Run') 
tish1 <- select(tish, ID, `Date Time`, Value)

out <- list()

for ( i in 1:nrow(tish1)){
  out[i] <- ecoliExceedances_NewStd(tish1[i,])
  
}


# Tish's spreadsheet
tish <- read_excel('exampleData/20172018_bacteria.xlsx', sheet = 'Cub Run') 
tish1 <- select(tish, ID, `Date Time`, Value)
tish1$`Date Time` <- as.Date(tish1$`Date Time`,format = '%Y-%m-%d')

x <- tish1
# Figure out if other data in 90 day window of input date time

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
  mutate(newInt = NA)#intervals = lubridate::interval(`Date Window Starts`,`Date Window Ends`),
  #       uniqueInterval = NA) #%>%
  #fill(intervals)

#results$newInt <- NA
#x <- results$`Date Window Ends`[1]
#if(results$`Date Window Starts`[2] < x){results$newInt[2] <- as.character(x)}else{x <- results$`Date Window Ends`[2]}
#if(results$`Date Window Starts`[3] < x){results$newInt[3] <- as.character(x)}else{x <- results$`Date Window Ends`[3]}
#if(results$`Date Window Starts`[4] < x){results$newInt[4] <- as.character(x)}else{x <- results$`Date Window Ends`[4]}
##change
#if(results$`Date Window Starts`[5] < x){results$newInt[5] <- as.character(x)}else{x <- results$`Date Window Ends`[5]}
#if(results$`Date Window Starts`[6] < x){results$newInt[6] <- as.character(x)}else{x <- results$`Date Window Ends`[6]}
#if(results$`Date Window Starts`[7] < x){results$newInt[7] <- as.character(x)}else{x <- results$`Date Window Ends`[7]}
#if(results$`Date Window Starts`[8] < x){results$newInt[8] <- as.character(x)}else{x <- results$`Date Window Ends`[8]}

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




results$newInt <- NA
for(i in 1: nrow(results)){
  if(i == 1){    x <- results$`Date Window Ends`[1]  }
  results$newInt[i] <- ifelse(results$`Date Window Ends`[i] > results$`Date Window Starts`[i+1], T, F)
}
series <- results$intervals
  

  x <- 0
  repeat {
    x <- x + 1
    print(x)
    results$uniqueInterval <- x
    if(!(results$`Date Window Starts` %within% series[x])){
      results$uniqueInterval <- x
    }
    #b <- snap_bufferMethod(POINT,MULTILINESTRING,bufferDistances[x])
    #if (nrow(b) > 0 | x == length(bufferDistances)) break   }
  
  
  
  #group_by(`Date Window Starts`) %>%
 # mutate(tmp = `Date Window Ends`[`Date Window Ends` < intervals])
  for (i in 1:nrow(results)){
    currentInterval <- lubridate::interval(results$`Date Window Starts`[i],results$`Date Window Ends`[i])
    if(results$`Date Window Starts`[i+1] %within% currentInterval){results$intervals[i] <- currentInterval}
  }
  
  #rowwise() %>%
  #dplyr::summarize(int_overlaps(intervals))

  mutate(`Lag Window End` = lag(`Date Window Ends`, 1), # lag the end of previous window by 1 to line up with next window start
         `Unique Window` = ifelse(`Date Window Starts` > `Lag Window End`, T, F))
  
results$`Date Window Ends`[1] %within% lubridate::interval(ymd(results$`Date Window Starts`[2]),ymd(results$`Date Window Ends`[2]))
results$`Date Window Starts`[2] >   results$`Date Window Ends`[1] 

z <- lubridate::interval(results$`Date Window Starts`[1],results$`Date Window Ends`[1])

int_overlaps()

time1 <- x$`Date Time`[1]
  timePlus90 <- ymd(x$`Date Time`[1]) + days(90)
  z <- filter(x, `Date Time` >= time1 & `Date Time` <= timePlus90) %>% 
    mutate(nSamples = n(), E.COLI_geomean = FSA::geomean(Value)) 
  return(tibble(`Samples in 90 Day Window` = unique(z$nSamples), associatedData = list(z)))
}

z <- ecoliExceedances_NewStd(tish1)


bacteria_Assessment <- function(x){
  bacteria <- dplyr::select(x,FDT_DATE_TIME2,E.COLI) %>% # Just get relavent columns, 
    filter(!is.na(E.COLI))%>% #get rid of NA's
    mutate(SampleDate = format(FDT_DATE_TIME2,"%m/%d/%y"), # Separate sampling events by day
           singleSampleMaximum=ifelse(E.COLI>235,T,F), # Find any single sample exceedances
           previousSample=lag(SampleDate,1),
           previousSampleECOLI=lag(E.COLI,1)) %>% # Line up previous sample with current sample line
    rowwise() %>% 
    mutate(sameSampleMonth= as.numeric(strsplit(SampleDate,'/')[[1]][1])  -  as.numeric(strsplit(previousSample,'/')[[1]][1])) %>% # See if sample months are the same, e.g. more than one sample per calendar month
    filter(singleSampleMaximum == T | sameSampleMonth == 0) %>% # keep only rows with single sample exceedances or multiple samples per calendar month to then test for geomean
    rowwise() %>% 
    mutate(geoMeanCalendarMonth=FSA::geomean(c(E.COLI,previousSampleECOLI))) %>% # Calculate geomean
    filter(singleSampleMaximum == T | geoMeanCalendarMonth > 126) %>% #find exceedances to either rule
    dplyr::select(FDT_DATE_TIME2 , E.COLI) # only keep columns that will be important to assessors
  return(bacteria)
}


ecoliExceedances_NewStd <- function(x){
  ecoli <- dplyr::select(x, FDT_DATE_TIME2, `E.COLI`) %>% # Just get relevant columns
    filter(!is.na(`E.COLI`)) 
  elapsedTime <- min(ecoli$FDT_DATE_TIME2) %--% max(ecoli$FDT_DATE_TIME2)
  as.duration(elapsedTime)
  
}
