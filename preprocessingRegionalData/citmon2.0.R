suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(readxl))
suppressPackageStartupMessages(library(sf))

# citizen start
cit <- read_excel('processedStationData/citmon/CitMon_stations_PM_QA.xlsx', 'Paula_NEW')

cit$RorL <- NA
for(i in 1: nrow(cit)){
  cit$RorL[i] <- substr(strsplit(as.character(cit$ID305B_1[i]), '-')[[1]][2] ,4,4)
  
}

# work with just riverine for now since no word on lake guidance as of yet
citR <- filter(cit, RorL == 'R')


# add WQS information to the R stations
source('snappingFunctions/snapFunctions.R') # snapping functions
source('snappingFunctions/snapOrganizationFunctions_citmon.R') # functions to do stuff with snapping functions

# make spatial first
citR <- st_as_sf(citR, coords = c("Longitude", "Latitude"),  # make spatial layer using these columns
                 remove = F, # don't remove these lat/lon cols from df
                 crs = 4326) # add coordinate reference system, needs to be geographic for now bc entering lat/lng,


regionalOutput <- list()
# Loop through multiple basins
#for (i in 1:length(unique(citR$Basin))){
  Regional_Sites_AU_basin <- filter(citR, Basin %in% unique(citR$Basin)[1] )
  WQSsnaps <- snapAndOrganizeWQS_citmon(Regional_Sites_AU_basin, 'GIS/updatedWQS/', 
                                 basinNameSwitcher(unique(Regional_Sites_AU_basin$Basin)),  seq(10,80,by=10))
  regionalOutput[[1]] <- WQSsnaps
#}

# smash it all back together, change from lists to single dataframe
RegionalResults <- do.call("rbind", lapply(regionalOutput, data.frame))
#View(RegionalResults)
rm(WQSsnaps)# clean up workspace



# These are easy because we don't need to change any FDT_STA_ID's so just take columns we need
template <- read_csv('processedStationData/final2020data/RegionalResultsRiverine_BRROFINAL.csv')
RegionalResults2 <- mutate(RegionalResults, VAHU6=VAHU6.x) %>%
  dplyr::select( names(template))


# ADD TO final stations table 
RegionalResults3 <- read_csv('processedStationData/final2020data/RegionalResultsRiverine_BRROFINAL.csv') %>%
  rbind(RegionalResults2)


# Make the rest riverine
#riverStations <- filter(RegionalResults2, lakeStation != 'lake') %>%
#  select(-c(lakeStation, RorL))
regionCode <- 'BRRO'
write.csv(RegionalResults3, paste('processedStationData/final2020data/RegionalResultsRiverine_',
                                  regionCode,'CitMon.csv',sep=''), row.names=FALSE)



# now get raw data into conventionals format for app

citRaw <- read_csv('C:/HardDriveBackup/R/GitHub/Rivers-StreamsAssessment/CitizenNonAgency/2020IR Citizen Ambient4.14.19 (2).csv') %>%
  dplyr::select(Group_Station_ID:`DEQ QA Comments`) # drop junk columns

cit1 <- distinct(citRaw, Group_Station_ID, FDT_STA_ID, Latitude, Longitude, .keep_all =T)  %>%
  dplyr::select(Group_Station_ID,  FDT_STA_ID:FDT_SPG_CODE, Latitude:STA_CBP_NAME)# drop data to avoid any confusion

citCounts <- cit1 %>% 
  group_by(Group_Station_ID, Latitude, Longitude) %>%
  summarize(`Number of Station Sampling Events` = n())

citDetailscit <- cit1 %>% 
  group_by(Group_Station_ID, Latitude, Longitude) %>%
  mutate(`Number of Station Sampling Events` = n(),
         STA_DESC_norm = str_replace_all(STA_DESC, "[^[:alnum:]]", " ")) %>%
  ungroup() %>%
  distinct() %>%
  arrange(Group_Station_ID) 
citDetailscit$rowname <- c(1:nrow(citDetailscit))
# ugly addition singe add_rownames() alter data type


# now attach the rowname identifier to raw data
citRaw1 <- left_join(citRaw, 
                     dplyr::select(citDetailscit, Group_Station_ID, FDT_STA_ID,
                                   STA_DESC, Latitude, Longitude, rowname),
                     by= c("Group_Station_ID", "FDT_STA_ID", "STA_DESC","Latitude" ,                        
                           "Longitude"  ))
# make sure citRaw1 has same rows as citRaw or some weird join may have happened
nrow(citRaw1)==nrow(citRaw)

# now just get rows with rowname ( unique stations)
citRaw2 <- filter(citRaw1, !is.na(rowname))



# Now filter raw cit data to just sites Mary wants to assess
citRawFinal <- filter(citRaw2, rowname %in% unique(RegionalResults$rowname))

# now make citRawFinal match conventionals
conventionals <- read_csv('data/final2020data/CEDSWQM_2020_IR_DATA-CONVENTIONALS_20190305.csv') %>%
  filter(!is.na(Latitude)|!is.na(Longitude)) # remove sites without coordinates
conventionals$FDT_DATE_TIME2 <- as.POSIXct(conventionals$FDT_DATE_TIME, format="%m/%d/%Y %H:%M") # turn character date format into proper date time format

citRawFinal1 <- mutate(citRawFinal, 
                      FDT_COMMENT = paste('FDT_COMMENT:', FDT_COMMENT, 
                                          'QAQC_Comments:',QAQC_Comments,
                                          'DEQ QA Comments:',`DEQ QA Comments`),
                      FDT_DATE_TIME2 = as.POSIXct(FDT_DATE_TIME, format="%m/%d/%y %H:%M")) %>%
  dplyr::select(names(conventionals))

# Combine with real conventionals
conventionals2 <- rbind(conventionals, citRawFinal1)


## Before we can run the riverine app, we need to attach correct Ecoregion information. Benthics data needs ecoregion attached to unique sites to determine SCI to use.



# save new Conventionals data
write.csv(conventionals2, 'data/conventionals_final2020_citmon1.csv', row.names = F)




# Bring in Level 3 ecoregion
library(sf)
ecoregion <- st_read('GIS/VA_level3ecoregion.shp') %>%
  st_transform( st_crs(4326)) # transform to WQS84 for spatial intersection

# Bring back in assessment regions information one last time because conventionals
#doesn't have Basin in 2020

assessmentLayer <- st_read('GIS/AssessmentRegions_VA84_basins.shp') %>%
  st_transform( st_crs(4326)) # transform to WQS84 for spatial intersection


# Make conventionals spatial object
conventionals_sf <- conventionals2 %>%
  distinct(FDT_STA_ID, .keep_all = T) %>%
  st_as_sf( coords = c("Longitude", "Latitude"), 
            remove = F, # don't remove these lat/lon cols from df
            crs = 4326)  # add projection, needs to be geographic bc entering lat/lng

# make sure everything will work
identical(st_crs(conventionals_sf),st_crs(ecoregion))
identical(st_crs(conventionals_sf),st_crs(assessmentLayer))

# first get Basin information for conventionals, then Ecoregion
conventionals_sf <-  st_join(conventionals_sf, 
                             select(assessmentLayer, Basin), join = st_intersects) %>%
  st_join(select(ecoregion, US_L3CODE, US_L3NAME), join = st_intersects) %>%
  st_set_geometry(NULL) # take away spatial component so it can go into csv nicely

saveRDS(conventionals_sf, 'data/conventionals_sf_final2020_citmon1.RDS')
