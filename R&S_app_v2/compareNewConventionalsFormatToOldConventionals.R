library(tidyverse)


# bring in new conventionals
conventionals <- read_csv('C:/HardDriveBackup/R/GitHub/Rivers-StreamsAssessment/data/draft2020data/CEDSWQM_2020_IR_DATA-CONVENTIONALS_20190213.csv')[1:25,] %>%
  filter(!is.na(Latitude)|!is.na(Longitude)) # remove sites without coordinates
conventionals$FDT_DATE_TIME2 <- as.POSIXct(conventionals$FDT_DATE_TIME, format="%m/%d/%Y %H:%M") 


# bring in old conventionals
conventionalsOLD <- suppressWarnings(suppressMessages(read_csv('C:/HardDriveBackup/R/GitHub/Rivers-StreamsAssessment/workingDatasets/CONVENTIONALS_20171010.csv')))[1:25,]


names(conventionals)==names(conventionalsOLD)
names(conventionalsOLD)[!(names(conventionalsOLD) %in% names(conventionals))] # names in old conventionals that arent in new conventionals


[1] "DO"             "NITROGEN"       "AMMONIA"        "NH3_DISS"       "NH3_TOTAL"      "PHOSPHORUS"     "FECAL_COLI"     "E.COLI"         "ENTEROCOCCI"    "CHLOROPHYLL"    "SSC"           
[12] "SSC_RMK"        "NITRATE"        "CHLORIDE"       "SULFATE_TOTAL"  "SULFATE_DISS"   "Majorbasincode" "Majorbasinname" "Basin"          "Subbasin"      

conventionalsRename <- rename(conventionals, 'DO' = "DO_mg/L", "NITROGEN" = "NITROGEN_mg/L",  "AMMONIA" = "AMMONIA_mg/L" ,
                              #"NH3_DISS" = , "NH3_TOTAL"  , 
                              "PHOSPHORUS"= "PHOSPHORUS_mg/L" , "FECAL_COLI" = 'STORET_31616', "E.COLI" = 'ECOLI_CFU/100mL', 
                              "ENTEROCOCCI" = 'STORET_31649', "CHLOROPHYLL" = 'STORET_32211', "SSC" = "STORET_SSC-TOTAL" , 
                              "SSC_RMK" = "RMK_SSC-TOTAL" , "NITRATE" = "NITRATE_mg/L",  "CHLORIDE" = "CHLORIDE_mg/L" , 
                              "SULFATE_TOTAL" = "SULFATE_mg/L",   "SULFATE_DISS" = 'STORET_00946')

# Check again
names(conventionalsOLD)[!(names(conventionalsOLD) %in% names(conventionalsRename))] # names in old conventionals that arent in new conventionals
[1] "NH3_DISS"       "NH3_TOTAL"      "Majorbasincode" "Majorbasinname" "Basin"          "Subbasin" 

