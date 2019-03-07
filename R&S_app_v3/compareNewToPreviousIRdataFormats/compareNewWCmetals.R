WCmetals <- read_excel('data/draft2020data/CEDSWQM_2020_IR_DATA-WATER_METALS_ASSESSMENT_20190207.xlsx')

WCmetalsOld <- read_excel('data/WATER_METALS_20170712.xlsx')

names(WCmetals)==names(WCmetalsOld)

names(WCmetals)[34:40]
names(WCmetalsOld)[34:40]


# so new version doesnt include any raw data, need to add that in
# bring in raw values
WCmetalsVals <- read_excel('data/draft2020data/CEDSWQM_2020_IR_DATA-WATER_METALS_VALUES_20190207.xlsx')
WCmetals <- select(WCmetals, STA_ID:`ZINC ALL OTHER SURFACE WATERS`) 
WCmetalsVals2 <- left_join(WCmetalsVals, 
                           select(WCmetals,FDT_STA_ID,FDT_DATE_TIME, 
                                  `ANTIMONY HUMAN HEALTH PWS`:`ZINC ALL OTHER SURFACE WATERS`),
                           by= c("FDT_STA_ID","FDT_DATE_TIME"))
names(WCmetalsVals2) %in% names(WCmetalsOld)
# so only 
names(WCmetalsVals2)[83]
# added to new version, but that's cool.

# save dataset out for use in app
write.csv(WCmetalsVals2, 'data/draft2020data/CEDSWQM_2020_IR_DATA-WATER_METALS_VALUES_20190207_EVJ.csv',row.names = F)
