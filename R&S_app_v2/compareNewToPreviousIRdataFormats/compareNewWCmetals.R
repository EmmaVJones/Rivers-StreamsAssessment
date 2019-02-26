WCmetals <- read_excel('data/draft2020data/CEDSWQM_2020_IR_DATA-WATER_METALS_ASSESSMENT_20190207.xlsx')

WCmetalsOld <- read_excel('data/WATER_METALS_20170712.xlsx')

names(WCmetals)==names(WCmetalsOld)

names(WCmetals)[34:40]
names(WCmetalsOld)[34:40]


# so new version doesnt include any raw data, need to add that in