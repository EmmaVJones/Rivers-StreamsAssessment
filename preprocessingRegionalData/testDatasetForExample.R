# this script makes a test dataset for demoing data preprocessing steps from BRRO

stationTable <- read_excel('data/BRRO/2018StnsDB_APP.xlsx', sheet='2018IR_Stations_EMMA') %>%
  rename('COMMENTS'='Notes' )

conventionalsKeep <- filter(conventionals_D_Region, FDT_STA_ID %in% c('4ABWR029.22','4ALVL003.26',
                                                                      '4ABWC001.00','4ABWR061.20','4ACAN000.80',
                                                                      '4ABWA002.00','4ACRR003.56','4ACRV005.10')) 
saveRDS(conventionalsKeep, 'data/conventionalsKeep.RDS')
