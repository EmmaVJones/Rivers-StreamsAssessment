SmetalsOld <- read_excel('data/SEDIMENT_20170712.xlsx') %>% #fix column name duplications
  dplyr::select(FDT_STA_ID,`ACENAPHTHENE..194`:`COMMENT..227`) 
names(SmetalsOld)[2:35] <- gsub( "[..].*", "", names(Smetals)[2:35] )

Smetals <- read_excel('data/draft2020data/CEDSWQM_2020_IR_DATA-CEDSWQM_SEDIMENT_20190213.xlsx') %>%
  dplyr::select(FDT_STA_ID:ZINC..70, COMMENT..89)
names(Smetals) <- gsub( "[..].*", "", names(Smetals)) # remove anything after .. in name

FDT_STA_ID:ENDRINT; FDT_STA_ID:FDT_COMMENT, ARSENIC..54:COMMENT..89