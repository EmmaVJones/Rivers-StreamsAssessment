Smetals <- read_excel('data/SEDIMENT_20170712.xlsx') %>% #fix column name duplications
  dplyr::select(FDT_STA_ID,`ACENAPHTHENE..194`:`COMMENT..227`) 
names(Smetals)[2:35] <- gsub( "[..].*", "", names(Smetals)[2:35] )

