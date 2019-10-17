suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(readxl))
suppressPackageStartupMessages(library(sf))
suppressPackageStartupMessages(library(leaflet))
suppressPackageStartupMessages(library(mapview))


# too big to read in using read_excel
conventionals <- read_csv('data/final2020data/CEDSWQM_2020_IR_DATA-CONVENTIONALS_20190305.csv') %>%
  filter(!is.na(Latitude)|!is.na(Longitude)) # remove sites without coordinates
conventionals$FDT_DATE_TIME2 <- as.POSIXct(conventionals$FDT_DATE_TIME, format="%m/%d/%Y %H:%M") # turn character date format into proper date time format

# Bring in James' citizen dataset
cit <- read_csv('CitizenNonAgency/2020IR Citizen Ambient4.14.19.csv') %>%
  dplyr::select(Group_Station_ID:`DEQ QA Comments`) # drop junk columns
#nonA <-  readxl::read_excel('CitizenNonAgency/2020IR NONA ambient.xlsx')

# Looks decent so far, all but one column from conventionals is in cit dataset and I created it
# go James!
names(cit) %in% names(conventionals)
names(conventionals) %in% names(cit)


# Need to map DO to DO

# need to clarify parameter remark system, either factor with level or somehting else
# unique(cit$FDT_TEMP_CELCIUS_RMK); unique(cit$FDT_SPECIFIC_CONDUCTANCE_RMK)
# unique(cit$RMK_00600)


#unique(cit$`NITROGEN_TOTAL_00600_mg/L`) true or false???







# work with spatial aspect, play with different ways of getting unique identifiers- group station Id (citizen assigned) and lat/long combos

# Count occurrences of each Group_Station_ID
cit1 <- dplyr::select(cit, Group_Station_ID, FDT_STA_ID, STA_DESC, Latitude, Longitude)

citCounts <- cit1 %>% 
  group_by(Group_Station_ID, Latitude, Longitude) %>%
  summarize(`Number of Station Sampling Events` = n())

citDetailscit <- cit1 %>% 
  group_by(Group_Station_ID, Latitude, Longitude) %>%
  mutate(`Number of Station Sampling Events` = n(),
         STA_DESC_norm = str_replace_all(STA_DESC, "[^[:alnum:]]", " ")) %>%
  distinct() %>%
  arrange(Group_Station_ID) %>%
  tibble::rownames_to_column() %>%
  as_tibble()

missingLocation <- filter(citDetailscit, is.na(Latitude) | is.na(Longitude) | Latitude == 0 | Longitude == 0)

citDetailscit_sf <- citDetailscit %>%
  filter(!(rowname %in% missingLocation$rowname)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"),  # make spatial layer using these columns
           remove = F, # don't remove these lat/lon cols from df
           crs = 4326) # add coordinate reference system, needs to be geographic for now bc entering lat/lng,


# get rid of weird characters to plot points
weirdThingsFixed <- citDetailscit_sf %>% dplyr::select(-STA_DESC)
mapview(weirdThingsFixed, label=weirdThingsFixed$Group_Station_ID)


#weirdThingsFixed1 <-weirdThingsFixed %>% st_set_geometry(NULL) 
#weirdThingsFixed1 <- as.data.frame(weirdThingsFixed1)
#leaflet(weirdThingsFixed1) %>%
#m <- mapview(weirdThingsFixed) 
#leaflet::addMarkers(m, lng=weirdThingsFixed1$Longitude, lat=weirdThingsFixed1$Latitude, clusterOptions=markerClusterOptions())

write.csv(weirdThingsFixed, 'CitizenNonAgency/CitMonUniqueSamples.csv',row.names = FALSE)
write.csv(missingLocation, 'CitizenNonAgency/MissingLocation.csv', row.names = FALSE)

# problems 
# duplicate group station ID names
# missing lat longs or 0 for lat/lng
# weird characters in comment fields: STA_DESC 134:  "Lime Kiln�Road, Rt. 733.�Go 1.2 mi to site on left side of road.�Walk down bank & upstream to riffles."
write.csv(citDetailscit, 'SpatialDu')
