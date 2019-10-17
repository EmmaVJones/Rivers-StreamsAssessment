# figure out non agency not in app


the_data <- reactive({assessmentLayer})
region_filter <- shiny::callModule(dynamicSelect, "DEQregionSelection", the_data, "ASSESS_REG" )
basin_filter <- shiny::callModule(dynamicSelect, "basinSelection", region_filter, "Basin" )
huc6_filter <- shiny::callModule(dynamicSelect, "HUC6Selection", basin_filter, "VAHU6" )
AUs <- reactive({req(huc6_filter(), regionalAUs)
  suppressWarnings(st_intersection(st_zm(regionalAUs), huc6_filter()))})

region_filter <- filter(assessmentLayer, ASSESS_REG == 'BRRO')
basin_filter <- filter(region_filter, Basin == 'James River Basin')
huc6_filter <- filter(basin_filter, VAHU6 == 'JM01')
AUs <- st_intersection(st_zm(regionalAUs), huc6_filter)
  
  
z <- filter(conventionals, Huc6_Vahu6 %in% huc6_filter$VAHU6) %>%
  distinct(FDT_STA_ID, .keep_all = TRUE) %>%
  select(FDT_STA_ID:FDT_SPG_CODE, STA_LV2_CODE:STA_CBP_NAME) %>% 
  mutate(`Analyzed By App` = ifelse(FDT_STA_ID %in% unique(stationTable()$FDT_STA_ID), 'yes','no'))




2MTT-VT77-UVA