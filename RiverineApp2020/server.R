# Run in R 3.5.1
source('global.R')
source('AUshapefileLocation.R')



# Draft 2020 data
conventionals <- read_csv('data/final2020data/CEDSWQM_2020_IR_DATA-CONVENTIONALS_20190305.csv') %>%
  filter(!is.na(Latitude)|!is.na(Longitude)) %>% # remove sites without coordinates
  rename("FDT_TEMP_CELCIUS"  ="TEMPERATURE_00010_DEGREES CENTIGRADE",
         "FDT_TEMP_CELCIUS_RMK" = "FDT_TEMP_CELCIUS_RMK",  
         "FDT_FIELD_PH" = "pH_00400_STD_UNITS" ,          
         "FDT_FIELD_PH_RMK"  ="FDT_FIELD_PH_RMK", 
         "DO" =  "DO_mg/L",       
         "DO_RMK"  ="DO_RMK",    
         "FDT_SPECIFIC_CONDUCTANCE"="SPECIFIC_CONDUCTANCE_00095_UMHOS/CM @ 25C",    
         "FDT_SPECIFIC_CONDUCTANCE_RMK" ="FDT_SPECIFIC_CONDUCTANCE_RMK" ,
         "FDT_SALINITY" = "SALINITY_00480_PPT" ,            
         "FDT_SALINITY_RMK"  ="FDT_SALINITY_RMK",  
         "NITROGEN" = "NITROGEN_mg/L" ,                    
         "AMMONIA" ="AMMONIA_mg/L",
         "PHOSPHORUS" =  "PHOSPHORUS_mg/L",
         "FECAL_COLI" = "FECAL_COLIFORM_31616_NO/100mL" ,
         "E.COLI" = "ECOLI_CFU/100mL",                       
         "ENTEROCOCCI" =  "ENTEROCOCCI_31649_NO/100mL",
         "CHLOROPHYLL" ="CHLOROPHYLL_32211_ug/L",                
         "SSC" ="SSC-TOTAL_00530_mg/L" , 
         "NITRATE" ="NITRATE_mg/L", 
         "CHLORIDE" ="CHLORIDE_mg/L",
         "SULFATE_TOTAL" ="SULFATE_TOTAL_00945_mg/L",              
         "SULFATE_DISS" ="SULFATE_DISSOLVED_00946_mg/L")
conventionals$FDT_DATE_TIME2 <- as.POSIXct(conventionals$FDT_DATE_TIME, format="%m/%d/%Y %H:%M")

WCmetals <- read_csv('data/final2020data/CEDSWQM_2020_IR_DATA-WATER_METALS_VALUES_20190207_EVJ.csv')
Smetals <- read_excel('data/final2020data/CEDSWQM_2020_IR_DATA-CEDSWQM_SEDIMENT_20190213.xlsx') %>%
  dplyr::select(FDT_STA_ID:ZINC...70, COMMENT...89)
names(Smetals) <- gsub( "[..].*", "", names(Smetals)) # remove anything after .. in name

# Statewide Assessment layer
assessmentLayer <- st_read('GIS/AssessmentRegions_VA84_basins.shp') %>%
  st_transform( st_crs(4326)) 

# Bring in latest EDAS VSCI and (combined) VCPMI queries, not current for 2020 IR because biologists won't be finished IDing and entering bugs into EDAS until about MAY
VSCI <- read_excel('data/Family Metrics VSCI Calculation.xlsx')%>%
  filter(RepNum == 1 & Target_Count == 110 &
           CollDate >= assessmentPeriod[1] )
VCPMI <- read_excel('data/Family Metrics - CPMI Combined.xlsx')%>%
  filter(RepNum == 1 & Target_Count == 110 &
           CollDate >= assessmentPeriod[1] )


mapviewOptions(basemaps = c( "OpenStreetMap",'Esri.WorldImagery'),
               vector.palette = colorRampPalette(brewer.pal(8, "Set1")),
               na.color = "magenta",
               legend=FALSE)



shinyServer(function(input, output, session) {

  # display the loading feature until data loads into app
  load_data()
  
  # Reactive Value to store all site data
  siteData <- reactiveValues()
  
  ## Data Upload Tab
  #stationTable <- reactive({read_csv('data/RegionalResults_AU_WQS.csv') %>%
  #    # fix periods in column names from excel
  #    as_tibble() %>%
  #    dplyr::rename(`Point Unique Identifier` ="Point.Unique.Identifier", `Buffer Distance` = "Buffer.Distance")})#'data/BRRO_Sites_AU_WQS.csv')})#readRDS('data/BRROsites_ROA_sf.RDS')})
  # Where I will go after testing
  stationTable <- reactive({
    req(input$stationsTable)
    inFile <- input$stationsTable
    read_csv(inFile$datapath) %>%
   #fix periods in column names from excel
    as_tibble() %>%
    dplyr::rename(`Point Unique Identifier` ="Point.Unique.Identifier", `Buffer Distance` = "Buffer.Distance") %>%
      mutate(VAHU6 = ifelse(is.na(VAHU6), Huc6_Vahu6, VAHU6)) %>% # station table fix if necessary
      # New CLASS II fixes
      mutate(CLASS_BASIN = paste(CLASS,substr(BASIN, 1,1), sep="_")) %>%
      mutate(CLASS_BASIN = ifelse(CLASS_BASIN == 'II_7', "II_7", CLASS))
    
  })
  
 
  
  
  
  ## Watershed Selection Tab
  
  # Query VAHUC6's By Selectize arguments
  the_data <- reactive({assessmentLayer})
  region_filter <- shiny::callModule(dynamicSelect, "DEQregionSelection", the_data, "ASSESS_REG" )
  basin_filter <- shiny::callModule(dynamicSelect, "basinSelection", region_filter, "Basin" )
  huc6_filter <- shiny::callModule(dynamicSelect, "HUC6Selection", basin_filter, "VAHU6" )
  AUs <- reactive({req(huc6_filter(), regionalAUs)
    suppressWarnings(st_intersection(st_zm(regionalAUs), huc6_filter()))})
 
  
  # Station Map
  output$VAmap <- renderLeaflet({
    req(region_filter(), basin_filter(), huc6_filter())
    m <- mapview(basin_filter(),label= basin_filter()$VAHU6, layer.name = 'Basin Chosen',
                 popup= popupTable(basin_filter(), zcol=c('VAHU6',"VaName","VAHU5","ASSESS_REG"))) + 
      mapview(huc6_filter(), color = 'yellow',lwd= 5, label= huc6_filter()$VAHU6, layer.name = c('Selected HUC6'),
              popup= popupTable(huc6_filter(), zcol=c('VAHU6',"VaName","VAHU5","ASSESS_REG")))
    m@map %>% setView(st_bbox(huc6_filter())$xmax[[1]],st_bbox(huc6_filter())$ymax[[1]],zoom = 9) })
  
  # Table of AUs within Selected VAHU6
  output$AUSummary <-  DT::renderDataTable({ req(regionalAUs,AUs())
    DT::datatable(AUs() %>% st_set_geometry(NULL), rownames = FALSE, 
                  options= list(scrollX = TRUE, pageLength = nrow(AUs()), scrollY = "300px", dom='Bt')) 
  })
  
  # Table of Stations within Selected VAHU6
  output$stationSummary <- DT::renderDataTable({
    req(region_filter(), basin_filter(), huc6_filter())
    z <- filter(conventionals, Huc6_Vahu6 %in% huc6_filter()$VAHU6) %>%
      distinct(FDT_STA_ID, .keep_all = TRUE) %>%
      select(FDT_STA_ID:FDT_SPG_CODE, STA_LV2_CODE:STA_CBP_NAME) %>% 
      mutate(`Analyzed By App` = ifelse(FDT_STA_ID %in% unique(stationTable()$FDT_STA_ID), 'yes','no'))
    DT::datatable(z, rownames = FALSE, options= list(scrollX = TRUE, pageLength = nrow(z), scrollY = "300px", dom='Bt')) %>%
      formatStyle('Analyzed By App', target = 'row', backgroundColor = styleEqual(c('yes','no'), c('lightgray', 'yellow')))
  })
  
  
  # Button to visualize modal map of AUs in selected VAHU6
  observeEvent(input$reviewAUs,{
    showModal(modalDialog(
      title="Preview Assessment Units for Selected VAHU6",
      leafletOutput('AUmap'),
      easyClose = TRUE))  })
  
  # modal map
  output$AUmap <- renderLeaflet({
    req(region_filter(), basin_filter(), huc6_filter())
    
    z <- AUs()
    z$ID305B <- factor(z$ID305B) # drop extra factor levels so colors come out right
    
    m <- mapview(huc6_filter(), color = 'yellow',lwd= 5, label= NULL, layer.name = c('Selected HUC6'),
                 popup= popupTable(huc6_filter(), zcol=c('VAHU6',"VaName","VAHU5","ASSESS_REG"))) + 
      mapview(z, label= z$ID305B, layer.name = c('AUs in Selected HUC6'), zcol = "ID305B", legend=FALSE,
              popup= popupTable(z, zcol=c("ID305B","MILES","CYCLE","WATER_NAME","LOCATION" )))
    m@map })
  
  
  
  
  
  ## Assessment Unit Review Tab
  
  
  # Show selected AU
  output$selectedHUC <- DT::renderDataTable({
    datatable(huc6_filter() %>% st_set_geometry(NULL) %>% select(VAHU6, VaName, Basin),
              rownames = FALSE, options= list(pageLength = 1, scrollY = "35px", dom='t'))})
  
  
  # Don't let user click pull data button if no conventionals data for VAHU6
  observe({
    shinyjs::toggleState('pullHUCdata', nrow(AUs())!=0 )
  })
  
  
  # Pull Conventionals data for selected AU on click
  conventionals_HUC <- eventReactive( input$pullHUCdata, {
    z <- filter(conventionals, Huc6_Vahu6 %in% huc6_filter()$VAHU6) %>%
      left_join(dplyr::select(stationTable(), FDT_STA_ID, SEC, CLASS_BASIN, CLASS, SPSTDS,PWS, ID305B_1, 
                              ID305B_2, ID305B_3,STATION_TYPE_1, STATION_TYPE_2, STATION_TYPE_3, Basin
                              ), by='FDT_STA_ID') %>%
      filter(!is.na(ID305B_1))
    print(z)
    return(z)})
  
  output$AUSelection_ <- renderUI({ req(conventionals_HUC())
    selectInput('AUSelection', 'Assessment Unit Selection', choices = unique(conventionals_HUC()$ID305B_1))  })
  
  output$selectedAU <- DT::renderDataTable({req(conventionals_HUC(),input$AUSelection)
    z <- filter(regionalAUs, ID305B %in% input$AUSelection) %>% st_set_geometry(NULL) %>% as.data.frame()
    datatable(z, rownames = FALSE, 
              options= list(pageLength = nrow(z),scrollX = TRUE, scrollY = "200px", dom='t'))})

  output$stationSelection_ <- renderUI({ req(conventionals_HUC(), input$AUSelection)
    z <- filter(conventionals_HUC(), ID305B_1 %in% input$AUSelection | 
                   ID305B_2 %in% input$AUSelection | 
                   ID305B_2 %in% input$AUSelection) %>%
      distinct(FDT_STA_ID)
    fluidRow(selectInput('stationSelection', 'Station Selection', choices = unique(z$FDT_STA_ID)),
             helpText("The stations available in the drop down are limited to stations with an ID305B_1 designation equal 
                      to the selected AU. All AU's associated with the selected station can be viewed in the map below."))})
  
  AUData <- eventReactive( input$AUSelection, {
    filter(conventionals_HUC(), ID305B_1 %in% input$AUSelection | 
             ID305B_2 %in% input$AUSelection | 
             ID305B_2 %in% input$AUSelection) %>% 
      left_join(WQSvalues, by = 'CLASS_BASIN') %>%
      dplyr::select(-c(CLASS.y,CLASS_BASIN)) %>%
      rename('CLASS' = 'CLASS.x')
    }) 
  
  stationData <- eventReactive( input$stationSelection, {
    filter(AUData(), FDT_STA_ID %in% input$stationSelection) })
  
  output$stationInfo <- DT::renderDataTable({ req(stationData())
    z <- filter(stationTable(), FDT_STA_ID == input$stationSelection) %>% 
      select(FDT_STA_ID:STA_CBP_NAME, `Point Unique Identifier`:Shape_Leng ) %>%
      t() %>% as.data.frame() %>% rename(`Station and WQS Information` = 1)
    DT::datatable(z, options= list(pageLength = nrow(z), scrollY = "250px", dom='t'))  })
  
  output$stationMap <- renderLeaflet({
    req(stationData())
    point <- select(stationData()[1,],  FDT_STA_ID:FDT_SPG_CODE, STA_LV2_CODE:ID305B_3, Latitude, Longitude ) %>%
      st_as_sf(coords = c("Longitude", "Latitude"), 
               remove = F, # don't remove these lat/lon cols from df
               crs = 4269) # add projection, needs to be geographic for now bc entering lat/lng
    segment <- filter(regionalAUs, ID305B %in% as.character(point$ID305B_1) |
                        ID305B %in% as.character(point$ID305B_2) |
                        ID305B %in% as.character(point$ID305B_3))
    map1 <- mapview(segment,zcol = 'ID305B', label= segment$ID305B, layer.name = 'Assessment Unit (ID305B_1)',
                    popup= popupTable(segment, zcol=c("ID305B","MILES","CYCLE","WATER_NAME"))) + 
      mapview(point, color = 'yellow', lwd = 5, label= point$FDT_STA_ID, layer.name = c('Selected Station'),
              popup=NULL)
    map1@map
  })
  
  output$stationHistoricalInfo <- DT::renderDataTable({ req(stationData())
    z <- filter(stationTable(), FDT_STA_ID == input$stationSelection) %>% 
      select(STATION_ID:COMMENTS) %>%
      t() %>% as.data.frame() %>% rename(`Station Information From Last Cycle` = 1)
    DT::datatable(z, options= list(pageLength = nrow(z), scrollY = "250px", dom='t'))  })
  
  
  
  ## Station Table View Section
  observe(siteData$StationTablePrelimStuff <- StationTableStartingData(stationData()))
  
  observe(siteData$StationTableResults1 <- cbind(tempExceedances(stationData()), 
                                                 DOExceedances_Min(stationData()), pHExceedances(stationData()),
                                                 bacteriaExceedances_OLD(bacteria_Assessment_OLD(stationData(), 'E.COLI', 126, 235),'E.COLI') %>% 
                                                   dplyr::rename('ECOLI_VIO' = 'E.COLI_VIO', 'ECOLI_SAMP'='E.COLI_SAMP', 'ECOLI_STAT'='E.COLI_STAT'),
                                                 # new bacteria exceedance stats
                                                 bacteriaExceedances_NEW(stationData(),'E.COLI', 10, 410, 126),
                                                 bacteriaExceedances_OLD(bacteria_Assessment_OLD(stationData(), 'ENTEROCOCCI', 35, 104),'ENTEROCOCCI') %>% 
                                                   dplyr::rename('ENTER_VIO' = 'ENTEROCOCCI_VIO', 'ENTER_SAMP'='ENTEROCOCCI_SAMP', 'ENTER_STAT'='ENTEROCOCCI_STAT'),
                                                 # new bacteria exceedance stats
                                                 bacteriaExceedances_NEW(stationData(),'ENTEROCOCCI',10, 35, 104),
                                                 metalsExceedances(filter(WCmetals, FDT_STA_ID %in% stationData()$FDT_STA_ID) %>% 
                                                                     dplyr::select(`ANTIMONY HUMAN HEALTH PWS`:`ZINC ALL OTHER SURFACE WATERS`), 'WAT_MET'))%>%
            dplyr::select(-ends_with('exceedanceRate')))
  
  
  output$stationTableDataSummary <- DT::renderDataTable({
    req(stationData())
    # Crazy data manipulation here to make sure factor NA's converted to real NA and string edits for reactive object creative solution
    z <- siteData$StationTablePrelimStuff%>%
      mutate_all(as.character)
    z[z =="NA"] <- NA
    watID <-  substr(strsplit(as.character(z$ID305B_1), '-')[[1]][2] , 1, 3)
    z$WATERSHED_ID <- watID 
    AMM <- acuteNH3exceedance(stationData()) %>% # ammonia function being a pain so forcing it in
      dplyr::select(AcuteAmmonia_VIO, AcuteAmmonia_STAT) %>% 
      dplyr::rename('WAT_TOX_VIO' ='AcuteAmmonia_VIO','WAT_TOX_STAT' = 'AcuteAmmonia_STAT')#data.frame(WAT_TOX_VIO='Not Analyzed by App', WAT_TOX_STAT='Not Analyzed by App'),# Placeholder for water toxics
    chronicAmmonia <- chronicNH3exceedance(stationData()) %>% # ammonia function being a pain so forcing it in
      dplyr::select(ChronicAmmonia_VIO)
    
    more <- cbind(metalsExceedances(filter(Smetals, FDT_STA_ID %in% stationData()$FDT_STA_ID) %>% 
                                      dplyr::select(ARSENIC:ZINC), 'SED_MET') %>%
                    dplyr::select(-ends_with('exceedanceRate')),
                  data.frame(SED_TOX_VIO='Not Analyzed by App', SED_TOX_STAT='Not Analyzed by App'),# Placeholder for sediment toxics
                  data.frame(FISH_MET_VIO='Not Analyzed by App', FISH_MET_STAT='Not Analyzed by App'), # Placeholder for fish metals
                  data.frame(FISH_TOX_VIO='Not Analyzed by App', FISH_TOX_STAT='Not Analyzed by App'),# Placeholder for fish toxics
                  benthicAssessment(stationData(),conventionals_sf,VSCI,VCPMI),
                  countTP(stationData()),
                  countchla(stationData()),
                  data.frame(COMMENTS=  paste('WAT_TOX fields indicate acute ammonia calculations. Chronic Ammonia Violations: ',chronicAmmonia[1,]))) %>%
      dplyr::select(-ends_with('exceedanceRate'))
    z2 <- cbind(z, siteData$StationTableResults1, AMM, more)
    
    datatable(z2, extensions = 'Buttons', escape=F, rownames = F, editable = TRUE,
              options= list(scrollX = TRUE, pageLength = nrow(z2),
                            # hide certain columns
                            #columnDefs = list(list(targets = 6, visible = FALSE)),
                            dom='Bt', buttons=list('copy',
                                                   list(extend='csv',filename=paste('AssessmentResults_',paste(assessmentCycle,input$stationSelection, collapse = "_"),Sys.Date(),sep='')),
                                                   list(extend='excel',filename=paste('AssessmentResults_',paste(assessmentCycle,input$stationSelection, collapse = "_"),Sys.Date(),sep=''))))) %>% 
      formatStyle(c('TEMP_SAMP','TEMP_VIO','TEMP_STAT'), 'TEMP_STAT', backgroundColor = styleEqual(c('Review', '10.5% Exceedance'), c('yellow','red'))) %>%
      formatStyle(c('DO_SAMP','DO_VIO','DO_STAT'), 'DO_STAT', backgroundColor = styleEqual(c('Review', '10.5% Exceedance'), c('yellow','red'))) %>%
      formatStyle(c('PH_SAMP','PH_VIO','PH_STAT'), 'PH_STAT', backgroundColor = styleEqual(c('Review', '10.5% Exceedance'), c('yellow','red'))) %>%
      formatStyle(c('ECOLI_SAMP','ECOLI_VIO','ECOLI_STAT'), 'ECOLI_STAT', backgroundColor = styleEqual(c('Review', '10.5% Exceedance'), c('yellow','red'))) %>%
      formatStyle(c('ECOLI_STV_VIO','ECOLI_GEOMEAN_VIO','ECOLI_STAT_NEW'), 'ECOLI_STAT_NEW', backgroundColor = styleEqual(c('Review', '10.5% Exceedance'), c('yellow','red'))) %>%
      formatStyle(c('ENTER_SAMP','ENTER_VIO','ENTER_STAT'), 'ENTER_STAT', backgroundColor = styleEqual(c('Review', '10.5% Exceedance'), c('yellow','red'))) %>%
      formatStyle(c('ENTER_STV_VIO','ENTER_GEOMEAN_VIO','ENTER_STAT_NEW'), 'ENTER_STAT_NEW', backgroundColor = styleEqual(c('Review', '10.5% Exceedance'), c('yellow','red'))) %>%
      formatStyle(c('WAT_MET_VIO','WAT_MET_STAT'), 'WAT_MET_STAT', backgroundColor = styleEqual(c('Review', '10.5% Exceedance'), c('yellow','red'))) %>%
      formatStyle(c('WAT_TOX_VIO','WAT_TOX_STAT'), 'WAT_TOX_STAT', backgroundColor = styleEqual(c('Review', '10.5% Exceedance'), c('yellow','red'))) %>%
      formatStyle(c('SED_MET_VIO','SED_MET_STAT'), 'SED_MET_STAT', backgroundColor = styleEqual(c('Review', '10.5% Exceedance'), c('yellow','red'))) %>%
      formatStyle(c('BENTHIC_STAT'), 'BENTHIC_STAT', backgroundColor = styleEqual(c('Review'), c('yellow')))  })
  
  output$PWStable <- DT::renderDataTable({
    req(stationData())
    if(is.na(unique(stationData()$PWS))){
      PWSconcat <- data.frame(STATION_ID = unique(stationData()$FDT_STA_ID),
                              PWS= 'PWS Standards Do Not Apply To Station')
      DT::datatable(PWSconcat, escape=F, rownames = F, options= list(scrollX = TRUE, pageLength = nrow(PWSconcat), dom='t'))
      
    } else {
      PWSconcat <- cbind(data.frame(STATION_ID = unique(stationData()$FDT_STA_ID)),
                         nitratePWS(stationData()),
                         chloridePWS(stationData()),
                         TSulfatePWS(stationData())) %>%
        dplyr::select(-ends_with('exceedanceRate'))
      
      DT::datatable(PWSconcat, escape=F, rownames = F, options= list(scrollX = TRUE, pageLength = nrow(PWSconcat), dom='t')) %>% 
        formatStyle(c("PWS_Acute_Nitrate_VIO","PWS_Acute_Nitrate_SAMP","PWS_Acute_Nitrate_STAT"), "PWS_Acute_Nitrate_STAT", backgroundColor = styleEqual(c('Review'), c('red'))) %>%
        formatStyle(c("PWS_Acute_Chloride_VIO","PWS_Acute_Chloride_SAMP","PWS_Acute_Chloride_STAT"), "PWS_Acute_Chloride_STAT", backgroundColor = styleEqual(c('Review'), c('red'))) %>%
        formatStyle(c("PWS_Acute_Total_Sulfate_VIO","PWS_Acute_Total_Sulfate_SAMP","PWS_Acute_Total_Sulfate_STAT"), "PWS_Acute_Total_Sulfate_STAT", backgroundColor = styleEqual(c('Review'), c('red'))) 
    }
    
  })
  
  
  
  #### Data Sub Tab ####---------------------------------------------------------------------------------------------------
  
  # Display Data 
  output$AURawData <- DT::renderDataTable({ AUData()
    DT::datatable(AUData(), extensions = 'Buttons', escape=F, rownames = F, 
                  options= list(scrollX = TRUE, pageLength = nrow(AUData()), scrollY = "300px", 
                                dom='Btf', buttons=list('copy',
                                                        list(extend='csv',filename=paste('AUData_',paste(input$stationSelection, collapse = "_"),Sys.Date(),sep='')),
                                                        list(extend='excel',filename=paste('AUData_',paste(input$stationSelection, collapse = "_"),Sys.Date(),sep='')))))})
  # Summarize data
  output$stationDataTableRecords <- renderText({
    req(AUData())
    paste(nrow(AUData()), 'records were retrieved for',as.character(input$AUSelection),sep=' ')})
  output$uniqueStationDataTableRecords <- renderTable({
    req(AUData())
    plyr::count(AUData(), vars = c("FDT_STA_ID"))%>%dplyr::rename('Number of Records'='freq')})
  output$stationDataTableAssessmentWindow <- renderText({
    req(AUData())
    withinAssessmentPeriod(AUData())})
  
  # Need this as a reactive to regenerate below modules when user changes station 
  stationSelected <- reactive({input$stationSelection})
  
  ## Temperature Sub Tab ##------------------------------------------------------------------------------------------------------
  
  callModule(temperaturePlotlySingleStation,'temperature', AUData, stationSelected)
  callModule(temperatureExceedanceAnalysis,'temperature_ExceedanceAnalysis', AUData)
  
  ## pH Sub Tab ##------------------------------------------------------------------------------------------------------
  
  callModule(pHPlotlySingleStation,'pH', AUData, stationSelected)
  callModule(pHExceedanceAnalysis,'pH_ExceedanceAnalysis', AUData)
  
  ## DO Sub Tab ##------------------------------------------------------------------------------------------------------
  callModule(DOPlotlySingleStation,'DO', AUData, stationSelected)
  callModule(DOExceedanceAnalysis,'DO_ExceedanceAnalysis', AUData)
  
  ## Specific Conductance Sub Tab ##------------------------------------------------------------------------------------------------------
  callModule(SpCondPlotlySingleStation,'SpCond', AUData, stationSelected)
  
  ## Salinity Sub Tab ##------------------------------------------------------------------------------------------------------
  callModule(salinityPlotlySingleStation,'salinity', AUData, stationSelected)
  
  ## Total Nitrogen Sub Tab ##------------------------------------------------------------------------------------------------------
  callModule(TNPlotlySingleStation,'TN', AUData, stationSelected)
  
  ## Ammonia Sub Tab ##------------------------------------------------------------------------------------------------------
  callModule(AmmoniaPlotlySingleStation,'Ammonia', AUData, stationSelected)
  
  ## Total Phosphorus Sub Tab ##------------------------------------------------------------------------------------------------------
  callModule(TPPlotlySingleStation,'TP', AUData, stationSelected)
  
  ## Fecal Coliform Sub Tab ##------------------------------------------------------------------------------------------------------
  callModule(fecalPlotlySingleStation,'fecal', AUData, stationSelected)
  
  ## E.coli Sub Tab ##------------------------------------------------------------------------------------------------------
  callModule(EcoliPlotlySingleStation,'Ecoli', AUData, stationSelected)
  
  ## Enteroccoci Sub Tab ##------------------------------------------------------------------------------------------------------
  callModule(EnteroPlotlySingleStation,'Entero', AUData, stationSelected)
  
  ## Chlorophyll a Sub Tab ##------------------------------------------------------------------------------------------------------
  callModule(chlAPlotlySingleStation,'chlA', AUData, stationSelected)
  
  ## Suspended Sediments Sub Tab ##------------------------------------------------------------------------------------------------------
  callModule(SSCPlotlySingleStation,'SSC', AUData, stationSelected)
  
  ## Nitrate Sub Tab ##------------------------------------------------------------------------------------------------------
  callModule(NitratePlotlySingleStation,'Nitrate', AUData, stationSelected)
  
  ## Chloride Sub Tab ##------------------------------------------------------------------------------------------------------
  callModule(ClPlotlySingleStation,'Cl', AUData, stationSelected)
  
  ## Sulfate Sub Tab ##------------------------------------------------------------------------------------------------------
  callModule(DSulfatePlotlySingleStation,'DSulfate', AUData, stationSelected)
  
  
  
  
  #### Benthics Sub Tab ####---------------------------------------------------------------------------------------------------
  callModule(BenthicsPlotlySingleStation,'Benthics', AUData, stationSelected, conventionals_sf, VSCI, VCPMI)
  
  
  #### Metals Sub Tab ####---------------------------------------------------------------------------------------------------
  callModule(metalsTableSingleStation,'metals', AUData, WCmetals ,Smetals, stationSelected)
  
  
  
  
})

