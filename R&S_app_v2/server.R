# Run in R 3.5.1
source('global.R')
source('AUshapefileLocation.R')



# Draft 2020 data
conventionals <- read_csv('data/draft2020data/CEDSWQM_2020_IR_DATA-CONVENTIONALS_20190213.csv') %>%
  filter(!is.na(Latitude)|!is.na(Longitude)) %>% # remove sites without coordinates
  rename('DO' = "DO_mg/L", "NITROGEN" = "NITROGEN_mg/L",  "AMMONIA" = "AMMONIA_mg/L" ,
       #"NH3_DISS" = , "NH3_TOTAL"  , 
       "PHOSPHORUS"= "PHOSPHORUS_mg/L" , "FECAL_COLI" = 'STORET_31616', "E.COLI" = 'ECOLI_CFU/100mL', 
       "ENTEROCOCCI" = 'STORET_31649', "CHLOROPHYLL" = 'STORET_32211', "SSC" = "STORET_SSC-TOTAL" , 
       "SSC_RMK" = "RMK_SSC-TOTAL" , "NITRATE" = "NITRATE_mg/L",  "CHLORIDE" = "CHLORIDE_mg/L" , 
       "SULFATE_TOTAL" = "SULFATE_mg/L",   "SULFATE_DISS" = 'STORET_00946')
conventionals$FDT_DATE_TIME2 <- as.POSIXct(conventionals$FDT_DATE_TIME, format="%m/%d/%Y %H:%M")

WCmetals <- read_csv('data/draft2020data/CEDSWQM_2020_IR_DATA-WATER_METALS_VALUES_20190207_EVJ.csv')
Smetals <- read_excel('data/draft2020data/CEDSWQM_2020_IR_DATA-CEDSWQM_SEDIMENT_20190213.xlsx') %>%
  dplyr::select(FDT_STA_ID:ZINC..70, COMMENT..89)
names(Smetals) <- gsub( "[..].*", "", names(Smetals)) # remove anything after .. in name


# Change global.R to read
#conventionals_sf <- readRDS('data/conventionals_sf_draft2020.RDS')

# Change AUshapefileLocation.R

# old data
#conventionals <- suppressWarnings(read_csv('data/CONVENTIONALS_20171010.csv'))
#conventionals$FDT_DATE_TIME2 <- as.POSIXct(conventionals$FDT_DATE_TIME, format="%m/%d/%Y %H:%M")


#WCmetals <- read_excel('data/WATER_METALS_20170712.xlsx')
#Smetals <- read_excel('data/SEDIMENT_20170712.xlsx') %>% #fix column name duplications
#  dplyr::select(FDT_STA_ID,`ACENAPHTHENE..194`:`COMMENT..227`) 
#names(Smetals)[2:35] <- gsub( "[..].*", "", names(Smetals)[2:35] )

# Statewide Assessment layer
assessmentLayer <- st_read('GIS/AssessmentRegions_VA84_basins.shp') %>%
  st_transform( st_crs(4326)) 

# Bring in latest EDAS VSCI and (combined) VCPMI queries
VSCI <- read_excel('data/Family Metrics VSCI Calculation.xlsx')%>%
  filter(RepNum == 1 & Target_Count == 110 &
           CollDate >= assessmentPeriod[1] )
VCPMI <- read_excel('data/Family Metrics - CPMI Combined.xlsx')%>%
  filter(RepNum == 1 & Target_Count == 110 &
           CollDate >= assessmentPeriod[1] )

# unused data
#stationTable <- read_csv('data/BRRO_Sites_AU_WQS.csv')
#stationTable1 <- readRDS('data/BRROsites_ROA_sf.RDS')
#commentList <- readRDS('Comments/commentList.RDS')
#monStationTemplate <- read_excel('data/tbl_ir_mon_stations_template.xlsx') # from X:\2018_Assessment\StationsDatabase\VRO



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
    dplyr::rename(`Point Unique Identifier` ="Point.Unique.Identifier", `Buffer Distance` = "Buffer.Distance")
  })
  
  
##### NEED TO FIX #######################################################################  
  #output$stationTableMissingStations <- DT::renderDataTable({
  #  req(stationTable())
  #  # decide which region data was input from
  #  Region <- unique(stationTable()$Deq_Region)
  #  z <- filter(conventionals, Deq_Region == 'Blue Ridge') %>%
  #    distinct(FDT_STA_ID, .keep_all = TRUE) %>%
  #    filter(FDT_STA_ID %in% stationTable()$FDT_STA_ID) %>%
  #    select(FDT_STA_ID:FDT_SPG_CODE, STA_LV2_CODE:STA_CBP_NAME)
  #  DT::datatable(z,rownames = FALSE, options= list(scrollX = TRUE, pageLength = 20, scrollY = "200px", dom='Bt'))
  #})
############################################################################################  
  
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
  
  # Table of AUs and Stations within Selected VAHU6
  output$AUSummary <-  DT::renderDataTable({ req(regionalAUs,AUs())
    DT::datatable(AUs() %>% st_set_geometry(NULL), rownames = FALSE, 
                  options= list(scrollX = TRUE, pageLength = nrow(AUs()), scrollY = "300px", dom='Bt')) 
  })
  
  output$stationSummary <- DT::renderDataTable({
    req(region_filter(), basin_filter(), huc6_filter())
    z <- filter(conventionals, Huc6_Vahu6 %in% huc6_filter()$VAHU6) %>%
      distinct(FDT_STA_ID, .keep_all = TRUE) %>%
      select(FDT_STA_ID:FDT_SPG_CODE, STA_LV2_CODE:STA_CBP_NAME)
    DT::datatable(z, rownames = FALSE, options= list(scrollX = TRUE, pageLength = nrow(z), scrollY = "300px", dom='Bt'))  })
  
  observeEvent(input$reviewAUs,{
    showModal(modalDialog(
      title="Preview Assessment Units for Selected VAHU6",
      leafletOutput('AUmap'),
      easyClose = TRUE))  })
  
  output$AUmap <- renderLeaflet({
    req(region_filter(), basin_filter(), huc6_filter())
    m <- mapview(huc6_filter(), color = 'yellow',lwd= 5, label= NULL, layer.name = c('Selected HUC6'),
                 popup= popupTable(huc6_filter(), zcol=c('VAHU6',"VaName","VAHU5","ASSESS_REG"))) + 
      mapview(AUs(), label= AUs()$ID305B, layer.name = c('AUs in Selected HUC6'), zcol = "ID305B", legend=FALSE,
              popup= popupTable(AUs(), zcol=c("ID305B","MILES","CYCLE","WATER_NAME","LOCATION" )))
    m@map })
  
  ## Station Review Tab
  # Show selected AU
  output$selectedHUC <- DT::renderDataTable({
    datatable(huc6_filter() %>% st_set_geometry(NULL) %>% select(VAHU6, VaName, Basin),
              rownames = FALSE, options= list(pageLength = 20, scrollY = "35px", dom='Bt'))})
  
  # Pull Conventionals data for selected AU on click
  conventionals_HUC <- eventReactive( input$pullHUCdata, {
    z <- filter(conventionals, Huc6_Vahu6 %in% huc6_filter()$VAHU6) %>%
      left_join(dplyr::select(stationTable(), FDT_STA_ID, SEC, CLASS, SPSTDS, ID305B_1, ID305B_2, ID305B_3,
                              STATION_TYPE_1, STATION_TYPE_2, STATION_TYPE_3, Basin
                              ), by='FDT_STA_ID')})
  
  output$AUSelection_ <- renderUI({ req(conventionals_HUC())
    selectInput('AUSelection', 'Assessment Unit Selection', choices = conventionals_HUC()$ID305B_1) })
  
  output$selectedAU <- DT::renderDataTable({req(conventionals_HUC(),input$AUSelection)
    z <- filter(regionalAUs, ID305B %in% input$AUSelection) %>% st_set_geometry(NULL) %>% as.data.frame()
    datatable(z, rownames = FALSE, 
              options= list(pageLength = nrow(z),scrollX = TRUE, scrollY = "200px", dom='t'))})

  output$stationSelection_ <- renderUI({ req(conventionals_HUC(), input$AUSelection)
    z <- filter(conventionals_HUC(), ID305B_1 %in% input$AUSelection | 
                   ID305B_2 %in% input$AUSelection | 
                   ID305B_2 %in% input$AUSelection) %>%
      distinct(FDT_STA_ID)
    selectInput('stationSelection', 'Station Selection', choices = unique(z$FDT_STA_ID))  })
  
  AUData <- eventReactive( input$AUSelection, {
    filter(conventionals_HUC(), ID305B_1 %in% input$AUSelection | 
             ID305B_2 %in% input$AUSelection | 
             ID305B_2 %in% input$AUSelection) %>% 
      left_join(WQSvalues, by = 'CLASS') }) 
  
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
  #observe(siteData$StationTableResults <- cbind(tempExceedances(stationData()), 
  #                                              DOExceedances_Min(stationData()), pHExceedances(stationData()),
  #                                              bacteriaExceedances_OLD(bacteria_Assessment_OLD(stationData(), 'E.COLI', 126, 235),'E.COLI') %>% 
  #                                                dplyr::rename('ECOLI_VIO' = 'E.COLI_VIO', 'ECOLI_SAMP'='E.COLI_SAMP', 'ECOLI_STAT'='E.COLI_STAT'),
  #                                              bacteriaExceedances_OLD(bacteria_Assessment_OLD(stationData(), 'ENTEROCOCCI', 35, 104),'ENTEROCOCCI') %>% 
  #                                                dplyr::rename('ENTER_VIO' = 'ENTEROCOCCI_VIO', 'ENTER_SAMP'='ENTEROCOCCI_SAMP', 'ENTER_STAT'='ENTEROCOCCI_STAT'),
  #                                              
  #                                              metalsExceedances(filter(WCmetals, FDT_STA_ID %in% stationData()$FDT_STA_ID) %>% 
  #                                                                  dplyr::select(`ANTIMONY HUMAN HEALTH PWS`:`ZINC ALL OTHER SURFACE WATERS`), 'WAT_MET'),
  #                                              acuteNH3exceedance(stationData()) %>% 
  #                                                dplyr::select(AcuteAmmonia_VIO, AcuteAmmonia_STAT) %>% 
  #                                                dplyr::rename('WAT_TOX_VIO' ='AcuteAmmonia_VIO','WAT_TOX_STAT' = 'AcuteAmmonia_STAT'),#data.frame(WAT_TOX_VIO='Not Analyzed by App', WAT_TOX_STAT='Not Analyzed by App'),# Placeholder for water toxics
  #                                              
  #                                              # Placeholder for water toxics
  #                                              metalsExceedances(filter(Smetals, FDT_STA_ID %in% stationData()$FDT_STA_ID) %>% 
  #                                                                  dplyr::select(`ACENAPHTHENE`:ZINC), 'SED_MET'),
  #                                              
  #                                              data.frame(SED_TOX_VIO='Not Analyzed by App', SED_TOX_STAT='Not Analyzed by App'),# Placeholder for sediment toxics
  #                                              data.frame(FISH_MET_VIO='Not Analyzed by App', FISH_MET_STAT='Not Analyzed by App'), # Placeholder for fish metals
  #                                              data.frame(FISH_TOX_VIO='Not Analyzed by App', FISH_TOX_STAT='Not Analyzed by App'),# Placeholder for fish toxics
  #                                              benthicAssessment(stationData(),conventionals_sf,VSCI,VCPMI),
  #                                              countTP(stationData()),
  #                                              countchla(stationData()),
  #                                              #data.frame(NUT_TP_VIO='Not Analyzed by App',NUT_TP_SAMP= 'Not Analyzed by App', NUT_TP_STAT='Not Analyzed by App'), # Placeholder bc only applies to Lakes or Cbay
  #                                              #data.frame(NUT_CHLA_VIO='Not Analyzed by App', NUT_CHLA_SAMP='Not Analyzed by App', NUT_CHLA_STAT='Not Analyzed by App'),# Placeholder bc only applies to Lakes or Cbay
  #                                              data.frame(COMMENTS= 'Not Analyzed by App') # Assessor Comments
  #)%>%
  #  dplyr::select(-ends_with('exceedanceRate')))
# 
  observe(siteData$StationTableResults1 <- cbind(tempExceedances(stationData()), 
                                                 DOExceedances_Min(stationData()), pHExceedances(stationData()),
                                                 bacteriaExceedances_OLD(bacteria_Assessment_OLD(stationData(), 'E.COLI', 126, 235),'E.COLI') %>% 
                                                   dplyr::rename('ECOLI_VIO' = 'E.COLI_VIO', 'ECOLI_SAMP'='E.COLI_SAMP', 'ECOLI_STAT'='E.COLI_STAT'),
                                                 bacteriaExceedances_OLD(bacteria_Assessment_OLD(stationData(), 'ENTEROCOCCI', 35, 104),'ENTEROCOCCI') %>% 
                                                   dplyr::rename('ENTER_VIO' = 'ENTEROCOCCI_VIO', 'ENTER_SAMP'='ENTEROCOCCI_SAMP', 'ENTER_STAT'='ENTEROCOCCI_STAT'),
                                                 
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
    more <- cbind(metalsExceedances(filter(Smetals, FDT_STA_ID %in% stationData()$FDT_STA_ID) %>% 
                                      dplyr::select(ARSENIC:ZINC), 'SED_MET') %>%
                    dplyr::select(-ends_with('exceedanceRate')),
                  data.frame(SED_TOX_VIO='Not Analyzed by App', SED_TOX_STAT='Not Analyzed by App'),# Placeholder for sediment toxics
                  data.frame(FISH_MET_VIO='Not Analyzed by App', FISH_MET_STAT='Not Analyzed by App'), # Placeholder for fish metals
                  data.frame(FISH_TOX_VIO='Not Analyzed by App', FISH_TOX_STAT='Not Analyzed by App'),# Placeholder for fish toxics
                  benthicAssessment(stationData(),conventionals_sf,VSCI,VCPMI),
                  countTP(stationData()),
                  countchla(stationData()),
                  data.frame(COMMENTS= 'Not Analyzed by App')) %>% dplyr::select(-ends_with('exceedanceRate'))
    z2 <- cbind(z, siteData$StationTableResults1, AMM, more)
    
    datatable(z2, extensions = 'Buttons', escape=F, rownames = F, editable = TRUE,
              options= list(scrollX = TRUE, pageLength = nrow(z2),
                            # hide certain columns
                            #columnDefs = list(list(targets = 6, visible = FALSE)),
                            dom='Bt', buttons=list('copy',
                                                    list(extend='csv',filename=paste('AssessmentResults_',paste(assessmentCycle,input$stationSelection, collapse = "_"),Sys.Date(),sep='')),
                                                    list(extend='excel',filename=paste('AssessmentResults_',paste(assessmentCycle,input$stationSelection, collapse = "_"),Sys.Date(),sep=''))))) %>% 
      # format cell background color based on hidden column
      formatStyle(c('TEMP_SAMP','TEMP_VIO','TEMP_STAT'), 'TEMP_STAT', backgroundColor = styleEqual(c('Review'), c('red'))) %>%
      formatStyle(c('DO_SAMP','DO_VIO','DO_STAT'), 'DO_STAT', backgroundColor = styleEqual(c('Review'), c('red'))) %>%
      formatStyle(c('PH_SAMP','PH_VIO','PH_STAT'), 'PH_STAT', backgroundColor = styleEqual(c('Review'), c('red'))) %>%
      formatStyle(c('ECOLI_SAMP','ECOLI_VIO','ECOLI_STAT'), 'ECOLI_STAT', backgroundColor = styleEqual(c('Review'), c('red'))) %>%
      formatStyle(c('ENTER_SAMP','ENTER_VIO','ENTER_STAT'), 'ENTER_STAT', backgroundColor = styleEqual(c('Review'), c('red'))) %>%
      formatStyle(c('WAT_MET_VIO','WAT_MET_STAT'), 'WAT_MET_STAT', backgroundColor = styleEqual(c('Review'), c('red'))) %>%
      formatStyle(c('WAT_TOX_VIO','WAT_TOX_STAT'), 'WAT_TOX_STAT', backgroundColor = styleEqual(c('Review'), c('red'))) %>%
      formatStyle(c('SED_MET_VIO','SED_MET_STAT'), 'SED_MET_STAT', backgroundColor = styleEqual(c('Review'), c('red'))) %>%
      formatStyle(c('BENTHIC_STAT'), 'BENTHIC_STAT', backgroundColor = styleEqual(c('Review'), c('red'))) 
    
    # for yellow vs red, maybe %>% just a format style of yellow if _VIO column >1 and then run the red review code?
      

  })
  
  #output$stationTableDataSummary2 <- DT::renderDataTable({
  #  req(stationData())
  #  z <- cbind(data.frame(StationID = unique(stationData()$FDT_STA_ID)), acuteNH3exceedance(stationData())) %>%
  #    select(-ends_with('exceedanceRate'))
  #  datatable(z, extensions = 'Buttons', escape=F, rownames = F, editable = TRUE,
  #            options= list(scrollX = TRUE, pageLength = nrow(z),
  #                          # hide certain columns
  #                          columnDefs = list(list(targets = 6, visible = FALSE)),
  #                          dom='Bt', buttons=list('copy',
  #                                                 list(extend='csv',filename=paste('AssessmentResults_',paste(assessmentCycle,input$stationSelection, collapse = "_"),Sys.Date(),sep='')),
  #                                                 list(extend='excel',filename=paste('AssessmentResults_',paste(assessmentCycle,input$stationSelection, collapse = "_"),Sys.Date(),sep=''))))) %>% 
  #    # format cell background color based on hidden column
  #    formatStyle(c('AcuteAmmonia_SAMP','AcuteAmmonia_VIO','AcuteAmmonia_STAT'), 'AcuteAmmonia_STAT', backgroundColor = styleEqual(c('Review'), c('red')))
  #  
  #})
  
  
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

