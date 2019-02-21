source('global.R')
source('AUshapefileLocation.R')

assessmentLayer <- st_read('GIS/AssessmentRegions_VA84_basins.shp') %>%
  st_transform( st_crs(4326)) 
conventionals <- suppressWarnings(read_csv('data/CONVENTIONALS_20171010.csv'))
conventionals$FDT_DATE_TIME2 <- as.POSIXct(conventionals$FDT_DATE_TIME, format="%m/%d/%Y %H:%M")
##commentList <- readRDS('Comments/commentList.RDS')
monStationTemplate <- read_excel('data/tbl_ir_mon_stations_template.xlsx') # from X:\2018_Assessment\StationsDatabase\VRO
WCmetals <- read_excel('data/WATER_METALS_20170712.xlsx')
Smetals <- read_excel('data/SEDIMENT_20170712.xlsx')
## Bring in latest EDAS VSCI and (combined) VCPMI queries
VSCI <- read_excel('data/Family Metrics VSCI Calculation.xlsx')%>%
  filter(RepNum == 1 & Target_Count == 110 &
           CollDate >= assessmentPeriod[1] )
VCPMI <- read_excel('data/Family Metrics - CPMI Combined.xlsx')%>%
  filter(RepNum == 1 & Target_Count == 110 &
           CollDate >= assessmentPeriod[1] )



ui <- shinyUI(fluidPage(theme="yeti.css",
                        navbarPage(paste("VDEQ ",assessmentCycle," Rivers and Streams Assessment Tool", sep=''),
                        tabPanel('Watershed Selection',
                                 sidebarPanel(
                                   dynamicSelectInput("DEQregionSelection", "Select DEQ Assessment Region", multiple = FALSE),
                                   dynamicSelectInput("basinSelection", "Select Major Basin", multiple = FALSE),
                                   dynamicSelectInput("HUC6Selection", "Select VAHU6", multiple = FALSE),
                                   br(),
                                   actionButton('reviewAUs',"Preview Assesment Units",class='btn-block')),
                                 mainPanel(
                                   leafletOutput('VAmap'),
                                   br(),
                                   h5('Assessment Units in Selected VAHU6'),
                                   DT::dataTableOutput('AUSummary'),
                                   h5('Stations in Selected VAHU6'),
                                   DT::dataTableOutput('stationSummary')
                                 )
                        ),
                        tabPanel('Assessment Unit Review',
                                 fluidRow(column(9, DT::dataTableOutput('selectedHUC')),
                                          column(3,br(),actionButton('pullHUCdata','Select Watershed for analysis'))),
                                 hr(),
                                 uiOutput('AUSelection_'),
                                 DT::dataTableOutput('selectedAU'),br(),
                                 uiOutput('stationSelection_'),
                                 fluidRow(column(4, DT::dataTableOutput('stationInfo')),
                                          column(4, leafletOutput('stationMap', height = 300, width = 300)),
                                          column(4, DT::dataTableOutput('stationHistoricalInfo'))),
                                 hr(),
                                 h3('Station Results for Review'),
                                 helpText('This table outputs the site specific results for direct export to the Station Table. It also serves to highlight
                                          where exceedances are present and should be reviewed in the individual parameter visualization tabs below.'),
                                 h4('Official Station Results Table'),
                                 helpText('Note that WAT_TOX_VIO AND WAT_TOX_STAT are only reflecting ammonia analysis.'),
                                 verbatimTextOutput('test'),
                                 DT::dataTableOutput('stationTableDataSummary')))))


server <- shinyServer(function(input, output, session) {
  # Reactive Value to store all site data
  siteData <- reactiveValues()
  
  ## Data Upload Tab
  stationTable <- reactive({read_csv('data/RegionalResults_AU_WQS.csv')})#read_csv('data/BRRO_Sites_AU_WQS.csv')})#readRDS('data/BRROsites_ROA_sf.RDS')})
  
  # Query VAHUC6's By Selectize arguments
  the_data <- reactive({assessmentLayer})
  region_filter <- shiny::callModule(dynamicSelect, "DEQregionSelection", the_data, "ASSESS_REG" )
  basin_filter <- shiny::callModule(dynamicSelect, "basinSelection", region_filter, "Basin" )
  huc6_filter <- shiny::callModule(dynamicSelect, "HUC6Selection", basin_filter, "VAHU6" )
  AUs <- reactive({req(huc6_filter(), regionalAUs)
    suppressWarnings(st_intersection(st_zm(regionalAUs), huc6_filter()))})
  
  # Pull Conventionals data for selected AU on click
  conventionals_HUC <- eventReactive( input$pullHUCdata, {
    z <- filter(conventionals, Huc6_Vahu6 %in% huc6_filter()$VAHU6) %>%
      left_join(dplyr::select(stationTable(), FDT_STA_ID, SEC, CLASS, SPSTDS, ID305B_1, ID305B_2, ID305B_3,
                              STATION_TYPE_1, STATION_TYPE_2, STATION_TYPE_3
      ), by='FDT_STA_ID')})
  
  output$AUSelection_ <- renderUI({ req(conventionals_HUC())
    selectInput('AUSelection', 'Assessment Unit Selection', choices = conventionals_HUC()$ID305B_1)  })
  
  output$selectedAU <- DT::renderDataTable({req(conventionals_HUC(),input$AUSelection)
    z <- filter(regionalAUs, ID305B %in% input$AUSelection) %>% st_set_geometry(NULL) %>% as.data.frame()
    datatable(z, rownames = FALSE, 
              options= list(pageLength = nrow(z),scrollX = TRUE, scrollY = "200px", dom='Bt'))})
  
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
  
  observe(siteData$StationTablePrelimStuff <- StationTableStartingData(stationData()))
  observe(siteData$StationTableResults1 <- cbind(tempExceedances(stationData()), 
                                                DOExceedances_Min(stationData()), pHExceedances(stationData()),
                                                bacteriaExceedances_OLD(bacteria_Assessment_OLD(stationData(), 'E.COLI', 126, 235),'E.COLI') %>% 
                                                  dplyr::rename('ECOLI_VIO' = 'E.COLI_VIO', 'ECOLI_SAMP'='E.COLI_SAMP', 'ECOLI_STAT'='E.COLI_STAT'),
                                                bacteriaExceedances_OLD(bacteria_Assessment_OLD(stationData(), 'ENTEROCOCCI', 35, 104),'ENTEROCOCCI') %>% 
                                                  dplyr::rename('ENTER_VIO' = 'ENTEROCOCCI_VIO', 'ENTER_SAMP'='ENTEROCOCCI_SAMP', 'ENTER_STAT'='ENTEROCOCCI_STAT'),
                                                metalsExceedances(filter(WCmetals, FDT_STA_ID %in% stationData()$FDT_STA_ID) %>% 
                                                                    dplyr::select(`ANTIMONY HUMAN HEALTH PWS`:`ZINC ALL OTHER SURFACE WATERS`), 'WAT_MET'))%>%
    dplyr::select(-ends_with('exceedanceRate')))
  
  
  #observe(siteData$StationTableResults2 <- cbind(data.frame(SED_TOX_VIO='Not Analyzed by App', SED_TOX_STAT='Not Analyzed by App'),# Placeholder for sediment toxics
  #                                              data.frame(FISH_MET_VIO='Not Analyzed by App', FISH_MET_STAT='Not Analyzed by App'), # Placeholder for fish metals
  #                                              data.frame(FISH_TOX_VIO='Not Analyzed by App', FISH_TOX_STAT='Not Analyzed by App'),# Placeholder for fish toxics
  #                                              benthicAssessment(stationData(),conventionals_sf,VSCI,VCPMI),
  #                                              countTP(stationData()),
  #                                              countchla(stationData()),
  #                                              #data.frame(NUT_TP_VIO='Not Analyzed by App',NUT_TP_SAMP= 'Not Analyzed by App', NUT_TP_STAT='Not Analyzed by App'), # Placeholder bc only applies to Lakes or Cbay
  #                                              #data.frame(NUT_CHLA_VIO='Not Analyzed by App', NUT_CHLA_SAMP='Not Analyzed by App', NUT_CHLA_STAT='Not Analyzed by App'),# Placeholder bc only applies to Lakes or Cbay
  #                                              data.frame(COMMENTS= 'Not Analyzed by App')) %>% # Assessor Comments
  #          dplyr::select(-ends_with('exceedanceRate')))
  
            #data.frame(ID305B_1= concatinateUnique(stationData()$ID305B_1),
                                          #      ID305B_2= concatinateUnique(stationData()$ID305B_2), ID305B_3= concatinateUnique(stationData()$ID305B_3),
                                          #      DEPTH = concatinateUnique(stationData()$FDT_DEPTH_DESC),
                                          #      STATION_ID = concatinateUnique(stationData()$FDT_STA_ID),
                                          #      REGION = changeDEQRegionName(concatinateUnique(stationData()$Deq_Region)),
                                          #      STATION_TYPE_1= concatinateUnique(stationData()$STATION_TYPE_1), STATION_TYPE_2=concatinateUnique(stationData()$STATION_TYPE_2), 
                                          #      STATION_TYPE_3= concatinateUnique(stationData()$STATION_TYPE_3), STATION_LAT = concatinateUnique(stationData()$Latitude), 
                                          #      STATION_LON = concatinateUnique(stationData()$Longitude) ,
                                          #      WATERSHED_ID= concatinateUnique(stationData()$ID305B_1),# substr(strsplit(as.character(concatinateUnique(stationData()$ID305B_1)), '-')[[1]][2], 1, 3),
                                          #      VAHU6 = concatinateUnique(stationData()$Huc6_Vahu6)))
  
  output$test <- renderPrint({
    metalsExceedances(filter(Smetals, FDT_STA_ID %in% stationData()$FDT_STA_ID) %>% 
                        dplyr::select(`ACENAPHTHENE`:ZINC), 'SED_MET') %>%
      dplyr::select(-ends_with('exceedanceRate'))
  #  metalsExceedances(filter(Smetals, FDT_STA_ID %in% stationData()$FDT_STA_ID) %>% 
  #                                                                  dplyr::select(`ACENAPHTHENE`:ZINC), 'SED_MET') %>%
  })
    
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
                                        dplyr::select(`ACENAPHTHENE`:ZINC), 'SED_MET') %>%
                      dplyr::select(-ends_with('exceedanceRate')),
                    data.frame(SED_TOX_VIO='Not Analyzed by App', SED_TOX_STAT='Not Analyzed by App'),# Placeholder for sediment toxics
                    data.frame(FISH_MET_VIO='Not Analyzed by App', FISH_MET_STAT='Not Analyzed by App'), # Placeholder for fish metals
                    data.frame(FISH_TOX_VIO='Not Analyzed by App', FISH_TOX_STAT='Not Analyzed by App'),# Placeholder for fish toxics
                    benthicAssessment(stationData(),conventionals_sf,VSCI,VCPMI),
                    countTP(stationData()),
                    countchla(stationData()),
                    #data.frame(NUT_TP_VIO='Not Analyzed by App',NUT_TP_SAMP= 'Not Analyzed by App', NUT_TP_STAT='Not Analyzed by App'), # Placeholder bc only applies to Lakes or Cbay
                    #data.frame(NUT_CHLA_VIO='Not Analyzed by App', NUT_CHLA_SAMP='Not Analyzed by App', NUT_CHLA_STAT='Not Analyzed by App'),# Placeholder bc only applies to Lakes or Cbay
                    data.frame(COMMENTS= 'Not Analyzed by App')) %>% dplyr::select(-ends_with('exceedanceRate'))
    z2 <- cbind(z, siteData$StationTableResults1, AMM, more)#, siteData$StationTableResults2)
    
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
      formatStyle(c('PH_SAMP','PH_VIO','PH_STAT'), 'PH_STAT', backgroundColor = styleEqual(c('Review'), c('red'))) #%>%
      #formatStyle(c('ECOLI_SAMP','ECOLI_VIO','ECOLI_STAT'), 'ECOLI_STAT', backgroundColor = styleEqual(c('Review'), c('red'))) %>%
      #formatStyle(c('ENTER_SAMP','ENTER_VIO','ENTER_STAT'), 'ENTER_STAT', backgroundColor = styleEqual(c('Review'), c('red'))) %>%
      #formatStyle(c('WAT_MET_VIO','WAT_MET_STAT'), 'WAT_MET_STAT', backgroundColor = styleEqual(c('Review'), c('red'))) %>%
      #      #formatStyle(c('WAT_TOX_VIO','WAT_TOX_STAT'), 'WAT_TOX_STAT', backgroundColor = styleEqual(c('Review'), c('red'))) %>%
      #formatStyle(c('SED_MET_VIO','SED_MET_STAT'), 'SED_MET_STAT', backgroundColor = styleEqual(c('Review'), c('red'))) %>%
      #formatStyle(c('BENTHIC_STAT'), 'BENTHIC_STAT', backgroundColor = styleEqual(c('Review'), c('red'))) 
  })
  
})

shinyApp(ui,server)
  