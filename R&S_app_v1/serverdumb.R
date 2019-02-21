# Run in R 3.5.1
source('global.R')


fakeConventionals <- read.csv('exampleData/fakeConventionals.csv') %>%
  dplyr::select(FDT_STA_ID, STA_DESC, Basin, Huc6_Huc_8_Name,
          Huc6_Name, Huc6_Vahu5, Huc6_Vahu6) %>%
  distinct(FDT_STA_ID, .keep_all = T)

x <- data.frame(x = c('StationID','Station Description',
                      'Level 1 Code', 'Level 3 Code', 'SPG Code', 
                      'ID305B_1', 'ID305B_3',
                      'ID305B_3','Station Type 1', 
                      'Station Type 2', 'Station Type 3', 'WQS Section',
                      'WQS Class', 'Special Standards',
                      'WQS Description'),
                y = c('XXXXXX', 'Route XXX bridge','STREAM','AWTSHD', 'AW', 
                      'VAW-J01R_JMS01A00','VAW-J01R_JMS01A01',
                      'VAW-J01R_JMS01A02','AW',  'B',  'PA', '2','IV','PWS',
                      'James River from blah to blah'))
names(x) <- c(' ',"  ")



assessmentLayer <- st_read('GIS/AssessmentRegions_VA84_basins.shp')




shinyServer(function(input, output, session) {
  
  # display the loading feature until data loads into app
  load_data()
  
  ## Data Upload Tab
  stationTable <- reactive({readRDS('data/BRROsites_ROA.RDS')})
  # Where I will go after testing
  #stationTable <- reactive({
  #  req(input$stationsTable)
  #  inFile <- input$stationsTable
  #  readRDS(inFile$datapath)
  #})
  
  comments <- reactive({
    req(input$commentFile)
    inFile <- input$commentFile
    read_csv(inFile$datapath)
  })
  
  
  ## Watershed Selection Tab
  
  # Query VAHUC6's By Selectize arguments
  the_data <- reactive({assessmentLayer %>%  st_set_geometry(NULL) })
  region_filter <- shiny::callModule(dynamicSelect, "DEQregionSelection", the_data, "ASSESS_REG" )
  basin_filter <- shiny::callModule(dynamicSelect, "basinSelection", region_filter, "Basin" )
  
  output$table <- renderTable({
    table <- region_filter()
    table
  })
  
  output$VAmap <- renderLeaflet({
    
    leaflet() %>% setView(-79.2,37.7,zoom=7)%>%
      addProviderTiles(providers$OpenStreetMap,group='Open Street Map')%>%
      addProviderTiles(providers$Esri.WorldImagery,group='Esri World Imagery')%>%
      addProviderTiles(providers$Stamen.TerrainBackground,group='Stamen Terrain Background')%>%
      addLayersControl(baseGroups=c('Open Street Map','Esri World Imagery','Stamen Terrain Background'),
                       #overlayGroups=c('Lake Monitoring Stations','All Lake Monitoring Stations'),
                       options=layersControlOptions(collapsed=T),
                       position='topleft')%>%
      #addHomeButton(extent(lakeStations_shp),"All Lake Monitoring Stations")%>%
      mapview::addMouseCoordinates(style='basic')
  })
  
  
  output$allStationsInVAHUC6 <- DT::renderDataTable({
    DT::datatable(fakeConventionals, escape=F, rownames = F,
                  options=list(dom='Bt',scrollX = TRUE,pageLength = nrow(x), scrollY = "200px"))})
  
  ## Individual Station Review Tab
  
  output$stationInfo <- DT::renderDataTable({
    
    DT::datatable(x, escape=F, rownames = F,
                  options=list(dom='Bt',scrollX = TRUE,pageLength = nrow(x), scrollY = "200px"))
  })
  
  
  output$stationMap <- renderLeaflet({
    
    leaflet() %>% setView(-79.2,37.7,zoom=12)%>%
      addProviderTiles(providers$OpenStreetMap,group='Open Street Map')%>%
      addProviderTiles(providers$Esri.WorldImagery,group='Esri World Imagery')%>%
      addProviderTiles(providers$Stamen.TerrainBackground,group='Stamen Terrain Background')%>%
      addLayersControl(baseGroups=c('Open Street Map','Esri World Imagery','Stamen Terrain Background'),
                       #overlayGroups=c('Lake Monitoring Stations','All Lake Monitoring Stations'),
                       options=layersControlOptions(collapsed=T),
                       position='topleft')%>%
      #addHomeButton(extent(lakeStations_shp),"All Lake Monitoring Stations")%>%
      mapview::addMouseCoordinates(style='basic')
  })
})
