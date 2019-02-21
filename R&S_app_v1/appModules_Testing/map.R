# Map 

# used sp and s4 objects bc could not figure out how to get sf to work nicely in modules


HUCmapUI <- function(id){
  ns <- NS(id)
  tagList(
    tableOutput(ns('test')),
    leafletOutput(ns("map"), height =400, width = 650))
}

HUCmap <- function(input, output, session, userDataset_sp, allHUCS_sp){
  ns <- session$ns
  

  
  output$map <- renderLeaflet({
    
    leaflet(userDataset_sp) %>% setView(-79.2,37.7,zoom=7)%>%
      addProviderTiles(providers$OpenStreetMap,group='Open Street Map')%>%
      addProviderTiles(providers$Esri.WorldImagery,group='Esri World Imagery')%>%
      addProviderTiles(providers$Stamen.TerrainBackground,group='Stamen Terrain Background')%>%
      
      #addPolygons(data=userDataset_sp,color='black',fill=0.1,stroke=0.1,group="Virginia HUC61",
      #            popup = popupTable(userDataset_sp,zcol=c(1,2,5,13,22))) %>% 
      
      addPolygons(data=allHUCS_sp,color='black',fill=0.1,stroke=0.1,group="Virginia HUC6",
                  popup = popupTable(allHUCS_sp,zcol=c(1,2,5,13,22))) %>% hideGroup('Virginia HUC6') %>%
      addLayersControl(baseGroups=c('Open Street Map','Esri World Imagery','Stamen Terrain Background'),
                       overlayGroups=c('Virginia HUC61','Virginia HUC6'),
                       options=layersControlOptions(collapsed=T),
                       position='topleft')%>%
      #addHomeButton(extent(lakeStations_shp),"All Lake Monitoring Stations")%>%
      mapview::addMouseCoordinates(style='basic')
  })
  
  observe({
    leafletProxy('map')  %>% clearControls() %>%
      addPolygons(data=userDataset_sp,color='black',fill=0.1,stroke=0.1,group="Virginia HUC61",
                  popup = popupTable(userDataset_sp,zcol=c(1,2,5,13,22))) %>% 
      addLayersControl(baseGroups=c('Open Street Map','Esri World Imagery','Stamen Terrain Background'),
                           overlayGroups=c("Selected HUC6",'Virginia HUC6'),
                           options=layersControlOptions(collapsed=T),position='topleft')})
    
  
  
  
}

#huc6_filter <- assessmentLayer[1,]
#huc6_filter <- 'JM01'

#ui <- fluidPage(
#  HUCmapUI('VAmap')
#)
#server <- function(input,output,session){
#  HUC6 <- reactive({assessmentLayer_sp[assessmentLayer_sp$VAHU6 ==  huc6_filter,]})
  
  #output$test <- renderTable({
   # HUC6()
  #})
  
#  callModule(HUCmap, 'VAmap', HUC6(), assessmentLayer_sp)
#}
#shinyApp(ui, server)