# Map 
HUCmapUI <- function(id){
  ns <- NS(id)
  tagList(
    tableOutput(ns('test')),
    leafletOutput(ns("map"), height =400, width = 650))
}

HUCmap <- function(input, output, session, userDataset, userDataset_sf){
  ns <- session$ns
  
  #HUC6 <- reactive({userDataset_sp[userDataset_sp$VAHU6== userDataset()$VAHU6,]})
  
  HUC6 <- reactive({select(userDataset_sf(), VAHU6)})
    #filter(userDataset_sf, VAHUC6 %in% userDataset()$VAHU6)})
  output$test <- renderTable({
    HUC6()
  })
  
  
  
}

assessmentLayer <- st_read('GIS/AssessmentRegions_VA84_basins.shp') %>%
  st_transform( st_crs(4326))
huc6_filter <- assessmentLayer[1,]%>%  st_set_geometry(NULL) 

ui <- fluidPage(
  HUCmapUI('VAmap')
)
server <- function(input,output,session){
  callModule(HUCmap, 'VAmap', huc6_filter, assessmentLayer)
}
shinyApp(ui, server)