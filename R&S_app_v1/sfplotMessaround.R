assessmentLayer <- st_read('GIS/AssessmentRegions_VA84_basins.shp') %>%
  st_transform( st_crs(4326))

# plot everything
plot(st_geometry(assessmentLayer))

ggplot() + geom_sf(data = assessmentLayer)


james <- filter(assessmentLayer, Basin == 'James River Basin')
selectedHUC <- filter(james, VAHU6 == 'JM28')# %>%
 # st_centroid() 
ggplot() + geom_sf(data = james) + geom_sf(data = selectedHUC, aes(fill = 'yellow'))
  
m <- mapview(james) + mapview(selectedHUC, color = 'yellow',lwd= 5, layer.name = c('Selected HUC6'))
m@map %>% setView(st_bbox(selectedHUC)$xmax[[1]],st_bbox(selectedHUC)$ymax[[1]],zoom = 9)






HUCmapUI <- function(id){
  ns <- NS(id)
  tagList(
    tableOutput(ns('test')),
    leafletOutput(ns("map"), height =400, width = 650))
}

HUCmap <- function(input, output, session, selectedHUC, james){
  ns <- session$ns

  output$map <- renderLeaflet({
    m <- mapview(james,label= selectedHUC$VAHU6, layer.name = 'Basin Chosen') + #james$Basin) + 
      mapview(selectedHUC, color = 'yellow',lwd= 5, label= selectedHUC$VAHU6, layer.name = c('Selected HUC6'),
              popup= popupTable(selectedHUC, zcol=c('VAHU6',"VaName","VAHU5","ASSESS_REG")))
    m@map %>% setView(st_bbox(selectedHUC)$xmax[[1]],st_bbox(selectedHUC)$ymax[[1]],zoom = 9)
  })
}

ui <- fluidPage(
  HUCmapUI('VAmap')
)
server <- function(input,output,session){


  output$test <- renderTable({ selectedHUC })

  callModule(HUCmap, 'VAmap', selectedHUC, james)
}
shinyApp(ui, server)

    