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