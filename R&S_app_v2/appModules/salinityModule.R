salinityPlotlySingleStationUI <- function(id){
  ns <- NS(id)
  tagList(
    wellPanel(
      h4(strong('Single Station Data Visualization')),
      uiOutput(ns('salinity_oneStationSelectionUI')),
      plotlyOutput(ns('salinityplotly'))  )
  )
}


salinityPlotlySingleStation <- function(input,output,session, AUdata, stationSelectedAbove){
  ns <- session$ns
  
  # Select One station for individual review
  output$salinity_oneStationSelectionUI <- renderUI({
    req(stationSelectedAbove)
    selectInput(ns('salinity_oneStationSelection'),strong('Select Station to Review'),
                choices= sort(unique(c(stationSelectedAbove(),AUdata()$FDT_STA_ID))), # Change this based on stationSelectedAbove
                width='300px', selected = stationSelectedAbove())})
  
  salinity_oneStation <- reactive({
    req(ns(input$salinity_oneStationSelection))
    filter(AUdata(),FDT_STA_ID %in% input$salinity_oneStationSelection)})
  
  output$salinityplotly <- renderPlotly({
    req(input$salinity_oneStationSelection, salinity_oneStation())
    dat <- salinity_oneStation()
    dat$SampleDate <- as.POSIXct(dat$FDT_DATE_TIME2, format="%m/%d/%y")
    plot_ly(data=dat)%>%
      add_markers(x= ~SampleDate, y= ~FDT_SALINITY,mode = 'scatter', name="Salinity (ppt)", marker = list(color= '#535559'),
                  hoverinfo="text",text=~paste(sep="<br>",
                                               paste("Date: ",SampleDate),
                                               paste("Depth: ",FDT_DEPTH, "m"),
                                               paste("Salinity: ",format(FDT_SALINITY,digits=2),"ppt")))%>%
      layout(showlegend=FALSE,
             yaxis=list(title="Salinity (ppt)"),
             xaxis=list(title="Sample Date",tickfont = list(size = 10))) })
}