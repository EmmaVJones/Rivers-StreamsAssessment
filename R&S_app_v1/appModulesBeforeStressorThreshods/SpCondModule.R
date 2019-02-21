SpCondPlotlySingleStationUI <- function(id){
  ns <- NS(id)
  tagList(
    wellPanel(
      h4(strong('Single Station Data Visualization')),
      uiOutput(ns('SpCond_oneStationSelectionUI')),
      plotlyOutput(ns('SpCondplotly'))  )
  )
}


SpCondPlotlySingleStation <- function(input,output,session, AUdata, stationSelectedAbove){
  ns <- session$ns
  
  # Select One station for individual review
  output$SpCond_oneStationSelectionUI <- renderUI({
    req(AUdata)
    selectInput(ns('SpCond_oneStationSelection'),strong('Select Station to Review'),
                choices= sort(unique(c(stationSelectedAbove(),AUdata()$FDT_STA_ID))), # Change this based on stationSelectedAbove
                width='300px', selected = stationSelectedAbove())})
  
  SpCond_oneStation <- reactive({
    req(ns(input$SpCond_oneStationSelection))
    filter(AUdata(),FDT_STA_ID %in% input$SpCond_oneStationSelection)})
  
  output$SpCondplotly <- renderPlotly({
    req(input$SpCond_oneStationSelection, SpCond_oneStation())
    dat <- SpCond_oneStation()
    dat$SampleDate <- as.POSIXct(dat$FDT_DATE_TIME2, format="%m/%d/%y")
    plot_ly(data=dat)%>%
      add_markers(x= ~SampleDate, y= ~FDT_SPECIFIC_CONDUCTANCE,mode = 'scatter', name="Specific Conductivity (uS/cm)",
                  hoverinfo="text",text=~paste(sep="<br>",
                                               paste("Date: ",SampleDate),
                                               paste("Depth: ",FDT_DEPTH, "m"),
                                               paste("Specific Conductivity: ",FDT_SPECIFIC_CONDUCTANCE,"uS/cm")))%>%
      layout(showlegend=FALSE,
             yaxis=list(title="Specific Conductivity (uS/cm)"),
             xaxis=list(title="Sample Date",tickfont = list(size = 10)))
  })
  
}