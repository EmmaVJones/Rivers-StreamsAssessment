TNPlotlySingleStationUI <- function(id){
  ns <- NS(id)
  tagList(
    wellPanel(
      h4(strong('Single Station Data Visualization')),
      uiOutput(ns('TN_oneStationSelectionUI')),
      plotlyOutput(ns('TNplotly'))  )
  )
}


TNPlotlySingleStation <- function(input,output,session, AUdata, stationSelectedAbove){
  ns <- session$ns
  
  # Select One station for individual review
  output$TN_oneStationSelectionUI <- renderUI({
    req(#AUdata, 
      stationSelectedAbove)
    selectInput(ns('TN_oneStationSelection'),strong('Select Station to Review'),choices= sort(unique(c(stationSelectedAbove(),AUdata()$FDT_STA_ID))),
                width='300px', selected = stationSelectedAbove())})# "2-JMS279.41" )})
  
  TN_oneStation <- reactive({
    req(ns(input$TN_oneStationSelection))
    filter(AUdata(),FDT_STA_ID %in% input$TN_oneStationSelection)})
  
  output$TNplotly <- renderPlotly({
    req(input$TN_oneStationSelection, TN_oneStation())
    dat <- TN_oneStation()
    dat$SampleDate <- as.POSIXct(dat$FDT_DATE_TIME2, format="%m/%d/%y")
    plot_ly(data=dat)%>%
      add_markers(x= ~SampleDate, y= ~NITROGEN,mode = 'scatter', name="Total Nitrogen (mg/L)", marker = list(color= '#535559'),
                  hoverinfo="text",text=~paste(sep="<br>",
                                               paste("Date: ",SampleDate),
                                               paste("Depth: ",FDT_DEPTH, "m"),
                                               paste("Total Nitrogen: ",NITROGEN,"mg/L")))%>%
      layout(showlegend=FALSE,
             yaxis=list(title="Total Nitrogen (mg/L)"),
             xaxis=list(title="Sample Date",tickfont = list(size = 10))) })
}
