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
    req(stationSelectedAbove)
    selectInput(ns('TN_oneStationSelection'),strong('Select Station to Review'),choices= sort(unique(c(stationSelectedAbove(),AUdata()$FDT_STA_ID))),#unique(AUdata())$FDT_STA_ID,
                width='300px', selected = stationSelectedAbove())})# "2-JMS279.41" )})
  
  
  TN_oneStation <- reactive({
    req(ns(input$TN_oneStationSelection))
    filter(AUdata(),FDT_STA_ID %in% input$TN_oneStationSelection)})
  
  output$TNplotly <- renderPlotly({
    req(input$TN_oneStationSelection, TN_oneStation())
    dat <- TN_oneStation()
    dat$SampleDate <- as.POSIXct(dat$FDT_DATE_TIME2, format="%m/%d/%y")
    
    maxheight <- ifelse(max(dat$NITROGEN, na.rm=T) < 2, 2.5, max(dat$NITROGEN, na.rm=T)* 1.2)
    box1 <- data.frame(SampleDate = c(min(dat$SampleDate), min(dat$SampleDate), max(dat$SampleDate),max(dat$SampleDate)), y = c(2, maxheight, maxheight, 2))
    box2 <- data.frame(x = c(min(dat$SampleDate), min(dat$SampleDate), max(dat$SampleDate),max(dat$SampleDate)), y = c(1, 2, 2, 1))
    box3 <- data.frame(x = c(min(dat$SampleDate), min(dat$SampleDate), max(dat$SampleDate),max(dat$SampleDate)), y = c(0.5, 1, 1, 0.5))
    box4 <- data.frame(x = c(min(dat$SampleDate), min(dat$SampleDate), max(dat$SampleDate),max(dat$SampleDate)), y = c(0, 0.5, 0.5, 0))
    
    
    plot_ly(data=dat)%>%
      add_polygons(x = ~SampleDate, y = ~y, data = box1, fillcolor = "firebrick",opacity=0.6, line = list(width = 0),
                   hoverinfo="text", name =paste('High Probability of Stress to Aquatic Life')) %>%
      add_polygons(data = box2, x = ~x, y = ~y, fillcolor = "#F0E442",opacity=0.6, line = list(width = 0),
                   hoverinfo="text", name =paste('Medium Probability of Stress to Aquatic Life')) %>%
      add_polygons(data = box3, x = ~x, y = ~y, fillcolor = "#009E73",opacity=0.6, line = list(width = 0),
                   hoverinfo="text", name =paste('Low Probability of Stress to Aquatic Life')) %>%
      add_polygons(data = box4, x = ~x, y = ~y, fillcolor = "#0072B2",opacity=0.6, line = list(width = 0),
                   hoverinfo="text", name =paste('No Probability of Stress to Aquatic Life')) %>%
      add_markers(data=dat, x= ~SampleDate, y= ~NITROGEN,mode = 'scatter', name="Total Nitrogen (mg/L)",marker = list(color= '#535559'),
                  hoverinfo="text",text=~paste(sep="<br>",
                                               paste("Date: ",SampleDate),
                                               paste("Depth: ",FDT_DEPTH, "m"),
                                               paste("Total Nitrogen: ",NITROGEN,"mg/L")))%>%
      layout(showlegend=FALSE,
             yaxis=list(title="Total Nitrogen (mg/L)"),
             xaxis=list(title="Sample Date",tickfont = list(size = 10)))
  })
  
}
