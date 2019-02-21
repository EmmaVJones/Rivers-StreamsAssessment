temperatureSubTabUI <- function(id){
  ns <- NS(id)
  tagList(
    plotlyOutput(ns('Tempplotly')),
    br(),hr(),br(),
    h5('All temperature records that are above the criteria for the ',span(strong('selected site')),' are highlighted below.'),
    div(style = 'height:150px;overflow-y: scroll', tableOutput(ns('TempRangeTableSingleSite')))
  )
}

temperatureSubTab <- function(input,output,session, oneStationData){
  ns <- session$ns
  
  output$Tempplotly <- renderPlotly({
    req(oneStationData)
    dat <- mutate(oneStationData(),top = `Max Temperature (C)`)
    dat$SampleDate <- as.POSIXct(dat$FDT_DATE_TIME2, format="%m/%d/%y")
    plot_ly(data=dat)%>%
      add_lines(x=~SampleDate,y=~top, mode='line',line = list(color = '#E50606'),
                hoverinfo = "WQS", name="Temperature Standard") %>%
      add_markers(x= ~SampleDate, y= ~FDT_TEMP_CELCIUS,mode = 'scatter', name="Temperature (Celsius)",
                  hoverinfo="text",text=~paste(sep="<br>",
                                               paste("Date: ",SampleDate),
                                               paste("Depth: ",FDT_DEPTH, "m"),
                                               paste("Temperature: ",FDT_TEMP_CELCIUS,"C")))%>%
      layout(showlegend=FALSE,
             yaxis=list(title="Temperature (Celsius)"),
             xaxis=list(title="Sample Date",tickfont = list(size = 10)))
  })
  
  output$TempRangeTableSingleSite <- renderTable({
    req(oneStationData)
    z <- temp_Assessment(oneStationData())
    if(nrow(z)>0){return(z%>%dplyr::select(-FDT_STA_ID))}else{return(z)}
    
  })
}