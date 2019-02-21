
AmmoniaPlotlySingleStationUI <- function(id){
  ns <- NS(id)
  tagList(
    wellPanel(
      h4(strong('Single Station Data Visualization')),
      uiOutput(ns('Ammonia_oneStationSelectionUI')),
      plotlyOutput(ns('Ammoniaplotly')),
      br(),hr(),br(),
      fluidRow(
        column(8, h5('All ammonia records that are above the criteria for the ',span(strong('selected site')),' are highlighted below.'),
               div(style = 'height:150px;overflow-y: scroll', tableOutput(ns('AmmoniaRangeTableSingleSite')))),
        column(4, h5('Individual ammonia exceedance statistics for the ',span(strong('selected site')),' are highlighted below.',span(strong('Note: the ammonia
                                                                                                                                             samples from the last three years of data collected are the only samples utilized for exceedance calculations.'))),
               tableOutput(ns("stationAmmoniaExceedanceRate"))))
        )
      )
}


AmmoniaPlotlySingleStation <- function(input,output,session, AUdata, stationSelectedAbove){
  ns <- session$ns
  
  # Select One station for individual review
  output$Ammonia_oneStationSelectionUI <- renderUI({
    req(stationSelectedAbove)
    selectInput(ns('Ammonia_oneStationSelection'),strong('Select Station to Review'),choices= sort(unique(c(stationSelectedAbove(),AUdata()$FDT_STA_ID))),#unique(AUdata())$FDT_STA_ID,
                width='300px', selected = stationSelectedAbove())})# "2-JMS279.41" )})
  
  
  Ammonia_oneStation <- reactive({
    req(ns(input$Ammonia_oneStationSelection))
    filter(AUdata(),FDT_STA_ID %in% input$Ammonia_oneStationSelection)})
  
  output$Ammoniaplotly <- renderPlotly({
    req(input$Ammonia_oneStationSelection, Ammonia_oneStation())
    dat <- acuteNH3limit(Ammonia_oneStation()) %>%
      filter(!is.na(AMMONIA)) %>%
      mutate(over=ifelse(AMMONIA > NH3limit, '#D11814', '#535559'))# 'VIOLATION', 'GOOD'))
    dat$SampleDate <- as.POSIXct(as.POSIXct(dat$FDT_DATE_TIME2, format="%m/%d/%Y %H:%M"), format="%m/%d/%y")
    
    last3years <- mutate(dat, sampleYear = lubridate::year(SampleDate)) %>%
      filter(sampleYear %in% lastXyears(dat, 'SampleDate', 3))
    box1 <- data.frame(SampleDate = c(min(last3years$FDT_DATE_TIME2), min(last3years$FDT_DATE_TIME2),
                                      max(last3years$FDT_DATE_TIME2),max(last3years$FDT_DATE_TIME2)), 
                       y = c(min(dat$AMMONIA), max(dat$AMMONIA), max(dat$AMMONIA), min(dat$AMMONIA)))
    
    
    if(nrow(dat) > 0){ 
      plot_ly(data=dat)%>%
        add_polygons(x = ~SampleDate, y = ~y, data = box1, fillcolor = "#B0B3B7",opacity=0.6, line = list(width = 0),
                     hoverinfo="text", name =paste('Most recent three years of data in assessment window')) %>%
        
        add_markers(data=dat, x= ~SampleDate, y= ~AMMONIA,mode = 'scatter', name="Ammonia (mg/L as N)",marker = list(color= ~over),#'#535559'),
                    hoverinfo="text",text=~paste(sep="<br>",
                                                 paste("Date: ",SampleDate),
                                                 paste("Depth: ",FDT_DEPTH, "m"),
                                                 paste("Ammonia: ",AMMONIA,"mg/L as N"),
                                                 paste('Acute Ammonia Limit: ',format(NH3limit, digits=3),"mg/L as N"),
                                                 paste('pH: ', FDT_FIELD_PH, '(unitless)')))%>%
        layout(showlegend=FALSE,
               yaxis=list(title="Ammonia (mg/L as N)"),
               xaxis=list(title="Sample Date",tickfont = list(size = 10)))
    }
    
  })
  
  
  
  output$AmmoniaRangeTableSingleSite <- renderTable({
    req(Ammonia_oneStation())
    acuteNH3limit(Ammonia_oneStation()) %>%
      mutate(Exceedance = ifelse(AMMONIA > NH3limit, TRUE, FALSE)) %>%
      filter(Exceedance == TRUE)})
  
  output$stationAmmoniaExceedanceRate <- renderTable({
    req(input$Ammonia_oneStationSelection, Ammonia_oneStation())
    acuteNH3exceedance(Ammonia_oneStation()) %>%
      dplyr::select(1:3) %>%# don't give assessment determination for single station
      dplyr::rename(nSamples = AcuteAmmonia_SAMP,nExceedance= AcuteAmmonia_VIO,exceedanceRate= AcuteAmmonia_exceedanceRate)}) # make it match everything else
  
  
}


