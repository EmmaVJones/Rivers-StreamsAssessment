DOPlotlySingleStationUI <- function(id){
  ns <- NS(id)
  tagList(
    wellPanel(
      h4(strong('Single Station Data Visualization')),
      uiOutput(ns('DO_oneStationSelectionUI')),
      plotlyOutput(ns('DOplotly')),
      br(),hr(),br(),
      fluidRow(
        column(8, h5('All DO records that are outside the criteria for the ',span(strong('selected site')),' are highlighted below.'),
               div(style = 'height:350px;overflow-y: scroll', 
                   h6('All Dissolved Oxygen Measures'),tableOutput(ns('DOMinTableSingleSite')), br(), hr(), br(),
                   h6('All Daily Average Dissolved Oxygen Measures'),tableOutput(ns('DODailyAverageTableSingleSite')))),
        column(4, h5('Individual DO exceedance statistics for the ',span(strong('selected site')),' are highlighted below.'),
               h6('All Dissolved Oxygen Measures'), tableOutput(ns("stationDOExceedanceRate")),
               h6('All Daily Average Dissolved Oxygen Measures'),tableOutput(ns("stationDO_DailyAverageExceedanceRate"))))
    )
  )
}

DOPlotlySingleStation <- function(input,output,session, AUdata){
  ns <- session$ns
  
  # Select One station for individual review
  output$DO_oneStationSelectionUI <- renderUI({
    req(AUdata)
    selectInput(ns('DO_oneStationSelection'),strong('Select Station to Review'),choices=unique(AUdata())$FDT_STA_ID,width='300px')})
  
  DO_oneStation <- reactive({
    req(ns(input$DO_oneStationSelection))
    filter(AUdata(),FDT_STA_ID %in% input$DO_oneStationSelection)})
  
  output$DOplotly <- renderPlotly({
    req(input$DO_oneStationSelection, DO_oneStation())
    dat <- mutate(DO_oneStation(), bottom = `Dissolved Oxygen Min (mg/L)`)
    dat$SampleDate <- as.POSIXct(dat$FDT_DATE_TIME2, format="%m/%d/%y")
    plot_ly(data=dat)%>%
      add_lines(x=~SampleDate,y=~bottom, mode='line',line = list(color = '#E50606'),
                hoverinfo = "none", name="DO Standard") %>%
      add_markers(x= ~SampleDate, y= ~DO,mode = 'scatter', name="DO (mg/L)",
                  hoverinfo="text",text=~paste(sep="<br>",
                                               paste("Date: ",SampleDate),
                                               paste("Depth: ",FDT_DEPTH, "m"),
                                               paste("DO: ",DO," (mg/L)")))%>%
      layout(showlegend=FALSE,
             yaxis=list(title="DO (unitless)"),
             xaxis=list(title="Sample Date",tickfont = list(size = 10)))
  })
  
  output$DOMinTableSingleSite <- renderTable({
    req(DO_oneStation())
    DO_Assessment_Min(DO_oneStation())})
  
  output$DODailyAverageTableSingleSite <- renderTable({
    req(DO_oneStation())
    DO_Assessment_DailyAvg(DO_oneStation())})
  
  output$stationDOExceedanceRate <- renderTable({
    req(input$DO_oneStationSelection, DO_oneStation())
    exceedance_DO(DO_oneStation()) %>%
      dplyr::select(nSamples,nExceedance,exceedanceRate)}) # don't give assessment determination for single station})
  
  output$stationDO_DailyAverageExceedanceRate <- renderTable({
    req(input$DO_oneStationSelection, DO_oneStation())
    exceedance_DO_DailyAvg(DO_oneStation()) %>%
      dplyr::select(nSamples,nExceedance,exceedanceRate)}) # don't give assessment determination for single station})
}

DOExceedanceAnalysisUI <- function(id){
  ns <- NS(id)
  tagList(
    h5('All DO records that are outside the criteria for the',span(strong('assessment unit')),' are highlighted below. 
       If no records are presented in the table below, then no data falls below the DO minimum.'),
    tableOutput(ns('DOMinTableAU')), br(), hr(), br(), 
    tableOutput(ns('DODailyAverageTableAU'))
    )
}


DOExceedanceAnalysis <- function(input, output, session, AUdata){
  ns <- session$ns
  
  # DO Raw Exceedance Results (all AU)
  output$DOMinTableAU <- renderTable({
    req(AUdata)
    DO_Assessment_Min(AUdata())})
  
  # DO Raw DAILY AVERAGE Exceedance Results (all AU)
  output$DODailyAverageTableAU <- renderTable({
    req(AUdata)
    DO_Assessment_DailyAvg(AUdata())})
}