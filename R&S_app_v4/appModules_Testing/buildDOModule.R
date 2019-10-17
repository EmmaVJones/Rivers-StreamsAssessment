source('testingDataset.R')

#AUData <- filter(conventionals_HUC, ID305B_1 %in% 'VAW-H01R_JMS04A00' | 
#                   ID305B_2 %in% 'VAW-H01R_JMS04A00' | 
#                   ID305B_2 %in% 'VAW-H01R_JMS04A00')%>% 
#  left_join(WQSvalues, by = 'CLASS')

x <-filter(conventionals_HUC, FDT_STA_ID %in% '4APKP-4-DRBA')# '4APKP-4-DRBA') '8-YRK022.70')#






DOPlotlySingleStationUI <- function(id){
  ns <- NS(id)
  tagList(
    wellPanel(
      h4(strong('Single Station Data Visualization')),
      fluidRow(column(6,uiOutput(ns('DO_oneStationSelectionUI'))),
               column(6,actionButton(ns('reviewData'),"Review Raw Parameter Data",class='btn-block', width = '250px'))),
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

DOPlotlySingleStation <- function(input,output,session, AUdata, stationSelectedAbove){
  ns <- session$ns
  
  # Select One station for individual review
  output$DO_oneStationSelectionUI <- renderUI({
    req(AUdata)
    selectInput(ns('DO_oneStationSelection'),strong('Select Station to Review'),
                choices= sort(unique(c(stationSelectedAbove(),AUdata()$FDT_STA_ID))), # Change this based on stationSelectedAbove
                width='300px', selected = stationSelectedAbove())})
  
  DO_oneStation <- reactive({
    req(ns(input$DO_oneStationSelection))
    filter(AUdata(),FDT_STA_ID %in% input$DO_oneStationSelection) %>%
      filter(!is.na(DO))})
  
  # Button to visualize modal table of available parameter data
  observeEvent(input$reviewData,{
    showModal(modalDialog(
      title="Review Raw Data for Selected Station and Parameter",
      helpText('This table subsets the conventionals raw data by station selected in Single Station Visualization Section drop down and
               parameter currently reviewing. Scroll right to see the raw parameter values and any data collection comments. Data analyzed
               by app is highlighted in gray (all DEQ data and non agency/citizen monitoring Level III), data counted by app and noted in
               comment fields is highlighed in yellow (non agency/citizen monitoring Level II), and data NOT CONSIDERED in app is noted in
               orange (non agency/citizen monitoring Level I).'),
      DT::dataTableOutput(ns('parameterData')),
      easyClose = TRUE))  })
  
  # modal parameter data
  output$parameterData <- DT::renderDataTable({
    req(DO_oneStation())
    parameterFilter <- dplyr::select(DO_oneStation(), FDT_STA_ID:FDT_COMMENT, DO, DO_RMK)
    
    DT::datatable(parameterFilter, rownames = FALSE, 
                  options= list(dom= 't', pageLength = nrow(parameterFilter), scrollX = TRUE, scrollY = "400px", dom='t')) %>%
      formatStyle(c('DO','DO_RMK'), 'DO_RMK',backgroundColor = styleEqual(c('Level II', 'Level I'), c('yellow','orange'), default = 'lightgray'))
  })
  
  output$DOplotly <- renderPlotly({
    req(input$DO_oneStationSelection, DO_oneStation())
    dat <- mutate(DO_oneStation(), bottom = `Dissolved Oxygen Min (mg/L)`)
    dat$SampleDate <- as.POSIXct(dat$FDT_DATE_TIME2, format="%m/%d/%y")
    
    maxheight <- ifelse(max(dat$DO, na.rm=T) < 10, 12, max(dat$DO, na.rm=T)* 1.2)
    box1 <- data.frame(SampleDate = c(min(dat$SampleDate), min(dat$SampleDate), max(dat$SampleDate),max(dat$SampleDate)), y = c(10, maxheight, maxheight, 10))
    box2 <- data.frame(x = c(min(dat$SampleDate), min(dat$SampleDate), max(dat$SampleDate),max(dat$SampleDate)), y = c(8, 10, 10, 8))
    box3 <- data.frame(x = c(min(dat$SampleDate), min(dat$SampleDate), max(dat$SampleDate),max(dat$SampleDate)), y = c(7, 8, 8, 7))
    box4 <- data.frame(x = c(min(dat$SampleDate), min(dat$SampleDate), max(dat$SampleDate),max(dat$SampleDate)), y = c(0, 7, 7, 0))
    
    plot_ly(data=box1)%>%
      add_polygons(x = ~SampleDate, y = ~y, data = box1, fillcolor = "#0072B2",opacity=0.6, line = list(width = 0),
                   hoverinfo="text", name =paste('No Probability of Stress to Aquatic Life')) %>%
      add_polygons(data = box2, x = ~x, y = ~y, fillcolor = "#009E73",opacity=0.6, line = list(width = 0),
                   hoverinfo="text", name =paste('Low Probability of Stress to Aquatic Life')) %>%
      add_polygons(data = box3, x = ~x, y = ~y, fillcolor = "#F0E442",opacity=0.6, line = list(width = 0),
                   hoverinfo="text", name =paste('Medium Probability of Stress to Aquatic Life')) %>%
      add_polygons(data = box4, x = ~x, y = ~y, fillcolor = "firebrick",opacity=0.6, line = list(width = 0),
                   hoverinfo="text", name =paste('High Probability of Stress to Aquatic Life')) %>%
      add_lines(data=dat, x=~SampleDate,y=~bottom, mode='line', line = list(color = 'black'),
                hoverinfo = "text", text="DO Standard", name="DO Standard") %>%
      add_markers(data=dat, x= ~SampleDate, y= ~DO,mode = 'scatter', name="DO (mg/L)", marker = list(color= '#535559'),
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



ui <- fluidPage(
  helpText('Review each site using the single site visualization section, then 
           proceed to the bottom of the page to find exceedance rate for the entire assessment unit.',br(),
           span(strong('NOTE: The DO exceedance analysis results at the bottom of the page include data
                       from ALL stations within the assessment unit.'))),
  DOPlotlySingleStationUI('DO'),
  br(),hr(),br(),
  DOExceedanceAnalysisUI('DO_ExceedanceAnalysis')     )

server <- function(input,output,session){
  
  stationData <- eventReactive( input$stationSelection, {
    filter(AUData, FDT_STA_ID %in% input$stationSelection) })
  stationSelected <- reactive({input$stationSelection})
  
  
  AUData <- reactive({filter(conventionals_HUC, FDT_STA_ID %in% c('2-JKS018.68','4APKP-4-DRBA' ))})

  
  callModule(DOPlotlySingleStation,'DO', AUData, stationSelected)
  #callModule(DOExceedanceAnalysis,'DO_ExceedanceAnalysis', AUData)
  
}

shinyApp(ui,server)

