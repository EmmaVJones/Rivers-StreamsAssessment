source('testingDataset.R')

#AUData <- filter(conventionals_HUC, ID305B_1 %in% 'VAW-H01R_JMS04A00' | 
#                   ID305B_2 %in% 'VAW-H01R_JMS04A00' | 
#                   ID305B_2 %in% 'VAW-H01R_JMS04A00')%>% 
#  left_join(WQSvalues, by = 'CLASS')

x <-filter(conventionals_HUC, FDT_STA_ID %in% '4APKP-4-DRBA')# '4APKP-4-DRBA') '8-YRK022.70')#




pHPlotlySingleStationUI <- function(id){
  ns <- NS(id)
  tagList(
    wellPanel(
      h4(strong('Single Station Data Visualization')),
      fluidRow(column(6,uiOutput(ns('pH_oneStationSelectionUI'))),
               column(6,actionButton(ns('reviewData'),"Review Raw Parameter Data",class='btn-block', width = '250px'))),
      plotlyOutput(ns('pHplotly')),
      br(),hr(),br(),
      fluidRow(
        column(8, h5('All pH records that are outside the criteria for the ',span(strong('selected site')),' are highlighted below.'),
               div(style = 'height:150px;overflow-y: scroll', tableOutput(ns('pHRangeTableSingleSite')))),
        column(4, h5('Individual pH exceedance statistics for the ',span(strong('selected site')),' are highlighted below.'),
               tableOutput(ns("stationpHExceedanceRate"))))
    )
  )
}

pHPlotlySingleStation <- function(input,output,session, AUdata, stationSelectedAbove){
  ns <- session$ns
  
  # Select One station for individual review
  output$pH_oneStationSelectionUI <- renderUI({
    req(AUdata)
    selectInput(ns('pH_oneStationSelection'),strong('Select Station to Review'),
                choices= sort(unique(c(stationSelectedAbove(),AUdata()$FDT_STA_ID))), # Change this based on stationSelectedAbove
                width='300px', selected = stationSelectedAbove())})
  
  pH_oneStation <- reactive({
    req(ns(input$pH_oneStationSelection))
    filter(AUdata(),FDT_STA_ID %in% input$pH_oneStationSelection) %>%
      filter(!is.na(FDT_FIELD_PH))})
  
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
    req(pH_oneStation())
    parameterFilter <- dplyr::select(pH_oneStation(), FDT_STA_ID:FDT_COMMENT, FDT_FIELD_PH, FDT_FIELD_PH_RMK)
    
    DT::datatable(parameterFilter, rownames = FALSE, 
                  options= list(dom= 't', pageLength = nrow(parameterFilter), scrollX = TRUE, scrollY = "400px", dom='t')) %>%
      formatStyle(c('FDT_FIELD_PH','FDT_FIELD_PH_RMK'), 'FDT_FIELD_PH_RMK', backgroundColor = styleEqual(c('Level II', 'Level I'), c('yellow','orange'), default = 'lightgray'))
  })
  
  output$pHplotly <- renderPlotly({
    req(input$pH_oneStationSelection, pH_oneStation())
    dat <- mutate(pH_oneStation(),top = `pH Max`, bottom = `pH Min`)
    dat$SampleDate <- as.POSIXct(dat$FDT_DATE_TIME2, format="%m/%d/%y")
    
    box1 <- data.frame(x = c(min(dat$SampleDate), min(dat$SampleDate), max(dat$SampleDate),max(dat$SampleDate)), y = c(9, 14, 14, 9))
    box2 <- data.frame(x = c(min(dat$SampleDate), min(dat$SampleDate), max(dat$SampleDate),max(dat$SampleDate)), y = c(6, 9, 9, 6))
    box3 <- data.frame(x = c(min(dat$SampleDate), min(dat$SampleDate), max(dat$SampleDate),max(dat$SampleDate)), y = c(0, 6, 6, 0))
    
    plot_ly(data=dat)%>%
      add_polygons(data = box1, x = ~x, y = ~y, fillcolor = "#F0E442",opacity=0.6, line = list(width = 0),
                   hoverinfo="text", name =paste('Medium Probability of Stress to Aquatic Life')) %>%
      add_polygons(data = box2, x = ~x, y = ~y, fillcolor = "#009E73",opacity=0.6, line = list(width = 0),
                   hoverinfo="text", name =paste('Low Probability of Stress to Aquatic Life')) %>%
      add_polygons(data = box3, x = ~x, y = ~y, fillcolor = "#F0E442",opacity=0.6, line = list(width = 0),
                   hoverinfo="text", name =paste('Medium Probability of Stress to Aquatic Life')) %>%
      
      add_lines(data=dat, x=~SampleDate,y=~top, mode='line',line = list(color = 'black'),
                hoverinfo = "text",text="pH Standard", name="pH Standard") %>%
      add_lines(data=dat, x=~SampleDate,y=~bottom, mode='line',line = list(color = 'black'),
                hoverinfo = "text", text="pH Standard", name="pH Standard") %>%
      add_markers(data=dat, x= ~SampleDate, y= ~FDT_FIELD_PH,mode = 'scatter', name="pH (unitless)",  marker = list(color= '#535559'),
                  hoverinfo="text",text=~paste(sep="<br>",
                                               paste("Date: ",SampleDate),
                                               paste("Depth: ",FDT_DEPTH, "m"),
                                               paste("pH: ",FDT_FIELD_PH," (unitless)")))%>%
      layout(showlegend=FALSE,
             yaxis=list(title="pH (unitless)"),
             xaxis=list(title="Sample Date",tickfont = list(size = 10)))
  })
  
  output$pHRangeTableSingleSite <- renderTable({
    req(pH_oneStation())
    pH_rangeAssessment(pH_oneStation())})
  
  output$stationpHExceedanceRate <- renderTable({
    req(input$pH_oneStationSelection, pH_oneStation())
    exceedance_pH(pH_oneStation()) %>%
      dplyr::select(nSamples,nExceedance,exceedanceRate)}) # don't give assessment determination for single station})
  
}


pHExceedanceAnalysisUI <- function(id){
  ns <- NS(id)
  tagList(
    #fluidRow(
    # column(6,
    h5('All pH records that are outside the range for the',span(strong('assessment unit')),' are highlighted below. 
                If no records are presented in the table below, then no data falls outside the pH range.'),
    tableOutput(ns('pHRangeTable'))#),
    #column(6,
    #       wellPanel(
    #         h5('Station Exceedance Rate:'),
    #         uiOutput(ns('stationpHExceedanceRateSelect_UI')),
    #         tableOutput(ns("stationpHExceedanceRate")))))#,
    #hr(),
    #h5('Assessment Unit Exceedance Rate:'),
    #tableOutput(ns("pHExceedanceRate"))))
  )
}


pHExceedanceAnalysis <- function(input, output, session, AUdata){
  ns <- session$ns
  
  # pH Raw Exceedance Results (all AU)
  output$pHRangeTable <- renderTable({
    req(AUdata)
    pH_rangeAssessment(AUdata())})
  
  # pHStation Exceedance Rate
  #output$stationpHExceedanceRateSelect_UI <- renderUI({
  #  req(AUdata)
  #  selectInput(ns('stationpHExceedanceRateSelect'),strong('Select Station to Review for individual pH range statistics'),
  #              choices=unique(AUdata())$FDT_STA_ID,width='300px')})
  
  #output$stationpHExceedanceRate <- renderTable({
  #  req(input$stationpHExceedanceRateSelect)
  #  z <- filter(AUdata(),FDT_STA_ID %in% input$stationpHExceedanceRateSelect)
  #  exceedance_pH(z) %>%
  #    dplyr::select(nSamples,nExceedance,exceedanceRate)}) # don't give assessment determination for single station})
  
  # pH AU Exceedance Rate
  #output$pHExceedanceRate <- renderTable({
  #  req(AUdata)
  #  exceedance_pH(AUdata())})
  
}

ui <- fluidPage(
  helpText('Review each site using the single site visualization section, then 
           proceed to the bottom of the page to find exceedance rate for the entire assessment unit.',br(),
           span(strong('NOTE: The pH exceedance analysis results at the bottom of the page include data
                       from ALL stations within the assessment unit.'))),
  pHPlotlySingleStationUI('pH'),
  br(),hr(),br(),
  pHExceedanceAnalysisUI('pH_ExceedanceAnalysis')      )

server <- function(input,output,session){
  
  stationData <- eventReactive( input$stationSelection, {
    filter(AUData, FDT_STA_ID %in% input$stationSelection) })
  stationSelected <- reactive({input$stationSelection})
  
  
  AUData <- reactive({filter(conventionals_HUC, FDT_STA_ID %in% c('2-JKS018.68','4APKP-4-DRBA','2CES-VT22-UVA' ))})
  #AUData <- reactive({filter(conventionals_HUC, ID305B_1 %in% 'VAW-H01R_JMS04A00' | 
  #                             ID305B_2 %in% 'VAW-H01R_JMS04A00' | 
  #                             ID305B_2 %in% 'VAW-H01R_JMS04A00')%>% 
  #    left_join(WQSvalues, by = 'CLASS')})
  
  callModule(pHPlotlySingleStation,'pH', AUData, stationSelected)
  #callModule(pHExceedanceAnalysis,'pH_ExceedanceAnalysis', AUData)
  
}

shinyApp(ui,server)

