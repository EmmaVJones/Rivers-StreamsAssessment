source('testingDataset.R')

#AUData <- filter(conventionals_HUC, ID305B_1 %in% 'VAW-H01R_JMS04A00' | 
#                   ID305B_2 %in% 'VAW-H01R_JMS04A00' | 
#                   ID305B_2 %in% 'VAW-H01R_JMS04A00')%>% 
#  left_join(WQSvalues, by = 'CLASS')

x <-filter(conventionals_HUC, FDT_STA_ID %in% '4APKP-4-DRBA')# '4APKP-4-DRBA') '8-YRK022.70')#


# No Assessment functions bc no std but still need to count samples taken

TPPlotlySingleStationUI <- function(id){
  ns <- NS(id)
  tagList(
    wellPanel(
      h4(strong('Single Station Data Visualization')),
      fluidRow(column(6, uiOutput(ns('TP_oneStationSelectionUI'))),
               column(6,actionButton(ns('reviewData'),"Review Raw Parameter Data",class='btn-block', width = '250px'))),
      plotlyOutput(ns('TPplotly')),
      h5('Total Phosphorus exceedances of 0.2 mg/L for the ',span(strong('selected site')),' are highlighted below.'),
      fluidRow(
        column(6, h6('If there is no data listed below then none of the measures exceeded 0.2 mg/L Total Phosphorus.'), tableOutput(ns("stationTPExceedance"))),
        column(6, h6('Total Phosphorus > 0.2 mg/L exceedance rate'),tableOutput(ns("stationTPExceedanceRate")))))  
  )
}


TPPlotlySingleStation <- function(input,output,session, AUdata, stationSelectedAbove){
  ns <- session$ns
  
  # Select One station for individual review
  output$TP_oneStationSelectionUI <- renderUI({
    req(stationSelectedAbove)
    selectInput(ns('TP_oneStationSelection'),strong('Select Station to Review'),choices= sort(unique(c(stationSelectedAbove(),AUdata()$FDT_STA_ID))),#unique(AUdata())$FDT_STA_ID,
                width='300px', selected = stationSelectedAbove())})# "2-JMS279.41" )})
  
  
  TP_oneStation <- reactive({
    req(ns(input$TP_oneStationSelection))
    filter(AUdata(),FDT_STA_ID %in% input$TP_oneStationSelection) %>%
      filter(!is.na(PHOSPHORUS))})
  
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
    req(TP_oneStation())
    parameterFilter <- dplyr::select(TP_oneStation(), FDT_STA_ID:FDT_COMMENT, PHOSPHORUS, RMK_PHOSPHORUS)
    
    DT::datatable(parameterFilter, rownames = FALSE, 
                  options= list(dom= 't', pageLength = nrow(parameterFilter), scrollX = TRUE, scrollY = "400px", dom='t')) %>%
      formatStyle(c('PHOSPHORUS','RMK_PHOSPHORUS'), 'RMK_PHOSPHORUS', 
                  backgroundColor = styleEqual(c('Level II', 'Level I'), c('yellow','orange'), default = 'lightgray'))
  })
  
  output$TPplotly <- renderPlotly({
    req(input$TP_oneStationSelection, TP_oneStation())
    dat <- TP_oneStation()
    dat$SampleDate <- as.POSIXct(dat$FDT_DATE_TIME2, format="%m/%d/%y")
    
    maxheight <- ifelse(max(dat$PHOSPHORUS, na.rm=T) < 0.1, 0.12, max(dat$PHOSPHORUS, na.rm=T)* 1.2)
    box1 <- data.frame(SampleDate = c(min(dat$SampleDate), min(dat$SampleDate), max(dat$SampleDate),max(dat$SampleDate)), y = c(0.1, maxheight, maxheight, 0.1))
    box2 <- data.frame(x = c(min(dat$SampleDate), min(dat$SampleDate), max(dat$SampleDate),max(dat$SampleDate)), y = c(0.05, 0.1, 0.1, 0.05))
    box3 <- data.frame(x = c(min(dat$SampleDate), min(dat$SampleDate), max(dat$SampleDate),max(dat$SampleDate)), y = c(0.02, 0.05, 0.05, 0.02))
    box4 <- data.frame(x = c(min(dat$SampleDate), min(dat$SampleDate), max(dat$SampleDate),max(dat$SampleDate)), y = c(0, 0.02, 0.02, 0))
    
    
    plot_ly(data=dat)%>%
      add_polygons(x = ~SampleDate, y = ~y, data = box1, fillcolor = "firebrick",opacity=0.6, line = list(width = 0),
                   hoverinfo="text", name =paste('High Probability of Stress to Aquatic Life')) %>%
      add_polygons(data = box2, x = ~x, y = ~y, fillcolor = "#F0E442",opacity=0.6, line = list(width = 0),
                   hoverinfo="text", name =paste('Medium Probability of Stress to Aquatic Life')) %>%
      add_polygons(data = box3, x = ~x, y = ~y, fillcolor = "#009E73",opacity=0.6, line = list(width = 0),
                   hoverinfo="text", name =paste('Low Probability of Stress to Aquatic Life')) %>%
      add_polygons(data = box4, x = ~x, y = ~y, fillcolor = "#0072B2",opacity=0.6, line = list(width = 0),
                   hoverinfo="text", name =paste('No Probability of Stress to Aquatic Life')) %>%
      add_markers(data=dat, x= ~SampleDate, y= ~PHOSPHORUS,mode = 'scatter', name="Total Phosphorus (mg/L)",marker = list(color= '#535559'),
                  hoverinfo="text",text=~paste(sep="<br>",
                                               paste("Date: ",SampleDate),
                                               paste("Depth: ",FDT_DEPTH, "m"),
                                               paste("Total Phosphorus: ",PHOSPHORUS,"mg/L")))%>%
      layout(showlegend=FALSE,
             yaxis=list(title="Total Phosphorus (mg/L)"),
             xaxis=list(title="Sample Date",tickfont = list(size = 10)))
  })
  
  output$stationTPExceedance <- renderTable({
    req(input$TP_oneStationSelection, TP_oneStation())
    dplyr::select(TP_oneStation(), FDT_STA_ID, FDT_DATE_TIME, FDT_DEPTH, PHOSPHORUS) %>%
      filter(PHOSPHORUS > 0.2)
  })
  
  
  output$stationTPExceedanceRate <- renderTable({
    req(input$TP_oneStationSelection, TP_oneStation())
    TPexceed(TP_oneStation()) %>% dplyr::select(-NUT_TP_STAT)
  })
}


ui <- fluidPage(
  helpText('Review each site using the single site visualization section, then 
           proceed to the bottom of the page to find exceedance rate for the entire assessment unit.',br(),
           span(strong('NOTE: The pH exceedance analysis results at the bottom of the page include data
                       from ALL stations within the assessment unit.'))),
  TPPlotlySingleStationUI('TP')     )

server <- function(input,output,session){
  
  stationData <- eventReactive( input$stationSelection, {
    filter(AUData, FDT_STA_ID %in% input$stationSelection) })
  stationSelected <- reactive({input$stationSelection})
  
  
  AUData <- reactive({filter(conventionals_HUC, FDT_STA_ID %in% c('2-JKS018.68','4APKP-4-DRBA' ))})
  callModule(TPPlotlySingleStation,'TP', AUData, stationSelected)
  
}

shinyApp(ui,server)


