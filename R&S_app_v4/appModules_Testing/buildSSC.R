source('testingDataset.R')

#AUData <- filter(conventionals_HUC, ID305B_1 %in% 'VAW-H01R_JMS04A00' | 
#                   ID305B_2 %in% 'VAW-H01R_JMS04A00' | 
#                   ID305B_2 %in% 'VAW-H01R_JMS04A00')%>% 
#  left_join(WQSvalues, by = 'CLASS')

x <-filter(conventionals_HUC, FDT_STA_ID %in% '4APKP-4-DRBA')# '4APKP-4-DRBA') '8-YRK022.70')#


# No Assessment functions bc no std


SSCPlotlySingleStationUI <- function(id){
  ns <- NS(id)
  tagList(
    wellPanel(
      h4(strong('Single Station Data Visualization')),
      fluidRow(column(6,      uiOutput(ns('SSC_oneStationSelectionUI'))),
               column(6,actionButton(ns('reviewData'),"Review Raw Parameter Data",class='btn-block', width = '250px'))),
      plotlyOutput(ns('SSCplotly'))  )
  )
}


SSCPlotlySingleStation <- function(input,output,session, AUdata, stationSelectedAbove){
  ns <- session$ns
  
  # Select One station for individual review
  output$SSC_oneStationSelectionUI <- renderUI({
    req(stationSelectedAbove)
    selectInput(ns('SSC_oneStationSelection'),strong('Select Station to Review'),choices= sort(unique(c(stationSelectedAbove(),AUdata()$FDT_STA_ID))),#unique(AUdata())$FDT_STA_ID,
                width='300px', selected = stationSelectedAbove())})# "2-JMS279.41" )})
  
  
  SSC_oneStation <- reactive({
    req(ns(input$SSC_oneStationSelection))
    filter(AUdata(),FDT_STA_ID %in% input$SSC_oneStationSelection) %>%
      filter(!is.na(SSC))})
  
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
    req(SSC_oneStation())
    parameterFilter <- dplyr::select(SSC_oneStation(), FDT_STA_ID:FDT_COMMENT, SSC, `RMK_SSC-TOTAL`)
    
    DT::datatable(parameterFilter, rownames = FALSE, 
                  options= list(dom= 't', pageLength = nrow(parameterFilter), scrollX = TRUE, scrollY = "400px", dom='t')) %>%
      formatStyle(c('SSC','RMK_SSC-TOTAL'), 'RMK_SSC-TOTAL', backgroundColor = styleEqual(c('Level II', 'Level I'), c('yellow','orange'), default = 'lightgray'))
  })
  
  output$SSCplotly <- renderPlotly({
    req(input$SSC_oneStationSelection, SSC_oneStation())
    dat <- SSC_oneStation()
    dat$SampleDate <- as.POSIXct(dat$FDT_DATE_TIME2, format="%m/%d/%y")
    
    
    plot_ly(data=dat)%>%
      add_markers(data=dat, x= ~SampleDate, y= ~SSC,mode = 'scatter', name="Suspended Sediment Concentration (units) ",marker = list(color= '#535559'),
                  hoverinfo="text",text=~paste(sep="<br>",
                                               paste("Date: ",SampleDate),
                                               paste("Depth: ",FDT_DEPTH, "m"),
                                               paste("Suspended Sediment Concentration: ",SSC,"units")))%>%
      layout(showlegend=FALSE,
             yaxis=list(title="Suspended Sediment Concentration (units)"),
             xaxis=list(title="Sample Date",tickfont = list(size = 10)))
  })
  
}



ui <- fluidPage(
  helpText('Review each site using the single site visualization section, then 
           proceed to the bottom of the page to find exceedance rate for the entire assessment unit.',br(),
           span(strong('NOTE: The pH exceedance analysis results at the bottom of the page include data
                       from ALL stations within the assessment unit.'))),
  SSCPlotlySingleStationUI('SSC')
)

server <- function(input,output,session){
  
  stationData <- eventReactive( input$stationSelection, {
    filter(AUData, FDT_STA_ID %in% input$stationSelection) })
  stationSelected <- reactive({input$stationSelection})
  
  
  AUData <- reactive({filter(conventionals_HUC, FDT_STA_ID %in% c('2-JKS018.68','4APKP-4-DRBA' ))})
  
  callModule(SSCPlotlySingleStation,'SSC', AUData, stationSelected)
}

shinyApp(ui,server)

