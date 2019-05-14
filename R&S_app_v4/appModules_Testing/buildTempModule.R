source('testingDataset.R')



#Max Temperature Exceedance Function
temp_Assessment <- function(x){
  temp <- dplyr::select(x,FDT_DATE_TIME,FDT_TEMP_CELCIUS, `Max Temperature (C)`)%>% # Just get relevant columns, 
    filter(!is.na(FDT_TEMP_CELCIUS))%>% #get rid of NA's
    mutate(TemperatureExceedance=ifelse(FDT_TEMP_CELCIUS > `Max Temperature (C)`,T,F))%>% # Identify where above max Temperature, 
    filter(TemperatureExceedance==TRUE) # Only return temp measures above threshold
  return(temp)
}

# Exceedance Rate Temperature
exceedance_temp <- function(x){
  temp <- dplyr::select(x,FDT_DATE_TIME,FDT_TEMP_CELCIUS,`Max Temperature (C)`)%>% # Just get relevant columns, 
    filter(!is.na(FDT_TEMP_CELCIUS)) #get rid of NA's
  temp_Assess <- temp_Assessment(x)
  
  temp_results <- assessmentDetermination(temp,temp_Assess,"temperature","Aquatic Life")
  return(temp_results)
}


temperaturePlotlySingleStationUI <- function(id){
  ns <- NS(id)
  tagList(
    wellPanel(
      h4(strong('Single Station Data Visualization')),
      uiOutput(ns('temperature_oneStationSelectionUI')),
      plotlyOutput(ns('Tempplotly')),
      br(),hr(),br(),
      h5('All temperature records that are above the criteria for the ',span(strong('selected site')),' are highlighted below.'),
      div(style = 'height:150px;overflow-y: scroll', tableOutput(ns('TempRangeTableSingleSite')))
    )
  )
}

temperaturePlotlySingleStation <- function(input,output,session, AUdata){
  ns <- session$ns
  
  # Select One station for individual review
  output$temperature_oneStationSelectionUI <- renderUI({
    req(AUdata)
    selectInput(ns('temperature_oneStationSelection'),strong('Select Station to Review'),choices=unique(AUdata())$FDT_STA_ID,width='300px')})
  
  temperature_oneStation <- reactive({
    req(ns(input$temperature_oneStationSelection))
    filter(AUdata(),FDT_STA_ID %in% input$temperature_oneStationSelection)})
  
  output$Tempplotly <- renderPlotly({
    req(input$temperature_oneStationSelection, temperature_oneStation())
    dat <- mutate(temperature_oneStation(),top = `Max Temperature (C)`)
    dat$SampleDate <- as.POSIXct(dat$FDT_DATE_TIME2, format="%m/%d/%y")
    plot_ly(data=dat)%>%
      add_lines(x=~SampleDate,y=~top, mode='line',line = list(color = '#E50606'),
                hoverinfo = "none",text="Temperature Standard",name="Temperature Standard") %>%
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
    req(temperature_oneStation())
    temp_Assessment(temperature_oneStation())})
}

temperatureExceedanceAnalysisUI <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(6,
             h5('All temperature records that exceed the threshold for the',span(strong('assessment unit')),' are highlighted below. 
                If no records are presented in the table below, then no data exceedes the temperature threshold.'),
             tableOutput(ns('tempRangeTable'))),
      column(6,
             wellPanel(
               h5('Station Exceedance Rate:'),
               uiOutput(ns('stationTempExceedanceRateSelect_UI')),
               tableOutput(ns("stationTempExceedanceRate"))),
             hr(),
             h5('Assessment Unit Exceedance Rate:'),
             tableOutput(ns("tempExceedanceRate"))))
    
    )
}

temperatureExceedanceAnalysis <- function(input, output, session, AUdata){
  ns <- session$ns
  
  # Temperature Raw Exceedance Results (all AU)
  output$tempRangeTable <- renderTable({
    req(AUdata)
    temp_Assessment(AUdata())})
  
  # Temperature Station Exceedance Rate
  output$stationTempExceedanceRateSelect_UI <- renderUI({
    req(AUdata)
    selectInput(ns('stationTempExceedanceRateSelect'),strong('Select Station to Review for individual temperature exceedance statistics'),
                choices=unique(AUdata())$FDT_STA_ID,width='300px')})
  
  output$stationTempExceedanceRate <- renderTable({
    req(input$stationTempExceedanceRateSelect)
    z <- filter(AUdata(),FDT_STA_ID %in% input$stationTempExceedanceRateSelect)
    exceedance_temp(z) %>%
      dplyr::select(nSamples,nExceedance,exceedanceRate)}) # don't give assessment determination for single station})
  
  # Temperature AU Exceedance Rate
  output$tempExceedanceRate <- renderTable({
    req(AUdata)
    exceedance_temp(AUdata())})
  
}

ui <- fluidPage(
  helpText('Review each site using the single site visualization section, then 
           proceed to the bottom of the page to find exceedance rate for the entire assessment unit.',br(),
           span(strong('NOTE: The temperature exceedance analysis results at the bottom of the page include data
                       from ALL stations within the assessment unit.'))),
  temperaturePlotlySingleStationUI('temperature'),
  br(),hr(),br(),
  temperatureExceedanceAnalysisUI('temperature_ExceedanceAnalysis')
)

server <- function(input,output,session){
  
  AUData <- reactive({filter(conventionals_HUC, ID305B_1 %in% 'VAW-H01R_JMS04A00' | 
                               ID305B_2 %in% 'VAW-H01R_JMS04A00' | 
                               ID305B_2 %in% 'VAW-H01R_JMS04A00')%>% 
      left_join(WQSvalues, by = 'CLASS')})
  
  callModule(temperaturePlotlySingleStation,'temperature', AUData)
  
  callModule(temperatureExceedanceAnalysis,'temperature_ExceedanceAnalysis', AUData)
  
  
}

shinyApp(ui,server)



