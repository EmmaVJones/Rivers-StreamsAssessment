source('testingDataset.R')

AUData <- filter(conventionals_HUC, ID305B_1 %in% 'VAW-H01R_JMS04A00' | 
                   ID305B_2 %in% 'VAW-H01R_JMS04A00' | 
                   ID305B_2 %in% 'VAW-H01R_JMS04A00')%>% 
  left_join(WQSvalues, by = 'CLASS')

x <-filter(AUData, FDT_STA_ID %in% '2-JMS279.41') 

pH_rangeAssessment <- function(x){
  pH <- dplyr::select(x,FDT_STA_ID,FDT_DATE_TIME,FDT_DEPTH,FDT_FIELD_PH,`pH Min`,`pH Max`)%>% # Just get relevant columns, 
    filter(!is.na(FDT_FIELD_PH))%>% #get rid of NA's
    rowwise()%>% mutate(interval=findInterval(FDT_FIELD_PH,c(`pH Min`,`pH Max`)))%>% # Identify where pH outside of assessment range
    ungroup()%>%
    mutate(pHrange=ifelse(interval==1,T,F))%>% # Highlight where pH doesn't fall into assessment range
    filter(pHrange==FALSE)%>% # Only return pH measures outside of assessement range
    dplyr::select(-c(interval,pHrange)) # Don't show user interval column, could be confusing to them, T/F in pHrange column sufficient
  return(pH)
}


exceedance_pH <- function(x){
  pH <- dplyr::select(x,FDT_STA_ID,FDT_DATE_TIME,FDT_DEPTH,FDT_FIELD_PH,`pH Min`,`pH Max`)%>% # Just get relavent columns, 
    filter(!is.na(FDT_FIELD_PH)) #get rid of NA's
  pH_rangeAssess <- pH_rangeAssessment(x)
  pH_results <- assessmentDetermination(pH, pH_rangeAssess,"pH","Aquatic Life")
  return(pH_results)
}


pHPlotlySingleStationUI <- function(id){
  ns <- NS(id)
  tagList(
    wellPanel(
      h4(strong('Single Station Data Visualization')),
      uiOutput(ns('pH_oneStationSelectionUI')),
      plotlyOutput(ns('pHplotly')),
      br(),hr(),br(),
      h5('All pH records that are outside the criteria for the ',span(strong('selected site')),' are highlighted below.'),
      div(style = 'height:150px;overflow-y: scroll', tableOutput(ns('pHRangeTableSingleSite')))
    )
  )
}

pHPlotlySingleStation <- function(input,output,session, AUdata){
  ns <- session$ns
  
  # Select One station for individual review
  output$pH_oneStationSelectionUI <- renderUI({
    req(AUdata)
    selectInput(ns('pH_oneStationSelection'),strong('Select Station to Review'),choices=unique(AUdata())$FDT_STA_ID,width='300px')})
  
  pH_oneStation <- reactive({
    req(ns(input$pH_oneStationSelection))
    filter(AUdata(),FDT_STA_ID %in% input$pH_oneStationSelection)})
  
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
                hoverinfo = "text",text="pH Standard",  name="pH Standard") %>%
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
}


pHExceedanceAnalysisUI <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(6,
             h5('All pH records that are outside the range for the',span(strong('assessment unit')),' are highlighted below. 
                If no records are presented in the table below, then no data falls outside the pH range.'),
             tableOutput(ns('pHRangeTable'))),
      column(6,
             wellPanel(
               h5('Station Exceedance Rate:'),
               uiOutput(ns('stationpHExceedanceRateSelect_UI')),
               tableOutput(ns("stationpHExceedanceRate"))),
             hr(),
             h5('Assessment Unit Exceedance Rate:'),
             tableOutput(ns("pHExceedanceRate"))))
    )
}


pHExceedanceAnalysis <- function(input, output, session, AUdata){
  ns <- session$ns
  
  # pH Raw Exceedance Results (all AU)
  output$pHRangeTable <- renderTable({
    req(AUdata)
    pH_rangeAssessment(AUdata())})
  
  # pHStation Exceedance Rate
  output$stationpHExceedanceRateSelect_UI <- renderUI({
    req(AUdata)
    selectInput(ns('stationpHExceedanceRateSelect'),strong('Select Station to Review for individual pH range statistics'),
                choices=unique(AUdata())$FDT_STA_ID,width='300px')})
  
  output$stationpHExceedanceRate <- renderTable({
    req(input$stationpHExceedanceRateSelect)
    z <- filter(AUdata(),FDT_STA_ID %in% input$stationpHExceedanceRateSelect)
    exceedance_pH(z) %>%
      dplyr::select(nSamples,nExceedance,exceedanceRate)}) # don't give assessment determination for single station})
  
  # pH AU Exceedance Rate
  output$pHExceedanceRate <- renderTable({
    req(AUdata)
    exceedance_pH(AUdata())})
  
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
  
  AUData <- reactive({filter(conventionals_HUC, ID305B_1 %in% 'VAW-H01R_JMS04A00' | 
                               ID305B_2 %in% 'VAW-H01R_JMS04A00' | 
                               ID305B_2 %in% 'VAW-H01R_JMS04A00')%>% 
      left_join(WQSvalues, by = 'CLASS')})
  
  callModule(pHPlotlySingleStation,'pH', AUData)
  
  callModule(pHExceedanceAnalysis,'pH_ExceedanceAnalysis', AUData)
  
  
}

shinyApp(ui,server)

