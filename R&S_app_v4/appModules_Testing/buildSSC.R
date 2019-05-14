source('testingDataset.R')

AUData <- filter(conventionals_HUC, ID305B_1 %in% 'VAW-H01R_JMS04A00' | 
                   ID305B_2 %in% 'VAW-H01R_JMS04A00' | 
                   ID305B_2 %in% 'VAW-H01R_JMS04A00')%>% 
  left_join(WQSvalues, by = 'CLASS')

x <-filter(AUData, FDT_STA_ID %in% '2-JMS279.41') 

# No Assessment functions bc no std


SSCPlotlySingleStationUI <- function(id){
  ns <- NS(id)
  tagList(
    wellPanel(
      h4(strong('Single Station Data Visualization')),
      uiOutput(ns('SSC_oneStationSelectionUI')),
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
    filter(AUdata(),FDT_STA_ID %in% input$SSC_oneStationSelection)})
  
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
  helpText('Review each site using the single site visualization section. There are no WQS for Specific Conductivity.'),
  SSCPlotlySingleStationUI('SSC')
)

server <- function(input,output,session){
  stationData <- eventReactive( input$stationSelection, {
    filter(AUData, FDT_STA_ID %in% input$stationSelection) })
  stationSelected <- reactive({input$stationSelection})
  
  AUData <- reactive({filter(conventionals_HUC, ID305B_1 %in% 'VAW-H01R_JMS04A00' | 
                               ID305B_2 %in% 'VAW-H01R_JMS04A00' | 
                               ID305B_2 %in% 'VAW-H01R_JMS04A00')%>% 
      left_join(WQSvalues, by = 'CLASS')})
  
  callModule(SSCPlotlySingleStation,'SSC', AUData, stationSelected)
  
}

shinyApp(ui,server)

