source('testingDataset.R')

AUData <- filter(conventionals_HUC, ID305B_1 %in% 'VAW-H01R_JMS04A00' | 
                   ID305B_2 %in% 'VAW-H01R_JMS04A00' | 
                   ID305B_2 %in% 'VAW-H01R_JMS04A00')%>% 
  left_join(WQSvalues, by = 'CLASS')

x <-filter(AUData, FDT_STA_ID %in% '2-JMS279.41') 

# No Assessment functions bc no std


salinityPlotlySingleStationUI <- function(id){
  ns <- NS(id)
  tagList(
    wellPanel(
      h4(strong('Single Station Data Visualization')),
      uiOutput(ns('salinity_oneStationSelectionUI')),
      plotlyOutput(ns('salinityplotly'))  )
  )
}


salinityPlotlySingleStation <- function(input,output,session, AUdata, stationSelectedAbove){
  ns <- session$ns
  
  # Select One station for individual review
  output$salinity_oneStationSelectionUI <- renderUI({
    req(#AUdata, 
      stationSelectedAbove)
    #print(unique(c(stationSelectedAbove(),AUdata()$FDT_STA_ID)))
    selectInput(ns('salinity_oneStationSelection'),strong('Select Station to Review'),choices= sort(unique(c(stationSelectedAbove(),AUdata()$FDT_STA_ID))),#unique(AUdata())$FDT_STA_ID,
                width='300px', selected = stationSelectedAbove())})# "2-JMS279.41" )})
  
  salinity_oneStation <- reactive({
    req(ns(input$salinity_oneStationSelection))
    filter(AUdata(),FDT_STA_ID %in% input$salinity_oneStationSelection)})
  
  output$salinityplotly <- renderPlotly({
    req(input$salinity_oneStationSelection, salinity_oneStation())
    dat <- salinity_oneStation()
    dat$SampleDate <- as.POSIXct(dat$FDT_DATE_TIME2, format="%m/%d/%y")
    plot_ly(data=dat)%>%
      add_markers(x= ~SampleDate, y= ~FDT_SALINITY,mode = 'scatter', name="Salinity (ppt)",
                  hoverinfo="text",text=~paste(sep="<br>",
                                               paste("Date: ",SampleDate),
                                               paste("Depth: ",FDT_DEPTH, "m"),
                                               paste("Salinity: ",format(FDT_SALINITY,digits=2),"ppt")))%>%
      layout(showlegend=FALSE,
             yaxis=list(title="Salinity (ppt)"),
             xaxis=list(title="Sample Date",tickfont = list(size = 10))) })
}


ui <- fluidPage(
  selectInput('stationSelection', 'Station Selection', choices = unique(AUData$FDT_STA_ID)),
  helpText('Review each site using the single site visualization section. There are no WQS for Salinity.'),
  salinityPlotlySingleStationUI('salinity')
)

server <- function(input,output,session){
  stationData <- eventReactive( input$stationSelection, {
    filter(AUData, FDT_STA_ID %in% input$stationSelection) })
  stationSelected <- reactive({input$stationSelection})
  
  AUData <- reactive({filter(conventionals_HUC, ID305B_1 %in% 'VAW-H01R_JMS04A00' | 
                               ID305B_2 %in% 'VAW-H01R_JMS04A00' | 
                               ID305B_2 %in% 'VAW-H01R_JMS04A00')%>% 
      left_join(WQSvalues, by = 'CLASS')})
  
  callModule(salinityPlotlySingleStation,'salinity', AUData, stationSelected)#input$stationSelection)
  
}

shinyApp(ui,server)

