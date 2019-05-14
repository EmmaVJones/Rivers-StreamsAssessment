source('testingDataset.R')

AUData <- filter(conventionals_HUC, ID305B_1 %in% 'VAW-H01R_JMS04A00' | 
                   ID305B_2 %in% 'VAW-H01R_JMS04A00' | 
                   ID305B_2 %in% 'VAW-H01R_JMS04A00')%>% 
  left_join(WQSvalues, by = 'CLASS')

x <-filter(AUData, FDT_STA_ID %in% '2-JMS279.41') 

# No Assessment functions bc no std


TNPlotlySingleStationUI <- function(id){
  ns <- NS(id)
  tagList(
    wellPanel(
      h4(strong('Single Station Data Visualization')),
      uiOutput(ns('TN_oneStationSelectionUI')),
      plotlyOutput(ns('TNplotly'))  )
  )
}


TNPlotlySingleStation <- function(input,output,session, AUdata, stationSelectedAbove){
  ns <- session$ns
  
  # Select One station for individual review
  output$TN_oneStationSelectionUI <- renderUI({
    req(#AUdata, 
      stationSelectedAbove)
    #print(unique(c(stationSelectedAbove(),AUdata()$FDT_STA_ID)))
    selectInput(ns('TN_oneStationSelection'),strong('Select Station to Review'),choices= sort(unique(c(stationSelectedAbove(),AUdata()$FDT_STA_ID))),#unique(AUdata())$FDT_STA_ID,
                width='300px', selected = stationSelectedAbove())})# "2-JMS279.41" )})
  
  TN_oneStation <- reactive({
    req(ns(input$TN_oneStationSelection))
    filter(AUdata(),FDT_STA_ID %in% input$TN_oneStationSelection)})
  
  output$TNplotly <- renderPlotly({
    req(input$TN_oneStationSelection, TN_oneStation())
    dat <- TN_oneStation()
    dat$SampleDate <- as.POSIXct(dat$FDT_DATE_TIME2, format="%m/%d/%y")
    plot_ly(data=dat)%>%
      add_markers(x= ~SampleDate, y= ~NITROGEN,mode = 'scatter', name="Total Nitrogen (mg/L)",
                  hoverinfo="text",text=~paste(sep="<br>",
                                               paste("Date: ",SampleDate),
                                               paste("Depth: ",FDT_DEPTH, "m"),
                                               paste("Total Nitrogen: ",NITROGEN,"mg/L")))%>%
      layout(showlegend=FALSE,
             yaxis=list(title="Total Nitrogen (mg/L)"),
             xaxis=list(title="Sample Date",tickfont = list(size = 10))) })
}


ui <- fluidPage(
  selectInput('stationSelection', 'Station Selection', choices = unique(AUData$FDT_STA_ID)),
  helpText('Review each site using the single site visualization section. There are no WQS for Total Nitrogen.'),
  TNPlotlySingleStationUI('TN')
)

server <- function(input,output,session){
  stationData <- eventReactive( input$stationSelection, {
    filter(AUData, FDT_STA_ID %in% input$stationSelection) })
  stationSelected <- reactive({input$stationSelection})
  
  AUData <- reactive({filter(conventionals_HUC, ID305B_1 %in% 'VAW-H01R_JMS04A00' | 
                               ID305B_2 %in% 'VAW-H01R_JMS04A00' | 
                               ID305B_2 %in% 'VAW-H01R_JMS04A00')%>% 
      left_join(WQSvalues, by = 'CLASS')})
  
  callModule(TNPlotlySingleStation,'TN', AUData, stationSelected)#input$stationSelection)
  
}

shinyApp(ui,server)

