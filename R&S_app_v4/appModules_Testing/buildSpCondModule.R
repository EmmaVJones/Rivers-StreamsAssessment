source('testingDataset.R')

#AUData <- filter(conventionals_HUC, ID305B_1 %in% 'VAW-H01R_JMS04A00' | 
#                   ID305B_2 %in% 'VAW-H01R_JMS04A00' | 
#                   ID305B_2 %in% 'VAW-H01R_JMS04A00')%>% 
#  left_join(WQSvalues, by = 'CLASS')

x <-filter(conventionals_HUC, FDT_STA_ID %in% '4APKP-4-DRBA')# '4APKP-4-DRBA') '8-YRK022.70')#



SpCondPlotlySingleStationUI <- function(id){
  ns <- NS(id)
  tagList(
    wellPanel(
      h4(strong('Single Station Data Visualization')),
      fluidRow(column(6,uiOutput(ns('SpCond_oneStationSelectionUI'))),
               column(6,actionButton(ns('reviewData'),"Review Raw Parameter Data",class='btn-block', width = '250px'))),
      plotlyOutput(ns('SpCondplotly'))  )
  )
}


SpCondPlotlySingleStation <- function(input,output,session, AUdata, stationSelectedAbove){
  ns <- session$ns
  
  # Select One station for individual review
  output$SpCond_oneStationSelectionUI <- renderUI({
    req(AUdata)
    selectInput(ns('SpCond_oneStationSelection'),strong('Select Station to Review'),
                choices= sort(unique(c(stationSelectedAbove(),AUdata()$FDT_STA_ID))), # Change this based on stationSelectedAbove
                width='300px', selected = stationSelectedAbove())})
  
  SpCond_oneStation <- reactive({
    req(ns(input$SpCond_oneStationSelection))
    filter(AUdata(),FDT_STA_ID %in% input$SpCond_oneStationSelection)  %>%
      filter(!is.na(FDT_SPECIFIC_CONDUCTANCE))})
  
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
    req(SpCond_oneStation())
    parameterFilter <- dplyr::select(SpCond_oneStation(), FDT_STA_ID:FDT_COMMENT, FDT_SPECIFIC_CONDUCTANCE, FDT_SPECIFIC_CONDUCTANCE_RMK)
    
    DT::datatable(parameterFilter, rownames = FALSE, 
                  options= list(dom= 't', pageLength = nrow(parameterFilter), scrollX = TRUE, scrollY = "400px", dom='t')) %>%
      formatStyle(c('FDT_SPECIFIC_CONDUCTANCE','FDT_SPECIFIC_CONDUCTANCE_RMK'), 'FDT_SPECIFIC_CONDUCTANCE_RMK', 
                  backgroundColor = styleEqual(c('Level II', 'Level I'), c('yellow','orange'), default = 'lightgray'))
  })
  
  
  output$SpCondplotly <- renderPlotly({
    req(input$SpCond_oneStationSelection, SpCond_oneStation())
    dat <- SpCond_oneStation()
    dat$SampleDate <- as.POSIXct(dat$FDT_DATE_TIME2, format="%m/%d/%y")
    maxheight <- ifelse(max(dat$FDT_SPECIFIC_CONDUCTANCE, na.rm=T) < 500, 600, max(dat$FDT_SPECIFIC_CONDUCTANCE, na.rm=T)* 1.2)
    box1 <- data.frame(SampleDate = c(min(dat$SampleDate), min(dat$SampleDate), max(dat$SampleDate),max(dat$SampleDate)), y = c(500, maxheight, maxheight, 500))
    box2 <- data.frame(x = c(min(dat$SampleDate), min(dat$SampleDate), max(dat$SampleDate),max(dat$SampleDate)), y = c(350, 500, 500, 350))
    box3 <- data.frame(x = c(min(dat$SampleDate), min(dat$SampleDate), max(dat$SampleDate),max(dat$SampleDate)), y = c(250, 350, 350, 250))
    box4 <- data.frame(x = c(min(dat$SampleDate), min(dat$SampleDate), max(dat$SampleDate),max(dat$SampleDate)), y = c(0, 250, 250, 0))
    
    
    plot_ly(data=dat)%>%
      add_polygons(x = ~SampleDate, y = ~y, data = box1, fillcolor = "firebrick",opacity=0.6, line = list(width = 0),
                   hoverinfo="text", name =paste('High Probability of Stress to Aquatic Life')) %>%
      add_polygons(data = box2, x = ~x, y = ~y, fillcolor = "#F0E442",opacity=0.6, line = list(width = 0),
                   hoverinfo="text", name =paste('Medium Probability of Stress to Aquatic Life')) %>%
      add_polygons(data = box3, x = ~x, y = ~y, fillcolor = "#009E73",opacity=0.6, line = list(width = 0),
                   hoverinfo="text", name =paste('Low Probability of Stress to Aquatic Life')) %>%
      add_polygons(data = box4, x = ~x, y = ~y, fillcolor = "#0072B2",opacity=0.6, line = list(width = 0),
                   hoverinfo="text", name =paste('No Probability of Stress to Aquatic Life')) %>%
      add_markers(data=dat, x= ~SampleDate, y= ~FDT_SPECIFIC_CONDUCTANCE,mode = 'scatter', name="Specific Conductivity (uS/cm)",marker = list(color= '#535559'),
                  hoverinfo="text",text=~paste(sep="<br>",
                                               paste("Date: ",SampleDate),
                                               paste("Depth: ",FDT_DEPTH, "m"),
                                               paste("Specific Conductivity: ",FDT_SPECIFIC_CONDUCTANCE,"uS/cm")))%>%
      layout(showlegend=FALSE,
             yaxis=list(title="Specific Conductivity (uS/cm)"),
             xaxis=list(title="Sample Date",tickfont = list(size = 10)))
  })
  
}




ui <- fluidPage(
  helpText('Review each site using the single site visualization section, then 
           proceed to the bottom of the page to find exceedance rate for the entire assessment unit.',br(),
           span(strong('NOTE: The pH exceedance analysis results at the bottom of the page include data
                       from ALL stations within the assessment unit.'))),
  SpCondPlotlySingleStationUI('SpCond')    )

server <- function(input,output,session){
  
  stationData <- eventReactive( input$stationSelection, {
    filter(AUData, FDT_STA_ID %in% input$stationSelection) })
  stationSelected <- reactive({input$stationSelection})
  
  
  AUData <- reactive({filter(conventionals_HUC, FDT_STA_ID %in% c('2-JKS018.68','4APKP-4-DRBA' ))})
  callModule(SpCondPlotlySingleStation,'SpCond', AUData, stationSelected)
  
  }

shinyApp(ui,server)

