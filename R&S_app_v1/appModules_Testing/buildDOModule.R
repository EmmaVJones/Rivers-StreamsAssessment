source('testingDataset.R')

AUData <- filter(conventionals_HUC, ID305B_1 %in% 'VAW-H01R_JMS04A00' | 
                   ID305B_2 %in% 'VAW-H01R_JMS04A00' | 
                   ID305B_2 %in% 'VAW-H01R_JMS04A00')%>% 
  left_join(WQSvalues, by = 'CLASS')

x <-filter(AUData, FDT_STA_ID %in% '2-JMS279.41') 
# Testing dataset for daily averages
xtest <-x
xtest$FDT_DATE_TIME2[2] <- '2011-01-31 12:30:00'
xtest$FDT_DATE_TIME2[3] <- '2011-01-31 13:30:00'
xtest$FDT_DATE_TIME2[4] <- '2011-01-31 14:30:00'
xtest$FDT_DATE_TIME2[6] <- '2011-04-26 14:30:00'
xtest$FDT_DATE_TIME2[7] <- '2011-04-26 15:30:00'
xtest$FDT_DATE_TIME2[8] <- '2011-04-26 16:30:00'



#Min DO Exceedance Function ###############

DO_Assessment_Min <- function(x){ 
  dplyr::select(x,FDT_STA_ID,FDT_DATE_TIME, FDT_DATE_TIME2,FDT_DEPTH,DO,`Dissolved Oxygen Min (mg/L)`,`Dissolved Oxygen Daily Avg (mg/L)`)%>% # Just get relevant columns, 
    filter(!is.na(DO)) %>% 
    mutate(DOExceedanceMin=ifelse(DO < `Dissolved Oxygen Min (mg/L)`,T,F))%>% # Identify where above max Temperature, 9VAC25-260-50 ClassIII= 32C
    filter(DOExceedanceMin==TRUE) %>% # Only return DO measures below threshold
    dplyr::select(-c(DOExceedanceMin,FDT_DATE_TIME2,`Dissolved Oxygen Daily Avg (mg/L)`)) # Don't show user column, could be confusing to them
}
#DO_Assessment_Min(x)

# Daily Average exceedance function
DO_Assessment_DailyAvg <- function(x){ 
  dplyr::select(x,FDT_STA_ID,FDT_DATE_TIME, FDT_DATE_TIME2,FDT_DEPTH,DO,`Dissolved Oxygen Min (mg/L)`,`Dissolved Oxygen Daily Avg (mg/L)`)%>% # Just get relevant columns, 
    filter(!is.na(DO)) %>% #get rid of NA's
    mutate(date = as.Date(FDT_DATE_TIME2, format="%m/%d/%Y")) %>% 
    group_by(date) %>%
    mutate(n_Samples_Daily = n()) %>% # how many samples per day?
    filter(n_Samples_Daily > 1) %>%
    mutate(DO_DailyAverage = mean(DO), DOExceedanceDailyAvg=ifelse(DO_DailyAverage < `Dissolved Oxygen Daily Avg (mg/L)`,T,F)) %>% # Identify where above max Temperature, 9VAC25-260-50 ClassIII= 32C
    ungroup() %>% 
    filter( DOExceedanceDailyAvg==TRUE) %>% # Only return DO measures below threshold
    dplyr::select(-c(FDT_DATE_TIME2, `Dissolved Oxygen Min (mg/L)`,date))
}

#DO_Assessment_DailyAvg(xtest)

# Exceedance Rate DO, for all samples
exceedance_DO <- function(x){
  DO <- dplyr::select(x,FDT_STA_ID,FDT_DATE_TIME, FDT_DATE_TIME2,FDT_DEPTH,DO,`Dissolved Oxygen Min (mg/L)`,`Dissolved Oxygen Daily Avg (mg/L)`)%>% # Just get relevant columns, 
    filter(!is.na(DO)) #get rid of NA's
  DO_Assess <- DO_Assessment_Min(x)
  DO_results <- assessmentDetermination(DO,DO_Assess,"Dissolved Oxygen","Aquatic Life")
  return(DO_results)
}

# Exceedance Rate DO, for daily average samples
exceedance_DO_DailyAvg <- function(x){
  DO <- dplyr::select(x,FDT_STA_ID,FDT_DATE_TIME, FDT_DATE_TIME2,FDT_DEPTH,DO,`Dissolved Oxygen Min (mg/L)`,`Dissolved Oxygen Daily Avg (mg/L)`)%>% # Just get relevant columns, 
    filter(!is.na(DO)) %>% #get rid of NA's
    mutate(date = as.Date(FDT_DATE_TIME2, format="%m/%d/%Y")) %>% 
    group_by(date) %>%
    mutate(n_Samples_Daily = n()) %>% # how many samples per day?
    filter(n_Samples_Daily > 1)  # only keep days with > 1 sample 
  DO_Assess <- suppressMessages(DO_Assessment_DailyAvg(x))
  DO_results <- assessmentDetermination(DO %>% distinct(date),DO_Assess,"Dissolved Oxygen Daily Average","Aquatic Life")
  return(DO_results)
}

#exceedance_DO_DailyAvg(xtest)






DOPlotlySingleStationUI <- function(id){
  ns <- NS(id)
  tagList(
    wellPanel(
      h4(strong('Single Station Data Visualization')),
      uiOutput(ns('DO_oneStationSelectionUI')),
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

DOPlotlySingleStation <- function(input,output,session, AUdata){
  ns <- session$ns
  
  # Select One station for individual review
  output$DO_oneStationSelectionUI <- renderUI({
    req(AUdata)
    selectInput(ns('DO_oneStationSelection'),strong('Select Station to Review'),choices=unique(AUdata())$FDT_STA_ID,width='300px')})
  
  DO_oneStation <- reactive({
    req(ns(input$DO_oneStationSelection))
    filter(AUdata(),FDT_STA_ID %in% input$DO_oneStationSelection)})
  
  output$DOplotly <- renderPlotly({
    req(input$DO_oneStationSelection, DO_oneStation())
    dat <- mutate(DO_oneStation(), bottom = `Dissolved Oxygen Min (mg/L)`)
    dat$SampleDate <- as.POSIXct(dat$FDT_DATE_TIME2, format="%m/%d/%y")
    
    box1 <- data.frame(x = c(min(dat$SampleDate), min(dat$SampleDate), max(dat$SampleDate),max(dat$SampleDate)), y = c(10, 18, 18, 10))
    box2 <- data.frame(x = c(min(dat$SampleDate), min(dat$SampleDate), max(dat$SampleDate),max(dat$SampleDate)), y = c(8, 10, 10, 8))
    box3 <- data.frame(x = c(min(dat$SampleDate), min(dat$SampleDate), max(dat$SampleDate),max(dat$SampleDate)), y = c(7, 8, 8, 7))
    box4 <- data.frame(x = c(min(dat$SampleDate), min(dat$SampleDate), max(dat$SampleDate),max(dat$SampleDate)), y = c(0, 7, 7, 0))
    
    plot_ly(data=dat)%>%
      add_polygons(data = box1, x = ~x, y = ~y, fillcolor = "#0072B2",opacity=0.6, line = list(width = 0),
                   hoverinfo="text", name =paste('no probability of stress to aquatic life')) %>%
      add_polygons(data = box2, x = ~x, y = ~y, fillcolor = "#009E73",opacity=0.6, line = list(width = 0),
                   hoverinfo="text", name =paste('low probability of stress to aquatic life')) %>%
      add_polygons(data = box3, x = ~x, y = ~y, fillcolor = "#F0E442",opacity=0.6, line = list(width = 0),
                   hoverinfo="text", name =paste('medium probability of stress to aquatic life')) %>%
      add_polygons(data = box4, x = ~x, y = ~y, fillcolor = "firebrick",opacity=0.6, line = list(width = 0),
                   hoverinfo="text", name =paste('high probability of stress to aquatic life')) %>%
      add_lines(data = dat, x=~SampleDate,y=~bottom, mode='line', line = list(color = 'black'),
                hoverinfo = "none", name="DO Standard") %>%
      add_markers(data = dat, x= ~SampleDate, y= ~DO,mode = 'scatter', name="DO (mg/L)", marker = list(color= '#535559'),
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
  DOExceedanceAnalysisUI('DO_ExceedanceAnalysis')     
)

server <- function(input,output,session){
  
  AUData <- reactive({filter(conventionals_HUC, ID305B_1 %in% 'VAW-H01R_JMS04A00' | 
                               ID305B_2 %in% 'VAW-H01R_JMS04A00' | 
                               ID305B_2 %in% 'VAW-H01R_JMS04A00')%>% 
      left_join(WQSvalues, by = 'CLASS')})
  
  callModule(DOPlotlySingleStation,'DO', AUData)
  
  callModule(DOExceedanceAnalysis,'DO_ExceedanceAnalysis', AUData)
}

shinyApp(ui,server)

