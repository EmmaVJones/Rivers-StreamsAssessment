source('testingDataset.R')

#AUData <- filter(conventionals_HUC, ID305B_1 %in% 'VAW-H01R_JMS04A00' | 
#                   ID305B_2 %in% 'VAW-H01R_JMS04A00' | 
#                   ID305B_2 %in% 'VAW-H01R_JMS04A00')%>% 
#  left_join(WQSvalues, by = 'CLASS')

x <-filter(conventionals_HUC, FDT_STA_ID %in% '4APKP-4-DRBA')# '4APKP-4-DRBA') '8-YRK022.70')#



NitratePlotlySingleStationUI <- function(id){
  ns <- NS(id)
  tagList(
    wellPanel(
      h4(strong('Single Station Data Visualization')),
      fluidRow(column(6,uiOutput(ns('Nitrate_oneStationSelectionUI'))),
               column(6,actionButton(ns('reviewData'),"Review Raw Parameter Data",class='btn-block', width = '250px'))),
      plotlyOutput(ns('Nitrateplotly')),
      fluidRow(
        column(8, h5('All nitrate records that are above the PWS criteria (where applicable) for the ',span(strong('selected site')),' are highlighted below.'),
               div(style = 'height:150px;overflow-y: scroll', tableOutput(ns('NitrateRangeTableSingleSite')))),
        column(4, h5('Individual nitrate exceedance statistics for the ',span(strong('selected site')),' are highlighted below.
                     If no data is presented, then the PWS criteria is not applicable to the station.'),
               tableOutput(ns("stationNitrateExceedanceRate"))))
    )
  )
}


NitratePlotlySingleStation <- function(input,output,session, AUdata, stationSelectedAbove){
  ns <- session$ns
  
  # Select One station for individual review
  output$Nitrate_oneStationSelectionUI <- renderUI({
    req(stationSelectedAbove)
    selectInput(ns('Nitrate_oneStationSelection'),strong('Select Station to Review'),choices= sort(unique(c(stationSelectedAbove(),AUdata()$FDT_STA_ID))),#unique(AUdata())$FDT_STA_ID,
                width='300px', selected = stationSelectedAbove())})# "2-JMS279.41" )})
  
  
  Nitrate_oneStation <- reactive({
    req(ns(input$Nitrate_oneStationSelection))
    filter(AUdata(),FDT_STA_ID %in% input$Nitrate_oneStationSelection) %>%
      filter(!is.na(NITRATE))})
  
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
    req(Nitrate_oneStation())
    parameterFilter <- dplyr::select(Nitrate_oneStation(), FDT_STA_ID:FDT_COMMENT, NITRATE, RMK_NITRATE)
    
    DT::datatable(parameterFilter, rownames = FALSE, 
                  options= list(dom= 't', pageLength = nrow(parameterFilter), scrollX = TRUE, scrollY = "400px", dom='t')) %>%
      formatStyle(c('NITRATE','RMK_NITRATE'), 'RMK_NITRATE', backgroundColor = styleEqual(c('Level II', 'Level I'), c('yellow','orange'), default = 'lightgray'))
  })
  
  output$Nitrateplotly <- renderPlotly({
    req(input$Nitrate_oneStationSelection, Nitrate_oneStation())
    dat <- Nitrate_oneStation()
    dat$SampleDate <- as.POSIXct(dat$FDT_DATE_TIME2, format="%m/%d/%y")
    
    maxheight <- ifelse(max(dat$NITRATE, na.rm=T) < 50, 55, max(dat$NITRATE, na.rm=T)* 1.2)
    box1 <- data.frame(SampleDate = c(min(dat$SampleDate), min(dat$SampleDate), max(dat$SampleDate),max(dat$SampleDate)), y = c(50, maxheight, maxheight, 50))
    box2 <- data.frame(x = c(min(dat$SampleDate), min(dat$SampleDate), max(dat$SampleDate),max(dat$SampleDate)), y = c(25, 50, 50, 25))
    box3 <- data.frame(x = c(min(dat$SampleDate), min(dat$SampleDate), max(dat$SampleDate),max(dat$SampleDate)), y = c(10, 25, 25, 10))
    box4 <- data.frame(x = c(min(dat$SampleDate), min(dat$SampleDate), max(dat$SampleDate),max(dat$SampleDate)), y = c(0, 10, 10, 0))
    
    
    if(grepl('PWS', unique(Nitrate_oneStation()$SPSTDS))){
      
      dat <- mutate(dat, PWS = 250)
      
      box1 <- data.frame(SampleDate = c(min(dat$SampleDate), min(dat$SampleDate), max(dat$SampleDate),max(dat$SampleDate)), y = c(50, 260, 260, 50))
      
      
      plot_ly(data=dat)%>%
        add_polygons(x = ~SampleDate, y = ~y, data = box1, fillcolor = "firebrick",opacity=0.6, line = list(width = 0),
                     hoverinfo="text", name =paste('High Probability of Stress to Aquatic Life')) %>%
        add_polygons(data = box2, x = ~x, y = ~y, fillcolor = "#F0E442",opacity=0.6, line = list(width = 0),
                     hoverinfo="text", name =paste('Medium Probability of Stress to Aquatic Life')) %>%
        add_polygons(data = box3, x = ~x, y = ~y, fillcolor = "#009E73",opacity=0.6, line = list(width = 0),
                     hoverinfo="text", name =paste('Low Probability of Stress to Aquatic Life')) %>%
        add_polygons(data = box4, x = ~x, y = ~y, fillcolor = "#0072B2",opacity=0.6, line = list(width = 0),
                     hoverinfo="text", name =paste('No Probability of Stress to Aquatic Life')) %>%
        add_lines(data=dat, x=~SampleDate,y=~PWS, mode='line', line = list(color = 'black'),
                  hoverinfo = "text", text= "PWS Criteria (250 mg/L)", name="PWS Criteria (250 mg/L)") %>%
        add_markers(data=dat, x= ~SampleDate, y= ~NITRATE,mode = 'scatter', name="Dissolved Nitrate (mg/L)",marker = list(color= '#535559'),
                    hoverinfo="text",text=~paste(sep="<br>",
                                                 paste("Date: ",SampleDate),
                                                 paste("Depth: ",FDT_DEPTH, "m"),
                                                 paste("Dissolved Nitrate: ",NITRATE,"mg/L")))%>%
        layout(showlegend=FALSE,
               yaxis=list(title="Dissolved Nitrate (mg/L)"),
               xaxis=list(title="Sample Date",tickfont = list(size = 10)))
    } else {
      plot_ly(data=dat)%>%
        add_polygons(x = ~SampleDate, y = ~y, data = box1, fillcolor = "firebrick",opacity=0.6, line = list(width = 0),
                     hoverinfo="text", name =paste('High Probability of Stress to Aquatic Life')) %>%
        add_polygons(data = box2, x = ~x, y = ~y, fillcolor = "#F0E442",opacity=0.6, line = list(width = 0),
                     hoverinfo="text", name =paste('Medium Probability of Stress to Aquatic Life')) %>%
        add_polygons(data = box3, x = ~x, y = ~y, fillcolor = "#009E73",opacity=0.6, line = list(width = 0),
                     hoverinfo="text", name =paste('Low Probability of Stress to Aquatic Life')) %>%
        add_polygons(data = box4, x = ~x, y = ~y, fillcolor = "#0072B2",opacity=0.6, line = list(width = 0),
                     hoverinfo="text", name =paste('No Probability of Stress to Aquatic Life')) %>%
        add_markers(data=dat, x= ~SampleDate, y= ~NITRATE,mode = 'scatter', name="Dissolved Nitrate (mg/L)",marker = list(color= '#535559'),
                    hoverinfo="text",text=~paste(sep="<br>",
                                                 paste("Date: ",SampleDate),
                                                 paste("Depth: ",FDT_DEPTH, "m"),
                                                 paste("Dissolved Nitrate: ",NITRATE,"mg/L")))%>%
        layout(showlegend=FALSE,
               yaxis=list(title="Dissolved Nitrate (mg/L)"),
               xaxis=list(title="Sample Date",tickfont = list(size = 10)))
    }
  })
  
  
  output$NitrateRangeTableSingleSite <- renderTable({
    req(Nitrate_oneStation())
    if(grepl('PWS', unique(Nitrate_oneStation()$SPSTDS))){
      return(dplyr::select(citmonOutOfBacteria(Nitrate_oneStation(), NITRATE, RMK_NITRATE), FDT_DATE_TIME, FDT_DEPTH, NITRATE) %>%
               filter(!is.na(NITRATE)) %>% #get rid of NA's
               mutate(PWSlimit = 250) %>%
               mutate(exceeds = ifelse(NITRATE > PWSlimit, T, F)) %>% # Identify where above PWS limit
               filter(exceeds == TRUE))  
    }else {
      return('Station not designated PWS')
    }  })
  
  output$stationNitrateExceedanceRate <- renderTable({
    req(input$Nitrate_oneStationSelection, Nitrate_oneStation())
    if(grepl('PWS', unique(Nitrate_oneStation()$SPSTDS))){
      x <- nitratePWS(citmonOutOfBacteria(Nitrate_oneStation(), NITRATE, RMK_NITRATE))
      if(nrow(x) >0) {
        return(dplyr::select(x,1:3) %>%# don't give assessment determination for single station
                dplyr::rename(nSamples = PWS_Acute_Nitrate_SAMP,nExceedance= PWS_Acute_Nitrate_VIO,exceedanceRate= PWS_Acute_Nitrate_exceedanceRate) ) } # make it match everything else
    } else {
      return('Station not designated PWS')
      }  }) 
      
  
  
}



ui <- fluidPage(
  helpText('Review each site using the single site visualization section. There are no WQS for Specific Conductivity.'),
  NitratePlotlySingleStationUI('Nitrate')
)

server <- function(input,output,session){
  stationData <- eventReactive( input$stationSelection, {
    filter(AUData, FDT_STA_ID %in% input$stationSelection) })
  stationSelected <- reactive({input$stationSelection})
  
  
  AUData <- reactive({filter(conventionals_HUC, FDT_STA_ID %in% c('2-JKS023.61','4APKP-4-DRBA' ))})
  
  callModule(NitratePlotlySingleStation,'Nitrate', AUData, stationSelected)
  
}

shinyApp(ui,server)

