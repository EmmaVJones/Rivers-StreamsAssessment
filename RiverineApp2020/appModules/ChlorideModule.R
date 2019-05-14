ClPlotlySingleStationUI <- function(id){
  ns <- NS(id)
  tagList(
    wellPanel(
      h4(strong('Single Station Data Visualization')),
      uiOutput(ns('Cl_oneStationSelectionUI')),
      plotlyOutput(ns('Clplotly')),
      fluidRow(
        column(8, h5('All chloride records that are above the PWS criteria (where applicable) for the ',span(strong('selected site')),' are highlighted below.'),
               div(style = 'height:150px;overflow-y: scroll', tableOutput(ns('ChlorideRangeTableSingleSite')))),
        column(4, h5('Individual chloride exceedance statistics for the ',span(strong('selected site')),' are highlighted below.
                     If no data is presented, then the PWS criteria is not applicable to the station.'),
               tableOutput(ns("stationChlorideExceedanceRate"))))
      )
      )
}


ClPlotlySingleStation <- function(input,output,session, AUdata, stationSelectedAbove){
  ns <- session$ns
  
  # Select One station for individual review
  output$Cl_oneStationSelectionUI <- renderUI({
    req(stationSelectedAbove)
    selectInput(ns('Cl_oneStationSelection'),strong('Select Station to Review'),choices= sort(unique(c(stationSelectedAbove(),AUdata()$FDT_STA_ID))),#unique(AUdata())$FDT_STA_ID,
                width='300px', selected = stationSelectedAbove())})# "2-JMS279.41" )})
  
  
  Cl_oneStation <- reactive({
    req(ns(input$Cl_oneStationSelection))
    filter(AUdata(),FDT_STA_ID %in% input$Cl_oneStationSelection)})
  
  output$Clplotly <- renderPlotly({
    req(input$Cl_oneStationSelection, Cl_oneStation())
    dat <- Cl_oneStation()
    dat$SampleDate <- as.POSIXct(dat$FDT_DATE_TIME2, format="%m/%d/%y")
    
    maxheight <- ifelse(max(dat$CHLORIDE, na.rm=T) < 50, 55, max(dat$CHLORIDE, na.rm=T)* 1.2)
    box1 <- data.frame(SampleDate = c(min(dat$SampleDate), min(dat$SampleDate), max(dat$SampleDate),max(dat$SampleDate)), y = c(50, maxheight, maxheight, 50))
    box2 <- data.frame(x = c(min(dat$SampleDate), min(dat$SampleDate), max(dat$SampleDate),max(dat$SampleDate)), y = c(25, 50, 50, 25))
    box3 <- data.frame(x = c(min(dat$SampleDate), min(dat$SampleDate), max(dat$SampleDate),max(dat$SampleDate)), y = c(10, 25, 25, 10))
    box4 <- data.frame(x = c(min(dat$SampleDate), min(dat$SampleDate), max(dat$SampleDate),max(dat$SampleDate)), y = c(0, 10, 10, 0))
    
    
    if(grepl('PWS', unique(Cl_oneStation()$SPSTDS))){
      
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
        add_markers(data=dat, x= ~SampleDate, y= ~CHLORIDE,mode = 'scatter', name="Dissolved Chloride (mg/L)",marker = list(color= '#535559'),
                    hoverinfo="text",text=~paste(sep="<br>",
                                                 paste("Date: ",SampleDate),
                                                 paste("Depth: ",FDT_DEPTH, "m"),
                                                 paste("Dissolved Chloride: ",CHLORIDE,"mg/L")))%>%
        layout(showlegend=FALSE,
               yaxis=list(title="Dissolved Chloride (mg/L)"),
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
        add_markers(data=dat, x= ~SampleDate, y= ~CHLORIDE,mode = 'scatter', name="Dissolved Chloride (mg/L)",marker = list(color= '#535559'),
                    hoverinfo="text",text=~paste(sep="<br>",
                                                 paste("Date: ",SampleDate),
                                                 paste("Depth: ",FDT_DEPTH, "m"),
                                                 paste("Dissolved Chloride: ",CHLORIDE,"mg/L")))%>%
        layout(showlegend=FALSE,
               yaxis=list(title="Dissolved Chloride (mg/L)"),
               xaxis=list(title="Sample Date",tickfont = list(size = 10)))
    }
  })
  
  
  output$ChlorideRangeTableSingleSite <- renderTable({
    req(Cl_oneStation())
    if(grepl('PWS', unique(Cl_oneStation()$SPSTDS))){
      return(dplyr::select(x, FDT_DATE_TIME, FDT_DEPTH, CHLORIDE) %>%
               filter(!is.na(CHLORIDE)) %>% #get rid of NA's
               mutate(PWSlimit = 250) %>%
               mutate(exceeds = ifelse(CHLORIDE > PWSlimit, T, F)) %>% # Identify where above PWS limit
               filter(exceeds == TRUE))  
    }else {
      return('Station not designated PWS')
    }  })
  
  output$stationChlorideExceedanceRate <- renderTable({
    req(#input$Chloride_oneStationSelection, 
      Cl_oneStation())
    chloridePWS(Cl_oneStation()) %>%
      dplyr::select(1:3) %>%# don't give assessment determination for single station
      dplyr::rename(nSamples = PWS_Acute_Chloride_SAMP,nExceedance= PWS_Acute_Chloride_VIO,exceedanceRate= PWS_Acute_Chloride_exceedanceRate)}) # make it match everything else

  
}
