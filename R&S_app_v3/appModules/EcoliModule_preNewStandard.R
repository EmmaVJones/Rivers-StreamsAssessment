EcoliPlotlySingleStationUI <- function(id){
  ns <- NS(id)
  tagList(
    wellPanel(
      h4(strong('Single Station Data Visualization')),
      uiOutput(ns('Ecoli_oneStationSelectionUI')),
      plotlyOutput(ns('Ecoliplotly')),
      br(),hr(),br(),
      fluidRow(
        column(6, h5('All E. coli records that are above the criteria for the ',span(strong('selected site')),' are highlighted below.'),
               div(style = 'height:250px;overflow-y: scroll', 
                   h6('Old Standard (STV= 235 CFU / 100 mL, geomean = 126 CFU / 100 mL)'),
                   tableOutput(ns('EcoliexceedancesOldStdTableSingleSitegeomean')),
                   tableOutput(ns('EcoliexceedancesOldStdTableSingleSiteSTV'))), br(), hr(), br(),
               div(style = 'height:250px;overflow-y: scroll', 
                   h6('New Standard (STV= 410 CFU / 100 mL, geomean = 126 CFU / 100 mL with additional sampling requirements)'),
                   tableOutput(ns('EcoliexceedancesNEWStdTableSingleSite')))),
        column(6, h5('Individual E. coli exceedance statistics for the ',span(strong('selected site')),' are highlighted below.'),
               div(style = 'height:250px;overflow-y: scroll', 
                   h6('Old Standard (STV= 235 CFU / 100 mL, geomean = 126 CFU / 100 mL)'), tableOutput(ns("EcoliOldStdTableSingleSite"))),
               div(style = 'height:250px;overflow-y: scroll', 
                   h6('New Standard (STV= 410 CFU / 100 mL, geomean = 126 CFU / 100 mL with additional sampling requirements)'), tableOutput(ns("EcoliNEWStdTableSingleSite"))))))
  )
}


EcoliPlotlySingleStation <- function(input,output,session, AUdata, stationSelectedAbove){
  ns <- session$ns
  
  # Select One station for individual review
  output$Ecoli_oneStationSelectionUI <- renderUI({
    req(stationSelectedAbove)
    selectInput(ns('Ecoli_oneStationSelection'),strong('Select Station to Review'),choices= sort(unique(c(stationSelectedAbove(),AUdata()$FDT_STA_ID))),#unique(AUdata())$FDT_STA_ID,
                width='300px', selected = stationSelectedAbove())})
  
  Ecoli_oneStation <- reactive({
    req(ns(input$Ecoli_oneStationSelection))
    filter(AUdata(),FDT_STA_ID %in% input$Ecoli_oneStationSelection)})
  
  output$Ecoliplotly <- renderPlotly({
    req(input$Ecoli_oneStationSelection, Ecoli_oneStation())
    dat <- Ecoli_oneStation() %>%
      mutate(newSTV = 410, geomean = 126, oldSTV = 235)
    dat$SampleDate <- as.POSIXct(dat$FDT_DATE_TIME2, format="%m/%d/%y")
    plot_ly(data=dat) %>%
      add_markers(x= ~SampleDate, y= ~E.COLI,mode = 'scatter', name="E. coli (CFU / 100 mL)", marker = list(color= '#535559'),
                  hoverinfo="text",text=~paste(sep="<br>",
                                               paste("Date: ",SampleDate),
                                               paste("Depth: ",FDT_DEPTH, "m"),
                                               paste("E. coli: ",E.COLI,"CFU / 100 mL")))%>%
      add_lines(data=dat, x=~SampleDate,y=~newSTV, mode='line', line = list(color = '#484a4c',dash = 'dot'),
                hoverinfo = "text", text= "New STV: 410 CFU / 100 mL", name="New STV: 410 CFU / 100 mL") %>%
      add_lines(data=dat, x=~SampleDate,y=~oldSTV, mode='line', line = list(color = 'black'),
                hoverinfo = "text", text= "Old STV: 235 CFU / 100 mL", name="Old STV: 235 CFU / 100 mL") %>%
      add_lines(data=dat, x=~SampleDate,y=~geomean, mode='line', line = list(color = 'black', dash= 'dash'),
                hoverinfo = "text", text= "Geomean: 126 CFU / 100 mL", name="Geomean: 126 CFU / 100 mL") %>%
      layout(showlegend=FALSE,
             yaxis=list(title="E. coli (CFU / 100 mL)"),
             xaxis=list(title="Sample Date",tickfont = list(size = 10))) 
  })
  
  output$EcoliexceedancesOldStdTableSingleSitegeomean <- renderTable({
    req(Ecoli_oneStation())
    bacteria_ExceedancesGeomeanOLD(
      Ecoli_oneStation() %>% 
        dplyr::select(FDT_DATE_TIME2,E.COLI)%>% # Just get relavent columns, 
        filter(!is.na(E.COLI)) #get rid of NA's
    ) %>%
      dplyr::select(FDT_DATE_TIME2, E.COLI, sampleMonthYear, geoMeanCalendarMonth, limit, samplesPerMonth) %>%
      rename(FDT_DATE_TIME = FDT_DATE_TIME2) %>%# for user view consistency, same data, just different format for R purposes
      filter(samplesPerMonth > 4, geoMeanCalendarMonth > limit) # minimum sampling rule for geomean to apply
    
  })
  
  output$EcoliexceedancesOldStdTableSingleSiteSTV <- renderTable({
    req(Ecoli_oneStation())
    z <- bacteria_ExceedancesSTV_OLD(Ecoli_oneStation() %>%
                                       dplyr::select(FDT_DATE_TIME2,E.COLI)%>% # Just get relavent columns, 
                                       filter(!is.na(E.COLI)) #get rid of NA's
    ) %>%
      filter(exceeds == T) %>%
      mutate(FDT_DATE_TIME = as.character(FDT_DATE_TIME2), E.COLI = parameter) %>%
      dplyr::select(FDT_DATE_TIME, E.COLI, limit, exceeds)
  })
  
  output$EcoliOldStdTableSingleSite <- renderTable({
    req(Ecoli_oneStation())
    bacteria_Assessment_OLD(Ecoli_oneStation()) %>% dplyr::select(`Assessment Method`,everything())})
}