source('testingDataset.R')
monStationTemplate <- read_excel('data/tbl_ir_mon_stations_template.xlsx') # from X:\2018_Assessment\StationsDatabase\VRO

# Single station data ----------------------------------------------------------------------
AUData <- filter(conventionals_HUC, ID305B_1 %in% 'VAW-H01R_JMS04A00' | 
                   ID305B_2 %in% 'VAW-H01R_JMS04A00' | 
                   ID305B_2 %in% 'VAW-H01R_JMS04A00')%>% 
  left_join(WQSvalues, by = 'CLASS')

x <-filter(AUData, FDT_STA_ID %in% '2-JMS279.41') 
#------------------------------------------------------------------------------------------
#parameterDataset <- bacteriaGeomean
#parameter <- 'ECOLI'

quickStats <- function(parameterDataset, parameter){
  results <- data.frame(SAMP = nrow(parameterDataset),
                        VIO = nrow(filter(parameterDataset, exceeds == TRUE))) %>%
    mutate(exceedanceRate = (VIO/SAMP)*100)
  
  if(results$exceedanceRate > 10.5 & results$SAMP > 10){outcome <- 'Review'}
  if(results$exceedanceRate < 10.5 & results$SAMP > 10){outcome <- 'S'}
  if(results$VIO >= 2 & results$SAMP < 10){outcome <- 'Review'}
  if(results$VIO < 2 & results$SAMP < 10){outcome <- 'Review'}
  
  results <- mutate(results, STAT = outcome)
  names(results) <- c(paste(parameter,names(results)[1], sep = '_'),
                      paste(parameter,names(results)[2], sep = '_'),
                      paste(parameter,names(results)[3], sep = '_'),
                      paste(parameter,names(results)[4], sep = '_'))
  #rename based on parameter entered
  return(results)
}

#bacteria geomean testing
bacteria1 <- bacteria
bacteria1[1,] <- c('2011-02-02 12:30:00',25)
bacteria1[2,] <- c('2011-02-04 12:30:00',432)
bacteria1[3,] <- c('2011-02-05 12:30:00',555)
bacteria1[4,] <- c('2011-02-06 12:30:00',18)
bacteria1[5,] <- c('2011-02-018 12:30:00',333)
bacteria1[30,] <- c('2013-06-01 15:25:00',25)
bacteria1[31,] <- c('2013-06-04 15:25:00',444)
bacteria1[32,] <- c('2013-06-10 15:25:00',555)
bacteria1[33,] <- c('2013-06-18 15:25:00',666)
bacteria1[34,] <- c('2013-06-22 15:25:00',777)
bacteria1[35,] <- c('2013-06-29 15:25:00',98)

bacteria_ExceedancesGeomeanOLD <- function(x){
  suppressWarnings(mutate(x, SampleDate = format(FDT_DATE_TIME2,"%m/%d/%y"), # Separate sampling events by day
         previousSample=lag(SampleDate,1),previousSampleECOLI=lag(E.COLI,1)) %>% # Line up previous sample with current sample line
    rowwise() %>% 
    mutate(sameSampleMonth= as.numeric(strsplit(SampleDate,'/')[[1]][1])  -  as.numeric(strsplit(previousSample,'/')[[1]][1])) %>% # See if sample months are the same, e.g. more than one sample per calendar month
    filter(sameSampleMonth == 0 | is.na(sameSampleMonth)) %>% # keep only rows with multiple samples per calendar month  or no previous sample (NA) to then test for geomean
    # USING CALENDAR MONTH BC THAT'S HOW WRITTEN IN GUIDANCE, rolling 4 wk windows would have been more appropriate
    mutate(sampleMonthYear = paste(month(as.Date(SampleDate,"%m/%d/%y")),year(as.Date(SampleDate,"%m/%d/%y")),sep='/')) %>% # grab sample month and year to group_by() for next analysis
    group_by(sampleMonthYear) %>%
    mutate(geoMeanCalendarMonth = FSA::geomean(as.numeric(E.COLI)), # Calculate geomean
           limit = 126, samplesPerMonth = n()))
}
 
bacteria_ExceedancesSTV_OLD <- function(x){                                    
  x %>% rename(parameter = !!names(.[2])) %>% # rename columns to make functions easier to apply
    mutate(limit = 235, exceeds = ifelse(parameter > limit, T, F)) # Single Sample Maximum 
}



# How bacteria is assessed
bacteria_Assessment_OLD <- function(x){
  bacteria <- dplyr::select(x,FDT_DATE_TIME2,E.COLI)%>% # Just get relavent columns, 
    filter(!is.na(E.COLI)) #get rid of NA's
  # Geomean Analysis (if enough n)
  bacteriaGeomean <- bacteria_ExceedancesGeomeanOLD(bacteria) %>%     
    distinct(sampleMonthYear, .keep_all = T) %>%
    filter(samplesPerMonth > 4, geoMeanCalendarMonth > limit) %>% # minimum sampling rule for geomean to apply
    mutate(exceeds = TRUE) %>%
    select(sampleMonthYear, geoMeanCalendarMonth, limit, exceeds, samplesPerMonth)
  geomeanResults <- quickStats(bacteriaGeomean, 'ECOLI') %>% mutate(ECOLI_STAT = recode(ECOLI_STAT, 'Review' = 'Review if ECOLI_VIO > 1' ),
                                                                    `Assessment Method` = 'Old Monthly Geomean')
  
  # Single Sample Maximum Analysis
  bacteriaSSM <- bacteria_ExceedancesSTV_OLD(bacteria) 
  SSMresults <- quickStats(bacteriaSSM, 'ECOLI') %>% mutate(`Assessment Method` = 'Old Single Sample Maximum')
  return( rbind(geomeanResults, SSMresults) )
}

#bacteria_Assessment_OLD(x)




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
  


ui <- fluidPage(
  selectInput('stationSelection', 'Station Selection', choices = unique(AUData$FDT_STA_ID)),
  helpText('Review each site using the single site visualization section. There are no WQS for Total Nitrogen.'),
  EcoliPlotlySingleStationUI('Ecoli')
)

server <- function(input,output,session){
  stationData <- eventReactive( input$stationSelection, {
    filter(AUData, FDT_STA_ID %in% input$stationSelection) })
  stationSelected <- reactive({input$stationSelection})
  
  AUData <- reactive({filter(conventionals_HUC, ID305B_1 %in% 'VAW-H01R_JMS04A00' | 
                               ID305B_2 %in% 'VAW-H01R_JMS04A00' | 
                               ID305B_2 %in% 'VAW-H01R_JMS04A00')%>% 
      left_join(WQSvalues, by = 'CLASS')})
  
  callModule(EcoliPlotlySingleStation,'Ecoli', AUData, stationSelected)#input$stationSelection)
  
}

shinyApp(ui,server)
