source('testingDataset.R')
source('global.R')
source('newBacteriaStandard_workingUpdatedRecSeason.R')

monStationTemplate <- read_excel('data/tbl_ir_mon_stations_template.xlsx') # from X:\2018_Assessment\StationsDatabase\VRO

citBact <- read_csv('data/2020IR bacteriaCitizen.csv') %>% # had to convert James' .xlsx to .csv bc date time data weird
  #fix data, only dealing with ecoli and enterococcus, every fucking name is different than non Agency...
  mutate(Entity='Citizen',
         FDT_STA_ID = `DEQ_Station_ID`, 
         FDT_DATE_TIME = ifelse(is.na(`mon_time`),paste(`mon_date`,'00:00:00'), paste(`mon_date`,`mon_time`)),
         #FDT_DEPTH = `Sample Depth m`, # apparently no depth information
         E.COLI = `E_coli_Method`,
         ENTEROCOCCI = `Enterococcus MPN/100ml`,
         LEVEL = ifelse(is.na(`E_coli_Method`), `Enterococcus Method`, `E_coli_Method`),
         QUALIFIER = ifelse(is.na(`Qualifier_E_coli`), `Qualifier Enterococcus`, `Qualifier_E_coli`)
  )

as.POSIXct(citBact$mon_date[1:10], format = '%m/%d/%Y')
as.POSIXct(citBact$mon_time[1:10], format = '%H:%m:%S')
as.POSIXct(strsplit(citBact$FDT_DATE_TIME[1:10],' ')[5][[1]][2], format = '%H:%M:%S')

as.POSIXct(citBact$FDT_DATE_TIME[1:10], format = '%m/%d/%Y %H:%m:%S')
nonABact <- read_csv('data/2020IR bacteriaNonAgency.csv') %>% # had to convert James' .xlsx to .csv bc date time data weird
  #fix data, only dealing with ecoli and enterococcus 
  mutate(Entity='NonAgency',
         FDT_STA_ID = `DEQ Station ID`, 
         FDT_DATE_TIME = ifelse(is.na(`mon time`),`mon date`, paste(`mon date`,`mon time`)),
         FDT_DEPTH = `Sample Depth m`, # WTF?
         E.COLI = `E coli 100ml`,
         ENTEROCOCCI = `Enterococcus per 100ml`,
         LEVEL = ifelse(is.na(`E coli Method`), `Enterococcus Method`, `E coli Method`),
         QUALIFIER = ifelse(is.na(`Qualifier E coli`), `Qualifier Enterococcus`, `Qualifier E coli`)
         )

#nonABact$`mon date`[1:10]<- 
as.POSIXct(nonABact$`mon date`[1:10],format = '%m/%D/%Y')

conventionals <-  mutate(conventionals, Entity='Citizen')
  
# Single station data ----------------------------------------------------------------------

x <-filter(conventionals, FDT_STA_ID %in% '2-JMS279.41')#'2-JMS282.28') 
#------------------------------------------------------------------------------------------




EcoliPlotlySingleStationUI <- function(id){
  ns <- NS(id)
  tagList(
    wellPanel(
      h4(strong('Single Station Data Visualization')),
      plotlyOutput(ns('Ecoliplotly')),
      br(),hr(),br(),
      fluidRow(
        column(6, h5('All E. coli records that are ',span(strong('above the criteria')),' for the ',
                     span(strong('selected site')),' are highlighted below.',
                     span(strong('If no data are reflected in below tables then no data exceeded the respective criteria.'))),
               h4(strong('Old Standard (Monthly Geomean = 126 CFU / 100 mL)')),
               DT::dataTableOutput(ns('EcoliexceedancesOldStdTableSingleSitegeomean')),
               h4(strong('Old Standard (Single Sample Maximum = 235 CFU / 100 mL)')),
               DT::dataTableOutput(ns('EcoliexceedancesOldStdTableSingleSiteSTV')), br(),  br(), hr(), br(), br(), 
               h4(strong('New Standard (STV= 410 CFU / 100 mL, geomean = 126 CFU / 100 mL with additional sampling requirements)')),
               helpText('The below table highlights all analyzed windows that have either STV violations OR geomean violations. Note
                  the number of samples in the window, STV Assessment, and Geomean Assessment columns for context. These violations
                  are important to understand the dataset, but assessment decision in the table to the right is where one should look
                  for assistance choosing which of the potential violations are driving the decision. Explore the dataset in 
                  90 day windows in the interactive graph below and the full dataset with assessment decisions paired with each window
                  in the bottom-most table.'),
               DT::dataTableOutput(ns('EcoliexceedancesNEWStdTableSingleSite'))),
        column(6, h5('Individual E. coli exceedance statistics for the ',span(strong('selected site')),' are highlighted below.'),
               h4(strong('Old Standard (Single Sample Maximum = 235 CFU / 100 mL, geomean = 126 CFU / 100 mL)')), 
               DT::dataTableOutput(ns("EcoliOldStdTableSingleSite")), br(), br(), br(),   br(),br(),br(), br(), br(),br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), hr(), br(), br(),
               h4(strong('New Standard (STV= 410 CFU / 100 mL, geomean = 126 CFU / 100 mL with additional sampling requirements)')), 
               DT::dataTableOutput(ns("EcoliNEWStdTableSingleSite")),
               h4(strong('See below section for detailed analysis with new recreation standard.')))),
      hr(),br(),
      h4(strong('New Recreation Standard Analysis')),
      helpText('Review the 90 day windows (identified by each sample date) for STV and geomean exceedances.
         Comments are specific to each row of data. To view the dataset within each 90 day window, use
         the drop down box to select the start of the window in question.'),
      fluidRow(
        column(6, helpText('Below is the raw data associated with the ',span('selected site'),'.'), 
               h5(strong('Raw Data')),DT::dataTableOutput(ns('rawData'))),
        column(6, uiOutput(ns('windowChoice')),
               plotlyOutput(ns('EcoliplotlyZoom')))),
      br(), br(),
      h5(strong('Analyzed Data (Each window with an individual assessment decision)')),
      DT::dataTableOutput(ns('analysisTable')))
  )
}


EcoliPlotlySingleStation <- function(input,output,session, stationSelectedAbove){
  ns <- session$ns
  
 Ecoli_oneStation <- reactive({
    req(stationSelectedAbove)
    z <- filter(conventionals,FDT_STA_ID %in% stationSelectedAbove()) %>%
      filter(!is.na(E.COLI))
    if(nrow(z)==0){return(NULL)} else {return(z)}})
 
 output$Ecoliplotly <- renderPlotly({
   req(Ecoli_oneStation())
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
               hoverinfo = "text", text= "Old SSM: 235 CFU / 100 mL", name="Old SSM: 235 CFU / 100 mL") %>%
     add_lines(data=dat, x=~SampleDate,y=~geomean, mode='line', line = list(color = 'black', dash= 'dash'),
               hoverinfo = "text", text= "Geomean: 126 CFU / 100 mL", name="Geomean: 126 CFU / 100 mL") %>%
     layout(showlegend=FALSE,
            yaxis=list(title="E. coli (CFU / 100 mL)"),
            xaxis=list(title="Sample Date",tickfont = list(size = 10))) 
 })
 
 output$EcoliexceedancesOldStdTableSingleSitegeomean <- DT::renderDataTable({
   req(Ecoli_oneStation())
   z <- bacteria_ExceedancesGeomeanOLD(Ecoli_oneStation() %>% 
                                         dplyr::select(FDT_DATE_TIME2,E.COLI)%>% # Just get relavent columns, 
                                         filter(!is.na(E.COLI)), #get rid of NA's
                                       'E.COLI', 126) %>%
     dplyr::select(FDT_DATE_TIME2, E.COLI, sampleMonthYear, geoMeanCalendarMonth, limit, samplesPerMonth) %>%
     rename(FDT_DATE_TIME = FDT_DATE_TIME2) %>%# for user view consistency, same data, just different format for R purposes
     filter(samplesPerMonth > 4, geoMeanCalendarMonth > limit) # minimum sampling rule for geomean to apply
   DT::datatable(z, rownames = FALSE, options= list(scrollX = TRUE, pageLength = nrow(z), scrollY = "250px", dom='t')) 
   
 })
 
 output$EcoliexceedancesOldStdTableSingleSiteSTV <- DT::renderDataTable({
   req(Ecoli_oneStation())
   z <- bacteria_ExceedancesSTV_OLD(Ecoli_oneStation() %>%
                                      dplyr::select(FDT_DATE_TIME2,E.COLI)%>% # Just get relavent columns, 
                                      filter(!is.na(E.COLI)) #get rid of NA's
                                    , 235 ) %>%
     filter(exceeds == T) %>%
     mutate(FDT_DATE_TIME = as.character(FDT_DATE_TIME2), E.COLI = parameter) %>%
     dplyr::select(FDT_DATE_TIME, E.COLI, limit, exceeds)
   DT::datatable(z, rownames = FALSE, options= list(scrollX = TRUE, pageLength = nrow(z), scrollY = "250px", dom='t')) 
 })
 
 output$EcoliOldStdTableSingleSite <- DT::renderDataTable({
   req(Ecoli_oneStation())
   z <- bacteria_Assessment_OLD(Ecoli_oneStation(), 'E.COLI', 126, 235) %>% dplyr::select(`Assessment Method`,everything())
   DT::datatable(z, rownames = FALSE, options= list(scrollX = TRUE, pageLength = nrow(z), scrollY = "250px", dom='t'))  })
 
 ### New standard ----------------------------------------------------------------------------------
 newSTDbacteriaData <- reactive({
   req(Ecoli_oneStation())
   conventionalsToBacteria(Ecoli_oneStation(), 'E.COLI')})  
 
 output$EcoliexceedancesNEWStdTableSingleSite <- DT::renderDataTable({
   req(Ecoli_oneStation(),newSTDbacteriaData())
   z <- bacteriaExceedances_NewStd(newSTDbacteriaData(), 10, 410, 126) %>% 
     filter(`STV Exceedances In Window` > 0 | `Geomean In Window` > 126) %>%
     dplyr::select(-associatedData) # remove embedded tibble to make table work
   z$`Date Window Starts` <- as.character(z$`Date Window Starts`)
   z$`Date Window Ends` <- as.character(z$`Date Window Ends`)
   DT::datatable(z, rownames = FALSE, options= list(scrollX = TRUE, pageLength = nrow(z), scrollY = "250px", dom='t')) 
 })
 
 
 output$EcoliNEWStdTableSingleSite <- DT::renderDataTable({
   req(Ecoli_oneStation(),newSTDbacteriaData())
   z <- bacteriaAssessmentDecision(newSTDbacteriaData(), 10, 410, 126)  %>%
     distinct(`Assessment Decision`) %>% # only grab 1 record
     mutate(`Assessment Method`= 'New Recreation Standard') %>%
     dplyr::select(`Assessment Method`, `Assessment Decision`) #only grab decision
   DT::datatable(z, rownames = FALSE, options= list(scrollX = TRUE, pageLength = nrow(z), scrollY = "250px", dom='t')) 
 })
 
 
 output$windowChoice <- renderUI({
   req(Ecoli_oneStation(),newSTDbacteriaData())
   fluidRow(
     column(4, selectInput(ns('windowChoice_'),'Select 90 day window start date',
                           choices = unique(newSTDbacteriaData()$`Date Time`), width = '100%')),
     column(8, helpText('Orange line corresponds to the window geomean; wide black dashed line
                        corresponds to the geomean criteria; thin black dashed line corresponds
                        to the STV limit.')))})
 
 output$EcoliplotlyZoom <- renderPlotly({
   req(input$windowChoice_, Ecoli_oneStation(),newSTDbacteriaData())
   windowStart <- bacteriaExceedances_NewStd(newSTDbacteriaData(), 10, 410, 126) %>%
     filter(`Date Window Starts` == input$windowChoice_) #'2011-02-17')#
   
   windowData <- dplyr::select(windowStart, associatedData) %>%
     unnest() %>%
     mutate(`Date Window Starts` = as.POSIXct(unique(windowStart$`Date Window Starts`, format="%m/%d/%y")),
            `Date Window Ends` = as.POSIXct(unique(windowStart$`Date Window Ends`, format="%m/%d/%y")),
            newSTV = 410, geomean = 126)
   windowData$`Date Time` <- as.POSIXct(strptime(windowData$`Date Time`, format="%Y-%m-%d"))#as.POSIXct(windowData$`Date Time`, format="%Y-%m-%d", tz='GMT') + as.difftime(1, units="days")
   
   plot_ly(data=windowData) %>%
     add_markers(x= ~`Date Time`, y= ~Value,mode = 'scatter', name="E. coli (CFU / 100 mL)", marker = list(color= '#535559'),
                 hoverinfo="text",text=~paste(sep="<br>",
                                              paste("Date: ",`Date Time`),
                                              paste("E. coli: ",Value,"CFU / 100 mL"))) %>%
     add_lines(data=windowData, x=~`Date Time`, y=~E.COLI_geomean, mode='line', line = list(color = 'orange', dash= 'dash'),
               hoverinfo = "text", text= ~paste("Window Geomean: ", format(E.COLI_geomean,digits=3)," CFU / 100 mL", sep=''), 
               name="Window Geomean") %>%
     add_lines(data=windowData, x=~`Date Time`,y=~newSTV, mode='line', line = list(color = '#484a4c',dash = 'dot'),
               hoverinfo = "text", text= "New STV: 410 CFU / 100 mL", name="New STV: 410 CFU / 100 mL") %>%
     add_lines(data=windowData, x=~`Date Time`,y=~geomean, mode='line', line = list(color = 'black', dash= 'dash'),
               hoverinfo = "text", text= "Geomean: 126 CFU / 100 mL", name="Geomean: 126 CFU / 100 mL") %>%
     layout(showlegend=FALSE,
            yaxis=list(title="E. coli (CFU / 100 mL)"),
            xaxis=list(title="Sample Date",tickfont = list(size = 10))) 
 })
 
 
 
 output$rawData <- DT::renderDataTable({
   req(input$windowChoice_, Ecoli_oneStation(),newSTDbacteriaData())
   z <- dplyr::select(Ecoli_oneStation(), FDT_STA_ID, FDT_DATE_TIME, E.COLI ,RMK_ECOLI)
   DT::datatable(z, options= list(scrollX = TRUE, pageLength = nrow(z), scrollY = "400px", dom='t'))
 })
 
 output$analysisTable <- DT::renderDataTable({
   req(Ecoli_oneStation(),newSTDbacteriaData())
   z <- bacteriaExceedances_NewStd(newSTDbacteriaData(), 10, 410, 126) %>%
     dplyr::select(-associatedData) # remove embedded tibble to make table work
   DT::datatable(z, rownames = FALSE, options= list(scrollX = TRUE, pageLength = nrow(z), scrollY = "400px", dom='t'))
 })
 
  }


EnteroPlotlySingleStationUI <- function(id){
  ns <- NS(id)
  tagList(
    wellPanel(
      h4(strong('Single Station Data Visualization')),
      uiOutput(ns('Entero_oneStationSelectionUI')),
      plotlyOutput(ns('Enteroplotly')),
      br(),hr(),br(),
      fluidRow(
        column(6, h5('All enterococci records that are above the criteria for the ',span(strong('selected site')),' are highlighted below.'),
               h4(strong('Old Standard (Monthly Geomean = 35 CFU / 100 mL)')),
               DT::dataTableOutput(ns('EnteroexceedancesOldStdTableSingleSitegeomean')),
               h4(strong('Old Standard (Single Sample Maximum = 104 CFU / 100 mL)')),
               DT::dataTableOutput(ns('EnteroexceedancesOldStdTableSingleSiteSTV')), br(), br(), hr(), br(), br(), 
               h4(strong('New Standard (STV= 130 CFU / 100 mL, geomean = 35 CFU / 100 mL with additional sampling requirements)')),
               helpText('The below table highlights all analyzed windows that have either STV violations OR geomean violations. Note
                        the number of samples in the window, STV Assessment, and Geomean Assessment columns for context. These violations
                        are important to understand the dataset, but assessment decision in the table to the right is where one should look
                        for assistance choosing which of the potential violations are driving the decision. Explore the dataset in 
                        90 day windows in the interactive graph below and the full dataset with assessment decisions paired with each window
                        in the bottom-most table.'),
               DT::dataTableOutput(ns('EnteroexceedancesNEWStdTableSingleSite'))),
        column(6, h5('Individual enterococci exceedance statistics for the ',span(strong('selected site')),' are highlighted below.'),
               h4(strong('Old Standard (Single Sample Maximum = 104 CFU / 100 mL, geomean = 35 CFU / 100 mL)')), 
               DT::dataTableOutput(ns("EnteroOldStdTableSingleSite")), br(), br(), br(),   br(),br(),br(), br(), br(),br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), hr(), br(), br(),
               h4(strong('New Standard (STV= 130 CFU / 100 mL, geomean = 35 CFU / 100 mL with additional sampling requirements)')), 
               DT::dataTableOutput(ns("EnteroNEWStdTableSingleSite")),
               h4(strong('See below section for detailed analysis with new recreation standard.')))),
      hr(),br(),
      h4(strong('New Recreation Standard Analysis')),
      helpText('Review the 90 day windows (identified by each sample date) for STV and geomean exceedances.
               Comments are specific to each row of data. To view the dataset within each 90 day window, use
               the drop down box to select the start of the window in question.'),
      fluidRow(
        column(6, helpText('Below is the raw data associated with the ',span('selected site'),'.'), 
               DT::dataTableOutput(ns('rawData'))),
        column(6, uiOutput(ns('windowChoice')),
               plotlyOutput(ns('EnteroplotlyZoom')))),
      br(), br(),
      h5(strong('Analyzed Data (Each window with an individual assessment decision)')),
      DT::dataTableOutput(ns('analysisTable')))
      )
}


EnteroPlotlySingleStation <- function(input,output,session, stationSelectedAbove){
  ns <- session$ns
  
  Entero_oneStation <- reactive({
    req(stationSelectedAbove)
    z <- filter(conventionals,FDT_STA_ID %in% stationSelectedAbove()) %>%
      filter(!is.na(ENTEROCOCCI))
    if(nrow(z)==0){return(NULL)} else {return(z)}})
  
  output$Enteroplotly <- renderPlotly({
    req(Entero_oneStation())
    dat <- Entero_oneStation() %>%
      mutate(newSTV = 130, geomean = 35, oldSTV = 104)
    dat$SampleDate <- as.POSIXct(dat$FDT_DATE_TIME2, format="%m/%d/%y")
    plot_ly(data=dat) %>%
      add_markers(x= ~SampleDate, y= ~ENTEROCOCCI,mode = 'scatter', name="Enterococci (CFU / 100 mL)", marker = list(color= '#535559'),
                  hoverinfo="text",text=~paste(sep="<br>",
                                               paste("Date: ",SampleDate),
                                               paste("Depth: ",FDT_DEPTH, "m"),
                                               paste("Enterococci: ",ENTEROCOCCI,"CFU / 100 mL")))%>%
      add_lines(data=dat, x=~SampleDate,y=~newSTV, mode='line', line = list(color = '#484a4c',dash = 'dot'),
                hoverinfo = "text", text= "New STV: 130 CFU / 100 mL", name="New STV: 130 CFU / 100 mL") %>%
      add_lines(data=dat, x=~SampleDate,y=~oldSTV, mode='line', line = list(color = 'black'),
                hoverinfo = "text", text= "Old SSM: 104 CFU / 100 mL", name="Old SSM: 104 CFU / 100 mL") %>%
      add_lines(data=dat, x=~SampleDate,y=~geomean, mode='line', line = list(color = 'black', dash= 'dash'),
                hoverinfo = "text", text= "Geomean: 35 CFU / 100 mL", name="Geomean: 35 CFU / 100 mL") %>%
      layout(showlegend=FALSE,
             yaxis=list(title="Enterococci (CFU / 100 mL)"),
             xaxis=list(title="Sample Date",tickfont = list(size = 10))) 
  })
  
  output$EnteroexceedancesOldStdTableSingleSitegeomean <- DT::renderDataTable({
    req(Entero_oneStation())
    z <- bacteria_ExceedancesGeomeanOLD(Entero_oneStation() %>% 
                                          dplyr::select(FDT_DATE_TIME2,ENTEROCOCCI)%>% # Just get relavent columns, 
                                          filter(!is.na(ENTEROCOCCI)), #get rid of NA's
                                        'ENTEROCOCCI', 35) %>%
      dplyr::select(FDT_DATE_TIME2, ENTEROCOCCI, sampleMonthYear, geoMeanCalendarMonth, limit, samplesPerMonth) %>%
      rename(FDT_DATE_TIME = FDT_DATE_TIME2) %>%# for user view consistency, same data, just different format for R purposes
      filter(samplesPerMonth > 4, geoMeanCalendarMonth > limit) # minimum sampling rule for geomean to apply
    DT::datatable(z, rownames = FALSE, options= list(scrollX = TRUE, pageLength = nrow(z), scrollY = "250px", dom='t')) 
    
  })
  
  output$EnteroexceedancesOldStdTableSingleSiteSTV <- DT::renderDataTable({
    req(Entero_oneStation())
    print(Entero_oneStation())
    z <- bacteria_ExceedancesSTV_OLD(Entero_oneStation() %>%
                                       dplyr::select(FDT_DATE_TIME2,ENTEROCOCCI)%>% # Just get relavent columns, 
                                       filter(!is.na(ENTEROCOCCI)) #get rid of NA's
                                     , 130 ) %>%
      filter(exceeds == T) %>%
      mutate(FDT_DATE_TIME = as.character(FDT_DATE_TIME2), ENTEROCOCCI = parameter) %>%
      dplyr::select(FDT_DATE_TIME, ENTEROCOCCI, limit, exceeds)
    DT::datatable(z, rownames = FALSE, options= list(scrollX = TRUE, pageLength = nrow(z), scrollY = "250px", dom='t')) 
  })
  
  output$EnteroOldStdTableSingleSite <- DT::renderDataTable({
    req(Entero_oneStation())
    z <- bacteria_Assessment_OLD(Entero_oneStation(), 'ENTEROCOCCI', 35, 104) %>% dplyr::select(`Assessment Method`,everything())
    DT::datatable(z, rownames = FALSE, options= list(scrollX = TRUE, pageLength = nrow(z), scrollY = "250px", dom='t'))  })
  
  ### New standard ----------------------------------------------------------------------------------
  newSTDbacteriaData <- reactive({
    req(Entero_oneStation())
    conventionalsToBacteria(Entero_oneStation(), 'ENTEROCOCCI')})  
  
  output$EnteroexceedancesNEWStdTableSingleSite <- DT::renderDataTable({
    req(Entero_oneStation(),newSTDbacteriaData())
    z <- bacteriaExceedances_NewStd(newSTDbacteriaData(), 10, 130, 35) %>% 
      filter(`STV Exceedances In Window` > 0 | `Geomean In Window` > 35) %>%
      dplyr::select(-associatedData) # remove embedded tibble to make table work
    z$`Date Window Starts` <- as.character(z$`Date Window Starts`)
    z$`Date Window Ends` <- as.character(z$`Date Window Ends`)
    DT::datatable(z, rownames = FALSE, options= list(scrollX = TRUE, pageLength = nrow(z), scrollY = "250px", dom='t')) 
  })
  
  
  output$EnteroNEWStdTableSingleSite <- DT::renderDataTable({
    req(Entero_oneStation(),newSTDbacteriaData())
    z <- bacteriaAssessmentDecision(newSTDbacteriaData(), 10, 130, 35)  %>%
      distinct(`Assessment Decision`) %>% # only grab 1 record
      mutate(`Assessment Method`= 'New Recreation Standard') %>%
      dplyr::select(`Assessment Method`, `Assessment Decision`) #only grab decision
    DT::datatable(z, rownames = FALSE, options= list(scrollX = TRUE, pageLength = nrow(z), scrollY = "250px", dom='t')) 
  })
  
  
  output$windowChoice <- renderUI({
    req(Entero_oneStation(),newSTDbacteriaData())
    fluidRow(
      column(4, selectInput(ns('windowChoice_'),'Select 90 day window start date',
                            choices = unique(newSTDbacteriaData()$`Date Time`), width = '100%')),
      column(8, helpText('Orange line corresponds to the window geomean; wide black dashed line
                         corresponds to the geomean criteria; thin black dashed line corresponds
                         to the STV limit.')))})
  
  output$EnteroplotlyZoom <- renderPlotly({
    req(input$windowChoice_, Entero_oneStation(),newSTDbacteriaData())
    windowStart <- bacteriaExceedances_NewStd(newSTDbacteriaData(), 10, 130, 35) %>%
      filter(`Date Window Starts` == input$windowChoice_) #'2011-05-24')#
    
    windowData <- dplyr::select(windowStart, associatedData) %>%
      unnest() %>%
      rename(ENTEROCOCCI_geomean='E.COLI_geomean') %>% # fix column name that comes out of associatedData slot
      mutate(`Date Window Starts` = as.POSIXct(unique(windowStart$`Date Window Starts`, format="%m/%d/%y")),
             `Date Window Ends` = as.POSIXct(unique(windowStart$`Date Window Ends`, format="%m/%d/%y")),
             newSTV = 130, geomean = 35)
    windowData$`Date Time` <- as.POSIXct(strptime(windowData$`Date Time`, format="%Y-%m-%d"))#as.POSIXct(windowData$`Date Time`, format="%Y-%m-%d", tz='GMT') + as.difftime(1, units="days")
    
    plot_ly(data=windowData) %>%
      add_markers(x= ~`Date Time`, y= ~Value,mode = 'scatter', name="Enterococci (CFU / 100 mL)", marker = list(color= '#535559'),
                  hoverinfo="text",text=~paste(sep="<br>",
                                               paste("Date: ",`Date Time`),
                                               paste("Enterococci: ",Value,"CFU / 100 mL"))) %>%
      add_lines(data=windowData, x=~`Date Time`, y=~ENTEROCOCCI_geomean, mode='line', line = list(color = 'orange', dash= 'dash'),
                hoverinfo = "text", text= ~paste("Window Geomean: ", format(ENTEROCOCCI_geomean,digits=3)," CFU / 100 mL", sep=''), 
                name="Window Geomean") %>%
      add_lines(data=windowData, x=~`Date Time`,y=~newSTV, mode='line', line = list(color = '#484a4c',dash = 'dot'),
                hoverinfo = "text", text= "New STV: 130 CFU / 100 mL", name="New STV: 130 CFU / 100 mL") %>%
      add_lines(data=windowData, x=~`Date Time`,y=~geomean, mode='line', line = list(color = 'black', dash= 'dash'),
                hoverinfo = "text", text= "Geomean: 35 CFU / 100 mL", name="Geomean: 35 CFU / 100 mL") %>%
      layout(showlegend=FALSE,
             yaxis=list(title="Enterococci (CFU / 100 mL)"),
             xaxis=list(title="Sample Date",tickfont = list(size = 10))) 
  })
  
  output$rawData <- DT::renderDataTable({
    req(input$windowChoice_, Entero_oneStation(),newSTDbacteriaData())
    z <- dplyr::select(Entero_oneStation(), FDT_STA_ID, FDT_DATE_TIME, ENTEROCOCCI ,RMK_31649)
    DT::datatable(z, options= list(scrollX = TRUE, pageLength = nrow(z), scrollY = "400px", dom='t'))
  })
  
  output$analysisTable <- DT::renderDataTable({
    req(Entero_oneStation(),newSTDbacteriaData())
    z <- bacteriaExceedances_NewStd(newSTDbacteriaData(), 10, 130, 35) %>%
      dplyr::select(-associatedData) # remove embedded tibble to make table work
    DT::datatable(z, rownames = FALSE, options= list(scrollX = TRUE, pageLength = nrow(z), scrollY = "400px", dom='t'))
  })
}


statsSingleStationUI <-  function(id){
  ns <- NS(id)
  tagList(DT::dataTableOutput(ns('stats')))
}

statsSingleStation <- function(input,output,session, stationSelectedAbove){
  
  output$stats <- DT::renderDataTable({
    req(stationSelectedAbove)
    
    x <- filter(conventionals,FDT_STA_ID %in% stationSelectedAbove())
    
    z <- cbind(data.frame(FDT_STA_ID = unique(x$FDT_STA_ID)),
               bacteriaExceedances_OLD(bacteria_Assessment_OLD(x, 'E.COLI', 126, 235),'E.COLI') %>% 
                 dplyr::rename('ECOLI_VIO' = 'E.COLI_VIO', 'ECOLI_SAMP'='E.COLI_SAMP', 'ECOLI_STAT'='E.COLI_STAT'),
               bacteriaExceedances_NEW(x,'E.COLI', 10, 410, 126),
               bacteriaExceedances_OLD(bacteria_Assessment_OLD(x, 'ENTEROCOCCI', 35, 104),'ENTEROCOCCI') %>% 
                 dplyr::rename('ENTER_VIO' = 'ENTEROCOCCI_VIO', 'ENTER_SAMP'='ENTEROCOCCI_SAMP', 'ENTER_STAT'='ENTEROCOCCI_STAT'),
               #new bacteria exceedance stats
               bacteriaExceedances_NEW(x,'ENTEROCOCCI',10, 35, 104)) %>% 
      dplyr::select(-ends_with('exceedanceRate'))
      
    DT::datatable(z, rownames = FALSE, extensions = 'Buttons', escape=F,
                  options= list(scrollX = TRUE, pageLength = nrow(z), scrollY = "90px", dom='Bt', buttons=list('copy'))) %>%
      formatStyle(c('ECOLI_SAMP','ECOLI_VIO','ECOLI_STAT'), 'ECOLI_STAT', backgroundColor = styleEqual(c('Review', '10.5% Exceedance'), c('yellow','red'))) %>%
      formatStyle(c('ECOLI_STV_VIO','ECOLI_GEOMEAN_VIO','ECOLI_STAT_NEW'), 'ECOLI_STAT_NEW', backgroundColor = styleEqual(c('Review', '10.5% Exceedance'), c('yellow','red'))) %>%
      formatStyle(c('ENTER_SAMP','ENTER_VIO','ENTER_STAT'), 'ENTER_STAT', backgroundColor = styleEqual(c('Review', '10.5% Exceedance'), c('yellow','red'))) %>%
      formatStyle(c('ENTER_STV_VIO','ENTER_GEOMEAN_VIO','ENTER_STAT_NEW'), 'ENTER_STAT_NEW', backgroundColor = styleEqual(c('Review', '10.5% Exceedance'), c('yellow','red')))
      
  })
}



ui <- fluidPage(theme="yeti.css",
                shinyjs::useShinyjs(),
                div(
                  id = "loading_page",
                  h1("Loading...")
                ),
                hidden(
                  div(
                    id = "main_content",
                    # suppress error messages as data loads, hacky
                    tags$style(type="text/css",
                               ".shiny-output-error { visibility: hidden; }",
                               ".shiny-output-error:before { visibility: hidden; }"
                    ),
                    navbarPage(paste("VDEQ ",assessmentCycle," IR Bacteria Assessment Tool", sep=''),
                              tabPanel('Assessment',
                                        selectInput('stationSelection', 'Station Selection', choices = unique(conventionals$FDT_STA_ID)),
                                       tabsetPanel(
                                         tabPanel(tags$em("E. coli"),
                                                  helpText('Review each site using the single site visualization section. The module analyzes both the old
                                                           and new criteria.'),
                                                  EcoliPlotlySingleStationUI('Ecoli')),
                                         tabPanel('Enterococci',
                                                  helpText('Review each site using the single site visualization section. The module analyzes both the old
                                                           and new criteria.'),
                                                  EnteroPlotlySingleStationUI('Entero')),
                                         tabPanel('Stations Table Stats',
                                                  statsSingleStationUI('stats'))
                                       )),
                              tabPanel('About')
                    )))
)

server <- function(input,output,session){
  # display the loading feature until data loads into app
  load_data()
  
  
  
  stationData <- eventReactive( input$stationSelection, {
    filter(conventionals, FDT_STA_ID %in% input$stationSelection) })
  stationSelected <- reactive({input$stationSelection})
 
  callModule(EcoliPlotlySingleStation,'Ecoli', stationSelected)#input$stationSelection)
  
  callModule(EnteroPlotlySingleStation,'Entero', stationSelected)#input$stationSelection)
  
  callModule(statsSingleStation,'stats', stationSelected)#input$stationSelection)
  
}

shinyApp(ui,server)
