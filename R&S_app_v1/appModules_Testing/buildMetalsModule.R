source('testingDataset.R')
monStationTemplate <- read_excel('data/tbl_ir_mon_stations_template.xlsx') # from X:\2018_Assessment\StationsDatabase\VRO

# Single station data ----------------------------------------------------------------------
conventionals_HUC<- left_join(conventionals, dplyr::select(stationTable, FDT_STA_ID, SEC, CLASS, SPSTDS, ID305B_1, ID305B_2, ID305B_3), by='FDT_STA_ID')

AUData <- filter(conventionals_HUC, ID305B_1 %in% 'VAW-I04R_JKS04A00' | 
                   ID305B_2 %in% 'VAW-I04R_JKS04A00' | 
                   ID305B_2 %in% 'VAW-I04R_JKS04A00')%>% 
  left_join(WQSvalues, by = 'CLASS')

x <-filter(conventionals_HUC, FDT_STA_ID %in% '2-JKS033.06') #'2-JMS279.41')#
x2 <- filter(conventionals_HUC, FDT_STA_ID %in% '2-JKS028.69')

# Bring in Roger's Metal Assessment
WCmetals <- read_excel('data/WATER_METALS_20170712.xlsx')
Smetals <- read_excel('data/SEDIMENT_20170712.xlsx')

WCmetals[558,97] <- 'NSP'

z <- filter(WCmetals, FDT_STA_ID %in% x$FDT_STA_ID)# %>%
 # dplyr::select(FDT_STA_ID:HARDNESS)

z <- dplyr::select(z, `ANTIMONY HUMAN HEALTH PWS`:`ZINC ALL OTHER SURFACE WATERS`)


s <- filter(Smetals, FDT_STA_ID %in% x2$FDT_STA_ID)



metalsExceedances <- function(x, metalType){
  # if any data given to function
  if(nrow(x) > 0){ VIO <- length(which(x == 'NSP')) 
  }else {
    VIO <- NA  }
  
  x <- data.frame(VIO = VIO, STAT = ifelse(VIO > 0, 'Review', 'S'))
  names(x) <- paste(metalType,names(x), sep='_')
  return(x)
}
metalsExceedances(z, 'WAT_MET')


metalsTableSingleStationUI <- function(id){
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel('Water Column Metals',
               wellPanel(
                 h4(strong('Single Station Data Visualization')),
                 uiOutput(ns('WCmetals_oneStationSelectionUI')),
                 h5('All water column metals data available for the ',span(strong('selected site')),' are available below. 
         If no data is presented, then the station does not have any water column metals data available.'),
                 DT::dataTableOutput(ns('WCmetalsRangeTableSingleSite')),br(), br(), br(),
                 h5('Metals assessments for the ',span(strong('selected site')),' are highlighted below.'),
                 DT::dataTableOutput(ns("WCstationmetalsExceedanceRate")))),
      tabPanel('Sediment Metals',
               wellPanel(
                 h4(strong('Single Station Data Visualization')),
                 uiOutput(ns('Smetals_oneStationSelectionUI')),
                 h5('All sediment metals data available for the ',span(strong('selected site')),' are available below. 
                    If no data is presented, then the station does not have any sediment metals data available.'),
                 DT::dataTableOutput(ns('SmetalsRangeTableSingleSite')),br(), br(), br(),
                 h5('Metals assessments for the ',span(strong('selected site')),' are highlighted below.'),
                 DT::dataTableOutput(ns("SstationmetalsExceedanceRate"))))
    
      ))
}


metalsTableSingleStation <- function(input,output,session, AUdata, WCmetals ,Smetals, stationSelectedAbove){
  ns <- session$ns
  
  # Select One station for individual review
  output$WCmetals_oneStationSelectionUI <- renderUI({
    req(stationSelectedAbove)
    selectInput(ns('WCmetals_oneStationSelection'),strong('Select Station to Review'),choices= sort(unique(c(stationSelectedAbove(),AUdata()$FDT_STA_ID))),#unique(AUdata())$FDT_STA_ID,
                width='300px', selected = stationSelectedAbove())})# "2-JMS279.41" )})
  
  
  WCmetals_oneStation <- reactive({
    req(ns(input$WCmetals_oneStationSelection))
    filter(WCmetals, FDT_STA_ID %in% input$WCmetals_oneStationSelection)})
  
  
  output$WCmetalsRangeTableSingleSite <- DT::renderDataTable({
    req(WCmetals_oneStation())
    z <- dplyr::select(WCmetals_oneStation(), FDT_STA_ID:HARDNESS)
    z$FDT_DATE_TIME <- as.character(as.POSIXct(z$FDT_DATE_TIME, format="%m/%d/%Y %H:%M"))
    DT::datatable(z, rownames = FALSE, options= list(scrollX = TRUE, pageLength = nrow(z), scrollY = "250px", dom='t'))     })
  
  output$WCstationmetalsExceedanceRate <- DT::renderDataTable({
    req(input$WCmetals_oneStationSelection, WCmetals_oneStation())
    z <- dplyr::select(WCmetals_oneStation(), FDT_STA_ID, `FDT_DATE_TIME`,`ANTIMONY HUMAN HEALTH PWS`:`ZINC ALL OTHER SURFACE WATERS`)
    z$FDT_DATE_TIME <- as.character(as.POSIXct(z$FDT_DATE_TIME, format="%m/%d/%Y %H:%M"))
    DT::datatable(z, rownames = FALSE, options= list(scrollX = TRUE, pageLength = nrow(z), scrollY = "250px", dom='t')) %>%
      formatStyle(names(z), backgroundColor = styleEqual(c('NSP'), c('red'))) # highlight cells red if not supporting
    }) 
  
  
  # Sediment Metals
  # Select One station for individual review
  output$Smetals_oneStationSelectionUI <- renderUI({
    req(stationSelectedAbove)
    selectInput(ns('Smetals_oneStationSelection'),strong('Select Station to Review'),choices= sort(unique(c(stationSelectedAbove(),AUdata()$FDT_STA_ID))),#unique(AUdata())$FDT_STA_ID,
                width='300px', selected = stationSelectedAbove())})# "2-JMS279.41" )})
  
  
  Smetals_oneStation <- reactive({
    req(ns(input$Smetals_oneStationSelection))
    filter(Smetals, FDT_STA_ID %in% input$Smetals_oneStationSelection)})
  
  
  output$SmetalsRangeTableSingleSite <- DT::renderDataTable({
    req(Smetals_oneStation())
    z <- dplyr::select(Smetals_oneStation(), FDT_STA_ID, FDT_DATE_TIME:`CHLORDANE_TOTAL`)
    z$FDT_DATE_TIME <- as.character(as.POSIXct(z$FDT_DATE_TIME, format="%m/%d/%Y %H:%M"))
    DT::datatable(z, rownames = FALSE, options= list(scrollX = TRUE, pageLength = nrow(z), scrollY = "250px", dom='t'))     })
  
  output$SstationmetalsExceedanceRate <- DT::renderDataTable({
    req(input$Smetals_oneStationSelection, Smetals_oneStation())
    z <- dplyr::select(Smetals_oneStation(), FDT_STA_ID, `FDT_DATE_TIME`,`ACENAPHTHENE`:COMMENT)
    z$FDT_DATE_TIME <- as.character(as.POSIXct(z$FDT_DATE_TIME, format="%m/%d/%Y %H:%M"))
    DT::datatable(z, rownames = FALSE, options= list(scrollX = TRUE, pageLength = nrow(z), scrollY = "250px", dom='t')) %>%
      formatStyle(names(z), backgroundColor = styleEqual(c('OE'), c('red'))) # highlight cells red if not supporting
  }) 
  
  
}




ui <- fluidPage(
  helpText('Review each site using the single site visualization section. There are no WQS for Specific Conductivity.'),
  metalsTableSingleStationUI('metals')
)

server <- function(input,output,session){
  stationData <- eventReactive( input$stationSelection, {
    filter(AUData, FDT_STA_ID %in% input$stationSelection) })
  stationSelected <- reactive({input$stationSelection})
  
  AUData <- reactive({filter(conventionals_HUC, ID305B_1 %in% 'VAW-I04R_JKS03A00' | 
                               ID305B_2 %in% 'VAW-I04R_JKS03A00' | 
                               ID305B_2 %in% 'VAW-I04R_JKS03A00')%>% 
      left_join(WQSvalues, by = 'CLASS')})
  
  callModule(metalsTableSingleStation,'metals', AUData, WCmetals ,Smetals, stationSelected)
  
}

shinyApp(ui,server)

