source('testingDataset.R')


z <-filter(conventionals, FDT_STA_ID %in% stationTable$FDT_STA_ID) %>%
  filter(FDT_STA_ID %in% '2-DCK003.94')


# No Assessment functions bc no std
# no chl a data in test dataset so need to find new one
#x <- filter(conventionals, STA_LV1_CODE == "STREAM") %>%
#       filter(!is.na(CHLOROPHYLL)) %>%
#  #distinct(FDT_STA_ID, .keep_all = T) %>%
#  #select(FDT_STA_ID, Huc6_Vahu6, everything())
#  filter(FDT_STA_ID == "2-JMS110.34")

conventionals_HUC<- filter(conventionals, Huc6_Vahu6 %in% 'JU44') %>%
  left_join(dplyr::select(stationTable, FDT_STA_ID, SEC, CLASS, SPSTDS, ID305B_1, ID305B_2, ID305B_3), by='FDT_STA_ID')

AUData <- filter(conventionals_HUC, ID305B_1 %in% 'VAW-I21R_DCK01A06' | 
                   ID305B_2 %in% 'VAW-I21R_DCK01A06' | 
                   ID305B_2 %in% 'VAW-I21R_DCK01A06')%>% 
  left_join(WQSvalues, by = 'CLASS')

x <-filter(AUData, FDT_STA_ID %in% '2-DCK003.94') 


# No Assessment functions bc no std but still need to count samples taken

countchla <- function(x){
  dplyr::select(x,FDT_STA_ID,FDT_DATE_TIME,PHOSPHORUS)%>% # Just get relevant columns
    filter(!is.na(PHOSPHORUS)) %>% #get rid of NA's
    summarize(NUT_CHLA_VIO= NA, NUT_CHLA_SAMP= n(), NUT_CHLA_STAT= NA)
}



chlAPlotlySingleStationUI <- function(id){
  ns <- NS(id)
  tagList(
    wellPanel(
      h4(strong('Single Station Data Visualization')),
      uiOutput(ns('chlA_oneStationSelectionUI')),
      plotlyOutput(ns('chlAplotly'))  )
  )
}


chlAPlotlySingleStation <- function(input,output,session, AUdata, stationSelectedAbove){
  ns <- session$ns
  
  # Select One station for individual review
  output$chlA_oneStationSelectionUI <- renderUI({
    req(#AUdata, 
      stationSelectedAbove)
    #print(unique(c(stationSelectedAbove(),AUdata()$FDT_STA_ID)))
    selectInput(ns('chlA_oneStationSelection'),strong('Select Station to Review'),choices= sort(unique(c(stationSelectedAbove(),AUdata()$FDT_STA_ID))),#unique(AUdata())$FDT_STA_ID,
                width='300px', selected = stationSelectedAbove())})# "2-JMS279.41" )})
  
  chlA_oneStation <- reactive({
    req(ns(input$chlA_oneStationSelection))
    filter(AUdata(),FDT_STA_ID %in% input$chlA_oneStationSelection)})
  
  output$chlAplotly <- renderPlotly({
    req(input$chlA_oneStationSelection, chlA_oneStation())
    dat <- chlA_oneStation()
    dat$SampleDate <- as.POSIXct(dat$FDT_DATE_TIME2, format="%m/%d/%y")
    plot_ly(data=dat)%>%
      add_markers(x= ~SampleDate, y= ~CHLOROPHYLL,mode = 'scatter', name="Chlorophyll a (ug/L)",
                  hoverinfo="text",text=~paste(sep="<br>",
                                               paste("Date: ",SampleDate),
                                               paste("Depth: ",FDT_DEPTH, "m"),
                                               paste("Chlorophyll a: ",CHLOROPHYLL,"ug/L")))%>%
      layout(showlegend=FALSE,
             yaxis=list(title="Chlorophyll a (ug/L)"),
             xaxis=list(title="Sample Date",tickfont = list(size = 10))) })
}


ui <- fluidPage(
  selectInput('stationSelection', 'Station Selection', choices = unique(AUData$FDT_STA_ID)),
  helpText('Review each site using the single site visualization section. There are no WQS for Total Nitrogen.'),
  chlAPlotlySingleStationUI('chlA')
)

server <- function(input,output,session){
  stationData <- eventReactive( input$stationSelection, {
    filter(AUData, FDT_STA_ID %in% input$stationSelection) })
  stationSelected <- reactive({input$stationSelection})
  
  AUData <- reactive({filter(conventionals_HUC, ID305B_1 %in% 'VAW-I21R_DCK01A06' | 
                               ID305B_2 %in% 'VAW-I21R_DCK01A06' | 
                               ID305B_2 %in% 'VAW-I21R_DCK01A06')%>% 
      left_join(WQSvalues, by = 'CLASS')})
  
  callModule(chlAPlotlySingleStation,'chlA', AUData, stationSelected)#input$stationSelection)
  
}

shinyApp(ui,server)

