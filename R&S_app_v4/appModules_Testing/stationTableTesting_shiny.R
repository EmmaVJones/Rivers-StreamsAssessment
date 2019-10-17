
ui <- fluidPage(
  uiOutput('selection'),
  br(),hr(),br(),
  verbatimTextOutput('verbatim')     )

server <- function(input,output,session){
  
  stationData <- eventReactive( input$stationSelection, {
    filter(AUData(), FDT_STA_ID %in% input$stationSelection) })
  stationSelected <- reactive({input$stationSelection})
  
  
  AUData <- reactive({filter(conventionals_HUC, FDT_STA_ID %in% c('4APKP-4-DRBA','2-JKS018.68','2CES-VT22-UVA'))})
  
  output$selection <- renderUI({
    req(AUData)
    selectInput('stationSelection', 'select',choices=unique(AUData()$FDT_STA_ID))
  })
  
  citmonAllDataByLevel <- reactive({
    # Only do this if citmon or NonAgency data
    if(unique(stationData()$STA_LV3_CODE) %in% c("CMON", "NONA", "NONA ")){
      a <- 12:117
      n <- length(a)
      
      holder <- NA
      
      for(i in a[seq(1,n,2)]){ 
        x <- stationData()[,c(i, i+1)]
        holder[i] <- countSamplesAtLevels(x)
      }
      return(paste0(holder[!is.na(holder)], collapse = '; '))
    }
  })
  
  output$verbatim <- renderPrint({
    citmonAllDataByLevel()
  })
  
}

shinyApp(ui,server)


collapseAllDataByLevel <- function(x, colNumbers){
  # Only do this if citmon or NonAgency data
  if(unique(x$STA_LV3_CODE) %in% c("CMON", "NONA", "NONA ")){
    #skip every other column (remarks)
    a <- colNumbers#12:117#(ncol(stationData)-2)
    n <- length(a)
    
    holder <- NA
    
    for(i in a[seq(1,n,2)]){ 
      x <- x[,c(i, i+1)]
      holder[i] <- countSamplesAtLevels(x)
    }
    
    return(paste0(holder[!is.na(holder)], collapse = '; ')) 
  } 
}

