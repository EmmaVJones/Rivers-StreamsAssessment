source('global.R')
source('modules.R')
source('newBacteriaStandard_workingUpdatedRecSeason.R')



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
                               tabPanel('About',
                                        p('This tool is analyzes bacteria data from the conventionals dataset using both the old and new bacteria standards. This app
                                          is a standalone app for users who do not want to use the full Lakes Assessment App or Riverine Assessment App. Pooling of 
                                          bacteria data (for lake assessments) is not covered in this application and is only addressed in the Lakes Assessment app.'),
                                        h5('For app troubleshooting, contact Emma Jones (emma.jones@deq.virginia.gov) .'),
                                        br(),br(),hr(),br(),
                                        h4('To use the app, navigate to the Assessment tab and choose a station to analyze. If no bacteria data is available for the 
                                           station in the assessment window, no plots or analyses will be presented. If bacteria data was collected in the window for the 
                                           station chosen, then the E. coli, Enterococci, and Stations Table Stats sub tabs will be populated. Users need to read the on
                                           screen instructions to understand the analyses being completed and data presented for full understanding. ') ),
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
                                          ))
                               
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
