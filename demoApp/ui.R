# Run in R 3.5.1
source('global.R')

shinyUI(fluidPage(theme="yeti.css",
                  shinyjs::useShinyjs(),
                  div(
                    id = "loading_page",
                    h1("Loading...")
                  ),
                  hidden(
                    div(
                      id = "main_content",
                      navbarPage("VDEQ 2020 Rivers and Streams Assessment Tool",
                                 tabPanel('Data Upload',
                                          p("This tab will walk users through uploading
                                            data back to the app. This data may include
                                            Roger's raw data (conventionals), WQS shapefile,
                                            comment files from previous sessions, etc.")),
                                 tabPanel('Watershed Selection',
                                          sidebarPanel(
                                            h3("Various drop downs to choose VAHUC6 assessor wants to start with"),
                                            selectInput('DEQregion',"DEQ Region",
                                                        choices=c('BRRO','SWRO','VRO','etc')),
                                            selectInput('majorBasin','Major Basin',
                                                        choices = c('James','Roanoke','etc')),
                                            selectInput('assessmentUnit','VAHUC6',
                                                        choices = c('JU01','JU02','JU03','etc')),
                                            actionButton('selectAU','Select Watershed for analysis')),
                                          mainPanel(
                                            leafletOutput('VAmap', height =400, width = 650),
                                            helpText('This map will update based on user selection, 
                                                     highlighting the VAHUC6 chosen, all stations that fall
                                                     into VAHUC6, WQS layer can be toggled on/off behind it.'),
                                            DT::dataTableOutput('allStationsInVAHUC6'))),
                                 tabPanel('Individual Station Review',
                                          fluidRow(column(4,h4('User Selected Assessment Unit:'), 
                                                          h5(strong('PRINT ASSESSMENT UNIT NAME HERE'))),
                                                   column(4,selectInput('userStation','Station Selection', 
                                                                        choices = c('Choices Based On VAHUC6 Selected','station 1','station 2')),
                                                          helpText('Maybe even include non Agency data if we get there with this 
                                                                        app iteration')),
                                                   column(4,actionButton('retrieveData',label='Retrieve VAHUC6 Data'))),
                                          hr(),
                                          fluidRow(
                                            column(4,h4('Table with information on station including: station type, applicable WQS, etc.'),
                                                   DT::dataTableOutput('stationInfo')),
                                            column(4,
                                                   leafletOutput('stationMap', height = 300, width = 300),
                                                   helpText('Maybe an interactive map here with WQS segment highlighted and station overlay?')),
                                            column(4,h4('Comments:'), 
                                                   textAreaInput('commentBox', label = NULL, height = 275,
                                                             placeholder = 'Comment box for assessor to insert station comments as they are assessing.'),
                                                   #actionButton('saveComment',label = 'Save Comment'),
                                                   submitButton("  Save Comment", icon("far fa-save")))),
                                          br(),br(),
                                          tabsetPanel(
                                            tabPanel('Raw Data',br(),
                                                     p('raw data table and general station sampling statistics: n samples taken in window')),
                                            
                                            tabPanel('Field Data',
                                                     tabsetPanel(
                                                       tabPanel('Temperature'),
                                                       tabPanel('Dissolved Oxygen'),
                                                       tabPanel('pH'),
                                                       tabPanel('Conductivity'))),
                                            tabPanel('Nutrients',
                                                     tabsetPanel(
                                                       tabPanel('Total Phosphorus'),
                                                       tabPanel('Total Nitrogen'),
                                                       tabPanel('Ammonia'))),
                                            tabPanel('Bacteria'),
                                            tabPanel('Macroinvertebrates',
                                                     p("Maybe even generate EDAS Biologist reports here for assessor to view from a few 
                                                       tables exported from EDAS using Lucy's Rmd code?")),
                                            tabPanel('Metals'),
                                            tabPanel('Sediment',
                                                     tabsetPanel(
                                                        tabPanel('Sediment metals'))),
                                            tabPanel('Fish Tissue'),
                                            tabPanel('Toxics'),
                                            tabPanel('Summary',
                                                     tabsetPanel(
                                                        tabPanel('Overall Summary',
                                                                 p('Make .Rmd format to easily export mini factsheet to PDF with each parameter
                                                                   statistics, map, assessor comments from above, category, use, etc?')),
                                                        tabPanel('Some other sort of summary',
                                                                 p('Something else more official??'))))
                                          )
                                 ))))
))
                                            
                                                        
                                            
                                            