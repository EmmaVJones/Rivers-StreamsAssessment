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
                                          h3('Tool Overview'),
                                          p("The Rivers and Streams Assessment Tool is designed to expedite analysis, assessment
                                            decisions, and quality assurance/quality control (QA/QC) procedures for Virginia's
                                            contribution to the 2020 Integrated Report (IR). The data window analyzed covers 
                                            January 1, 2013 to December 31, 2018. Tool users can expect significant time savings
                                            on repetitive procedures including: raw data organization from disparate databases, 
                                            geospatial organization of stations by assessment unit, standard/criteria calculations, 
                                            and data visualization."),
                                          br(),br(),br(),
                                          h3('Tool Inputs'),
                                          p('In order to reduce processing time and facilitate peristent data storage, users must
                                            upload certain datasets that follow a specified template. These include their regional
                                            Stations Table 2.0 and Comment Files from any previous analysis session.'),
                                          h5('Stations Table 2.0'),
                                          helpText('This dataset is derived before any Rivers and Streams Assessment Tool analysis 
                                                   procedures can commence using the ',span(strong('XXXXXX TOOL.')), 'After completing
                                                   the requisite analyses from the ',span(strong('XXXXXX TOOL')),'once, users can 
                                                   upload their results to the Rivers and Streams Assessment Tool each time they open
                                                   the tool for analysis.'),
                                          fileInput('stationsTable','Choose your Regional Stations Table 2.0.',
                                                    accept = c(".RDS")),
                                          h5('Comment Files'),
                                          helpText('Comment files are generated each time an assessor utlizes the Rivers and Streams 
                                                   Assessment Tool comment fields. Though entering information into these fields is not
                                                   necessary to complete any assessment actions, it is a best practice to include
                                                   information relevant to assessment decisions or historical information for archiving
                                                   in a single location. These files will be transferred with subsequent Rivers and 
                                                   Streams Assessment Tool updates across IR windows to maintain regional assessment records.'),
                                          fileInput('commentFile','Choose most recent comment file.',
                                                    accept = c(".csv"))),
                                 tabPanel('Watershed Selection',
                                          sidebarPanel(
                                            h3("Various drop downs to choose VAHUC6 assessor wants to start with"),
                                            dynamicSelectInput("DEQregionSelection", "Select DEQ Assessment Region", multiple = FALSE),
                                            dynamicSelectInput("basinSelection", "Select Major Basin", multiple = FALSE),
                                            
                                            selectInput('DEQregion',"DEQ Region",
                                                        choices=c('BRRO','SWRO','VRO','etc')),
                                            selectInput('majorBasin','Major Basin',
                                                        choices = c('James','Roanoke','etc')),
                                            selectInput('assessmentUnit','VAHUC6',
                                                        choices = c('JU01','JU02','JU03','etc')),
                                            actionButton('selectAU','Select Watershed for analysis')),
                                          mainPanel(
                                            
                                            tableOutput("table"),
                                            
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
                                            
                                                        
                                            
                                            