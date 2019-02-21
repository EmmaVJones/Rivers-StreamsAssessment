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
                      # suppress error messages as data loads, hacky
                      tags$style(type="text/css",
                                 ".shiny-output-error { visibility: hidden; }",
                                 ".shiny-output-error:before { visibility: hidden; }"
                      ),
                      navbarPage(paste("VDEQ ",assessmentCycle," Rivers and Streams Assessment Tool", sep=''),
                                 tabPanel('Data Upload',
                                          h3('Tool Overview'),
                                          p("The Rivers and Streams Assessment Tool is designed to expedite analysis, assessment
                                            decisions, and quality assurance/quality control (QA/QC) procedures for Virginia's
                                            contribution to the 2020 Integrated Report (IR). The data window analyzed covers 
                                            January 1, 2013 to December 31, 2018. Tool users can expect significant time savings
                                            on repetitive procedures including: raw data organization from disparate databases, 
                                            geospatial organization of stations by assessment unit, standard/criteria calculations, 
                                            and data visualization."),
                                          p('The application represents the first iteration of an automated assessment tool. The datasets
                                            and parameters chosen for analysis readily lend themselves to automated processing. Future versions
                                            of the tool may include additional datasets and analyses.'),
                                          p('For feedback, troubleshooting, and questions regarding analyses or missing data, please contact 
                                            Emma Jones (emma.jones@deq.virginia.gov)'),
                                          br(),
                                          h3('Tool Inputs'),br(),
                                          h4('Prepopulated Tool Inputs'),
                                          p("Some data sources are compiled for users and programmed into the app, requiring no user manipulation.
                                            These datasets include: "),
                                          tags$ul(
                                            tags$li('Conventionals- CEDS data pulled and filtered by Roger Stewart (roger.stewart@deq.virginia.gov) for each 
                                            Integrated Report data window.'), 
                                            tags$li('Water Column and Sediment Metals- CEDS data pulled, filtered, and analyzed by Roger Stewart for each 
                                            Integrated Report data window.'), 
                                            tags$li("Statewide Assessment (spatial) layer- The spatial dataset that identifies each regional office's watersheds."),
                                            tags$li("EDAS- Statewide macroinvertebrate data and stream condition indices are pulled and organized from the 
                                            Integrated Report data window. Contact Jason Hill (jason.hill@deq.virginia.gov) or your regional
                                                    biologist with dataset questions.")  ),
                                          br(),
                                          h4('User Defined Tool Inputs'),
                                          p('In order to reduce processing time and facilitate peristent data storage, users must
                                            upload certain datasets that follow a specified template. These include their regional
                                            Stations Table 2.0 and Regional Assessment Unit shapefiles.'),
                                          h5('Stations Table 2.0'),
                                          helpText('This dataset is derived before any Rivers and Streams Assessment Tool analysis 
                                                   procedures can commence using the ',span(strong('Assessment Unit and WQS Snapping Tool.')), 
                                                   'After completing the requisite analyses from the ',
                                                   span(strong('Assessment Unit and WQS Snapping Tool')),'once, users can 
                                                   upload their results to the Rivers and Streams Assessment Tool each time they open
                                                   the tool for analysis.'),
                                          fileInput('stationsTable','Choose your Regional Stations Table 2.0.',
                                                    accept = c(".csv")),
                                          h5('Regional Assessment Units'),
                                          helpText(span('This shapefile is the current working copy of the regional assessment units.',
                                                   strong('It will be uploaded to the app on startup for you to avoid shiny application 
                                                           data upload limits.'), ' Any time any spatial changes (performed in ArcGIS on
                                                   the file soured by the application) to the assessment units (e.g. split an assessment 
                                                   unit) are saved', strong('and the application is re-launched'), 'these changes will be
                                                   reflected in the Rivers and Streams Assessment Tool.')),
                                          h6(strong('To view or change the location of the Regional Assessment Units shapefile sourced
                                                    by the application, see the AUshapefileLocation.R script.')),
                                          #fileInput('regionalAUshapefile','Choose your Regional Assessment Unit shapefile.',
                                          #          accept = c(".dbf",".prj",".sbn",".sbx",".shp","shp.xml",".shx")),
                                #          h5('Comment Files'),
                                #          helpText('Comment files are generated each time an assessor utlizes the Rivers and Streams 
                                #                   Assessment Tool comment fields. Though entering information into these fields is not
                                #                   necessary to complete any assessment actions, it is a best practice to include
                                #                   information relevant to assessment decisions or historical information for archiving
                                #                   in a single location. These files will be transferred with subsequent Rivers and 
                                #                   Streams Assessment Tool updates across IR windows to maintain regional assessment records.'),
                                #          fileInput('commentFile','Choose most recent comment file.',
                                #                    accept = c(".csv")),
                                          hr(),br(),
                                          h4('Station Table QA'),
                                          helpText('This automated analysis section highlights any stations from the Conventionals dataset
                                                   that do not have appropriate Station Table 2.0 information.',
                                                   span(strong('Failure to address these stations will guarantee they will not be represented
                                                               in subsequent analyses by the Rivers and Streams Assessment tool.'))),
                                          DT::dataTableOutput('stationTableMissingStations')),
                                 tabPanel('Watershed Selection',
                                          sidebarPanel(
                                            dynamicSelectInput("DEQregionSelection", "Select DEQ Assessment Region", multiple = FALSE),
                                            dynamicSelectInput("basinSelection", "Select Major Basin", multiple = FALSE),
                                            dynamicSelectInput("HUC6Selection", "Select VAHU6", multiple = FALSE),
                                            br(),
                                            actionButton('reviewAUs',"Preview Assesment Units",class='btn-block')),
                                          mainPanel(
                                            leafletOutput('VAmap'),
                                            br(),
                                            h5('Assessment Units in Selected VAHU6'),
                                            DT::dataTableOutput('AUSummary'),
                                            h5('Stations in Selected VAHU6'),
                                            DT::dataTableOutput('stationSummary')
                                          )
                                 ),
                                tabPanel('Assessment Unit Review',
                                         fluidRow(column(9, DT::dataTableOutput('selectedHUC')),
                                                  column(3,br(),actionButton('pullHUCdata','Select Watershed for analysis'))),
                                         hr(),
                                         uiOutput('AUSelection_'),
                                         DT::dataTableOutput('selectedAU'),br(),
                                         uiOutput('stationSelection_'),
                                         fluidRow(column(4, DT::dataTableOutput('stationInfo')),
                                                  column(4, leafletOutput('stationMap', height = 300, width = 300)),
                                                  column(4, DT::dataTableOutput('stationHistoricalInfo'))),
                                         hr(),
                                         h3('Station Results for Review'),
                                         helpText('This table outputs the site specific results for direct export to the Station Table. It also serves to highlight
                                                  where exceedances are present and should be reviewed in the individual parameter visualization tabs below.'),
                                         h4('Official Station Results Table'),
                                         helpText('Note that WAT_TOX_VIO AND WAT_TOX_STAT are only reflecting ammonia analysis.'),
                                         DT::dataTableOutput('stationTableDataSummary'),
                                         #h4('Additional Results'),
                                         #DT::dataTableOutput('stationTableDataSummary2'),
                                         br(),hr(),br(),
                                         h3('Assessment Unit Raw Data Review and Visualization'),
                                         tabsetPanel(
                                           tabPanel('Conventionals Data',
                                                    tabsetPanel(
                                                      tabPanel('Raw Data',br(),
                                                               DT::dataTableOutput('AURawData'),
                                                               h4('Data Summary'),
                                                               h5('Records Retrieved in Assessment Unit:'),
                                                               fluidRow(column(1),column(10,textOutput('stationDataTableRecords'))),
                                                               h5('Field and Lab Data in Assessment Window:'),
                                                               fluidRow(column(1),column(10,tableOutput('uniqueStationDataTableRecords'))),
                                                               h5('Assessment Window:'),
                                                               fluidRow(column(1),column(10,textOutput('stationDataTableAssessmentWindow'))), br(),br()),
                                                      tabPanel('Temperature',
                                                               helpText('Review each site using the single site visualization section. The results from this analysis are reflected
                                                                        in the TEMP_VIO, TEMP_SAMP, and TEMP_STAT columns in the station table.'),
                                                               temperaturePlotlySingleStationUI('temperature')),
                                                               #br(),hr(),br(),
                                                               #temperatureExceedanceAnalysisUI('temperature_ExceedanceAnalysis')),
                                                      tabPanel('pH',
                                                               helpText('Review each site using the single site visualization section. The results from this analysis are reflected
                                                                        in the PH_VIO, PH_SAMP, and PH_STAT columns in the station table.'),
                                                               pHPlotlySingleStationUI('pH')),
                                                               #br(),hr(),br(),
                                                               #pHExceedanceAnalysisUI('pH_ExceedanceAnalysis')),
                                                      tabPanel("DO",
                                                               helpText('Review each site using the single site visualization section. The results from this analysis are reflected
                                                                        in the DO_VIO, DO_SAMP, and DO_STAT columns in the station table.'),
                                                               DOPlotlySingleStationUI('DO')),
                                                               #br(),hr(),br(),
                                                               #DOExceedanceAnalysisUI('DO_ExceedanceAnalysis')),
                                                      tabPanel("Specific Conductance",
                                                               helpText('Review each site using the single site visualization section. There are no WQS for Specific Conductivity.'),
                                                               SpCondPlotlySingleStationUI('SpCond')),
                                                      tabPanel("Salinity",
                                                               helpText('Review each site using the single site visualization section. There are no WQS for Salinity.'),
                                                               salinityPlotlySingleStationUI('salinity')),
                                                      tabPanel("Total Nitrogen",
                                                               helpText('Review each site using the single site visualization section. There are no WQS for Total Nitrogen.'),
                                                               TNPlotlySingleStationUI('TN')),
                                                      tabPanel("Ammonia",
                                                               helpText('Review each site using the single site visualization section. The results from this analysis are reflected
                                                                        in the WAT_TOX_VIO and WAT_TOX_STAT columns in the station table.'),br(),
                                                               AmmoniaPlotlySingleStationUI('Ammonia')),
                                                      tabPanel("Total Phosphorus",
                                                               helpText('Review each site using the single site visualization section. There are no WQS for Total Phosphorus; however,
                                                                        exceedances of 0.2 mg/L are noted in a table below the plot. Additionally, the sample counts are reflected in 
                                                                        the NUT_TP_SAMP column in the station table.'),
                                                               TPPlotlySingleStationUI('TP')),
                                                      tabPanel("Fecal Coliform",
                                                               helpText('Review each site using the single site visualization section.'),
                                                               fecalPlotlySingleStationUI('fecal')),
                                                      tabPanel("E. Coli",
                                                               helpText('Review each site using the single site visualization section. Both the old and the new E. coli assessment
                                                                        method statistics are presented in the station visualization section. The results from the old analysis method are reflected
                                                                        in the ECOLI_VIO, ECOLI_SAMP, and ECOLI_STAT columns in the station table.'),
                                                               EcoliPlotlySingleStationUI('Ecoli')),
                                                      tabPanel("Enterococci",
                                                               helpText('Review each site using the single site visualization section. Both the old and the new Enteroccoci assessment
                                                                        method statistics are presented in the station visualization section. The results from the old analysis method are reflected
                                                                        in the ENTEROCCOCI_VIO, ENTEROCCOCI_SAMP, and ENTEROCCOCI_STAT columns in the station table.'),
                                                               EnteroPlotlySingleStationUI('Entero')),
                                                      tabPanel("Chlorophyll a",
                                                               helpText('Review each site using the single site visualization section. Chlorophyll a standards only apply to some stations.
                                                                        However, the sample counts are reflected in the NUT_CHLA_SAMP column in the station table.'),
                                                               chlAPlotlySingleStationUI('chlA')),
                                                      tabPanel("Suspended Sediments",
                                                               helpText('Review each site using the single site visualization section.'),
                                                               SSCPlotlySingleStationUI('SSC')),
                                                      tabPanel("Nitrate",
                                                               helpText('Review each site using the single site visualization section. Nitrate criteria only apply to stations with PWS designation.'),
                                                               NitratePlotlySingleStationUI('Nitrate')),
                                                      tabPanel("Chloride",
                                                               helpText('Review each site using the single site visualization section. Chloride criteria only apply to stations with PWS designation.'),
                                                               ClPlotlySingleStationUI('Cl')),
                                                      tabPanel("Sulfate",
                                                               helpText('Review each site using the single site visualization section. Total and dissolved sulfate are presented. PWS standards for Total Sulfate only apply to some stations.'),
                                                               DSulfatePlotlySingleStationUI('DSulfate'))
                                                      )),
                                           tabPanel('EDAS Data', 
                                                    helpText('Review each site using the single site visualization section. If no benthic information is presented, then there is no macroinvertebrate data for
                                                             the station. Always review biologist fact sheets to assist assessment decisions.'),
                                                    BenthicsPlotlySingleStationUI('Benthics')
                                           ),
                                           tabPanel('Metals Data', 
                                                    helpText('Review each site using the single site visualization section.'),
                                                    metalsTableSingleStationUI('metals'))#,
                                           #tabPanel('Toxics Data', 
                                          #          tabsetPanel(
                                          #            tabPanel('Fish Tissue Data'),
                                          #            tabPanel('Water Column Toxics'),
                                          #            tabPanel('Sediment Toxics')))
                                             
                                         )
                                         
                                           
                                         )))
                    )))


#verbatimTextOutput("table"),