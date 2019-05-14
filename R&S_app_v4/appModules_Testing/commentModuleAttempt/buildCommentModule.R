

#commentVersionControlUI <- function(id){
#  ns <- NS(id)
#  tagList(
#    wellPanel(
#      selectInput(ns('stations'), 'Stations', choices = c('station1','station2','station3'))),
#      textAreaInput(ns('commentBox'), label = NULL, height = 275, 
#                    placeholder = 'Comments'),
#      actionButton(ns('commentCommit'),'Save Comment'),
#    verbatimTextOutput(ns('outputArea')),
#    verbatimTextOutput(ns('outputArea2'))
#    )
#}

#commentVersionControl <- function(input,output,session, startingList){
#  ns <- session$ns
#  
#  # Select One station for individual review
#  commentList <- reactive({
#    req(input$stations, input$commentBox)
#    startingList
    
    #z <- list(list(name = input$stations, 
    #          data = input$commentBox))
    #names(z) <- input$stations
#  })
  
  
  
  
#  output$outputArea <- renderText({ commentList() })
  #output$outputArea2 <- renderText({ commentList()[1][[1]] })
#  temperature_oneStation <- reactive({
#    req(ns(input$temperature_oneStationSelection))
#    filter(AUdata(),FDT_STA_ID %in% input$temperature_oneStationSelection)})
#}


ui <- fluidPage(
  wellPanel(
    selectInput('stations', 'Stations', choices = c('station1','station2','station3'))),
  textAreaInput('commentBox', label = NULL, height = 275, 
                placeholder = 'Comments'),
  actionButton('commentCommit','Save Comment'),
  verbatimTextOutput('outputArea'),
  verbatimTextOutput('outputArea2')
  #commentVersionControlUI('userComments')
)

server <- function(input,output,session){
  comments <- reactiveValues(template = list(date = NA, userComment = NA))
  
  observe(comments$comment1 <- input$commentBox)
  
  
  
  commentList <- reactive({
    req(input$stations, input$commentBox)
    list(station1 = list(a= 'dalkfjladsk'),
         station2 = list(a = 'dkl'),
         station3 = list(a = 'a')) })
  
  output$outputArea <- renderText({ str(comments) })
}
  
shinyApp(ui,server)



  #commentListSaved <- reactive({readRDS('Comments/commentList.RDS')})
  
  #commentList <- reactive({readRDS('Comments/commentList.RDS')})
  
  
  #comments <- reactive({
  #  commentList <- list(stationName = list(input$commentBox)) })
  
  
  #observeEvent(input$commentCommit,{
  #  saveRDS(comments(),'Comments/commentList.RDS')
  #})
  
  #output$commentTest <- renderPrint({commentList})
 # callModule(commentVersionControl,'userComments',startingList)
  

  
#}


