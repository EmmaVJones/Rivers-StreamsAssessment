#source('appModules/multipleDependentSelectizeArguments.R')

initialList <- list(`site 1` = data.frame(date = c(Sys.Date()-1, Sys.Date()-2,Sys.Date()-3), 
                                          comment = c('comment 1','comment 2','comment 3')),
                    `site 2` = data.frame(date =  c(Sys.Date()-4, Sys.Date()-5,Sys.Date()-6),
                                          comment = c('comment 1','comment 2','comment 3')),
                    `site 3` = data.frame(date = c(Sys.Date()-7, Sys.Date()-8,Sys.Date()-9), 
                                          comment = c('comment 1','comment 2','comment 3')))


shinyApp( 
  ui = fluidPage(
    column(2,
           selectInput('site','select site',choices = names(initialList)),
           #dynamicSelectInput("site1", "select site1", multiple = FALSE),
           selectInput('date','select date', ''),
           #uiOutput('dateSelect'),
           uiOutput("randcomment"),
           br(),
           actionButton("submit", "Submit", icon = icon("refresh"))
           
    ),column(1),
    column(3, verbatimTextOutput("commenttext")), 
    column(3,verbatimTextOutput('comment1'))
  ),
  
  server = function(input, output, session) {
    
    #initialList <- reactive({readRDS('appModules_Testing/testData.RDS')})
    
     reactiveDates <- reactive({initialList[[input$site]]$date})
    
    observe({ updateSelectInput(session, 'date', choices = reactiveDates())})
    
    #output$dateSelect <- renderUI({
    #  req(input$site)
    #  #z <- initialList[[input$site]]
    #  #selectInput('date','select date',choices = reactiveDates()$date) #z$date)
    #  updateSelectInput(session, "inSelect",
    #                    label = paste("Select input label", length(x)),
    #                    choices = x,
    #                    selected = tail(x, 1)
    #})
    
    # Reactive lists -------------------------------------------------------
    # setting the initial value of each to the same value.
    initial_string <- NULL#if(values())
    #NULL#stri_rand_lipsum(1)
    comment_value <- reactiveValues(comment = initial_string, #test(), #initial_string,
                                    submit = initialList)#initial_string)
    
    # Event observers ----------------------------------------------------
    
    # This prevents the comment_value$submit from changing until the 
    # Submit button is clicked. It changes to the value of the input
    # box, which is updated to a random value when the Random Comment
    # button is clicked.
    listObj <- eventReactive(input$submit,{
      initialList[[input$site]] <- rbind(initialList[[input$site]], data.frame(date = Sys.Date(), comment = 'test'))
      
    })
    
    observeEvent(input$submit,
                 {
                   #comment_value$submit <- input$comment#data.frame(date = Sys.Date(), comment = input$comment)
                   comment_value$submit[[input$site]] <- rbind(comment_value$submit[[input$site]], data.frame(date = Sys.Date(), comment = input$comment))
                 }
    )
    
    # Output Components -------------------------------------------------
    # Generate the textAreaInput
    output$randcomment <- renderUI({
      textAreaInput("comment", 
                    label = h3("Enter Course Comment"), 
                    value = comment_value$comment, 
                    height = '300px', 
                    width = '300px')
    })
    
    # Generate the submitted text display
    output$commenttext <- 
      renderPrint({ 
        #comment_value$submit 
        comment_value$submit[[input$site]]
      })
  
    #output$comment1 <- renderPrint({ str(listObj()[[input$site]])})#initialList})
  })
