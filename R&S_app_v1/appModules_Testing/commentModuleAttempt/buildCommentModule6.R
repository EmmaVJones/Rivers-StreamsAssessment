library(shiny)
library(shinyjs)
library(stringi)

shinyApp( 
  ui = fluidPage(
    column(2,
           selectInput('site','select site',choices = c('site 1','site 2','site 3')),
           selectInput('date','select date',choices = c(Sys.Date()-1, Sys.Date()-2,Sys.Date()-3)),
           uiOutput("randcomment"),
           br(),
           actionButton("submit", "Submit", icon = icon("refresh"))
           
    ),column(4),
    column(4, verbatimTextOutput("commenttext"), verbatimTextOutput('comment1'))
  ),
  
  server = function(input, output) {
    
    initialList <- list(`site 1` = data.frame(date = c(Sys.Date()-1, Sys.Date()-2,Sys.Date()-3), 
                                        comment = c('comment 1','comment 2','comment 3')),
                        `site 2` = data.frame(date =  c(Sys.Date()-1, Sys.Date()-2,Sys.Date()-3),
                                        comment = c('comment 1','comment 2','comment 3')),
                        `site 3` = data.frame(date = c(Sys.Date()-1, Sys.Date()-2,Sys.Date()-3), 
                                        comment = c('comment 1','comment 2','comment 3')))
    
    test <- reactive({req(input$site, input$date)
      print(input$date)
      z <- initialList[[input$site]]$input$date #%>% select(comment)
      return(z)})
      #initialList[[input$site]]$comment})
    
    output$comment1 <- renderPrint({
      req(input$site)
      initialList[[input$site]]  })
      #initialList$`site 1`$comment})
    
    # Reactive lists -------------------------------------------------------
    # setting the initial value of each to the same value.
    initial_string <- NULL#if(values())
      #NULL#stri_rand_lipsum(1)
    comment_value <- reactiveValues(comment = initial_string, #test(), #initial_string,
                                    submit = initial_string)
    
    # Event observers ----------------------------------------------------

    # This prevents the comment_value$submit from changing until the 
    # Submit button is clicked. It changes to the value of the input
    # box, which is updated to a random value when the Random Comment
    # button is clicked.
    observeEvent(input$submit,
                 {
                   comment_value$submit <- input$comment
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
      renderText({ 
        comment_value$submit 
      })
  }
)