library(shiny)
library(shinyjs)
library(stringi)

shinyApp( 
  ui = fluidPage(
    column(2,
           uiOutput("randcomment"),
           br(),
           div(
             actionButton("randtext", "Random Comment", icon = icon("quote-right")),
             div(actionButton("submit", "Submit", icon = icon("refresh")), style="float:right")
           )
           
    ),
    column(4, div(verbatimTextOutput("commenttext"), style = 'margin-top: 2cm;'))
  ),
  server = function(input, output) {
    
    # Reactive lists -------------------------------------------------------
    # setting the initial value of each to the same value.
    initial_string <- stri_rand_lipsum(1)
    comment_value <- reactiveValues(comment = initial_string,
                                    submit = initial_string)
    
    # Event observers ----------------------------------------------------
    observeEvent(input$randtext,
                 {
                   comment_value$comment <- stri_rand_lipsum(1)
                 }
    )
    
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