#library(shiny)
ui <- fluidPage(
  sidebarPanel(uiOutput('box'),
    #textAreaInput('commentBox', label = NULL, height = 275, 
              #               placeholder = 'Comments'),
               actionButton("update", "Update values")),
  mainPanel(verbatimTextOutput("example"))
)

server <- function(input, output) {
  
  # stores the current data frame, called by values() and set by values(new_data_table)
  values <- reactiveVal(list(a='this is where we start'))
  
  output$box <- renderUI({ 
    textAreaInput('commentBox', label = NULL, height = 275, value = values()$a)
                  #placeholder = values()$a)
  })
  
  # update values table on button click
  observeEvent(input$update,{
    
    old_values <- values()
    
    A_new <- input$commentBox
    
    #new_values <- data.frame(A=A_new)
    
    # attach the new line to the old data frame here:
    #new_df <- rbind(old_values, new_values)
    
    #store the result in values variable
    #values(new_df)
    values(A_new)
    
  })
  
  # Print the content of values$df
  output$example <- renderPrint({
    return(values())
  })
  
}

shinyApp(ui = ui, server = server)
