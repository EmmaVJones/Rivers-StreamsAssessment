ui <- fluidPage(
  #sliderInput("controller", "Controller", 0, 20, 10),
  textAreaInput("inText", "Input textarea"),
  textAreaInput("inText2", "Input textarea 2")
)

server <- function(input, output, session) {
  observe({
    # We'll use the input$controller variable multiple times, so save it as x
    # for convenience.
    x <- input$inText
    
    # This will change the value of input$inText, based on x
    updateTextAreaInput(session, "inText", value = paste("New text", x))
    
    # Can also set the label, this time for input$inText2
    updateTextAreaInput(session, "inText2",
                        label = 'test',
                        value = paste("New text", x))
  })
}

shinyApp(ui, server)
