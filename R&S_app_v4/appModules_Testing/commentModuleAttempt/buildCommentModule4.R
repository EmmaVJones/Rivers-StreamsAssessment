#library(shiny)

start <- data.frame(station= 'test1', commentDate = 'date1', comment ='this is where we start')

ui <- fluidPage(
  sidebarPanel(textInput("control_label",
                         "This controls some of the labels:",
                         "LABEL TEXT")),
    ##uiOutput('box'),
               ###textAreaInput('commentBox', label = NULL, height = 275, 
               ###               placeholder = 'Comments'),
               ##actionButton("update", "Update values")),
  mainPanel(verbatimTextOutput("example"),
            verbatimTextOutput("example2"))
)

server <- function(input, output, session) {
  
  observe({
  c_label <- input$control_label
  
  # Text =====================================================
  # Change both the label and the text
  updateTextInput(session, "inText",
                  label = paste("New", c_label),
                  value = paste("New text", c_label)
  )
  })
  
  
}

shinyApp(ui = ui, server = server)
