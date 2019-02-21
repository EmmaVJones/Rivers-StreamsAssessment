library(editData)
library(tidyverse)
library(shiny)

testData1 <- tooMany[1:3,]# %>% st_set_geometry(NULL)
testData2 <- tooMany[4:5,]# %>% st_set_geometry(NULL)
testData3 <- tooMany[6:7,]# %>% st_set_geometry(NULL)

siteWithTooMany <- filter(BRRO_Sites_noAU, FDT_STA_ID %in% unique(tooMany$`Point Unique Identifier`)) %>%
  st_transform(4326)# project to WGS84 for plotting



ui <- fluidPage(
  selectInput("data","Select data to edit",choices=c("testData1", "testData2","testData3"),multiple=FALSE),
  #verbatimTextOutput('segmentData'),
  leafletOutput('stationMap', height = 400, width = 400),
  uiOutput("editUI")
  
)
server <- function(input, output) {
  
  segments <- reactive({
    req(input$data)
    get(input$data)
  })
  
  output$segmentData <- renderPrint({
    req(segments())
    str(segments())
  })
  
  #output$stationMap <- renderLeaflet({
  #    req(segments())
  #  map1 <- mapview(segments(), label= segments()$ID305B, layer.name = c('AUs in Selected HUC6'), zcol = "ID305B", legend=FALSE,
  #                                    popup= popupTable(segments(), zcol=c("ID305B","MILES","CYCLE","WATER_NAME","LOCATION" )))
  #  map1@map
  #})

  
  
  output$editUI=renderUI({
    data<-uiname<-result<-mylist<-textname<-list()
    
    count=length(input$data)
    
    if(count>0) {
      for(i in  1:count){
        
        
        
        data[[i]]<-get(input$data[i])%>% st_set_geometry(NULL)
        uiname[[i]]<-paste0("table",i)
        title=paste0("File No:",i)
        mylist[[3*i-2]]<-h2(title)
        mylist[[3*i-1]]<-editableDTUI(uiname[[i]])
        textname[[i]]=paste0("text",i)
        mylist[[3*i]]<-verbatimTextOutput(textname[[i]])
        
        local({
          j<-i
          result[[j]]=callModule(editableDT,uiname[[j]],data=reactive(data[[j]]))
          
          output[[textname[[j]]]]=renderPrint({
            head(result[[j]]())
          })
        })
      }
      do.call(tagList,mylist)
    }
    
  })
  
}
shinyApp(ui, server)


#output$stationMap <- renderLeaflet({
#  req(input$data)
#  segments <- get(input$mydata)
#  print(segments)
#  point <- filter(siteWithTooMany, FDT_STA_ID %in% segments$`Point Unique Identifier`)
#  print(point)
#  map1 <- mapview(z, label= segments$ID305B, layer.name = c('AUs in Selected HUC6'), zcol = "ID305B", legend=FALSE,
#                  popup= popupTable(segments, zcol=c("ID305B","MILES","CYCLE","WATER_NAME","LOCATION" ))) +
#    mapview(point, color = 'yellow', lwd = 5, label= point$FDT_STA_ID, layer.name = c('Selected Station'),
#            popup=NULL)
#  
#  
#  map1@map
#})

