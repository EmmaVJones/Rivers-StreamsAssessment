
```{r fix tooMany}

plot(tooMany['Point Unique Identifier'][1,])
plot(filter(BRRO_Sites, FDT_STA_ID %in% tooMany[1,]$`Point Unique Identifier`)['FDT_STA_ID'])

library(mapview)
library(leaflet)
z <- filter(tooMany, `Point Unique Identifier` =='2-APP143.57')

mapview(z)
mapview(tooMany) + mapview(BRRO_Sites_noAU)

tooMany <- snapCheck(snapList_AU[['sf_output']] ) %>%
  st_transform(4326)# project to WGS84 for plotting

siteWithTooMany <- filter(BRRO_Sites_noAU, FDT_STA_ID %in% unique(tooMany$`Point Unique Identifier`)) %>%
  st_transform(4326)# project to WGS84 for plotting


leaflet(tooMany) %>% 
  addProviderTiles(providers$Esri.NatGeoWorldMap,group='Nat Geo World Map') %>%
  addProviderTiles(providers$Esri.WorldImagery,group='Esri World Imagery') %>%
  addProviderTiles(providers$OpenStreetMap,group='Open Street Map') %>%
  addPolylines(data=tooMany, group='WQS',
               color = ~colorNumeric(c("red", "green", "blue",'yellow'),OBJECTID)(OBJECTID),
               popup=popupTable(tooMany,zcol=c('Point Unique Identifier','Buffer Distance',
                                               "ID305B","MILES","CYCLE","WATER_NAME","LOCATION",
                                               "AU_COMMENT","CATEGORY","IMP_CAUSE","SOURCE",
                                               "AQUA_LIFE","DEEP_CHANN","DEEP_WATER","FISH_CONSU",
                                               "MIGRATORY","OPEN_WATER","PWS","RECREATION","SHELLFISH","SW_SAV","WILDLIFE"))) %>%
  addMarkers(data=siteWithTooMany,~Longitude,~Latitude,#~geometry[[1]][1],~geometry[[1]][1], 
             popup = siteWithTooMany$FDT_STA_ID, group='point') %>%
  addLayersControl(baseGroups=c('Nat Geo World Map','Esri World Imagery','Open Street Map'),
                   overlayGroups = c('WQS','point'),
                   options=layersControlOptions(collapsed=T),
                   position='topleft') 


mapview(tooMany) + mapview(BRRO_Sites_noAU)
m <- mapview(tooMany, color = 'yellow',lwd= 5, label= NULL, layer.name = c('Selected HUC6'),
             popup= popupTable(huc6_filter(), zcol=c('VAHU6',"VaName","VAHU5","ASSESS_REG"))) + 
  mapview(tooMany[1:4,], label= tooMany$ID305B, layer.name = c('AUs in Selected HUC6'), zcol = "ID305B", legend=FALSE,
          popup= popupTable(tooMany, zcol=c("ID305B","MILES","CYCLE","WATER_NAME","LOCATION" )))
m@map 

```







```


editData package to prevent making an app???
  https://cran.r-project.org/web/packages/editData/README.html

```{r editData solution???}
install.packages("editData")
library(editData)

mtcarsnew <- editData(mtcars )
```

```{r now with spatial data}
mapview(tooMany[1:3,], label= tooMany$ID305B, layer.name = c('AUs in Selected HUC6'), zcol = "ID305B", legend=FALSE,
        popup= popupTable(tooMany, zcol=c("ID305B","MILES","CYCLE","WATER_NAME","LOCATION" ))) +
  mapview(siteWithTooMany[1,], color = 'yellow', lwd = 5, label= siteWithTooMany[1,]$FDT_STA_ID, layer.name = c('Selected Station'),
          popup=NULL)


site1results <- editData(tooMany[1:3,] %>% st_set_geometry(NULL))


shiny::runApp(system.file('example',package='editData'))
shiny::runApp(system.file('multipleData',package='editData'))
shiny::runApp(system.file('multipleFiles',package='editData'))
```


```{r wrap it in a loop?}

for(i in 1:3){
  out <- list()
  siteAtHand <- filter(tooMany, `Point Unique Identifier` %in% unique(tooMany$`Point Unique Identifier`)[i]) %>% st_set_geometry(NULL)
  out[[i]] <- editData(siteAtHand)
}

out
```
Negative.

Next thought, shiny minipage with mapview map
https://shiny.rstudio.com/articles/gadget-ui.html
https://www.rstudio.com/resources/webinars/shiny-gadgets-interactive-tools/
  https://rpubs.com/seandavi/bioc2016_gadgets

```{r shiny minipage}
library(shiny)
library(miniUI)

myGadgetFunc <- function(inputValue1, inputValue2) {
  ui <- miniPage(
    gadgetTitleBar("My Gadget"),
    miniContentPanel(
      # Define layout, inputs, outputs
    )
  )
  
  server <- function(input, output, session) {
    # Define reactive expressions, outputs, etc.
    
    # When the Done button is clicked, return a value
    observeEvent(input$done, {
      returnValue <- ...
      stopApp(returnValue)
    })
  }
  runGadget(ui, server)
}
```


```{r}
regexTest = function(pattern="night", 
                     x = "We can turn day into night with this Gadget",
                     replace = "day") {
  
  ui = miniPage(
    gadgetTitleBar("Basic gsub tester"),
    miniContentPanel(
      textInput("text","Text:", x),
      textInput('pattern','Pattern to replace:', pattern),
      textInput("replacement","Text to substitute", replace),
      textOutput("out")
    )
  )
  
  server = function(input, output, session) {
    output$out = renderText( gsub(pattern = input$pattern,
                                  replace = input$replacement, 
                                  x = input$text) )
    observeEvent(input$done, {
      returnValue <- input$pattern
      stopApp(returnValue)
    })
  }
  runGadget(ui, server)
}

test <- regexTest(pattern="night", x = "We can turn day into night with this Gadget", replace = "day") 
```



```{r}
ggbrush <- function(dframe, xvar, yvar, viewer=paneViewer()) {
  
  ui <- miniPage(
    gadgetTitleBar("Drag to select points"),
    miniContentPanel(
      # The brush="brush" argument means we can listen for
      # brush events on the plot using input$brush.
      plotOutput("plot", height = "100%", brush = "brush")
    )
  )
  
  server <- function(input, output, session) {
    
    # Render the plot
    output$plot <- renderPlot({
      # Plot the data with x/y vars indicated by the caller.
      ggplot(dframe, aes_string(xvar, yvar)) + geom_point()
    })
    
    # Handle the Done button being pressed.
    observeEvent(input$done, {
      # Return the brushed points. See ?shiny::brushedPoints.
      stopApp(brushedPoints(dframe, input$brush))
    })
  }
  
  runGadget(ui, server)
}

ggbrush(dframe, xvar, yvar, viewer=paneViewer())
```

```{r}
shinyFunction = function() {
  require(shiny)
  server <- function(input, output) {
    output$distPlot <- renderPlot({
      hist(rnorm(input$obs), col = 'darkgray', border = 'white')
    })
  }
  
  ui <- fluidPage(
    sidebarLayout(
      sidebarPanel(
        sliderInput("obs", "Number of observations:", min = 10, max = 500, value = 100)
      ),
      mainPanel(plotOutput("distPlot"))
    )
  )
  
  shinyApp(ui = ui, server = server)
}

test <- shinyFunction()
```




```{r}
library(shiny)
library(miniUI)
library(leaflet)
library(ggplot2)

ui <- miniPage(
  gadgetTitleBar("Shiny gadget example"),
  miniTabstripPanel(
    miniTabPanel("Parameters", icon = icon("sliders"),
                 miniContentPanel(
                   sliderInput("year", "Year", 1978, 2010, c(2000, 2010), sep = "")
                 )
    ),
    miniTabPanel("Visualize", icon = icon("area-chart"),
                 miniContentPanel(
                   plotOutput("cars", height = "100%")
                 )
    ),
    miniTabPanel("Map", icon = icon("map-o"),
                 miniContentPanel(padding = 0,
                                  leafletOutput("map", height = "100%")
                 ),
                 miniButtonBlock(
                   actionButton("resetMap", "Reset")
                 )
    ),
    miniTabPanel("Data", icon = icon("table"),
                 miniContentPanel(
                   DT::dataTableOutput("table")
                 )
    )
  )
)

server <- function(input, output, session) {
  output$cars <- renderPlot({
    require(ggplot2)
    ggplot(cars, aes(speed, dist)) + geom_point()
  })
  
  output$map <- renderLeaflet({
    force(input$resetMap)
    
    leaflet(quakes, height = "100%") %>% addTiles() %>%
      addMarkers(lng = ~long, lat = ~lat)
  })
  
  output$table <- DT::renderDataTable({
    diamonds
  })
  
  observeEvent(input$done, {
    stopApp(TRUE)
  })
}

runGadget(shinyApp(ui, server), viewer = paneViewer())
```

