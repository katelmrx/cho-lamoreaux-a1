#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

library(leaflet)

library(readxl) 

restaurants_list <- read_excel("/cloud/project/FinalProject/restaurants_cleaned.xlsx")

getColor <- function(restaurants_list) {
  sapply(restaurants_list$top25_wm, function(top25_wm) {
    if(top25_wm == 1) {
      "red"
    } else {
      "blue"
    } })
}

icons <- awesomeIcons(
  icon = 'cutlery',
  markerColor = getColor(restaurants_list)
)


ui <- fluidPage(
  leafletOutput("testmap"),
  p(),
  actionButton("randomPointButton", "Pick for me!"),
  actionButton("refreshButton", "Refresh the map")
)

server <- function(input, output) {
  
  # Original data
  originalData <- reactiveVal(restaurants_list)
  
  # Render the map
  output$testmap <- renderLeaflet({
    testmap <- leaflet(data = originalData()) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addAwesomeMarkers(lng = ~longitude, lat = ~latitude, icon=icons, popup = ~as.character(name), label = ~as.character(name))
  })
  
  # Random point button event
  observeEvent(input$randomPointButton, {
    randomIndex <- sample(1:nrow(originalData()), 1)
    randomPoint <- originalData()[randomIndex, c("latitude", "longitude", "name")]
    
    leafletProxy("testmap") %>%
      clearMarkers() %>%
      addAwesomeMarkers(lng = randomPoint$longitude, lat = randomPoint$latitude, icon = icons, popup = randomPoint$name, label = randomPoint$name)
  })
  
  # Refresh button event
  observeEvent(input$refreshButton, {
    leafletProxy("testmap") %>%
      clearMarkers() %>%
      addAwesomeMarkers(data = originalData(), lng = ~longitude, lat = ~latitude, icon=icons, popup = ~as.character(name), label = ~as.character(name))
  })
  
  # Ensure that the map is initially drawn with the original data
  observe({
    leafletProxy("testmap") %>%
      clearMarkers() %>%
      addAwesomeMarkers(data = originalData(), lng = ~longitude, lat = ~latitude, icon=icons, popup = ~as.character(name), label = ~as.character(name))
  })
  
}

shinyApp(ui, server)