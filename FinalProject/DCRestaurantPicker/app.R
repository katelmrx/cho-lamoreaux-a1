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
  actionButton("recalc", "Pick for me!")
)

server <- function(input, output, session) {
  
  output$testmap <- renderLeaflet({
    testmap <- leaflet(data = restaurants_list) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%  # Add default OpenStreetMap map tiles 
      addAwesomeMarkers(lng = ~longitude, lat = ~latitude, icon=icons, popup = ~as.character(name), label = ~as.character(name))
  })
}

shinyApp(ui, server)