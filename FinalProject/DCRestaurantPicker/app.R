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

restaurants <- read_excel("/cloud/project/FinalProject/restaurants_cleaned.xlsx")

getColor <- function(restaurants) {
  sapply(restaurants$top25_wm, function(top25_wm) {
    if(top25_wm == 1) {
      "red"
    } else {
      "blue"
    } })
}

icons <- awesomeIcons(
  icon = 'cutlery',
  markerColor = getColor(restaurants)
)

##Adding in optimizing restaurant pick function

## 6. Writing a Function
find_restaurants <- function(genre = NULL, rating = NULL, price_level = NULL, top25_wm = NULL) {
  
  # use a copy of the original dataset
  result <- restaurants
  
  # filter based on genre
  if (!is.null(genre)) {
    result <- result[result$genre == genre, ]
  }
  
  # filter based on rating
  if (!is.null(rating)) {
    result <- result[result$rating_avg >= rating, ]
  }
  
  # filter based on price_level
  if (!is.null(price_level)) {
    result <- result[result$price_avg == price_level, ]
  }
  
  # filter based on top25_wm
  if (!is.null(top25_wm)) {
    result <- result[result$top25_wm == top25_wm, ]
  }
  
  #result <- result %>% filter(!is.na(restaurants$name))
  return(result)
}

# example usage:
filtered_restaurants <- find_restaurants(genre="Italian", rating=3, price_level=4, top25_wm=1)
filtered_restaurants 

###


ui <- fluidPage(
  leafletOutput("testmap"),
  p(),
  actionButton("randomPointButton", "Pick for me!"),
  actionButton("refreshButton", "Refresh the map"),
  textInput("userInputGenre", "Enter Type of Food", value = ""),
  numericInput("numRating", "Star Rating", value = NA),
  numericInput("numPrice", "Price-level (Enter 1-4)", value = NA),
  numericInput("numtop25", "Type 1 if you want a Top 25 Washingtonian Magazine Restaurant, otherwise type 0", value = NA),
  actionButton("userInputButton", "What's my match?")
)

server <- function(input, output) {
  
  # Original data
  originalData <- reactiveVal(restaurants)
  
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
  
  # User input button event
  observeEvent(input$userInputButton, {
    inputNames <- strsplit(input$userInputGenre, ",")[[1]]
    numRating <- input$numRating
    numPrice <- input$numPrice
    numtop25 <- input$numtop25

    selectedRestaurants <- find_restaurants(inputNames, numRating, numPrice, numtop25)
    
    leafletProxy("testmap") %>%
      clearMarkers() %>%
      addAwesomeMarkers(data = selectedRestaurants, lng = selectedRestaurants$longitude, lat = selectedRestaurants$latitude, icon = icons, popup = selectedRestaurants$name, label = selectedRestaurants$name)
  })
  
  
  # Ensure that the map is initially drawn with the original data
  observe({
    leafletProxy("testmap") %>%
      clearMarkers() %>%
      addAwesomeMarkers(data = originalData(), lng = ~longitude, lat = ~latitude, icon=icons, popup = ~as.character(name), label = ~as.character(name))
  })
  
}

shinyApp(ui, server)