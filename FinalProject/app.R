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

library(dplyr)

restaurants <- read_excel("restaurants_cleaned.xlsx")

restaurants$hyperlink <- paste("href='",restaurants$website,"'>")

restaurants$hyperlink  <- gsub(" ", "", restaurants$hyperlink)

restaurants$hyperlink  <- paste("<b><a",restaurants$hyperlink,restaurants$name,"</a></b>")


restaurants$labelcontent <- paste(sep = "<br/>",restaurants$hyperlink,
                                  restaurants$address,restaurants$phone_number,restaurants$genre,"<b><a", "Average Star Rating", restaurants$rating_avg,"</a></b>", "<b><a","Average Price in $s", restaurants$price_avg,"</a></b>")



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
  actionButton("top25Button", "Washingtonian Magazine's Top 25 Restaurants"),
  sliderInput("maxPrice", "Choose Maximum Price in Dollar Signs", min = 1, max = 4, value = 1),
  actionButton("filterPrice", "Filter by Price"),
  sliderInput("minStarRating", "Choose Minimum Star Rating", min = 3, max = 4.9, value = 3),
  actionButton("filterStarRating", "Filter by Star Rating"),
  textInput("userInputGenre", "Enter Type of Food", value = ""),
  numericInput("numRating", "Star Rating", value = NA),
  numericInput("numPrice", "Price-level (Enter 1-4)", value = NA),
  numericInput("numtop25", "Type 1 if you want a Top 25 Washingtonian Magazine Restaurant, otherwise type 0", value = NA),
  actionButton("userInputButton", "What's my match?")
)

server <- function(input, output, session) {
  
  # Original data
  originalData <- reactiveVal(restaurants)
  
  # Render the map
  output$testmap <- renderLeaflet({
    testmap <- leaflet(data = originalData()) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addAwesomeMarkers(lng = ~longitude, lat = ~latitude, icon=icons, popup = restaurants$labelcontent, label = ~as.character(name))
  })
  
  # Random point button event
  observeEvent(input$randomPointButton, {
    randomIndex <- sample(1:nrow(originalData()), 1)
    randomPoint <- originalData()[randomIndex, c("latitude", "longitude", "labelcontent","name")]
    
    leafletProxy("testmap") %>%
      clearMarkers() %>%
      addAwesomeMarkers(lng = randomPoint$longitude, lat = randomPoint$latitude, icon = icons, popup = randomPoint$labelcontent, label = randomPoint$name)
  })
  
  # Refresh button event
  observeEvent(input$refreshButton, {
    leafletProxy("testmap") %>%
      clearMarkers() %>%
      addAwesomeMarkers(data = originalData(), lng = ~longitude, lat = ~latitude, icon=icons, popup = restaurants$labelcontent, label = ~as.character(name))
  })
  
  # Show only Washingtonian Mag's Top 25 button event
  observeEvent(input$top25Button, {
    top25 <- restaurants[restaurants$top25_wm == 1, ]
    leafletProxy("testmap") %>%
      clearMarkers() %>%
      addAwesomeMarkers(data = top25, lng = ~longitude, lat = ~latitude, icon=icons, popup = restaurants$labelcontent, label = ~as.character(name))
  })
  
  # Filter button event
  observeEvent(input$filterPrice, {
    maxPrice <- input$maxPrice
    filteredData1<- (restaurants[restaurants$price_avg<= maxPrice, ])
    leafletProxy("testmap") %>%
      clearMarkers() %>%
      addAwesomeMarkers(data = filteredData1, lng = ~longitude, lat = ~latitude, icon=icons, popup = restaurants$labelcontent, label = ~as.character(name)) 
  })
  
  # Filter button event
  observeEvent(input$filterStarRating, {
    minStarRating <- input$minStarRating
    filteredData2<- (restaurants[restaurants$rating_avg >= minStarRating, ])
    leafletProxy("testmap") %>%
      clearMarkers() %>%
      addAwesomeMarkers(data = filteredData2, lng = ~longitude, lat = ~latitude, icon=icons, popup = restaurants$labelcontent, label = ~as.character(name)) 
  })
  
  
  # User input button event
  observeEvent(input$userInputButton, {
    inputNames <- strsplit(input$userInputGenre, ",")[[1]]
    numRating <- input$numRating
    numPrice <- input$numPrice
    numtop25 <- input$numtop25
    
    if (length(inputNames) > 0) {
    selectedRestaurants <- find_restaurants(inputNames, numRating, numPrice, numtop25)
    

   if (nrow(selectedRestaurants) > 0) {
      leafletProxy("testmap") %>%
        clearMarkers() %>%
       addAwesomeMarkers(data = selectedRestaurants, lng = selectedRestaurants$longitude, lat = selectedRestaurants$latitude, icon = icons, popup = selectedRestaurants$labelcontent, label = selectedRestaurants$name)
    } else {
      showModal(modalDialog(
        title = "Error",
        "No restaurants chosen. Please enter alternate specifications.",
        easyClose = TRUE,
        footer = NULL
      ))
    }
    } else {
      showModal(modalDialog(
        title = "Error",
        "Please enter genre and specifications.",
        easyClose = TRUE,
        footer = NULL
      ))
    }
    })
    
  
  # Ensure that the map is initially drawn with the original data
  observe({
    leafletProxy("testmap") %>%
      clearMarkers() %>%
      addAwesomeMarkers(data = originalData(), lng = ~longitude, lat = ~latitude, icon=icons, popup = restaurants$labelcontent, label = ~as.character(name))
  })
  
}

shinyApp(ui, server)