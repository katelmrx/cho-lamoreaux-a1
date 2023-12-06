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

# Define server logic required to draw a histogram
#server <- function(input, output) {

#    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
      #  x    <- faithful[, 2]
       # bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        #hist(x, breaks = bins, col = 'darkgray', border = 'white',
         #    xlab = 'Waiting time to next eruption (in mins)',
          #   main = 'Histogram of waiting times')
  #  })
#}

# Run the application 
#shinyApp(ui = ui, server = server)


r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

ui <- fluidPage(
  leafletOutput("testmap"),
  p(),
)

server <- function(input, output, session) {
  
  points <- eventReactive(input$recalc, {
    cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
  }, ignoreNULL = FALSE)
  
  content <- paste(sep = "<br/>",
                   "<b><a href='https://thedabney.com/'>1. The Dabney</a></b>",
                   "122 Blagden Alley, NW",
                   "Washington, DC 20001"
  )
  output$testmap <- renderLeaflet({
    testmap <- leaflet() %>%
      addTiles() %>%  # Add default OpenStreetMap map tiles
      addPopups(-77.02455,38.91130, content,
                options = popupOptions(closeButton = FALSE)
      )
  })
}

shinyApp(ui, server)