library(shiny)
library(ggplot2)
library(magrittr)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$distPlot <- renderPlot({

        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')

    })
    
    output$summary <- renderText(
      "This is my <strong>summary</strong>"
    )
    output$weightPlot <- renderPlot({
      dat = data.frame(date = lubridate::today() + 1:10, weight = rnorm(10), weight_norm = runif(10))
      dat %>% 
        ggplot() + 
        geom_point(aes(x = date, y = weight)) + 
        geom_line(aes(x = date, y = weight)) +
        geom_line(aes(x = date, y = weight_norm))
    })
    output$bfPlot <- renderPlot({
      dat = data.frame(date = lubridate::today() + 1:10, weight = rnorm(10), weight_norm = runif(10))
      dat %>% 
        ggplot() + 
        geom_point(aes(x = date, y = weight)) + 
        geom_line(aes(x = date, y = weight)) +
        geom_line(aes(x = date, y = weight_norm))
    })

})
