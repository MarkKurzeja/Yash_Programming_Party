#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Weight Manager"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            ############# Action button to update charts #############
            actionButton("action_updateCharts", "Update Charts"),
            
            ############# Action button to Check in #################
            actionButton("action_checkIn", "Check In")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            htmlOutput("summary"),
            plotOutput("weightPlot"),
            plotOutput("bfPlot"),
        )
    )
))
