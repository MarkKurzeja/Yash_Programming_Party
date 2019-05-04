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
            
            ############# Summmary ######################
            htmlOutput("summaryTitle"),
            selectInput("kgLossRate", label = h6(""), 
                        choices = c("Select Pace", "Medium", "Slow", "Aggressive"), 
                        selected = 1),
            htmlOutput("summary"),
            htmlOutput("calChange"),
            ############# Inputs for check in ###########
            numericInput("numW", label = h4("Weight"), value = 1),
            numericInput("numC", label = h4("Chest"), value = 1),
            numericInput("numA", label = h4("Abdominal"), value = 1),
            numericInput("numT", label = h4("Thigh"), value = 1),
            numericInput("numS", label = h4("Supailiac"), value = 1),
            
            ############# Action button to Check in #####
            actionButton("action_checkIn", "Check In")
            
        ),
        

        # Show a plot of the generated distribution
        mainPanel(
            htmlOutput("weighttitle"),
            plotOutput("weightPlot"),
            htmlOutput("bftitle"),
            plotOutput("bfPlot")
        )
    )
))
