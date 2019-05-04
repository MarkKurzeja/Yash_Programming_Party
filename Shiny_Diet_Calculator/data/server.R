library(shiny)
library(ggplot2)
library(magrittr)
library(rstudioapi)
library(dplyr)
library(lubridate)

dir = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(dir)

initdata = read.csv(file = "FLF_output_table.csv", header=TRUE, sep = ",")
curr_weight = initdata$Weight[nrow(initdata)]
one_weekWeight = initdata$Weight[nrow(initdata)-7]
two_weekWeight = initdata$Weight[nrow(initdata)-14]
curr_BF = initdata$PredictedBF[nrow(initdata)]
one_weekBF = initdata$PredictedBF[nrow(initdata)-7]
two_weekBF = initdata$PredictedBF[nrow(initdata)-14]

summarytext = paste("<strong><H3>Summary</H3></strong>Last Recorded Weight :<strong>" , format(curr_weight, digit = 3), "</strong><br>",
                    "Weight Change in the last week :<strong>", format(curr_weight - one_weekWeight, digit = 3), "</strong><br>",
                    "Weight Change in the last 2 weeks :<strong>", format(curr_weight - two_weekWeight, digit = 3), "</strong><br><br>",
                    "Last Recorded BF :<strong>" , format(curr_BF, digit = 3), "</strong><br>",
                    "BF Change in the last week :<strong>", format(curr_BF - one_weekBF, digit = 3), "</strong><br>",
                    "BF Change in the last 2 weeks :<strong>", format(curr_BF - two_weekBF, digit = 3), "</strong>")


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  gen_update <- eventReactive(input$action_checkIn, 
      {
        #browser()
        result = read.csv(file = "FLF_input_table.csv", header=TRUE, sep = ",")
        Date <-  c(lubridate::today(tzone = "") %>% {sprintf("%d/%d/%d", month(.), day(.), year(.))})
        Weight <- c(input$numW)
        Chest <- c(input$numC)
        Abdominal <-  c(input$numA)
        Thigh <-  c(input$numT)
        Supailiac <-  c(input$numS)
        tmptable <- data.frame(Weight, Date, Chest, Abdominal, Thigh, Supailiac)
        true_data = read.csv(file = "FLF_input_table.csv", header=TRUE, sep = ",")
        result <- rbind(true_data, tmptable)
        write.csv(result,  file = "FLF_input_table.csv", row.names = F)
      }
  )
    
  obs <- observe({
    gen_update()
  }) 
  
  output$distPlot <- renderPlot({
    
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
    
  })
  
  
  
  output$summary <- renderText(
    summarytext
  )
  output$weighttitle <- renderText(
    "<H3>Weight over Time</H3>"
  )
  
  output$bftitle <- renderText(
    "<H3>Body Fat over Time</H3>"
  )
  
  output$weightPlot <- renderPlot({
    data = read.table(file = "FLF_output_table.csv", header=TRUE, sep = ",")
    # browser()
    data %>% 
      mutate(Date = lubridate::ymd(Date)) %>%
      ggplot() + 
      geom_point(aes(x = Date, y = Weight)) + 
      #geom_line(aes(x = Date, y = Weight)) +
      geom_line(aes(x = Date, y = WeightAverage))
  })
  output$bfPlot <- renderPlot({
    data = read.table(file = "FLF_output_table.csv", header=TRUE, sep = ",")
    # browser()
    data %>% 
      mutate(Date = lubridate::ymd(Date)) %>%
      ggplot() + 
      geom_point(aes(x = Date, y = PredictedBF)) + 
      #geom_line(aes(x = Date, y = PredictedBF)) +
      geom_line(aes(x = Date, y = BFAverage))
  })
  
})

