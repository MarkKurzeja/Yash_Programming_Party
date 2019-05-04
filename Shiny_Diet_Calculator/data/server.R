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

summarytext = paste("Last Recorded Weight :<strong>" , format(curr_weight, digit = 3), "</strong><br>",
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
  
  calorie_update <- eventReactive(input$kgLossRate, {
    #browser()
    dir = dirname(rstudioapi::getSourceEditorContext()$path)
    setwd(dir)
    initdata = read.csv(file = "FLF_output_table.csv", header=TRUE, sep = ",")
    curr_weight = initdata$Weight[nrow(initdata)]
    one_weekWeight = initdata$Weight[nrow(initdata)-7]
    two_weekWeight = initdata$Weight[nrow(initdata)-14]
    curr_BF = initdata$PredictedBF[nrow(initdata)]
    one_weekBF = initdata$PredictedBF[nrow(initdata)-7]
    two_weekBF = initdata$PredictedBF[nrow(initdata)-14]
    

    target_loss_per_week = 0.453592 * curr_weight
    if(input$kgLossRate == "1") {target_loss_per_week = target_loss_per_week * 0.008}
    else if (input$kgLossRate == "2") {target_loss_per_week = target_loss_per_week * 0.004}
    else {target_loss_per_week = target_loss_per_week * 0.012}
    calorie_change = Compute_Calorie_Control_Linear(as.numeric(lubridate::today(tzone = "") - lubridate::ymd(initdata$Date[nrow(initdata)])),
                                                    target_loss_per_week,
                                                    beginning_weight_kg =initdata$Weight[nrow(initdata)-1],
                                                    ending_weight_kg = curr_weight,
                                                    beginning_bf = initdata$PredictedBF[nrow(initdata)-1],
                                                    ending_bf = curr_BF,
                                                    calories_in_deficit = (DetermineDeficitCals(curr_weight,
                                                                                               target_loss_per_week,
                                                                                               curr_BF*0.01))/7,
                                                    Determinegrams(curr_weight, target_loss_per_week, curr_BF*0.01), 0.08)
    #browser()
    calText = paste("<br>You need to change your Calorie Intake by :<strong>", calorie_change, "</strong>")
    output$calChange <- renderText(calText)

  })
    
  obs <- observe({
    gen_update()
  }) 
  
  observeEvent(input$kgLossRate,
               {
                 if(input$kgLossRate != "0") {
                   calorie_update()
                 }
               })
  
  # obs <- observe({
  #   calorie_update()
  # }) 
  
  output$distPlot <- renderPlot({
    
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
    
  })
  
  
  output$summaryTitle <- renderText(
    "<strong><H3>Summary</H3></strong>"
  )
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

Compute_Calorie_Control_Linear <- function(days_since_last_weighin,
                                           kgs_per_week, 
                                           beginning_weight_kg,
                                           ending_weight_kg,
                                           beginning_bf,
                                           ending_bf,
                                           calories_in_deficit,
                                           grams_protein, percentage_adjustment_input = 0.08) {
  
  # If one needs to see progress, layne suggests that they reduce
  # the amount of calories that they eat from non-protein sources
  # by somewhere between 5-15% (7-10% recommended) per week
  percentage_adjustment = percentage_adjustment_input
  kgs_lost_in_period = beginning_weight_kg - ending_weight_kg
  
  # Compute the number of cals not from protein and compute the number
  # of calories that one should subtract or add based on weightlost per week
  calories_remaining = calories_in_deficit - 4 * grams_protein
  calories_to_decrease_per_week = percentage_adjustment * calories_remaining
  
  # Scale up the calories based on the number of days since the last weigh-in since
  # Layne gives values as a weekly measure
  cals_per_period = calories_to_decrease_per_week * days_since_last_weighin / 7
  
  # Collar this amount to be somewhere between 15% of the total cals plus or minus
  cals_per_period = median(c(.15 * calories_in_deficit, -0.15 * calories_in_deficit, cals_per_period))
  
  # Use a linear prediction function with values of:
  # -cals_per_period at zero kgs/ period lost (we need to subtract cals_per_period cals since we didn't move at all)
  # 0 at exactly kgs_per_week lost (we predicted exactly right!)
  # cals_per_period at 2 * kgs_per_week lost (we need to add cals since we lost too much weight)
  predicted_change = cals_per_period / kgs_per_week * (kgs_lost_in_period - kgs_per_week)
  
  # Collar the predicted change to be within (-cals_per_period,cals_per_period) to ensure we don't get wild swings
  predicted_change = median(c(predicted_change, -cals_per_period, cals_per_period))
  
  # Return the predicted change to the user
  return(-predicted_change)
}

today()

as.numeric(today() - lubridate::make_date(1994, 01, 04))


#%% Setting up the BMR Functions
# Compute the Rosa Equation for BMR
MensRosaBMR = function(body_weight, height, age) {
  return (88.362 + (13.397 * body_weight) + (4.799 * height) - (5.677 * age))
}

# Compute the Muller Equation for BMR
MullerBMR = function(lean_body_mass, fat_mass, age) {
  # sex = 1 if male, 0 otherwise
  return ((13.587 * lean_body_mass) + (9.613 * fat_mass) + (198 * 1) - (3.351 * age) + 674)
}

# Return the Total Daily Energy Expendature estimate to the user so
# that they can get the best estimate of their maintenance calories
TotalDailyEnergyExpend = function(body_weight,
                                  percent_fat,
                                  height,
                                  age,
                                  activity_factor,
                                  adjustment) {
  stopifnot(percent_fat < 1 & percent_fat > 0)
  stopifnot(activity_factor >= 1.2 & activity_factor <= 1.9)
  rosa = MensRosaBMR(body_weight, height, age)
  muller = MullerBMR(body_weight * (1 - percent_fat), body_weight * percent_fat, age)
  avg = (rosa + muller) / 2
  return (activity_factor * avg + adjustment)
}


#%% Working on the deficit calculations

# Function for determining the amount of bodyfat lost as a function of your
# current activity rate as well as your bodyfat percentage. See Table One
# of fat loss forever for more details
DetermineLossInFat = function(percent_fat,
                              high_protein = FALSE,
                              weight_lifting = FALSE) {
  # Haven't implemented non-high-protein or non-weightlifting yet
  #    print(f"{percent_fat}")
  high_protein #not impl
  stopifnot(weight_lifting) #no_impl
  stopifnot(percent_fat <= 0.27) # not implemented yet
  stopifnot(percent_fat > 0) # constraint
  
  percent_fat = percent_fat + (pi / 1000000)# to prevent TRUE linear matches
  
  if (percent_fat > .245 & percent_fat <= .27) {
    return (.9)
  }
  else if (percent_fat >= .165) {
    y_high = 0.90
    y_low = 0.80
    x_high = 0.245
    x_low = 0.165
  }
  else if (percent_fat >= 0.11) {
    y_high = 0.80
    y_low = 0.70
    x_high = .165
    x_low = 0.11
  }
  else {
    return (0.7)
  }
  
  return ((y_high - y_low) / (x_high - x_low) * (percent_fat - x_low) +  y_low)
}

# Function for determining the number of calories per week that we need to be
# in a deficit for to see results
DetermineDeficitCals = function(body_weight,
                                target_loss_per_week,
                                percent_fat) {
  #browser()
  # Determine the amount lost per week in fat
  grams_lost_per_week = body_weight * target_loss_per_week * 1000
  loss_in_fat_per_week = DetermineLossInFat(percent_fat,
                                            high_protein = TRUE,
                                            weight_lifting = TRUE)
  loss_in_grams = grams_lost_per_week * loss_in_fat_per_week
  LIPID_PERCENT_IN_BF = 0.87
  loss_of_lipid_per_week = LIPID_PERCENT_IN_BF * loss_in_grams
  CALS_PER_GRAM_FAT = 9.0
  fat_cals = CALS_PER_GRAM_FAT * loss_of_lipid_per_week
  
  # Determine the amount lost per week in protein and LBM
  loss_in_grams = grams_lost_per_week * (1 - loss_in_fat_per_week)
  NON_WATER_LBM_LOST = 0.30 # 30 % of lbm is muscle and protein and 70 is water
  grams_protein = NON_WATER_LBM_LOST * loss_in_grams
  CALORIES_PER_GRAM_PROTEIN = 4
  lbm_cals = CALORIES_PER_GRAM_PROTEIN * grams_protein
  return (fat_cals + lbm_cals)
}

# Determine the amount of grams_protein
Determinegrams = function(body_weight, target_loss_per_week, percent_fat) {
  grams_lost_per_week = body_weight * target_loss_per_week * 1000
  loss_in_fat_per_week = DetermineLossInFat(percent_fat,
                                            high_protein = TRUE,
                                            weight_lifting = TRUE)
  loss_in_grams = grams_lost_per_week * (1 - loss_in_fat_per_week)
  NON_WATER_LBM_LOST = 0.30 # 30 % of lbm is muscle and protein and 70 is water
  grams_protein = NON_WATER_LBM_LOST * loss_in_grams
  return(grams_protein)
}

# Function for predicting the date that the diet will be done
DetermineProjectDate = function(body_weight,
                                percent_fat,
                                desired_fat,
                                target_loss_per_week,
                                day_to_offset = today()) {
  stopifnot(percent_fat < 1)
  stopifnot(percent_fat > 0)
  stopifnot(desired_fat < 1)
  stopifnot(target_loss_per_week < 1.5)
  stopifnot(desired_fat > 0)
  stopifnot(body_weight < 150) # the weight is in kilos
  
  # Determine the rate of weight loss on a weekly basis
  kgs_to_lose = (percent_fat - desired_fat) * body_weight
  kg_rate_of_loss = body_weight * target_loss_per_week
  
  weeks = kgs_to_lose / kg_rate_of_loss
  
  return (day_to_offset + weeks * 7)
}

#%% Beginning of the actual actual calculations

#def main():

# Inputs --------------------------------------------
working_date = today()
goal_bf = 0.10
goal_date = lubridate::make_date(2019, 5, 7)
current_weight_lbs = 217.99
current_body_fat = .1316
target_loss_per_week = .0069
tempage = (working_date - lubridate::make_date(1994, 1, 4))
current_age = as.numeric(tempage / 365)
# ---------------------------------------------------

# Computations
current_weight_kgs = current_weight_lbs / 2.2
lbm = (1 - current_body_fat) * current_weight_kgs



total_daily_energy_expend = TotalDailyEnergyExpend(
  body_weight = current_weight_kgs,
  percent_fat = current_body_fat,
  height = 70 * 2.54,
  age = current_age,
  activity_factor = 1.55,
  adjustment = -798.95
)
deficit_per_week = DetermineDeficitCals(
  body_weight = current_weight_kgs,
  target_loss_per_week = target_loss_per_week,
  percent_fat = current_body_fat
)

expected_date = DetermineProjectDate(
  body_weight = current_weight_kgs,
  percent_fat = current_body_fat,
  desired_fat = goal_bf,
  target_loss_per_week = target_loss_per_week,
  day_to_offset = working_date
)

