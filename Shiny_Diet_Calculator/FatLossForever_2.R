rm(list = ls())

# -*- coding: utf-8 -*-
# """
# Spyder Editor
#
# This is a temporary script file.
# """

# import math as math
# import numpy as np
## options(error = browser)
library(lubridate)
# library(commentr)

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


cat(sprintf(
  "===================================================================\n"
))
cat(sprintf("                Diet Calculator - Mark Kurzeja\n"))
cat(sprintf(
  "===================================================================\n"
))
cat(sprintf("Current Goal Date : {%.2f}\n", goal_date))
cat(sprintf("Current Goal Date : {%.2f}\n", goal_bf * 100))
cat(sprintf(
  "=================== Current Estimates to Goal =====================\n"
))
cat(sprintf("BMR Estimate      : {%.2f}\n", total_daily_energy_expend))

cat(sprintf("Deficit Estimate  : {%.2f}\n", deficit_per_week / 7))
cat(sprintf("                    --------"))
cat(sprintf("Calorie Estimate  : {%.2f}\n", total_daily_energy_expend - deficit_per_week / 7))
cat(
  sprintf(
    "Expected Date     : {%.2f}/{%.2f}/{%.2f}\n",
    month(expected_date),
    day(expected_date),
    year(expected_date)
  )
)
temp = (expected_date - goal_date)
if (expected_date < goal_date) {
  cat(sprintf("You are {%.2f} day(s) ahead of schedule!!\n",-1 * temp))
} else {
  cat(sprintf("You are {%.2f} day(s) behind schedule\n", temp))
}

#%% This is the testing block
print("\n\n\n\n")
print("-----------------------------------------------------------------------\n")
print("----------------------------- Testing Block ---------------------------\n")
print("-----------------------------------------------------------------------\n")

# Aux function for doing rounded tolerance
IsWithin = function(user_input, func_expectation, tol = 0.001) {
  result = abs(user_input - func_expectation) < tol
  
  if (!result) {
    cat(sprintf("===stopifnot(ion Error===\nUser input:\n  {%.2f}\n  Expecation: {%.2f})\n", user_input, func_expectation ))
  }
  return(result)
}

# IsWithin(5, 5.1)

MensRosaBMR_test = function() {
  stopifnot(IsWithin(MensRosaBMR(100, 182, 25), 2159.555))
  print("Rosa BMR passed")
}

MullerBMRTest = function() {
  stopifnot(IsWithin(MullerBMR(85, 15, 25),  2087.315))
  print("Muller BMR passed")
}

TDEETest = function() {
  stopifnot(
    IsWithin(
      TotalDailyEnergyExpend(
        body_weight = 100,
        height = 182,
        percent_fat = .15,
        age = 25,
        activity_factor = 1.55,
        adjustment = 0
      ),
      3291.324
    )
  )
  print("TDEE passed")
}

DetermineLossInFatTest = function() {
  stopifnot(
    IsWithin(
      DetermineLossInFat(
        percent_fat = .165,
        high_protein = TRUE,
        weight_lifting = TRUE
      ),
      .8
    ))
  stopifnot(
    IsWithin(
      DetermineLossInFat(
        percent_fat = .245,
        high_protein = TRUE,
        weight_lifting = TRUE
      ),
      .9
    ))
  stopifnot(
    IsWithin(
      DetermineLossInFat(
        percent_fat = .26,
        high_protein = TRUE,
        weight_lifting = TRUE
      ),
      .9
    ))
  stopifnot(
    IsWithin(
      DetermineLossInFat(
        percent_fat = .11,
        high_protein = TRUE,
        weight_lifting = TRUE
      ),
      .7
    ))
  stopifnot(
    IsWithin(
      DetermineLossInFat(
        percent_fat = .13,
        high_protein = TRUE,
        weight_lifting = TRUE
      ),
      0.736363636
    ))
  stopifnot(
    IsWithin(
      DetermineLossInFat(
        percent_fat = .18,
        high_protein = TRUE,
        weight_lifting = TRUE
      ),
      0.81875
    ))
  stopifnot(
    IsWithin(
      DetermineLossInFat(
        percent_fat = .08,
        high_protein = TRUE,
        weight_lifting = TRUE
      ),
      .7
    ))
  print("Determine Loss In Fat Function Passed")
}

DetermineDeficitCalsTest = function() {
  stopifnot(
    IsWithin(
      DetermineDeficitCals(
        body_weight = 100,
        target_loss_per_week = 0.005,
        percent_fat = .11
      ),
      2920.52,
      tol = 0.1
    ))
  stopifnot(
    IsWithin(
      DetermineDeficitCals(
        body_weight = 105,
        target_loss_per_week = 0.012,
        percent_fat = .11
      ),
      7359.71,
      tol = 0.1
    ))
  stopifnot(
    IsWithin(
      DetermineDeficitCals(
        body_weight = 95,
        target_loss_per_week = 0.004,
        percent_fat = .13
      ),
      2311.21,
      tol = 0.1
    ))
  print("Determing Deficit Cals Function Passed!")
}

DetermineProjectDateTest = function() {
  
  tempdt = (DetermineProjectDate(
    body_weight = 99.09,
    percent_fat = .1316,
    desired_fat = .0901,
    target_loss_per_week  = 0.0069,
    day_to_offset = lubridate::make_date(2019, 4, 4)
  ) -
    lubridate::make_date(2019, 5, 16)
  )
  
  # browser()
  stopifnot(
    IsWithin(
      tempdt , 0.000, tol = 0.2)
  )
  print("Project Date Test Passed!")
} 
  
# Run the Tests
MensRosaBMR_test()
MullerBMRTest()
TDEETest()

# Run the deficit tests
DetermineLossInFatTest()
DetermineDeficitCalsTest()

# Run the projection tests
DetermineProjectDateTest()


print("All Tests have Passed!")

