################################################################################
#                                                                              #
# A function for computing the linear approximation to the number of calories  #
# that should be lost in a given time_frame via a linear approximation that    #
# is collared to avoid drastic increases and decreases                         #
#                                                                              #
################################################################################

# This function aims to compute the calorie adjustment given 
# a variety of inputs
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

################################################################################
#                                                                              #
#                              Testing Functions                               #
#                                                                              #
################################################################################

# Create a Test for no change
print(Compute_Calorie_Control_Linear(days_since_last_weighin = 7, kgs_per_week = 0.5, 
                               beginning_weight_kg = 100.5,ending_weight_kg = 100,
                               beginning_bf = 14, ending_bf = 14, 
                               calories_in_deficit = 2800, 
                               grams_protein = 200, percentage_adjustment_input = 0.08))

# Create a test for a huge change and ensure it is bounded
Compute_Calorie_Control_Linear(days_since_last_weighin = 7, kgs_per_week = 0.5, 
                               beginning_weight_kg = 105,ending_weight_kg = 100,
                               beginning_bf = 14, ending_bf = 14, 
                               calories_in_deficit = 2800, 
                               grams_protein = 200, percentage_adjustment_input = 0.08)

# Create a test for a huge negative change and ensure it is bounded
Compute_Calorie_Control_Linear(days_since_last_weighin = 7, kgs_per_week = 0.5, 
                               beginning_weight_kg = 95,ending_weight_kg = 100,
                               beginning_bf = 14, ending_bf = 14, 
                               calories_in_deficit = 2800, 
                               grams_protein = 200, percentage_adjustment_input = 0.08)


# Ensure values are symmetric about desired point
Compute_Calorie_Control_Linear(days_since_last_weighin = 7, kgs_per_week = 1, 
                               beginning_weight_kg = 99.5, ending_weight_kg = 100,
                               beginning_bf = 14, ending_bf = 14, 
                               calories_in_deficit = 2800, 
                               grams_protein = 200, percentage_adjustment_input = 0.08)

################################################################################
#                                                                              #
#                      Plotting the Expectation Function                       #
#                                                                              #
################################################################################
# 
# ldply(seq(97, 101, by = 0.1), function(i) {
#   result <- Compute_Calorie_Control_Linear(days_since_last_weighin = 7, kgs_per_week = 1, 
#                                  beginning_weight_kg = 100, ending_weight_kg = i,
#                                  beginning_bf = 14, ending_bf = 14, 
#                                  calories_in_deficit = 2800, 
#                                  grams_protein = 200, percentage_adjustment_input = 0.08)
#   data.frame(x = i, calories = result)
# }) %>%
#   ggplot() +
#   geom_hline(yintercept = 0, color = "red", linetype = 2) +
#   geom_line(aes(x, calories)) +
#   ggtitle("Amount of Calories to Add conditional on Kgs Lost", "Red point is expected loss at the end of the week of 100-1 = 99kgs") +
#   geom_point(aes(x = x, y = y), color = "red", data = data.frame(x = 99, y = 0), size = 2) 








