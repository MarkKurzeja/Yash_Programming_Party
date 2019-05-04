# File for generating testing data for a Gaussian Process

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

rm(list = ls())
set.seed(123)
library(magrittr)
library(mvtnorm)
library(ggplot2)
library(MASS)
library(plyr)
library(dplyr)

# PARAMS
noise_variance = 0.01


# Generate the sample data
fx <- function(x)
{
  cos(3 * x) + x^(1/2) - sin(x)
  # cos(3 * x) 
  # sin(x)
}

lower = 0
upper = 7

x = seq(lower, upper, length = 30)
# x = runif(30, lower, upper)
# x = c(-4, -3, -2, -1, 1)
y = fx(x) + rnorm(length(x), 0, noise_variance)
newx = seq(lower,upper, length = 200)
true_x = seq(lower, upper, length = 200)
true_y = fx(true_x)

plot(x,y)
curve(fx, add = T)

# Write the data to the outfile
write.csv(x = data.frame(x = x, y = y), file = "../data/training_data.csv", row.names = F)
write.csv(x = data.frame(x = newx), file = "../data/testing_data.csv", row.names = F)
write.csv(x = data.frame(x = true_x, y = true_y), file = "../data/true_data.csv", row.names = F)

# Generate a config file
sink("../data/config.txt")
cat("Program:Stan|Error:Normal|")
sink()