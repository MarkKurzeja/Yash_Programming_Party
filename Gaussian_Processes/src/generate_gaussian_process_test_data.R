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
noise_variance = 0


# Generate the sample data
fx <- function(x)
{
  cos(3 * x) + x^(1/2) - sin(x)
}
x = seq(1, 10, length = 30)
y = fx(x) + rnorm(length(x), 0, noise_variance)

plot(x,y)

# Write the data to the outfile
write.csv(x = data.frame(x = x, y = y), file = "./gp_sample_data.csv", row.names = F)

# Generate a sample covariance file
sink("./config_covariance.txt")
cat("CovFxn:squared_exp|Length:2|")
