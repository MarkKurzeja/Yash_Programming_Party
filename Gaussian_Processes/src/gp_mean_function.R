rm(list = ls())

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

library(magrittr)
library(mvtnorm)
library(ggplot2)
library(MASS)
library(plyr)
library(dplyr)


set.seed(123)

# Read in the data
dat <- read.csv("gp_sample_data.csv")
x = dat$x
y = dat$y


# Compute an arbitary covariance matrix 
cov_mat_compute <- function(x, xstar, l_param = input$l_param, alpha_param = input$alpha_param, type_cov = c("rat_quad", "squared_exp")) {
  type_cov = match.arg(type_cov)
  if(type_cov == "squared_exp") {
    mx = matrix(x, nrow = length(x), ncol = length(xstar), byrow = F)
    my = matrix(xstar, nrow = length(x), ncol = length(xstar), byrow = T)
    D = diag(length(x)) / (-2 * l_param^2)
    t = exp(D %*% ((mx - my)^2)) 
  } else { # is a rational quadratic
    t = (1 + (outer(X = x, Y = xstar, FUN = function(x,y) {abs(x - y)})^2 / (2 * alpha_param * l_param^2)))^(-alpha_param)
  }
  return(t)
}

# Function that takes in a set of training data and outputs the mean function for a GP
# x = input x 
# y = input y 
# cov_mat = covariance matrix between x and y 
# noise_level = vector of len(x) that contains the noise for each x point  
# test_input_x = points within x that need to be evaluated
compute_mean_gp <- function(x, y, cov_func, noise_level, test_input_x) {
  # Compute the necessary covariance matricies
  k = cov_func(x,x)
  k_star = cov_func(x, test_input_x)
  
  mean_y = mean(y)
  y = y - mean(y)
  # Compute the matrix output
  # browser()
  # Input some noise into the system so that it is non-zero
  base_noise = matrix(1e-10, nrow = nrow(k), ncol = ncol(k))
  
  L = chol(k + diag(noise_level) + base_noise)
  alpha = solve(t(L), solve(L, y))
  f_hat_star = t(k_star) %*% alpha
  log_lik = -1/2 * t(y) %*% alpha - sum(log(diag(L))) - length(x) / 2 * log(2 * pi)
  list(x = x, 
       y = y + mean_y, 
       test_x = test_input_x, 
       f_hat = f_hat_star + mean_y, 
       log_lik = log_lik)
}


cov_func_sqexp <- function(l_param) {
  function(x, xstar) {
    cov_mat_compute(x, xstar, l_param = l_param, type_cov = "squared_exp")
  }
}

# result <- compute_mean_gp(x = x, 
#                 y = y, 
                # cov_func_sqexp(.35), rep(.01, length(x)), seq(1,10, length = 200))
result <- compute_mean_gp(x = x, 
                y = y, 
                cov_func_sqexp(.35), rep(.01, length(x)), x)

ldply(seq(0.001, .6, by = 0.01), function(i) {
  k = compute_mean_gp(x = x,
                  y = y,
                  cov_func_sqexp(i), rep(.01, length(x)), seq(1,10, length = 200))
  data.frame(l = i, log_lik = k$log_lik)
}) %>%
  ggplot() +
  geom_point(aes(l, log_lik))


# Plotting for debugging
data.frame(x = result$x, y = result$y) %>% 
  ggplot() +
  geom_point(aes(x = x, y = y)) + 
  geom_line(aes(x = tx, y = yhat), color = "red", data = data.frame(tx = result$test_x, yhat = result$f_hat)) +
  ggtitle(sprintf("%.4f", result$log_lik))
  # stat_function(fun = fx, linetype = 2)


