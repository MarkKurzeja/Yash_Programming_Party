rm(list = ls())

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

library(magrittr)
library(mvtnorm)
library(ggplot2)
library(MASS)
library(plyr)
library(fields)
library(dplyr)


set.seed(123)

# Read in the data
dat <- read.csv("../data/training_data.csv")
x = dat$x
y = dat$y
dat_pred <- read.csv("../data/testing_data.csv")
predx <- dat_pred$x

# Compute the covariance function
cov_func_sqexp <- function(l_param, rho) {
  function(x, xstar) {
    # mx = matrix(x, nrow = length(x), ncol = length(xstar), byrow = F)
    # my = matrix(xstar, nrow = length(x), ncol = length(xstar), byrow = T)
    # D = diag(length(x)) * rho^2 / (-2 * l_param^2)
    # t = exp(D %*% ((mx - my)^2))
    Exp.cov(x, xstar, theta = l_param, p=rho, distMat = NA,
            C = NA, marginal = FALSE, onlyUpper=FALSE)
    # stationary.cov(x, xstar, Covariance = "Matern", Distance = "rdist",
                   # Dist.args = NULL, theta = l_param, V = NULL, C = NA, marginal = FALSE,
                   # derivative = 0, distMat = NA, onlyUpper = FALSE)
  }
}


compute_gp <- function(x, y, cov_func, noise_level, test_input_x) {
  # Compute the necessary covariance matricies
  k = cov_func(x,x)
  k_star = cov_func(x, test_input_x)
  
  # browser()
  # mean_y = mean(y)
  # y_std = y - mean(y)
  
  mean_y = mean(y)
  y_std = y - mean_y
  
  # Compute the matrix output
  # Input some noise into the system so that it is non-zero
  # browser()
  L = chol(k + diag(noise_level, length(x)))
  # browser()
  alpha = forwardsolve(t(L), backsolve(L, y_std))
  f_hat_star = t(k_star) %*% alpha
  v = backsolve(L, k_star)
  V_mat = cov_func(test_input_x, test_input_x) - crossprod(v) + diag(noise_level, length(predx))
  
  log_lik = -1/2 * t(y_std) %*% alpha - sum(log(diag(L)))
  
  list(x = x, 
       y = y, 
       predx = test_input_x, 
       f_hat = f_hat_star + mean_y,
       Var_mat = V_mat,
       log_lik = log_lik)
}


gp_fit <- compute_gp(x, y, cov_func_sqexp(.1,1), .5, predx)

pout <- optim(c(.2,.5,.1), function(p) {
  print(p)
  -compute_gp(x, y, cov_func_sqexp(exp(p[1]),exp(p[2])), exp(p[3]), predx)$log_lik
}); p <- pout$par
gp_fit <- compute_gp(x, y, cov_func_sqexp(exp(p[1]),exp(p[2])), exp(p[3]), predx)

true_data <- read.csv("../data/true_data.csv")
margin_of_error <- 2 * sqrt(diag(gp_fit$Var_mat))
bounds <- data.frame(x = predx, y = gp_fit$f_hat, upper = gp_fit$f_hat + margin_of_error, lower = gp_fit$f_hat - margin_of_error)

rdply(.n = 100, .expr = {
  mvtnorm::rmvnorm(1, gp_fit$f_hat, gp_fit$Var_mat)
}) %>% 
  gather("key", "value", -.n) %>% 
  mutate(key = as.numeric(key)) %>% 
  right_join(data.frame(key = 1:length(predx), predx = predx)) %>% 
  ggplot() +
  geom_ribbon(aes(x = x, ymin = lower, ymax = upper), data = bounds, alpha = 0.1, color = "blue") +
  geom_line(aes(predx, value, group = .n), color = "grey80", alpha = 0.5) +
  geom_line(aes(x,y), data = true_data, fill = "red") +
  ggthemes::theme_few() + 
  geom_point(aes(x,y), data = dat)


# Plotting for debugging
  # ggplot(true_data) +
  # geom_line(aes(x = x, y = y)) + 
  # geom_line(aes(x = predx, y = f_hat), color = "red", data = data.frame(predx = gp_fit$predx, f_hat = gp_fit$f_hat)) +
  # geom_ribbon(aes(x = x, ymin = lower, ymax = upper), data = bounds, alpha = 0.1)

