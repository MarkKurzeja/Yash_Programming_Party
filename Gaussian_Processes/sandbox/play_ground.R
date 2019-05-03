library(magrittr)
library(mvtnorm)
library(ggplot2)
library(MASS)
library(plyr)
library(dplyr)

rm(list = ls())

set.seed(123)
fx <- function(x)
{
  cos(3 * x) + x^(1/2) - sin(x)
}
# x = seq(1, 10 * 1, length = 100)[c(1:30, 40, 41, 50:100)]
x = seq(1, 10 * 1, length = 75)
y = fx(x) + rnorm(length(x), 0, .25)

# plot(x,y)
curve(fx, add = T)

# Control_matrix for computing the grid search for the optimization problem over the parameters
# control_matrix = expand.grid(sigma_n = seq(0.001, .4, length = 5), cov_errors = c(seq(0.00, 0.02, by = 0.005)), l_param = seq(0.1, 1, length = 5), alpha_param = c(0.1, 0.2, 1, 0.5))
control_matrix = expand.grid(sigma_n = seq(0.025, 0.11, length = 10), cov_errors = seq(0, .5, length = 10), l_param = seq(0.3, 1, length = 10), alpha_param = seq(0.2, 3, length = 10))
# control_matrix = expand.grid(sigma_n = 00.1, cov_errors = 0.00, l_param = .4214)

results <- adply(control_matrix, 1, function(input) {
  cov_mat_compute <- function(x, xstar, l_param = input$l_param, alpha_param = input$alpha_param, type_cov = c("rat_quad", "squared_exp")) {
    type_cov = match.arg(type_cov)
    if(type_cov == "squared_exp") {
      t = exp(outer(X = x, Y = xstar, FUN = function(x,y) {x - y})^2 * -1 / (2 * l_param^2))
    } else { # is a rational quadratic
      t = (1 + (outer(X = x, Y = xstar, FUN = function(x,y) {abs(x - y)})^2 / (2 * alpha_param * l_param^2)))^(-alpha_param)
    }
    return(t)
  }
  
  
  prediction_x = seq(1, 10 * 1, length = 100) %>% sort
  # prediction_x = x %>% sort
  
  # Sigma_n is the standard deviation of the noise on the observations (a global constant for now)
  # and the noise matrix is diag iff the noise of each observation is uncorrelated
  sigma_n = input$sigma_n
  cov_errors = input$cov_errors # Covariance of the errors to eachother
  noise_matrix = sigma_n * diag(length(x)) + cov_errors
  
  # Compute the mean function using 2.23 Rasmussen
  hat_f = cov_mat_compute(prediction_x, x) %*% 
    solve(cov_mat_compute(x,x) + noise_matrix) %*% 
    y
  
  
  # So, looking at the process, we can see that the length parameter does not impact the computation of the mean. 
  # Sigma_n lets us tune how much error we think each of the observations have - if sigma_n is zero then the 
  # mean function will follow the points exactly and if it is high, then it will be bias towards zero (that is the prior assumption
  # on the entire mean of the process in general)
  
  # Compute the covariance matrix using 2.24 Rasmussen
  cov_f_hat = cov_mat_compute(prediction_x, prediction_x) - 
    cov_mat_compute(prediction_x, x) %*% 
    solve(cov_mat_compute(x,x) + noise_matrix) %*% 
    cov_mat_compute(x, prediction_x)
  
  # Generate samples from the predictive distribution
  sampless <- MASS::mvrnorm(n = 200, mu = hat_f, Sigma = cov_f_hat)
  col_lower = apply(X = sampless, 2, function(x) {quantile(x, probs = 0.05)})
  col_upper = apply(X = sampless, 2, function(x) {quantile(x, probs = 0.975)})
  
  bounds <- data.frame(x = prediction_x, lower = col_lower, upper = col_upper)
  
  # If there is only one control matrix var, then use it to show a plot of the data
  if(nrow(control_matrix) == 1) {
    myplot = ggplot() +
      geom_point(aes(x, y)) +
      geom_line(aes(prediction_x, hat_f)) +
      geom_segment(aes(x = x, y = lower, xend = x, yend = upper), data = bounds) 
  
    show(myplot)
  }
  
  # Compute the log of the error using 2.30 Rasmussen   
  K = cov_mat_compute(x, x)
  log_error = -1/2 * t(y) %*% solve(K + noise_matrix) %*% y - 
    1/2 * log(det(K + noise_matrix)) - 
    length(x) / 2 * log(2 * pi)
  data.frame(log_error = log_error)
}, .progress = progress_win())

results_ret <- results %>% 
  mutate(log_error = log_error - max(log_error, na.rm = T)) %>% 
  mutate(error = exp(log_error)) %>% 
  mutate(error = round(error / sum(error), 3)) %>% 
  arrange(-error) %>% dplyr::as_data_frame()
print(results_ret %>% head())

# Plot the posteriors of the parameters given a flat prior over the space
if (nrow(control_matrix) != 1) {
  
  g1 <- results_ret %>% 
    dplyr::select(sigma_n, error) %>% 
    dplyr::group_by(sigma_n) %>% 
    dplyr::summarize(error = sum(error)) %>% 
    ggplot() + 
    geom_bar(mapping = aes(x = sigma_n, y = error), stat = "identity")
  
  g2 = results_ret %>% 
    dplyr::select(l_param, error) %>% 
    dplyr::group_by(l_param) %>% 
    dplyr::summarize(error = sum(error)) %>% 
    ggplot() + 
    geom_bar(mapping = aes(x = l_param, y = error), stat = "identity")
  g3 = results_ret %>% 
    dplyr::select(alpha_param, error) %>% 
    dplyr::group_by(alpha_param) %>% 
    dplyr::summarize(error = sum(error)) %>% 
    ggplot() + 
    geom_bar(mapping = aes(x = alpha_param, y = error), stat = "identity")
  
  gridExtra::grid.arrange(g1, g2, g3)

}

# Plot a variety of the predictive posteriors using resampling
mmeans <- rdply(1000, {
  k = sample(x = 1:nrow(results_ret), size = 1, prob = results_ret$error)
  v = results_ret[k, ]
  
  cov_mat_compute <- function(x, xstar, l_param = v$l_param, alpha_param = v$alpha_param, type_cov = c("rat_quad", "squared_exp")) {
    type_cov = match.arg(type_cov)
    if(type_cov == "squared_exp") {
      t = exp(outer(X = x, Y = xstar, FUN = function(x,y) {x - y})^2 * -1 / (2 * l_param^2))
    } else { # is a rational quadratic
      t = (1 + (outer(X = x, Y = xstar, FUN = function(x,y) {abs(x - y)})^2 / (2 * alpha_param * l_param^2)))^(-alpha_param)
    }
    return(t)
  }
  prediction_x = seq(1, 10 * 1, length = 100) %>% sort
  sigma_n = v$sigma_n
  cov_errors = v$cov_errors # Covariance of the errors to eachother
  noise_matrix = sigma_n * diag(length(x)) + cov_errors
  hat_f = cov_mat_compute(prediction_x, x) %*% 
    solve(cov_mat_compute(x,x) + noise_matrix) %*% 
    y
  cov_f_hat = cov_mat_compute(prediction_x, prediction_x) - 
    cov_mat_compute(prediction_x, x) %*% 
    solve(cov_mat_compute(x,x) + noise_matrix) %*% 
    cov_mat_compute(x, prediction_x)
  
  sampless <- MASS::mvrnorm(n = 10, mu = hat_f, Sigma = cov_f_hat)
  data.frame(x = prediction_x, mean = hat_f, y=t(sampless))
  })

# Gather the data for plotting
mmeans %<>% tidyr::gather("y", "value", -`.n`, -x, -mean) %>% dplyr::as_data_frame()
mmeans_lower_upper <- mmeans %>% 
  dplyr::group_by(x) %>% 
  dplyr::summarize(lower = quantile(value, probs = 0.025), upper = quantile(value, probs = 0.975)) %>%
  dplyr::ungroup()
mmeans %>% head()

hat_f_mean = mmeans %>% group_by(x) %>% summarize(mean = mean(mean))

ggplot() +
  geom_ribbon(aes(x = x, ymin = lower, ymax = upper), data = mmeans_lower_upper, alpha = 0.1, color = "red") +
  geom_line(aes(x, mean), data = hat_f_mean, col = "blue") +
  geom_point(aes(x, y)) +
  # scale_x_continuous(limits = range(x)) + 
  # stat_function(fun = x) +
  geom_line(aes(x,y), data = data.frame(x = seq(1, 10 * 1, length = 100), y = fx(seq(1, 10 * 1, length = 100)))) + 
  ggtitle("Predictive Posterior of Gaussian Process")
  
