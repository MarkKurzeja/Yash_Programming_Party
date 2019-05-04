rm(list = ls())
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
library(rstan)
library(magrittr)
library(plyr)
library(dplyr)
library(ggplot2)
library(tidyr)
options(mc.cores = parallel::detectCores())

# Read in the training data
dat <- read.csv("../data/training_data.csv")
x <- dat$x
y <- dat$y

# Read in the testing data
dat <- read.csv("../data/testing_data.csv")
predx <- dat$x

# Create the data for the input to the algorithm
sdat <- list(
  n = length(x),
  x = x,
  y = y,
  pred_length = length(predx),
  pred_x = predx,
  nruns = 100
)

# Compile the model from source
norm_sq_exp_model <- stan_model(file="gp-predict-normal_sqexp.stan");
# save(norm_sq_exp_model, file = "./norm_sq_exp_model.model")
# load("./norm_sq_exp_model.model")

# Run the stan algorithm
res <- sampling(norm_sq_exp_model, data=sdat,  iter = 500, chains = 1)

output <- rstan::extract(res)

# Take the matrix of outputs from the model and bind them into a prediction matrix
b <- output$y2 %>% t() %>% data.frame() %>% cbind(data.frame(x = predx))

# Extract the relevant stats from the simulations
bounds <- b %>% gather("key", "value", -x) %>%
  dplyr::group_by(x) %>% 
  dplyr::summarize(low = quantile(value, probs = 0.025),
                   upper = quantile(value, probs = 0.975),
                   mmean = mean(value),
                   mmedian = median(value))

# Plot the prediction of the mean function
b %>% gather("key", "value", -x) %>%
  ggplot() +
  geom_ribbon(aes(x = x, ymin = low, ymax = upper), alpha = 0.1, data = bounds) + 
  geom_point(aes(x, y), data.frame(x = x, y = y), color = "black") +
  geom_line(aes(x, mmedian), data = bounds, linetype = 2) + 
  theme_bw() 
  

# Simulation of the posterior predictive

# x = c(xbegin, predx)
# N = length(x)
# 
# nrep <- 10
# 
# pp <- ldply(sample(1:nrow(output$rho), 200), function(rnum) {
#   K <- matrix(0, ncol = N, nrow = N)
#   
#   alpha = output$alpha[rnum]
#   rho = output$rho[rnum]
#   eta = output$eta[rnum,] %>% as.matrix()
#   
#   for (i in 1:(N - 1)) {
#     K[i, i] = 1 + alpha^2
#     for (j in (i + 1):N) {
#       K[i, j] = alpha^2 * exp(- 1/ (2 * rho^2) * (x[i] - x[j])^2)
#       K[j, i] = K[i, j]
#     }
#   }
#   K[N, N] = 1 + alpha^2;
#   # for(i in 1:N) K[i,i] = K[i,i] + 1e-9
#   # browser()
#   
#   matrix(rnorm(length(eta) * nrep, as.numeric(t(chol(K)) %*% eta), output$sigma[rnum]), nrow = nrep, byrow = T) 
#   
# })  %>% t() %>% data.frame() %>% cbind(data.frame(x = x)) %>% 
#   tidyr::gather("key", "value", -x)
# 
# ggplot(pp) + 
#   geom_line(aes(x, value, group = key), alpha = 0.01) +
#   theme_bw() + 
#   geom_point(aes(x,y), data.frame(x = x, y = tfun(x)), color = "blue")











  # ggplot(data.frame(x = 1, y = 1)) +
# stat_function()



# library(tidyr)
# library(magrittr)
# library(ggplot2)
# 
# x = mvtnorm::rmvnorm(100, mean = rep(0,10), sigma = diag(10))
# 
# x %<>% t() %>% data.frame()
# x %<>% cbind(data.frame(v = 1:nrow(x)))
# x %<>% gather("key", "value", -v)
# 
# ggplot(x) + geom_line(aes(v,value, group = key))







