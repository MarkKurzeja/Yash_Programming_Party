rm(list = ls())
library(rstan)
library(magrittr)
library(plyr)
library(dplyr)
library(ggplot2)
library(tidyr)
options(mc.cores = parallel::detectCores())
setwd("C:/Users/mtkur/Dropbox/Coding & Data Projects/Yash_Programming_Party")


fx <- function(x) {
  3 * cos(x) - .1 + 2 * x
}

# Create the sample data
x = seq(5,15, length = 40)
y = fx(x) + rnorm(length(x), 0, .5)
predx <- seq(5,20, length = 20)

# Plot the sample data
plot(x,y)
curve(fx, add = T)

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
mmod <- stan_model(file="gp-predict-mark.stan");

# Run the stan algorithm
res <- sampling(mmod, data=sdat,  iter = 500, chains = 1)

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
  geom_ribbon(aes(x = x, ymin = low, ymax = upper), bounds, alpha = 0.1) + 
  geom_point(aes(x, y), data.frame(x = x, y = y), color = "black") +
  stat_function(fun = fx) +
  geom_line(aes(x, mmedian), bounds, linetype = 2) + 
  theme_bw() + 
  ggtitle(type)
  

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







