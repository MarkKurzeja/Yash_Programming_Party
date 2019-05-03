rm(list = ls())
library(rstan)
library(magrittr)
library(plyr)
library(dplyr)
library(ggplot2)
library(tidyr)
options(mc.cores = parallel::detectCores())
setwd("C:/Users/mtkur/Dropbox/Coding & Data Projects/Gaussian Processes Example")

mmod <- stan_model(file="gp-predict-mark.stan");
tfun <- function(x) {
  3 * cos(x) - .1 + 2 * x
}

xbegin = seq(5,15, length = 40)
y = tfun(xbegin) + rnorm(length(xbegin), 0, .5)
predx <- seq(5,20, length = 20)


plot(xbegin,y)
curve(tfun, add = T)


sdat <- list(
  n = length(xbegin),
  x = xbegin,
  y = y,
  pred_length = length(predx),
  pred_x = predx,
  nruns = 100
)


res <- sampling(mmod, data=sdat,  iter = 2000, chains = 1)
    

if (type == "MLE") {
  dat <- cbind(data.frame(x = predx), data.frame(y = res$par[res$par %>% names() %>% grep(pattern = "y2\\[", x = .)]))
  dat <- plyr::adply(dat, 1, function(val) {
    data.frame(x = val$x, y = rnorm(500, val$y, res$par["sigma"]))
  }) 
  dat %<>% dplyr::group_by(x) %>% 
    dplyr::summarize(low = quantile(y, probs = 0.025),
                     upper = quantile(y, probs = 0.975),
                     mmean = mean(y),
                     mmedian = median(y))
  
  ggplot(data.frame(x = x, y = y)) + 
    stat_function(fun = tfun) +
    theme_bw() + 
    geom_ribbon(aes(x = x, ymin = low, ymax = upper), dat, alpha = 0.1) + 
    geom_line(aes(x, mmedian), dat, linetype = 2) + 
    geom_point(aes(x,y)) + 
    # scale_y_continuous(limits = c(-5,15)) + 
    # scale_x_continuous(limits = c(0,20)) +
    ggtitle(type)
  
} else {
  output <- rstan::extract(res)
  
  b <- output$y2 %>% t() %>% data.frame() %>% cbind(data.frame(x = predx))
  
  bounds <- b %>% gather("key", "value", -x) %>%
    dplyr::group_by(x) %>% 
    dplyr::summarize(low = quantile(value, probs = 0.025),
                     upper = quantile(value, probs = 0.975),
                     mmean = mean(value),
                     mmedian = median(value))
  
  
  b %>% gather("key", "value", -x) %>%
    ggplot() +
    geom_ribbon(aes(x = x, ymin = low, ymax = upper), bounds, alpha = 0.1) + 
    geom_point(aes(x, y), data.frame(x = x, y = y), color = "black") +
    stat_function(fun = tfun) +
    geom_line(aes(x, mmedian), bounds, linetype = 2) + 
    theme_bw() + 
    ggtitle(type)
  
}



x = c(xbegin, predx)
N = length(x)

nrep <- 10

pp <- ldply(sample(1:nrow(output$rho), 200), function(rnum) {
  K <- matrix(0, ncol = N, nrow = N)
  
  alpha = output$alpha[rnum]
  rho = output$rho[rnum]
  eta = output$eta[rnum,] %>% as.matrix()
  
  for (i in 1:(N - 1)) {
    K[i, i] = 1 + alpha^2
    for (j in (i + 1):N) {
      K[i, j] = alpha^2 * exp(- 1/ (2 * rho^2) * (x[i] - x[j])^2)
      K[j, i] = K[i, j]
    }
  }
  K[N, N] = 1 + alpha^2;
  # for(i in 1:N) K[i,i] = K[i,i] + 1e-9
  # browser()
  
  matrix(rnorm(length(eta) * nrep, as.numeric(t(chol(K)) %*% eta), output$sigma[rnum]), nrow = nrep, byrow = T) 
  
})  %>% t() %>% data.frame() %>% cbind(data.frame(x = x)) %>% 
  tidyr::gather("key", "value", -x)

ggplot(pp) + 
  geom_line(aes(x, value, group = key), alpha = 0.01) +
  theme_bw() + 
  geom_point(aes(x,y), data.frame(x = x, y = tfun(x)), color = "blue")











