library(rstan)
library(magrittr)
library(plyr)
library(dplyr)
library(ggplot2)
library(tidyr)
options(mc.cores = parallel::detectCores())
setwd("C:/Users/Mark K/Dropbox/Coding & Data Projects/Gaussian Processes Example")
rm(list = ls())
mmod <- stan_model(file="gp-predict-mark-fast.stan");

tfun <- function(x) {
  cos(x) 
}

x = seq(1,15,length = 60)
y = tfun(x) + rnorm(length(x), 0, .2)
predx <- seq(1,20, length = 40)


# plot(x,y)
# curve(tfun, add = T)

sdat <- list(
  N = length(x),
  x = x,
  y = y,
  pred_length = length(predx),
  pred_x = predx
)

res <- sampling(mmod, sdat, iter = 600, chains = 1, pars = c("rho", "alpha", "sigma", "y2"))  

print(res)

output <- extract(res)

# output$y2 %>% t() %>% 
#   data.frame() %>%
#   cbind(data.frame(x = x)) %>%
#   tidyr::gather("key", "value", -x) %>% 
#   ggplot() + 
#   geom_line(aes(x, value, group = key), alpha = 0.11) +
#   geom_point(aes(x,y), data.frame(x = x, y = y), color = "blue")


b <- output$y2 %>% t() %>% data.frame() %>% cbind(data.frame(x = x))

bounds <- b %>% gather("key", "value", -x) %>%
  dplyr::group_by(x) %>% 
  dplyr::summarize(low = quantile(value, probs = 0.025),
                   upper = quantile(value, probs = 0.975),
                   mmean = mean(value),
                   mmedian = median(value))


b %>% gather("key", "value", -x) %>%
  ggplot() +
  geom_ribbon(aes(x = x, ymin = low, ymax = upper), bounds, alpha = 0.1) + 
  # geom_line(aes(x, value, group = key), alpha = 0.01) + 
  geom_point(aes(x, y), data.frame(x = x, y = y), color = "black") +
  stat_function(fun = tfun) +
  # scale_y_continuous(limits = c(-5,15)) + 
  # scale_x_continuous(limits = c(0,20)) +
  geom_line(aes(x, mmedian), bounds, linetype = 2) + 
  theme_bw()  








# Optimizing Only?
# type = "MLE"
# type = "VB"
# type = "BAYES"
# 
# if (type != "MLE") {
#   if (type == "VB") {
#     res <- vb(mmod, data=sdat)
#     
#   } else {
#     res <- sampling(mmod, data=sdat, open_progress = F)
#     
#   }
# } else {
#   res <- optimizing(mmod, data=sdat)
# }
# 
# # print(res, pars = c('rho','alpha','sigma', "eta"))
# 
# if (type == "MLE") {
#   dat <- cbind(data.frame(x = predx), data.frame(y = res$par[res$par %>% names() %>% grep(pattern = "y2\\[", x = .)]))
#   dat <- plyr::adply(dat, 1, function(val) {
#     data.frame(x = val$x, y = rnorm(500, val$y, res$par["sigma"]))
#   }) 
#   dat %<>% dplyr::group_by(x) %>% 
#     dplyr::summarize(low = quantile(y, probs = 0.025),
#                      upper = quantile(y, probs = 0.975),
#                      mmean = mean(y),
#                      mmedian = median(y))
#   
#   ggplot(data.frame(x = x, y = y)) + 
#     stat_function(fun = tfun) +
#     theme_bw() + 
#     geom_ribbon(aes(x = x, ymin = low, ymax = upper), dat, alpha = 0.1) + 
#     geom_line(aes(x, mmedian), dat, linetype = 2) + 
#     geom_point(aes(x,y)) + 
#     scale_y_continuous(limits = c(-5,15)) + 
#     scale_x_continuous(limits = c(0,20)) +
#     ggtitle(type)
#   
# } else {
#   output <- extract(res)
#   
#   b <- output$y2 %>% t() %>% data.frame() %>% cbind(data.frame(x = predx))
#   
#   bounds <- b %>% gather("key", "value", -x) %>%
#     dplyr::group_by(x) %>% 
#     dplyr::summarize(low = quantile(value, probs = 0.025),
#                      upper = quantile(value, probs = 0.975),
#                      mmean = mean(value),
#                      mmedian = median(value))
#   
#   
#   b %>% gather("key", "value", -x) %>%
#     ggplot() +
#     geom_ribbon(aes(x = x, ymin = low, ymax = upper), bounds, alpha = 0.1) + 
#     # geom_line(aes(x, value, group = key), alpha = 0.01) + 
#     geom_point(aes(x, y), data.frame(x = x, y = y), color = "black") +
#     stat_function(fun = tfun) +
#     # scale_y_continuous(limits = c(-5,15)) + 
#     # scale_x_continuous(limits = c(0,20)) +
#     geom_line(aes(x, mmedian), bounds, linetype = 2) + 
#     theme_bw() + 
#     ggtitle(type)
#   
# }
# 
# 
# # ggplot(data.frame(x = 1, y = 1)) +
# # stat_function()
# 
# 
# 
# # library(tidyr)
# # library(magrittr)
# # library(ggplot2)
# # 
# # x = mvtnorm::rmvnorm(100, mean = rep(0,10), sigma = diag(10))
# # 
# # x %<>% t() %>% data.frame()
# # x %<>% cbind(data.frame(v = 1:nrow(x)))
# # x %<>% gather("key", "value", -v)
# # 
# # ggplot(x) + geom_line(aes(v,value, group = key))
# 
# 
# 
# 
# 
# 
# 
