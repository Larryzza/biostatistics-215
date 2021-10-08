rm(list = ls())

library(survival)
library(KMsurv)
library(tidyverse)
data("drug6mp")

# Q3.4 additional 2

logn_fit <- survreg(Surv(t2, relapse) ~ 1, 
                    data = drug6mp, dist = "lognormal")
summary(logn_fit)

mu    <- logn_fit$coefficients[1]
sigma <- logn_fit$scale

vol <- 10000000
.sim_fun <- function(x){
  prob <- 1-pnorm((log(12)-x[1])/x[2])
  return(prob)
}
set.seed(19969)
sim <- data.frame(sim_mu = rnorm(vol, mu, sqrt(logn_fit$var[1,1])),
                  sim_sigma = rnorm(vol, log(logn_fit$scale), 
                                    sqrt(logn_fit$var[2,2])) %>% exp)

start_time <- Sys.time()
prob <- apply(sim, 1, .sim_fun)
end_time <- Sys.time()
end_time - start_time

quantile(prob, c(0.025, 0.975))
