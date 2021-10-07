library(survival)
library(KMsurv)
library(tidyverse)
data("drug6mp")

logn_fit <- survreg(Surv(t2, relapse) ~ 1, 
                    data = drug6mp, dist = "lognormal")
summary(logn_fit)

mu    <- logn_fit$coefficients[1]
sigma <- logn_fit$scale


prob <- NULL
set.seed(12345)
for(i in 1:1000000){
  sim_mu <- rnorm(1, mu, sqrt(logn_fit$var[1,1]))
  sim_sigma <- rnorm(1, log(logn_fit$scale), sqrt(logn_fit$var[2,2])) %>% exp 
  prob <- c(prob, 1-pnorm((log(12)-sim_mu)/sim_sigma))
}
quantile(prob, c(0.025, 0.975))
