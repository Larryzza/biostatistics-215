library(KMsurv)
data("kidney")

library(survival)
require(SurvRegCensCov)
#12.1
data(tongue)
df1 <- tongue[which(tongue$type==1), ]
df2 <- tongue[-which(tongue$type==1), ]

fit.wei <- survreg(Surv(time, delta) ~ 1, data = df1, dist = "weibull")
summary(fit.wei)
ConvertWeibull(fit.wei)
fit.wei$var

summary(fit.wei)$table[2,1]/summary(fit.wei)$table[2,3]->z
2*pnorm(z, lower.tail = F)

fit.wei.ex <- survreg(Surv(time, delta) ~ 1, data = df1, dist = "exponential")
2*(fit.wei$loglik[2]-fit.wei.ex$loglik[2]) -> chi
1-pchisq(chi, df=1)

predict(fit.wei,type = "quantile", p = 0.5, newdata = data.frame(1))

matrix(91.8*c(1,-0.367),1,2) %*%
  matrix(c(0.049,0.0079*1.2,0.0079*1.2,0.023*1.2*1.2),2,2) %*%
  matrix(91.8*c(1,-0.367),2,1)


fit.wei <- survreg(Surv(time, delta) ~ 1, data = df2, dist = "weibull")
summary(fit.wei)
fit.wei$var

summary(fit.wei)$table[2,1]/summary(fit.wei)$table[2,3]->z
2*pnorm(z, lower.tail = F)

fit.wei.ex <- survreg(Surv(time, delta) ~ 1, data = df2, dist = "exponential")
2*(fit.wei$loglik[2]-fit.wei.ex$loglik[2]) -> chi
1-pchisq(chi, df=1)

predict(fit.wei,type = "quantile", p = 0.5, newdata = data.frame(1))

fit.wei <- survreg(Surv(time, delta) ~ type, data = tongue, dist = "weibull")
summary(fit.wei)
1-pchisq(2*(300.7-298.9), df=1)
fit.wei$var

matrix(c(1/1.24,0.669/1.24^2),1,2) %*%
  matrix(c(0.12,-0.0068*1.24,-0.0068*1.24,0.0134*1.24*1.24),2,2) %*%
  matrix(c(1/1.24,0.669/1.24^2),2,1)

# 12.2
df1 <- kidney[which(kidney$type==1), ]
df2 <- kidney[-which(kidney$type==1), ]
fit.wei <- survreg(Surv(time, delta) ~ 1, data = df1, dist = "weibull")
fit.wei.ex <- survreg(Surv(time, delta) ~ 1, data = df1, dist = "exponential")
summary(fit.wei)
summary(fit.wei.ex)
fit.wei$var
1-pchisq(4.4, df=1)

fit.wei <- survreg(Surv(time, delta) ~ 1, data = df2, dist = "weibull")
fit.wei.ex <- survreg(Surv(time, delta) ~ 1, data = df2, dist = "exponential")
summary(fit.wei)
summary(fit.wei.ex)
1-pchisq(7, df=1)
fit.wei$var

fit.wei <- survreg(Surv(time, delta) ~ type, data = kidney, dist = "weibull")
summary(fit.wei)
fit.wei$var

matrix(c(1/1.14,0.62/1.14^2),1,2) %*%
  matrix(c(0.22,-0.021*1.14,-0.021*1.14,0.0279*1.14*1.14),2,2) %*%
  matrix(c(1/1.14,0.62/1.14^2),2,1)
