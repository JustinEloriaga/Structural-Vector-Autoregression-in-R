#SVAR in R
#Justin S. Eloriaga

library(urca)
library(vars)
library(mFilter)
library(tseries)
library(TSstudio)
library(forecast)
library(tidyverse)

#Loading the Dataset

macro <- read_csv(file.choose())
head(macro)

#Creating thee Time Series Objectives

y <- ts(macro$`Output Gap`, start = c(2000,1,1), frequency = 4)
pi <- ts(macro$CPI, start = c(2000,1,1), frequency = 4)
r <- ts(macro$RRP, start = c(2000,1,1), frequency = 4)

#Time Series Plots

ts_plot(y, title = "Output Gap", Xtitle = "Time", Ytitle = "Output Gap")
ts_plot(pi, title = "Inflation Rate", Xtitle = "Time", Ytitle = "Inflation Rate")
ts_plot(r, title = "Overnight Reverse Repurchase Rate", Xtitle = "Time", Ytitle = "RRP")

#Setting the Restrictions

amat <- diag(3)
amat[2,1] <- NA
amat[3,1] <- NA
amat[3,2] <- NA
amat

#Buidling the Model

sv <- cbind(y, pi, r)
colnames(sv) <- cbind("OutputGap", "Inflation", "RRP")

lagselect <- VARselect(sv, lag.max = 8, type = "both")
lagselect$selection
lagselect$criteria

Model1 <- VAR(sv, p = 5, season = NULL, exog = NULL, type = "const") 
SVARMod1 <- SVAR(Model1, Amat = amat, Bmat = NULL, hessian = TRUE, estmethod = c("scoring", "direct"))
SVARMod1

#Impulse Response Functions

SVARog <- irf(SVARMod1, impulse = "OutputGap", response = "OutputGap")
SVARog
plot(SVARog)

SVARinf <- irf(SVARMod1, impulse = "OutputGap", response = "Inflation")
SVARinf
plot(SVARinf)

SVARrrp <- irf(SVARMod1, impulse = "Inflation", response = "RRP")
SVARrrp
plot(SVARrrp)

#Forecast Error Variance Decomposition

SVARfevd <- fevd(SVARMod1, n.ahead = 10)
SVARfevd
plot(SVARfevd)
