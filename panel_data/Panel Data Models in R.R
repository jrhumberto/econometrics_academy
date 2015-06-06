# Panel Data Models in R
# Copyright 2013 by Ani Katchova

# install.packages("plm")
library(plm)

mydata<- read.csv("panel_wage.csv")
attach(mydata)

Y <- cbind(lwage)
X <- cbind(exp, exp2, wks, ed)

# Set data as panel data
pdata <- plm.data(mydata, index=c("id","t"))

# Descriptive statistics
summary(Y)
summary(X)

# fixed effects using least squares dummy variable model
# optional to remove intercept
# same coefficients and standard errors as plm, but takes longer to calculate and has messy output
# also, oddly does not remove time invariant education variable??
d_fixed <- lm(Y ~ X + factor(id) - 1, data = pdata)
summary(d_fixed)

# Fixed effects or within estimator using plm
fixed <- plm(Y ~ X, data=pdata, model= "within")
summary(fixed)

# fixed effects with time effects using plm
fixed_time <- plm(Y ~ X + factor(t), data=pdata, model= "within")
fixed_time
# for some reason, summary(fixed_time) throws an error??
summary(fixed_time)

# random effects using plm
random <- plm(Y ~ X, data=pdata, model= "random")
summary(random)

# Hausman test for fixed versus random effects model 
# reject null means one model is inconsistent, so use fixed effects model
phtest(random, fixed)

# test for whether or not to include of time-fixed effects, null is no need to include time-fixed effects
pFtest(fixed_time, fixed)

# another test for inclusion of time-fixed effects, null is no need to include time-fixed effects
plmtest(fixed, c("time"), type = ("bp"))

# test for serial correlation, which can be a problem for long macro data sets, though not often for small micro panel data sets
# null is no serial correlation
pbgtest(fixed)

# breusch-pagan test for heteroskedasticity, null is no heteroskedasticity
# if heteroskedasticity is present, you can use robust covariance matrix to account for it
bptest (Y ~ X + factor(id), data = pdata, studentize = FALSE)

# controlling for heteroskedasticity in fixed effects model
# note the coefficients remain unchanged, but the standard errors are larger for vcovHC 
coeftest(fixed)

coeftest(fixed, vcovHC(fixed, method = "arellano"))
