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
# without intercept
d_fixed <- lm(Y ~ X + factor(id) - 1, data = pdata)
summary(d_fixed)
d_fixed$coefficients[1:4]

# with intercept
# different standard errors than without intercept
# interesting that main coefficients are the same, except the time invariant education
d_fixed1 <- lm(Y ~ X + factor(id), data = pdata)
summary(d_fixed)
d_fixed1$coefficients[2:5]

# test spelling out the equation
# same as pre-staged X and Y argument version, with and without intercept
d_fixed3 <- lm(lwage ~ exp + exp2 + wks + ed + factor(id) - 1, data = pdata)
summary(d_fixed)
d_fixed3$coefficients[1:4]

d_fixed4 <- lm(lwage ~ exp + exp2 + wks + ed + factor(id), data = pdata)
summary(d_fixed)
d_fixed4$coefficients[2:5]

# test of fixed effects with least squares dummy variable model
# d_fixed 5 and 6 both have same coefficients and standard errors as each other
# see if i get the same coefficients when taking out education, which is time invariant and dropped from fixed effects model
# it is the same coefficients as fixed effects model and same standard error
d_fixed5 <- lm(lwage ~ exp + exp2 + wks + factor(id) - 1, data = pdata)
summary(d_fixed5)
d_fixed5$coefficients[1:4]

d_fixed6 <- lm(lwage ~ exp + exp2 + wks + factor(id), data = pdata)
summary(d_fixed6)
d_fixed6$coefficients[2:5]

# ls dummy variable model with time dummies


# Fixed effects or within estimator using plm
# plm must automatically use "id" variable as the unit of fixed effects??
fixed <- plm(Y ~ X, data=pdata, model= "within")
summary(fixed)
fixed$coefficients[1:4]

# fixed estimator with variables added manually
# same coefficients and standard errors as using X and Y grouped variables
fixed2 <- plm(lwage ~ exp + exp2 + wks, data=pdata, model= "within")
summary(fixed2)
fixed2$coefficients[1:4]

# try including education as a variable to see if it gets dropped
# it does get dropped, though it doesn't tell you that
fixed3 <- plm(lwage ~ exp + exp2 + wks + ed, data=pdata, model= "within")
summary(fixed3)
fixed3$coefficients[1:4]

# one last test to see if it will include a newly made time-varying variable
# it does include the test, so ed is being properly recognized as time-invariant and thus dropped
# results are same as when I manually drop ed in lm model
# key question is how lm model allows me to estimate coefficient for ed?  
# is it bc ed is not perfectly collinear with any one variable, it doesn't drop it
# even though using "difference from individual average" method of fixed effects it does end up perfectly collinear
# bc dummy minus the average for a dummy is zero, so no variation to use
pdata$test <- pdata$lwage + pdata$ed
fixed4 <- plm(lwage ~ exp + exp2 + wks + test, data=pdata, model= "within")
summary(fixed4)
fixed4$coefficients[1:4]

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
