# Probit and Logit Models in R
# Copyright 2013 by Ani Katchova

mydata<- read.csv("probit_insurance.csv")
attach(mydata)

# Define variables
Y <- cbind(ins)
X <- cbind(retire, age, hstatusg, hhincome, educyear, married, hisp)

# Descriptive statistics
summary(Y)
summary(X)

table(Y)
table(Y)/sum(table(Y))

# Logit model coefficients
logit<- glm(Y ~ X, family=binomial (link = "logit"))
summary(logit) 

# Logit model odds ratios
exp(logit$coefficients)

# Probit model coefficients
probit<- glm(Y ~ X, family=binomial (link="probit"))
summary(probit)

# logit marginal effects at mean with mfx package
library(mfx)
logitmfx(Y ~ X, data = mydata)

# logit mean marginal effects with mfx package 
logitmfx(Y ~ X, data = mydata, atmean = FALSE)

# correctly predicted values
# note that logit$fitted automatically transforms the predicted Y from logit/probit to probability
table(true = Y, pred = round(probit$fitted))
table(true = Y, pred = round(logit$fitted)) 

confusionMatrix(Y, round(probit$fitted))
confusionMatrix(Y, round(logit$fitted))

# McFadden's Pseudo R-squared
probit0<-update(probit, formula= Y ~ 1)
McFadden<- 1-as.vector(logLik(probit)/logLik(probit0))
McFadden