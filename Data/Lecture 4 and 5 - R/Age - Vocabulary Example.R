setwd("C:\\Users\\Administrator\\Dropbox\\Documents\\Work and School\\Teaching\\CPLN 671 - Statistics and Data Mining\\Data\\Lecture 4 and 5 - R")
simpleregdata <- read.csv("Age - Vocabulary Example.csv")

#Avoid getting results in scientific notation!
options(scipen = 999)

# View the first few rows of the data
head(simpleregdata)

# Summarize each variable
summary(simpleregdata)

#Plot the variables to see if a line does a good job with summarizing the relationship
plot(simpleregdata$ChildAge, simpleregdata$Vocabulary)

#Running Simple Regression
fit <- lm(Vocabulary ~ ChildAge, data=simpleregdata)
summary(fit)

#Some other useful functions:
#Model coefficients
coefficients(fit) 
#CIs for model parameters 
confint(fit, level=0.95) 

#Predicted values
simpleregdata$predvals <- fitted(fit) 

#Residuals (and histogram of residuals)
simpleregdata$resids <- residuals(fit)
hist(simpleregdata$resids)

#Standardized Residuals
simpleregdata$stdres <- rstandard(fit)

#Let's look at the file simpleregdata now
simpleregdata

#Sum of squared errors (SSE)
SSE <- sum(simpleregdata$resids^2)
SSE

#Total sum of squares (SST)
                #(y                  - y-bar)^2
SST <- sum((simpleregdata$Vocabulary-mean(simpleregdata$Vocabulary))^2)
SST

#Regression sum of squares (SSR)
SSR <- SST - SSE
SSR

R.Square <- SSR/SST

Variance.Table <-cbind(SSE, SSR, SST, R.Square)
Variance.Table

#ANOVA table containing SSE and SSR (SST = SSE + SSR)
anova(fit) 
SST <- 4181333 + 199667

#Regression Diagnostics - standardized residual by predicted plot
plot(simpleregdata$predvals, simpleregdata$stdres)

#Another way to do diagnostic plots
plot(fit)

#How to extract some relevant information out of the regression results
#Let's first look at the contents of the list 'fit' and some relevant 
#components of that list.
str(fit)                            #str stands for structure (of an object)
coefficients <- fit$coefficients
residuals <- fit$residuals

str(summary(fit))
summary(fit)$call
summary(fit)$coefficients
tvals <- summary(fit)$coefficients[,3]  #3rd column in coefficients matrix
tvals
pvals <- summary(fit)$coefficients[,4]  #4th column in coefficients matrix
pvals