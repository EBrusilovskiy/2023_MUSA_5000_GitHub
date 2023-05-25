install.packages("DAAG")
install.packages("car")
install.packages("MASS")
install.packages("rsq")
library(DAAG)
library(car)  #to calculate VIF
library(MASS)
library(rsq)

#Avoid getting results in scientific notation!
options(scipen = 999)

setwd("C:/Users/eugeneby/Dropbox/Documents/Work and School/Teaching/CPLN 671 - Statistics and Data Mining/Data/Lecture 6 - R")
#MULTIPLE REGRESSION
multregdata <- read.csv("RegressionData.csv")
#https://subscription.packtpub.com/book/big_data_and_business_intelligence/9781783989065/1/ch01lvl1sec21/creating-dummies-for-categorical-variables 

multregdata$Police.A <- ifelse(multregdata$Police.Unit == 'A', 1, 0)
multregdata$Police.B <- ifelse(multregdata$Police.Unit == 'B', 1, 0)
multregdata$Police.C <- ifelse(multregdata$Police.Unit == 'C', 1, 0)
multregdata$Police.D <- ifelse(multregdata$Police.Unit == 'D', 1, 0)

reg <- lm(MEDHVAL ~ MEDHHINC, data=multregdata)
summary(reg)
reg <- lm(MEDHVAL ~ MEDHHINC + PCTSINGLES, data=multregdata)
summary(reg)

reg <- lm(MEDHVAL ~ MEDHHINC + PCTSINGLES + Police.A + Police.B + Police.C, data=multregdata)
summary(reg)
reg <- lm(MEDHVAL ~ MEDHHINC + PCTSINGLES + Police.A + Police.B + Police.D, data=multregdata)
summary(reg)

# View the first few rows of the data
head(multregdata)

# Summarize each variable and look at the distributions
summary(multregdata)
mean(multregdata$MEDHHINC)
mean(multregdata$MEDHVAL)
mean(multregdata$PCTVACANT)
mean(multregdata$PCTBACHMOR)
sd(multregdata$MEDHHINC)
sd(multregdata$MEDHVAL)
sd(multregdata$PCTVACANT)
sd(multregdata$PCTBACHMOR)


hist(multregdata$MEDHHINC, breaks=50)
hist(multregdata$MEDHVAL, breaks=50)
hist(multregdata$PCTVACANT)
hist(multregdata$PCTBACHMOR)


#Plot the variables to see if a line does a good job with summarizing the relationship
plot(multregdata$MEDHVAL, multregdata$MEDHHINC)
plot(multregdata$PCTVACANT, multregdata$MEDHHINC)
plot(multregdata$PCTBACHMOR, multregdata$MEDHHINC)

#Look at the correlations between the predictors
#We don't need the Zip Code ID in the data, and we want to save the data as a matrix
predictors <- as.matrix(cbind(multregdata$MEDHVAL, multregdata$PCTVACANT, multregdata$PCTBACHMOR))

# type can be pearson or spearman
cor(predictors)

#Running Multiple Regression
fit <- lm(MEDHHINC ~ MEDHVAL + PCTVACANT + PCTBACHMOR, data=multregdata)
summary(fit)
#ANOVA table containing SSE and SSR (SST = SSE + SSR)
#SSR is the Sum Sq for MEDHVAL, PCTVACANT and PCTBACHMOR
anova(fit) 
vif(fit)        #Variance Inflation Factor (in the package car)

#We can see that if we switch the order of the predictors, the p-values of
#each predictor in the ANOVA output will change - this is the p-value of the
#predictor after accounting for all predictors on the previous lines.
#On the other hand, the p-value of the predictors in the summary(fit) command
#Does not depend on the order of predictors. 
#For our intents and purposes, we look at the p-values in the summary(fit) command,
#which use t-tests.
#fit1 <-lm(MEDHHINC ~  PCTBACHMOR + MEDHVAL + PCTVACANT, data=multregdata)
#summary(fit1)
#anova(fit1)
#anova(fit)

#Partial R-Squared
#Partial r squared. These are partial correlations, squared, between predictor i
#and the dependent variable, y, with the influence of the other variables in the
#regression equation eliminated. The partial correlation coefficient squared is a
#measure of the extent to which that part of the variation in the dependent variable 
#which is not explained by the other predictors is explained by predictor i. 
#Source: http://www.unesco.org/webworld/portal/idams/html/english/E2regres.htm.
#For more information on partial R-squared, see:
#http://biol09.biol.umontreal.ca/borcardd/partialr2.pdf.

#NOTE: Partial R-squareds don't have to add up to the total R-squared for the 
#model because some of the variance in the dependent variable is explained by 
#more than one predictor - this happens when the predictors are correlated and their
#common variance is also the variance that they share with the dependent variable.
fit.glm <- glm(MEDHHINC ~ MEDHVAL + PCTVACANT + PCTBACHMOR, data=multregdata)
summary(fit.glm)
rsq.partial(fit.glm)



#Some other useful functions:
#Model coefficients
coefficients(fit) 
#CIs for model parameters 
confint(fit, level=0.95) 
#Predicted values (y-hats)
multregdata$predvals <- fitted(fit) 
#Residuals
multregdata$resids <- residuals(fit)
hist(multregdata$resids)
#Standardized Residuals
multregdata$stdres <- rstandard(fit)


#Regression Diagnostics - standardized residuals by predicted plot
#Classic example of heteroscedasticity: variance of residuals is small when y-hats
#(i.e., predicted values are small and large when residuals are large)
plot(multregdata$predvals, multregdata$stdres)

#We can also plot standardized residuals by each predictor to examine 
#heteroscedasticity more in depth
plot(multregdata$MEDHVAL, multregdata$stdres)
plot(multregdata$PCTVACANT, multregdata$stdres)
plot(multregdata$PCTBACHMOR, multregdata$stdres)


#Another way to do diagnostic plots
plot(fit)

#If we had to take the log of a variable (e.g., variable MEDHHINC)
multregdata$LNMEDHHINC = log(1+multregdata$MEDHHINC)
#We can see that the variable MEDHHINC is now in the data set, and we can use it
#in our analyses.
head(multregdata)


#CROSS-VALIDATION
#Model 1
fit <- lm(MEDHHINC ~ PCTVACANT + PCTSINGLES, data=multregdata)
summary(fit)
anova(fit)
#In the output: 
#Predicted (Predicted values using all observations) 
#cvpred (cross-validation predictions)
#CV residual = y (in this case, MEDHHINC) - cvpred
cv <- CVlm(data=multregdata, fit, m=5)				        #m=5 sets it to 5 folds
#Extracting MSEs
mse <- attr(cv, "ms")
mse
rmse <- sqrt(mse)						  #Obtaining RMSE for model 1
rmse

#Model 2
fit <- lm(MEDHHINC ~ PCTVACANT + MEDHVAL + PCTSINGLES, data=multregdata)
summary(fit)
anova(fit)
cv <- CVlm(data=multregdata, fit, m=5)				        #m=5 sets it to 5 folds
summary(cv)

#Extracting MSEs
mse <- attr(cv, "ms")
mse
rmse <- sqrt(mse)					                      #Obtaining RMSE for model 2
rmse


#STEPWISE REGRESSION
fit <- lm(MEDHVAL~ PCTBACHMOR + PCTVACANT + MEDHHINC + PCTSINGLES,data=multregdata)
step <- stepAIC(fit, direction="both")
# display results
#We see that in the final model PCTVACANT was dropped
step$anova



#number is the number of folds. Here, we have 10 folds
install.packages("tidyverse")
install.packages("caret")
library(tidyverse)
library(caret)
#number is the number of folds. Here, we have 10 folds
train_control=caret::trainControl(method="cv", number=10)

model <- caret::train(MEDHVAL~ PCTBACHMOR + PCTVACANT + MEDHHINC + PCTSINGLES, data=multregdata, method="lm", trControl=train_control)

#this will give us the RMSE, which is all we needprint(model)
print(model)