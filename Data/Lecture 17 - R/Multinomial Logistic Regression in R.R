#EUGENE BRUSILOVSKIY
#MUSA 501
#MULTINOMIAL LOGISTIC REGRESSION IN R


#install.packages("packagename")
install.packages("aod")
install.packages("ggplot2")
install.packages("rms")
install.packages("gmodels")
install.packages("nnet")

#library(packagename)
library(aod)
library(ggplot2)
library(rms)
library(gmodels)
library(nnet)

setwd("C:/Users/eugeneby/Dropbox/Documents/Work and School/Teaching/CPLN 671 - Statistics and Data Mining/Data/Lecture 17 - R")

mlr <-read.csv("Multinomial Logistic Regression.csv")
#Using the factor command to indicate to create the variable transit in dataset mlr. 
#This is the same variable as Transportation, but categorical and not continuous.
mlr$transit <- factor(mlr$Transportation)
#Using the relevel command to indicate that category 3 will be the reference category here (i.e., we're comparing driving (1) and public transportation (2) to walking (3))
mlr$transit.dv <- relevel(mlr$transit, ref = 3)
#Running the regression model
test <- multinom(transit.dv ~ TwoMiRadius + WeeklyIncome + Gender + Parent, data = mlr)
summary(test)

#Calculating the z-scores (Wald statistics). As usual, these are simply the Beta coefficients divided by the standard errors
z <- summary(test)$coefficients/summary(test)$standard.errors
#Calculating the p-values from the z-statistics (2-tailed test)
p <- (1 - pnorm(abs(z), 0, 1)) * 2

#The way that the Beta coefficients, standard errors, z-scores and p-values are presented are very hard to read
#We combine all the results into a single matrix. Here, recall that t() is the command for transpose.
#Again, we're transposing the coefficients matrix, transposing the standard error matrix, transposing the z-score matrix, and transposing the p-value matrix
#We combine (merge) these transposed matrices into a single matrix with the command cbind
coeffs <- cbind(t(summary(test)$coefficients),t(summary(test)$standard.errors),t(z),t(p))
coeffs

#Below, regression 1 is regression comparing Y=1 (driving) to Y=3 (walking) and regression 2 is comparing Y=2 (public transportation) to Y=3 (walking)
#You will notice that the first column of the coeffs matrix contains the coefficients in regression 1 (compar)
#First column of the coeffs matrix contains the coefficients in regression 1 (recall how we created the coeffs matrix)
coef1 <- coeffs[,1]
#Second column of the coeffs matrix contains the coefficients in regression 2
coef2 <- coeffs[,2]
#Third column of the coeffs matrix contains the standard errors in regression 1
se1 <- coeffs[,3]
#Fourth column of the coeffs matrix contains the standard errors in regression 2
se2 <- coeffs[,4]
#Fifth column of the coeffs matrix contains the z-scores in regression 1
z.score1 <- coeffs[,5]
#Sixth column in the coeffs matrix contains the z-scores in regression 2
z.score2 <- coeffs[,6]
#Seventh column in the coeffs matrix contains the p-values in regression 1
p.value1 <- coeffs[,7]
#Eighth column in the coeffs matrix contains the p-values in regression 2
p.value2 <- coeffs[,8]

#Now let's create the odds ratio by exponentiating the coefficients
or1 <- exp(coef1)
or2 <- exp(coef2)

#reg1 combines the coefficients, standard errors, z scores and p-valeus for regression 1
reg1 <- cbind(coef1, se1, z.score1, p.value1, or1)
#reg2 combines the coefficients, standard errors, z scores and p-valeus for regression 2
reg2 <- cbind(coef2, se2, z.score2, p.value2, or2)

#Now we can look at the output
reg1
reg2


#Comparing categories 1 and 2 (category 2 is the reference category)
mlr <-read.csv("Multinomial Logistic Regression.csv")
#Using the factor command to indicate to create the variable transit in dataset mlr. This is the same variable as Transportation, but categorical and not continuous.
mlr$transit <- factor(mlr$Transportation)
#Using the relevel command to indicate that category 3 will be the reference category here (i.e., we're comparing driving (1) and public transportation (2) to walking (3))
mlr$transit.dv <- relevel(mlr$transit, ref = 2)
#Running the regression model
test <- multinom(transit.dv ~ TwoMiRadius + WeeklyIncome + Gender + Parent, data = mlr)
summary(test)

#Calculating the z-scores (Wald statistics). As usual, these are simply the Beta coefficients divided by the standard errors
z <- summary(test)$coefficients/summary(test)$standard.errors
#Calculating the p-values from the z-statistics (2-tailed test)
p <- (1 - pnorm(abs(z), 0, 1)) * 2

#The way that the Beta coefficients, standard errors, z-scores and p-values are presented are very hard to read
#We combine all the results into a single matrix. Here, recall that t() is the command for transpose.
#Again, we're transposing the coefficients matrix, transposing the standard error matrix, transposing the z-score matrix, and transposing the p-value matrix
#We combine (merge) these transposed matrices into a single matrix with the command cbind
coeffs <- cbind(t(summary(test)$coefficients),t(summary(test)$standard.errors),t(z),t(p))
coeffs

#Below, regression 1 is regression comparing Y=1 (driving) to Y=3 (walking) and regression 2 is comparing Y=2 (public transportation) to Y=3 (walking)
#You will notice that the first column of the coeffs matrix contains the coefficients in regression 1 (compar)
#First column of the coeffs matrix contains the coefficients in regression 1 (recall how we created the coeffs matrix)
coef12 <- coeffs[,1]
#Third column of the coeffs matrix contains the standard errors in regression 1
se12 <- coeffs[,3]
#Fifth column of the coeffs matrix contains the z-scores in regression 1
z.score12 <- coeffs[,5]
#Seventh column in the coeffs matrix contains the p-values in regression 1
p.value12 <- coeffs[,7]

#Now let's create the odds ratio by exponentiating the coefficients
or12 <- exp(coef12)

#reg1 combines the coefficients, standard errors, z scores and p-valeus for regression 1
reg12 <- cbind(coef12, se12, z.score12, p.value12, or12)

#regression comparing categories 1 and 2
reg12