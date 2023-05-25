#EUGENE BRUSILOVSKIY
#MUSA 501
#LOGISTIC REGRESSION IN R

options(scipen=999)

#install.packages("packagename")
install.packages("aod")
install.packages("ggplot2")
install.packages("rms")
install.packages("gmodels")
install.packages("nnet")
install.packages("DAAG")
install.packages("ROCR")

#library(packagename)
library(aod)
library(ggplot2)
library(rms)
library(gmodels)
library(nnet)
library(DAAG)
library(ROCR)

setwd("C:/Users/eugeneby/Dropbox/Documents/Work and School/Teaching/CPLN 671 - Statistics and Data Mining/Data/Lecture 16 - R")
mydata <- read.csv("Logistic Regression Example.csv")

# View the first few rows of the data
head(mydata)

# Summarize each variable
summary(mydata)


#INTERCEPT ONLY LOGISTIC REGRESSION
mylogit <- glm(Hospital ~ 1, data = mydata, family = "binomial")
summary(mylogit)
table(mydata$Hospital)
prop.table(table(mydata$Hospital))
#Odds of there being a hospital = p(hospital)/(1-p(hospital)) = p(hospital)/p(no hospital) = .51/.49 = 1.04
0.51/0.49
#Or, let's exponentiate the beta0 coefficient!
exp(coefficients(mylogit))

#LOGISTIC REGRESSION WITH 1 CONTINUOUS PREDICTOR (Population)
#First let's run OLS regression - WE SHOULD NOT DO THIS WITH A BINARY DV!!

#Let's look at the summary of the predictor variable (Population)
summary(mydata$Population)

#Simple Logistic Regression
mylogit <- glm(Hospital ~ Population, data = mydata, family = "binomial") #Run a logit model
summary(mylogit)
head(mydata)

#95% confidence intervals
confint.default(mylogit)
#Odds Ratios
exp(cbind(OR = coef(mylogit), confint(mylogit)))

#R-Squared -- WE DO NOT HAVE R-SQUARED IN THE AS PART OF THE glm STATEMENT, so we use
#Another package (rms) to run logistic regression. The output of the rms package
#(with the lrm command below) gives R-squared as part of the output. Be careful about using R-squares
#in any model not estimated with least squares. It doesn't have the same interpretation, and the only 
#thing we can say about it is that higher is better.
lrm(Hospital ~ Population, data=mydata)



#LOGISTIC REGRESSION WITH 1 BINARY PREDICTOR (Urban)
mylogit <- glm(Hospital ~ Urban, data = mydata, family = "binomial")
summary(mylogit)
#Cross-tabulation
CrossTable(mydata$Hospital, mydata$Urban,prop.r=FALSE,prop.chisq=FALSE, chisq=FALSE,prop.t=FALSE)
#95% confidence intervals
confint.default(mylogit)
#Odds Ratios
exp(cbind(OR = coef(mylogit), confint(mylogit)))
#R-Squared
lrm(Hospital ~ Population, data=mydata)


mydata$Pop1000 = mydata$Population/1000


# MULTIPLE LOGISTIC REGRESSION
# First, Use factor command to treat NearbyHospital and Urban as categorical variables
# This isn't really necessary for binary variables, but for variables which have 
# more than 2 categories
mydata$NearbyHospital <- factor(mydata$NearbyHospital)
mydata$Urban <- factor(mydata$Urban)
summary(mydata)

mylogit <- glm(Hospital ~ Population + NearbyHospital + Urban, data = mydata, family = "binomial")
logitoutput <- summary(mylogit)
logitoutput
#95% confidence intervals
confint.default(mylogit)
#Wald Test
wald.test(b = coef(mylogit), Sigma = vcov(mylogit), Terms = 1:4)
#Odds Ratios
or_ci <- exp(cbind(OR = coef(mylogit), confint(mylogit)))
or_ci




#R-Squared
lrm(Hospital ~ Population + NearbyHospital + Urban, data=mydata)

#Merging odds ratios to beta coefficients
logitcoeffs <- logitoutput$coefficients
finallogitoutput <-cbind (logitcoeffs, or_ci)
finallogitoutput





#SENSITIVITY/SPECIFICITY/MISCLASSIFICATION/ROC ANALYSIS
#Get y-hats
fit <- mylogit$fitted
#Histogram of fitted values
hist(fit)
#Generating a dummy variable fit.binary that sets fit to 1 if it's 0.5 or greater 
#and 0 otherwise.
fit.binary = (fit>=0.5)
CrossTable(fit.binary, mydata$Hospital, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE)
#Sensitivity = d/(b+d) = 124/(29+124) = 124/153 (% actual positives correctly identified as such)
#Specificity = a/(a+c) = 129/(129+18) = 129/147 (% actual negatives correctly identified as such)
#Correct Classification Rate = (129+124)/300 = 253/300
#Misclassification Rate = 1 - Correct Classification Rate = 47/300

fit.binary = (fit>=0.3)
CrossTable(fit.binary, mydata$Hospital, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE)
#Correct Classification Rate = (124+138)/300 = 262/300 
#Misclassification Rate = 1 - Correct Classification Rate = 38/300





#ROC CURVE
#For more info, see: https://hopstat.wordpress.com/2014/12/19/a-small-introduction-to-the-rocr-package/

#a is a matrix combining the vectors containing y and y-hat in matrix a; first variable is
#Hospital, which is y; second variable is fit, which is y-hat

a <- cbind(mydata$Hospital, fit)

#Here, predictions are estimated probabilities (i.e., p or y-hat values)
#Also, labels are actual y-values

#From above, we see that matrix *a* has 2 columns: 
#1. The first one is mydata$Hospital, which are actual 
#values of y (i.e., labels)
#2. The second one is fit, which are predicted, or fitted values
#of y (i.e., predictions)

#Let's make the names of the variables easy to understand
colnames(a) <- c("labels","predictions")
head(a)
roc <- as.data.frame(a)

pred <- prediction(roc$predictions, roc$labels)
#Below, tpr = true positive rate, another term for sensitivity
#fpr = false positive rate, or 1-specificity
roc.perf = performance(pred, measure = "tpr", x.measure="fpr")
plot(roc.perf)
abline(a=0,b=1)

#Optimal cut-point, if you want to weigh specificity and
#sensitivity equally.
#There are a couple ways to identify the optimal cut point.
#One is the so-called Youden Index, which identifies the cut-off
#point for which (sensitivity + specificity) is maximized.

#Another one, calculated using the code below, is the cut-off
#value for which the ROC curve has the minimum distance from
#the upper left corner of the graph, where specificity = 1 and
#sensitivity = 1. (This is just a different way of maximizing 
#specificity and sensitivity). This is where the 
#d = (x - 0)^2 + (y-1)^2
#in the code below comes in.

opt.cut = function(perf, pred){
  cut.ind = mapply(FUN=function(x, y, p){
    d = (x - 0)^2 + (y-1)^2
    ind = which(d == min(d))
    c(sensitivity = y[[ind]], specificity = 1-x[[ind]], 
      cutoff = p[[ind]])
  }, perf@x.values, perf@y.values, pred@cutoffs)
}
#This will print the optimal cut-off point and the corresponding
#specificity and sensitivity 
print(opt.cut(roc.perf, pred))

#Area under the curve
#Source: http://gim.unmc.edu/dxtests/roc3.htm
#The prediction accuracy of the model depends on how well the 
#model predicts 1 responses as 1's and 0 responses as 0's. 
#Accuracy is measured by the area under the ROC curve. An area 
#of 1 represents a perfect test (prediction); an area of .5 
#represents a worthless test (prediction). A rough guide for
#interpreting area under ROC Curves:
# .90-1 = excellent (A)
# .80-.90 = good    (B)
# .70-.80 = fair    (C)
# .60-.70 = poor    (D)
# .50-.60 = fail    (F)

#These might be somewhat conservative estimates, and there will
#be statisticians who will say that area > .7 is just fine.
#Here, the area under the curve is 0.93
auc.perf = performance(pred, measure ="auc")
auc.perf@y.values

