#Class Exercise: Calculating Sensitivity and Specificity for Different values of fit
#Recall that fit is y-hat = p = P(Y=1) = P(There being a hospital in a zip code)

#Here, we're saying that a high probability of there being a hospital in a 
#zip code is >= 0.1, and a low probability of there being a hospital in a 
#zip code is <0.1. In the cross-tabulation, true corresponds to high probability
#and false corresponds to low probability.
#We will then go on to calculate the sensitivity, specificity, and misclassification
#rate for the cut-off of 0.1
fit.binary = (fit>=0.1)
CrossTable(fit.binary, mydata$Hospital, prop.r=FALSE, prop.t=FALSE, prop.c=FALSE, prop.chisq=FALSE)

#Doing everything we did above, except now the cut-offs are 0.2, 0.3, 0.4, 0.5,
#0.6, 0.7, 0.8, and 0.9.
fit.binary = (fit>=0.2)
CrossTable(fit.binary, mydata$Hospital, prop.r=FALSE, prop.t=FALSE, prop.c=FALSE, prop.chisq=FALSE)

fit.binary = (fit>=0.3)
CrossTable(fit.binary, mydata$Hospital, prop.r=FALSE, prop.t=FALSE, prop.c=FALSE, prop.chisq=FALSE)

fit.binary = (fit>=0.4)
CrossTable(fit.binary, mydata$Hospital, prop.r=FALSE, prop.t=FALSE, prop.c=FALSE, prop.chisq=FALSE)

fit.binary = (fit>=0.5)
CrossTable(fit.binary, mydata$Hospital, prop.r=FALSE, prop.t=FALSE, prop.c=FALSE, prop.chisq=FALSE)

fit.binary = (fit>=0.6)
CrossTable(fit.binary, mydata$Hospital, prop.r=FALSE, prop.t=FALSE, prop.c=FALSE, prop.chisq=FALSE)

fit.binary = (fit>=0.7)
CrossTable(fit.binary, mydata$Hospital, prop.r=FALSE, prop.t=FALSE, prop.c=FALSE, prop.chisq=FALSE)

fit.binary = (fit>=0.8)
CrossTable(fit.binary, mydata$Hospital, prop.r=FALSE, prop.t=FALSE, prop.c=FALSE, prop.chisq=FALSE)

fit.binary = (fit>=0.9)
CrossTable(fit.binary, mydata$Hospital, prop.r=FALSE, prop.t=FALSE, prop.c=FALSE, prop.chisq=FALSE)
