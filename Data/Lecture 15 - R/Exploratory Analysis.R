setwd("C:/Users/Administrator/Dropbox/Documents/Work and School/Teaching/CPLN 671 - Statistics and Data Mining/Data/Lecture 15 - R")
mydata <- read.csv("Exploratory Analysis.csv")
head(mydata)

options(scipen=999)

#Let's look at the number and proportion of block groups with hospitals
hosp.tab <- table(mydata$Hospital)
hosp.tab
prop.table(hosp.tab)

#Let's look at the number and proportion of urban, suburban and rural block groups
urb.tab <-table(mydata$Urban)
urb.tab
prop.table(urb.tab)



#Looking at the association between two categorical variables
#Chi-Square Test
#Chi-Square Distribution: sum of squares of k independent standard normal 
#random variables, where k is the degrees of freedom. Degrees of freedom can be
#calculated as (R-1)(C-1) where R is the number of rows in the cross-tabulation 
#table and C is the number of columns in the cross-tabulation table (i.e., C = # of
#categories in one of the variables and R is the # of categories in the other 
#variable.


#H0: the proportions of urban, suburban and rural block groups that have hospitals 
#are the same,

#vs.

#Ha: the proportion of urban, suburban, and rural block groups that have hospitals
#are not the same

#Alternatively:

#H0: the proportion of block groups that have hospitals are the same in urban, 
#suburban and rural block groups, 

#vs.

#Ha: the proportion of block groups that have hospitals are not the same in urban,
#suburban and rural block groups.

#Let's do a cross-tabulation of Urban and Hospitals
install.packages("gmodels")
library(gmodels)

CrossTable(mydata$Hospital, mydata$Urban, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE)




#Associations between two continuous variables
#Correlations

install.packages("Hmisc")
library(Hmisc)

corrvars<-as.matrix(cbind(mydata$Income, mydata$Population))

#H0: there's no association (correlation is 0)

#vs.

#Ha: there is an association (correlation isn't 0)

rcorr(corrvars,type="pearson")
rcorr(corrvars,type="spearman")

cor(corrvars, method=c("pearson"))
cor(corrvars, method=c("spearman"))



#Associations between one binary and one continuous variable
#Independent samples t-test
#First, let's look at the means of the Income variable for block groups that
#have a hospital and those that don't.
tapply(mydata$Income, mydata$Hospital, mean)
#We can also get standard deviations
tapply(mydata$Income, mydata$Hospital, sd)

#H0: block groups that have a hospital and block groups that don't have a hospital
#don't differ in their average incomes

#vs.

#Ha: block groups that have a hospital and block groups that don't have a hospital
#differ in terms of their average incomes

t.test(mydata$Income~mydata$Hospital)





#Associations between 1 categorical and 1 continuous variable
#ANOVAs (Analysis of Variance)

#H0: The mean of the continuous variable is the same for all groups. 

#vs.

#Ha: The mean of the continuous variable is not the same for all groups (i.e., means in at least 2
#groups are different)

aggregate(Income~Urban, data=mydata, FUN=mean)
anova <- aov(mydata$Income~mydata$Urban)
summary(anova)

#If we have significant results (p<0.05), we still don't know which groups are
#different from one another in terms of income. Tukey post hoc test helps us 
#figure this out - it does pairwise comparisons of all groups.
TukeyHSD(anova)