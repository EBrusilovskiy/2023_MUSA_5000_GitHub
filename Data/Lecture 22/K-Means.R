#Notes based heavily on:
#http://www.r-statistics.com/2013/08/k-means-clustering-from-r-in-action/

#install.packages("rattle")
#library(rattle)

install.packages("NbClust")
install.packages("flexclust")

library(NbClust)
library(flexclust)

setwd("C:/Users/eugeneby/Dropbox/Documents/Work and School/Teaching/CPLN 671 - Statistics and Data Mining/Data/Lecture 22")

#data(wine, package="rattle")
#head(wine)
#write.csv(wine, file = "wine.csv")

options(scipen=999)

wine <- read.csv("wine.csv")
head(wine)
#We remove the firrst 2 columns which shouldn't be subjected to the K-means
#The scale command standardizes the variables so that the means are 0 and s.d. = 1.
df <- data.frame(scale(wine[-2:-1]))
head(df)

#Determine number of clusters by looking at the scree plot.
#An appropriate cluster solution could be defined as the solution at which the
#reduction in SSE slows dramatically. This produces an "elbow" in the plot of 
#SSE against cluster solutions. The figure indicates that there is a distinct 
#drop in within groups sum of squares when moving from 1 to 3 clusters. After 
#three clusters, this decrease drops off, suggesting that a 3-cluster solution 
#may be a good fit to the data. 
wss <- (nrow(df)-1)*sum(apply(df,2,var))
for (i in 2:20) wss[i] <- sum(kmeans(df, 
                                     centers=i)$withinss)
plot(1:20, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")


#The NbClust package has 30 different methods to determine the optimal number 
#of clusters. We can select the index="alllong" option and get the results from
#all 30 indices. (Many use the option index="all" and get results from 26 most
#relevant indices). We then use the number of clusters that's chosen by the 
#largest number of indices. See pp. 4-6 of this document: 
#https://cran.r-project.org/web/packages/NbClust/NbClust.pdf.
#Note that not all 30 criteria can be calculated for every dataset.
set.seed(1234)
nc <- NbClust(df, min.nc=2, max.nc=15, method="kmeans", index="all")
table(nc$Best.n[1,])

barplot(table(nc$Best.n[1,]),
        xlab="Numer of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen by 26 Criteria")

#http://tagteam.harvard.edu/hub_feeds/1981/feed_items/240096:
#Since K-means cluster analysis starts with k randomly chosen centroids, 
#a different solution can be obtained each time the function is invoked. 
#Use the set.seed() function to guarantee that the results are 
#reproducible. Additionally, this clustering approach can be sensitive 
#to the initial selection of centroids. The kmeans() function has an 
#nstart option that attempts multiple initial configurations and reports 
#on the best one. For example, adding nstart=25 will generate 25 initial
#configurations. This approach is often recommended.
set.seed(1234)
fit.km <- kmeans(df, 3, nstart=25)
#Let's look at the number of observations in each cluster
fit.km$size

#fit.km$cluster provides the clustering results and fit.km$centers provides 
#the centroid vector (i.e., the mean) for each cluster.
round(fit.km$centers, 2)
fit.km$cluster

#Calculate the average value of each of the original variables in the wine
#dataset within each cluster. Again, we're excluding the 1st 2 variables 
#using the wine[-2:-1] command.
cbind(round(aggregate(wine[-2:-1], by=list(cluster=fit.km$cluster), mean),1),fit.km$size)

#In the wine data set, there's a variable called Type which tells us what type of
#wine it is. We can see how well our clustering actually corresponds to this type.
ct.km <- table(wine$Type, fit.km$cluster)
ct.km

#We can quantify the agreement between type and cluster, using an adjusted Rand
#index provided by the flexclust package. The adjusted Rand index provides a 
#measure of the agreement between two partitions, adjusted for chance. It ranges 
#from -1 (no agreement) to 1 (perfect agreement). Agreement between the wine 
#varietal type and the cluster solution is 0.9. Not bad at all.
round(randIndex(ct.km),1)
