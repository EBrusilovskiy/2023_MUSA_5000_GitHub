#Code adapted from the GWR in R Instructions:
#https://rstudio-pubs-static.s3.amazonaws.com/44975_0342ec49f925426fa16ebcdc28210118.html

#Avoid getting results in scientific notation!
options(scipen = 999)

install.packages("maptools")
install.packages("spdep")
install.packages("spgwr")
install.packages("rgdal")
install.packages("ggplot2")
install.packages("rgeos")
install.packages("ape")

library(maptools)
library(spdep)
library(spgwr)
library(rgdal)
library(ggplot2)
library(rgeos)
library(ape)

setwd("D:/Dropbox/Documents/Work and School/Teaching/CPLN 671 - Statistics and Data Mining/Data/Lecture 14 - R")
LondonWards <-read.csv("LondonData.csv")
attach(LondonWards)
head(LondonWards)
#625 Observations
dim(LondonWards)


#First, let's run OLS Regression
model1 <- lm(EmpRate ~ HousePri+MeanAge+PerForeign)
summary(model1)
#Saving OLS Residuals
resids <-residuals(model1)
colours <- c("dark blue", "blue", "red", "dark red") 
map.resids <- SpatialPointsDataFrame(coords=cbind(x,y), data=data.frame(resids)) 
#Plotting OLS Residuals on a map
spplot(map.resids, cuts=quantile(resids), col.regions=colours, cex=1) 

#Let's save residuals in the LondonWards data set as well.
LondonWards$OLSresids <- resids

#Now, we're ready to run GWR
#calculate kernel bandwidth
#The function gwr.sel finds a bandwidth for a given geographically weighted regression 
#by optimizing RMSE (through cross-validation) or the Akaike Information Criterion.
#That is, it does the exact same thing as ArcGIS.
GWRbandwidth <- gwr.sel(EmpRate ~ HousePri+MeanAge+PerForeign, data=LondonWards, coords=cbind(x,y),adapt=T, method="cv") 
#run the gwr model
gwr.model = gwr(EmpRate ~ HousePri+MeanAge+PerForeign, data=LondonWards, coords=cbind(x,y), adapt=GWRbandwidth, hatmatrix=TRUE, se.fit=TRUE) 
#print the results of the model
gwr.model

#Saving results as a data frame
results<-as.data.frame(gwr.model$SDF)
head(results)

#Saving coefficients of the predictors in the original data set (LondonWards)
LondonWards$coefHousePri <- results$HousePri
LondonWards$coefMeanAge <- results$MeanAge
LondonWards$coefPerForeign <- results$PerForeign

#Saving the GWR Residuals and Local R-squared in the original data set
LondonWards$gwr.e <- results$gwr.e
LondonWards$localR2 <- results$localR2

#Let's see if we can calculate the R-Squared that the GWR output gives us.
MeanEmpRate <- mean(LondonWards$EmpRate)
SST <- sum((LondonWards$EmpRate - MeanEmpRate)^2)
SSE <- sum((LondonWards$gwr.e)^2)
R.sq <- 1-(SSE/SST)
R.sq

#Here, we have a polygon shapefile that shows boroughs and their
#boundaries. We can read it in using the maptools function 
#readShapePoly
boroughs <- readShapePoly("London_Borough_Excluding_MHW.shp")

#Note: If we didn't have the polygon shapefile, we could still
#do everything we did here, much in the way we mapped residuals
#from OLS regression at the beginning of the program.


#fortify for use in ggpplot2
#The fortify command converts a generic R object into a data frame useful for plotting. 
boroughoutline <- fortify(boroughs, region="NAME")
#Here, NAME is a field in the shapefile and is an ID of each observation. If we don't 
#know what this field is called, #we can use the command str(boroughs), and it will 
#list the names of all the fields in the data set.

#Now plot the various GWR coefficients.
#If you get the warning message "Non Lab interpolation is deprecated", ignore it.
gwr.point1<-ggplot(LondonWards, aes(x=x,y=y))+geom_point(aes(colour=LondonWards$coefHousePri))+scale_colour_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0, space = "rgb", na.value = "grey50", guide = "colourbar", guide_legend(title="House Price Coefficients"))
gwr.point1+geom_path(data=boroughoutline,aes(long, lat, group=id), colour="grey")+coord_equal()

gwr.point1<-ggplot(LondonWards, aes(x=x,y=y))+geom_point(aes(colour=LondonWards$MeanAge))+scale_colour_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0, space = "rgb", na.value = "grey50", guide = "colourbar", guide_legend(title="Mean Age Coefficients"))
gwr.point1+geom_path(data=boroughoutline,aes(long, lat, group=id), colour="grey")+coord_equal()

gwr.point1<-ggplot(LondonWards, aes(x=x,y=y))+geom_point(aes(colour=LondonWards$PerForeign))+scale_colour_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0, space = "rgb", na.value = "grey50", guide = "colourbar", guide_legend(title="% Foreign Coefficients"))
gwr.point1+geom_path(data=boroughoutline,aes(long, lat, group=id), colour="grey")+coord_equal()

#Plot the GWR Residuals
gwr.point1<-ggplot(LondonWards, aes(x=x,y=y))+geom_point(aes(colour=LondonWards$gwr.e))+scale_colour_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0, space = "rgb", na.value = "grey50", guide = "colourbar", guide_legend(title="Residuals"))
gwr.point1+geom_path(data=boroughoutline,aes(long, lat, group=id), colour="grey")+coord_equal()

#Plot the Local R-Squared Results
gwr.point1<-ggplot(LondonWards, aes(x=x,y=y))+geom_point(aes(colour=LondonWards$localR2))+scale_colour_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0, space = "rgb", na.value = "grey50", guide = "colourbar", guide_legend(title="Local R-Squared"))
gwr.point1+geom_path(data=boroughoutline,aes(long, lat, group=id), colour="grey")+coord_equal()



#Let's calculate the Moran's I of OLS and GWR Residuals
#First, let's calculate a distance-based weights matrix. 
#Here, we are calculating distances between each pair of observations and
#Saving these distances in a matrix.
#For simplicity, we will treat the latitude and longitude as values on a 
#plane rather than on a sphere--our locations are close together and far 
#from the poles. When using latitude and longitude coordinates from more 
#distant locations, it's wise to calculate distances based on spherical 
#coordinates (the geosphere package can be used).
euc.dists <- as.matrix(dist(cbind(LondonWards$x, LondonWards$y)))
#Here, we are calculating inverse distances, so that observations which are
#closer have higher weights
euc.dists.inv <- 1/euc.dists
#Replacing diagonal elements of the distance weight matrix with 0's - i.e.,
#ward i shouldn't be its own neighbor
diag(euc.dists.inv) <- 0
#Let's look at the first 5 rows and columns of the matrix - looks symmetric,
#and has 0's on the diagonal - as it should be!
euc.dists.inv[1:5, 1:5]

#OLS residuals are spatially autocorrelated!
Moran.I(LondonWards$OLSresids, euc.dists.inv)

#GWR residuals aren't spatially autocorrelated!
Moran.I(LondonWards$gwr.e, euc.dists.inv)





