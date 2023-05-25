#EUGENE BRUSILOVSKIY
#MUSA 501
#POINT PATTERN ANALYSIS IN R: EXAMPLES
#QUADRAT ANALYSIS, AVERAGE NEAREST NEIGHBOR ANALYSIS, K-FUNCTIONS

options(scipen=999)
#https://www.e-education.psu.edu/geog586/l5_p9.html

#LOAD THE FOLLOWING PACKAGES THE FIRST TIME YOU USE R:
install.packages("sp")
install.packages("ISLR")
install.packages("MASS")
install.packages("spatstat")
install.packages("spatial")
install.packages("maptools")
install.packages("ppp")
install.packages("fossil")
install.packages("adehabitatHR")
install.packages("gdata")
install.packages("raster")
install.packages("rgdal")
install.packages("geostatsp")
install.packages("spatialEco")
install.packages("spatstat.core")

library(graphics)
library(maptools)
library(spatstat)
library(sp)
library(fossil)
library(spatial)
library(adehabitatHR)
library(gdata)
library(raster)
library(rgdal)
library(geostatsp)
library(spatialEco)
library(spatstat.core)

#Setting working directory
setwd("C:/Users/Administrator/Dropbox/Documents/Work and School/Teaching/CPLN 671 - Statistics and Data Mining/Point Pattern Analysis/Data/Quadrat Analysis Data")

#Reading Polygon Boundary.shp from the directory above
Boundary <- rgdal::readOGR('.', 'Boundary')

#Class "SpatialPolygons" holds polygon topology (without attributes)
BoundaryPolygons <- as(Boundary, "SpatialPolygons")

#The class "owin" is a way of specifying the observation window for a point pattern.
BoundaryPolygonsOW<- as(BoundaryPolygons, "owin")

#Plotting the Boundary Window
plot(BoundaryPolygonsOW, main=NULL)
title(main = "Point Pattern Analysis")

#Reading in the file with the points
Pts <- read.table("Quadrat Points.txt", header=T, sep="\t", colClasses = c("X"="double"))
#Very roughly speaking, using attach() in R is like relying on the implicit use of the 
#most recent data set. 
#http://www.r-bloggers.com/to-attach-or-not-attach-that-is-the-question/
#Only attach file once. If you do it more than once, use detach(Pts) command 1+ times
#to detach file
#attach(Pts)
#detach(Pts)
pp <- ppp(Pts$X, Pts$Y, window=BoundaryPolygonsOW)
#If the following error message is received: data contain duplicated points
#Use the command duplicated(X,Y) to see which points are duplicates.
#The command cbind(Pts,duplicated(X,Y)) will show you which points have duplicated values
#In general, unless you know that the duplicates shouldn't be there, you would ignore
#this warning.

#Now let's plot the points and the Boundary.
plot(pp, main=NULL)
title(main = "Point Pattern Analysis")

#But what if we want to use R to generate a minimum convex polygon?
#See http://cran.r-project.org/web/packages/adehabitatHR/vignettes/adehabitatHR.pdf
#for more details, but the code and basic explanations are below.

#First let's create a matrix that only includes the x & y coordinates of each point.
#This is simply the last two columns of matrix Pts.
xy <-Pts[,2:3]

#In order for R to consider xy a spatial object, we need to convert it to 
#class SpatialPoints. We do it like this:
xysp <-SpatialPoints(xy)

#Now, let's use the mcp (minimum convex polygon) function to generate the mcp and
#plot the points with the mcp
#If we wanted to exclude some outlier points (e.g., 5 of the 100 points in the data)
#we would specify a different percent value below (e.g., 95 instead of 100)
cp <- mcp(xysp, percent=100)

#The class "owin" is a way of specifying the observation window for a point pattern.
cpnew <- as(cp, "owin")

#Here, we are using the attached dataset Pts to plot X and Y coordinates within the
#window cpnew.
mcpoly <- ppp(Pts$X,Pts$Y, window=cpnew)
plot(mcpoly, main=NULL)


#A few pretty cool things we can do with the data:
#1. Generate the kernel density map of the points
plot(density(pp))
#2. Generate the countour map. The add=TRUE (or add=T) is a way for R to know that
#you want the new plot to be added onto the existing plot.
contour(density(pp), add=TRUE)
#3. Add points on top of the maps
plot(pp, add=T)







#NEAREST NEIGHBOR ANALYSIS
#Now let's try some Nearest Neighbor Analysis!
dev.off() 
#Computes the distance from each point to its nearest neighbour in a point pattern.
nnd <- nndist.ppp(pp)
#Using the formulas on the slides, we calculate Mean Observed Distance,
#Mean Expected Distance and the Standard Error.
MeanObsDist <- mean(nnd)
#The area.owin command calculates the area of the study area that you use.
#Here it's the minimum enclosing rectangle, but it doesn't have to be - it 
#could be any shapefile you import from ArcGIS (or generate in R) that 
#corresponds to the study area.
MeanExpDist <- 0.5 / sqrt(nrow(Pts) / area.owin(BoundaryPolygonsOW))
SE <- 0.26136 / sqrt(nrow(Pts)*nrow(Pts) / area.owin(BoundaryPolygonsOW))
#Calculating the z-score
zscore <- (MeanObsDist - MeanExpDist)/SE
#Statistical test
#Here, if the z score is positive, we do an upper-tailed test and if the 
#z score is negative we do a lower-tailed test to come up with the p-value.
pval<-ifelse(zscore > 0, 1 - pnorm(zscore), pnorm(zscore))
#Calculating the NNI
NNI <- MeanObsDist / MeanExpDist
pval
NNI

#spatialEco package has an nni function which unfortunately doesn't take 
#user-defined study area files
xy <- Pts[,c(2,3)]      
sp.Pts <- SpatialPointsDataFrame(coords=xy, data=Pts)   #Convert our data to a sp object
spatialEco::nni(sp.Pts,win="hull")      #Results are still pretty similar to results above




#K-FUNCTIONS
#What about K-functions? Of course we'll do K-functions as well!
#Setting working directory
setwd("C:/Users/Administrator/Dropbox/Documents/Work and School/Teaching/CPLN 671 - Statistics and Data Mining/Point Pattern Analysis/Data/K-Functions Taking Population Into Consideration")

#Reading Polygon Boundary.shp from the directory above
Boundary <- readOGR('.', 'PA_Albers')

#Class "SpatialPolygons" holds polygon topology (without attributes)
BoundaryPolygons <- as(Boundary, "SpatialPolygons")

#The class "owin" is a way of specifying the observation window for a point pattern.
BoundaryPolygonsOW<- as(BoundaryPolygons, "owin")

#Plotting the Boundary Window
#plot(r)
plot(BoundaryPolygonsOW, main="Point Pattern Analysis with K-Functions")#,add=T)

#Reading in the file with the points
Pts <- read.table("Hospitals_for_R.txt", header=T, sep="\t", colClasses = c("X"="double"))
#Very roughly speaking, using attach() in R is like relying on the implicit use of the 
#most recent data set. 
#http://www.r-bloggers.com/to-attach-or-not-attach-that-is-the-question/
#Only attach file once. If you do it more than once, use detach(Pts) command 1+ times
#to detach file
#attach(Pts)
#detach(Pts)
pp <- ppp(Pts$X, Pts$Y, window=BoundaryPolygonsOW)
#If the following error message is received: data contain duplicated points
#Use the command duplicated(X,Y) to see which points are duplicates.
#The command cbind(Pts,duplicated(X,Y)) will show you which points have duplicated values
#In general, unless you know that the duplicates shouldn't be there, you would ignore
#this warning.

#Now let's plot the points and the Boundary.
plot(pp,add=T)


#http://www.math.umt.edu/graham/stat544/ripleys.pdf
#If we double click on the khat data set on the right, it will have 513 observations
#and 5 variables. We are interested in 2 of the variables: 
#-- r, which is the distance that goes in increments of 138.8693
#-- iso, which is the k-function calculated with Ripley's edge correction
#K-Functions
khat <-Kest(pp, rmax=250000) #,correction="Ripley")
#Plots Ripley's K function calculated with Ripley's isotropic edge correction, with
#line width 2, axis labels, and a main title.
plot(khat$r,khat$iso,xlab="r", ylab="Ripley's K",
     main="Ripley's Estimated K-Function",
     cex.lab=1.6,cex.axis=1.5,cex.main=1.5,lty=1,lwd=2)
# Overlays the theoretical K-function under CSR with a dashed (lty=8) line.
lines(khat$r,khat$theo,lty=8, lwd=2) 
#Code to compute the Ripley's Simulation Confidence Envelopes
#Computes confidence envelopes using n=199 simulations. Here, nrank=1 means we're
#looking at the lowest and highest values of the simulated envelopes. Here,
#alpha = 2 * nrank/(1 + nsim) = 2*1/200 = 0.01
#spatstat::envelope is to specify that the envelope command is in the spatstat 
#library and not the boot library.
Kenv <- spatstat.core::envelope(pp,fun="Kest", rmax=250000, nsim=9, nrank=1) 
# Plots Ripley's K function with 99% simulation # envelopes, axis labels, and a title.
plot(Kenv,xlab="r",ylab="Khat(r)", cex.lab=1.6,cex.axis=1.5,main= 
       "Ripley's Khat with Confidence Envelopes",cex.main=1.5,lwd=2)

#L-Functions
#Computes Ripley's L* for each sample event
lhat <- Lest(pp, rmax=250000) 
#Plots Ripley's L function calculated with line width 2, 
#Ripley's isotropic edge correction, with axis labels, and a main title.
plot(lhat$r,lhat$iso-lhat$r, xlab="r",ylab="Ripley's L",cex.lab=1.6,  
     cex.axis=1.5,cex.main=1.5,lty=1,lwd=2, main="Ripley's Estimated L-Function") 
#Overlays the theoretical L-function under CSR with a dashed (lty=8) line.
lines(lhat$r,lhat$theo-lhat$r,lty=8,lwd=2) 

#Code to compute the Ripley's Simulation Confidence Envelopes
#Computes confidence envelopes using n=199 simulations. Here, nrank=1 means we're
#looking at the lowest and highest values of the simulated envelopes. Here,
#alpha = 2 * nrank/(1 + nsim) = 2*1/200 = 0.01
Lenv <- spatstat.core::envelope(pp,fun="Lest", rmax=250000, nsim=9,nrank=1)
# Plots Ripley's L function with 99% simulation envelopes, axis labels, and a title.
plot(Lenv,xlab="r",ylab="Lhat(r)", cex.lab=1.6,cex.axis=1.5,
     main= "Ripley's Lhat with Confidence Envelopes",cex.main=1.5,lwd=2,legend=F)
#A better way to view this is to rotate this plot 45 degrees clockwise.
#Gives the Ripley's data frame a new name L2.
L2 <- Lenv 
#Now we will subtract the distance r from the R-defined Ripley's L's
#(This will be done for the observed L, theoretical L, lower and uper envelopes)
L2$obs <- L2$obs-L2$r
L2$theo <- L2$theo-L2$r
L2$lo <- L2$lo-L2$r
L2$hi <- L2$hi-L2$r
# Plots Ripley's L function with 99% simulation envelopes, axis labels, and a title.
plot(L2,xlab="r",ylab="Lhat(r)", cex.lab=1.6,cex.axis=1.5,
     main= "Ripley's Lhat with Confidence Envelopes",cex.main=1.5,lwd=2,legend=F)


#Population-weighted k-functions
#This can serve as a guide to create spatially balanced samples
#https://survey-design-field-manual.github.io/set-up-r-to-generate-design
