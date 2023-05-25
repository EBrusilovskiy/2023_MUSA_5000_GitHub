#EUGENE BRUSILOVSKIY
#MUSA 501
#POINT PATTERN ANALYSIS IN R: EXAMPLES
#QUADRAT ANALYSIS, AVERAGE NEAREST NEIGHBOR ANALYSIS, K-FUNCTIONS


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
library(spatstat)

#K-FUNCTIONS
#What about K-functions? Of course we'll do K-functions as well!
#Setting working directory
setwd("D:/Dropbox/Documents/Work and School/Teaching/CPLN 671 - Statistics and Data Mining/Point Pattern Analysis/Data/K-Functions Taking Population Into Consideration")


r <- raster("D:/Dropbox/Documents/Work and School/Teaching/CPLN 671 - Statistics and Data Mining/Point Pattern Analysis/Data/K-Functions Taking Population Into Consideration/popraster")
#rasterimage <- as.im(X="r", W=BoundaryPolygonsOW)
plot(r)

#Reading Polygon Boundary.shp from the directory above
Boundary <- readShapePoly("PA_Albers.shp")

#Class "SpatialPolygons" holds polygon topology (without attributes)
BoundaryPolygons <- as(Boundary, "SpatialPolygons")


#The class "owin" is a way of specifying the observation window for a point pattern.
BoundaryPolygonsOW<- as(BoundaryPolygons, "owin")

#Plotting the Boundary Window
#plot(r)
plot(BoundaryPolygonsOW)#,add=T)
#title(main = "Point Pattern Analysis")

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
#title(main = "Point Pattern Analysis")


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
#Computes confidence envelopes using n=9 simulations. Here, nrank=1 means we're
#looking at the lowest and highest values of the simulated envelopes.
#spatstat::envelope is to specify that the envelope command is in the spatstat 
#library and not the boot library.
Kenv <- spatstat::envelope(pp,fun="Kest", rmax=250000, nsim=9, nrank=1) 
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
#Computes confidence envelopes using n=9 simulations. Here, nrank=1 means we're
#looking at the lowest and highest values of the simulated envelopes. 
Lenv <- spatstat::envelope(pp,fun="Lest", rmax=250000, nsim=9,nrank=1)
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

