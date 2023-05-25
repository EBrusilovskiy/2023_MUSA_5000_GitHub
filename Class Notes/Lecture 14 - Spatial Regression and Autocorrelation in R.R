setwd("C:\\Users\\eugeneby\\Dropbox\\Documents\\Work and School\\Teaching\\CPLN 671 - Statistics and Data Mining\\Homework Assignments\\HW 2")
options(scipen=999)

#install.packages(c("sp", "rgdal", "rgeos", "spdep", "spgwr", "tmap"))
library(sp)
library(rgdal)
library(rgeos)
library(spdep)
library(spgwr)
library(tmap)



#READING SHAPEFILE AND EXAMINING DISTRIBUTIONS OF VARIABLES
shp <- readOGR('.', 'Regression Data')

names(shp@data)

par(oma=c(0,0,2,0)) 
par(mfrow=c(1,3)) 
hist(shp@data$MEDHHINC, breaks = 50)
hist(shp@data$MEDHVAL, breaks = 50)
hist(shp@data$PCTVACANT, breaks = 50)

shp@data$LNMEDHVAL <- log(shp@data$MEDHVAL + 1)
shp@data$LNMEDHHINC <- log(shp@data$MEDHHINC + 1)
shp@data$LNPCTVACANT <- log(shp@data$PCTVACANT + 1)

par(oma=c(0,0,2,0)) 
par(mfrow=c(1,3)) 
hist(shp@data$LNMEDHHINC, breaks = 50)
hist(shp@data$LNMEDHVAL, breaks = 50)
hist(shp@data$PCTVACANT, breaks = 50)



#CREATING QUEEN WEIGHTS
queen<-poly2nb(shp, row.names=shp$POLY_ID)
summary(queen)

#plot links
par(mfrow=c(1,1)) 
plot(shp, col='grey90', lwd=2)
xy<-coordinates(shp)
par(mfrow=c(1,1)) 
plot(queen, xy, col='red', lwd=2, add=TRUE)
title(main='Contiguous Queen Neighbours')


#see which region has most neighbors
largestnbcard<-card(queen)
largestnb<-which(largestnbcard == max(largestnbcard))
fg1<-rep('grey90', length(largestnbcard))
fg1[largestnb]<-'red'
fg1[queen[[largestnb]]]<-'green'
plot(shp, col=fg1)
title(main='Region with 27 neighbors')

#see which region has only one neighbor
smallestnbcard<-card(queen) #extract neighbor matrix
smallestnb<-which(smallestnbcard == min(smallestnbcard)) #extract block groups with smallest number of neighbors
fg<-rep('grey90', length(smallestnbcard))
fg[smallestnb]<-'red' #color block groups red
fg[queen[[smallestnb[1]]]]<-'green' #color neighboring blocks green
fg[queen[[smallestnb[2]]]]<-'green'
fg[queen[[smallestnb[3]]]]<-'green'
fg[queen[[smallestnb[4]]]]<-'green'
plot(shp, col=fg)
title(main='Regions with only 1 neighbor')



#GLOBAL MORAN'S I
queenlist<-nb2listw(queen, style = 'W')
moran(shp$LNMEDHHINC, queenlist, n=length(queenlist$neighbours), S0=Szero(queenlist))$`I` 

#MCMC Permutations
moranMC<-moran.mc(shp$LNMEDHHINC, queenlist, nsim=999)  #We use 999 permutations
moranMC

moranMCres<-moranMC$res
hist(moranMCres, freq=10000000, nclass=100)   #Draws distribution of Moran's I's calculated from randomly permuted values
# Here, we draw a red vertical line at the observed value of our Moran's I
abline(v=moran(shp$LNMEDHHINC, queenlist, n=length(queenlist$neighbours), S0=Szero(queenlist))$`I`, col='red')  

#Create Moran plot (lagged value against observed value)
moran.plot(shp$LNMEDHHINC, queenlist) 



#LOCAL MORAN'S I (LISA STATISTICS)
LISA<-localmoran(shp$LNMEDHHINC, queenlist)
head(LISA)

#P-Values
moranSig.plot<-function(df,listw, title){
  local<-localmoran(x=df$LNMEDHHINC, listw=listw, zero.policy = FALSE)
  moran.map<-cbind(df, local)
  tm<-tm_shape(moran.map)+
    tm_borders(col='white')+
    tm_fill(col='Pr.z....E.Ii..', style='fixed', breaks=c(0,0.05, 1), title= 'p-value', palette = '-BuPu')+
    tm_layout(frame = FALSE, title = title)
  print(tm)
}

moranSig.plot(shp, queenlist, 'p-value')

#Positive vs. Negative local spatial autocorrelation
hl.plot<-function(df, listw){
  local<-localmoran(x=df$LNMEDHHINC, listw=listw, zero.policy = FALSE)
  quadrant<-vector(mode='numeric', length=323)
  m.prop<-df$LNMEDHHINC - mean(df$LNMEDHHINC)
  m.local<-local[,1]-mean(local[,1])
  signif<-0.05
  quadrant[m.prop >0 & m.local>0]<-4 #high MEDHHINC, high clustering
  quadrant[m.prop <0 & m.local<0]<-1 #low MEDHHINC, low clustering
  quadrant[m.prop <0 & m.local>0]<-2 #low MEDHINC, high clustering
  quadrant[m.prop >0 & m.local<0]<-3 #high MEDHHINC, low clustering
  quadrant[local[,5]>signif]<-0
  
  brks <- c(0,1,2,3,4)
  colors <- c("grey","light blue",'blue','pink',"red")
  plot<-plot(shp,border="gray90",lwd=1.0,col=colors[findInterval(quadrant,brks,all.inside=FALSE)])
}

hl.plot(shp, queenlist)
legend("bottomright",legend=c("insignificant","low-high","low-low","high-low","high-high"),
       fill=c("grey", "light blue", "blue", "pink", "red"),bty="n", cex = 0.5)



#OLS Regression
reg<-lm(formula=LNMEDHHINC ~ LNMEDHVAL + PCTVACANT, data=shp@data)
summary(reg)

standardised<-rstandard(reg)
#Looking at lagged residuals
resnb<-sapply(queen, function(x) mean(standardised[x]))

#This is a correlation, but we really should be regressing residuals on their nearest neighbors.
cor(standardised, resnb)

#Looking at Moran's I of OLS residuals
moran.mc(standardised, queenlist, 999)
moran.plot(standardised, queenlist)



#SPATIAL LAG REGRESSION
lagreg<-lagsarlm(formula=LNMEDHHINC ~ LNMEDHVAL + PCTVACANT, data=shp@data, queenlist)
summary(lagreg)

#Let's look at Moran's I of spatial lag residuals
reslag<-lagreg$residuals
lagMoranMc<-moran.mc(reslag, queenlist,999)
lagMoranMc



#SPATIAL ERROR REGRESSION
errreg<-errorsarlm(formula=LNMEDHHINC ~ LNMEDHVAL + PCTVACANT, data=shp@data, queenlist)
reserr<-residuals(errreg)
errresnb<-sapply(queen, function(x) mean(reserr[x]))
summary(errreg)

#Let's look at Moran's I of spatial error residuals
errMoranMc<-moran.mc(reserr, queenlist, 999)
errMoranMc



#GEOGRAPHICALLY WEIGHTED REGRESSION
#Setting an adaptive bandwidth. The output suggests a bandwidth of about 0.00786, or .786% of all 1720 observations (~13 observations)
#This code takes a while to run.
bw<-gwr.sel(formula=LNMEDHHINC~LNMEDHVAL+PCTVACANT, 
            data=shp,
            method = "aic",
            adapt = TRUE)
bw

#If we were to set a fixed bandwidth, we would use the following code. The optimal distance is the distance in the units of the 
#(projected) shapefile
bw_fixed<-gwr.sel(formula=LNMEDHHINC~LNMEDHVAL+PCTVACANT, 
                  data=shp,
                  method = "aic",
                  adapt = FALSE)

#Below, we run GWR:
gwrmodel<-gwr(formula=LNMEDHHINC~LNMEDHVAL+PCTVACANT,
              data=shp,
              adapt = bw, #adaptive bandwidth determined by proportion of observations accounted for
              gweight=gwr.Gauss,
              se.fit=TRUE, #to return local standard errors
              hatmatrix = TRUE)
gwrmodel
summary(gwrmodel$SDF)

#Mapping Coefficients
gwrresults<-as.data.frame(gwrmodel$SDF)
#Creating standardized coefficients (coefficients divided by standard errors)
shp$Std.coefLNMEDHVAL<-gwrresults$LNMEDHVAL/gwrresults$LNMEDHVAL_se
shp$Std.coefPCTVACANT<-gwrresults$PCTVACANT/gwrresults$PCTVACANT_se


#You can play around with the code to set the appropriate color scheme, such as the one suggested in 
coefLNMEDHVAL<-tm_shape(shp)+
  tm_fill(col='Std.coefLNMEDHVAL', style='fixed', title='Standardized Coefficient of variable', breaks = c(-100,-4,-2,0,2,4, 100), palette = '-RdBu')+
  tm_layout(frame=FALSE, title = 'Number Below Poverty (Log) - Standardized Coefficients')

coefPCTVACANT<-tm_shape(shp)+
  tm_fill(col='Std.coefPCTVACANT', style='fixed', title='Standardized Coefficient of variable', breaks = c(-100,-4,-2,0,2,4,100), palette='-RdBu')+
  tm_layout(frame=FALSE, title = 'Percentage of Housing Vacant - Standardized Coefficients')

tmap_arrange(coefLNMEDHVAL, coefPCTVACANT, ncol=2)

#Mapping Local R-Squares
shp$localR2<-gwrresults$localR2
tm_shape(shp)+
  tm_fill(col='localR2', style='fixed', breaks = c(0,0.2,0.4,0.6,0.8,1), palette = '-Blues', title='Local R-Squares')+
  tm_layout(frame=FALSE, title = 'Local R-Squares')


#Mapping Residuals
shp$gwrE<-gwrresults$gwr.e
tm_shape(shp)+
  tm_fill(col='gwrE', style='fixed', breaks = c(-10, -2, -1, 0, 1, 2, 10), palette = '-RdBu', title='GWR Residuals')+
  tm_layout(frame=FALSE, title = 'GWR Residuals')

#Let's look at Moran's I of GWR residuals
GWRMoranMc<-moran.mc(shp$gwrE, queenlist, 999)
GWRMoranMc