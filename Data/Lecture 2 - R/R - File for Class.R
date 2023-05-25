#R CAN BE USED AS A CALCULATOR. 
#You can just type your equation in the command window after the ">": 
10^2 + 36 

#If you use parentheses and forget to add the closing parenthesis, 
#the ">" on the command line changes into a "+". The "+" can also mean
#that R is still busy with some heavy computation. 

#(10^2 + 36

#If you want R to quit what it was doing and give back the ">", press ESC

#*****

#GETTING INTO THE CONCEPT OF WORKSPACES.
#You can give numbers a name. By doing so, they become so-called variables 
#which can be used later. For example, you can type in the command window: 

a = 4			#This can also be written as a <- 4
a <- 4

#You can see that a appears in the workspace window, which means that R 
#now remembers what a is. 
#You can also ask R what a is (just type a ENTER in the command window): 

a 

#You can do calculations with a:
a * 5 

#You can also assign a new value to a using the old one. 

a = a + 10 
a 

a1 = log(a)
a1

#To remove all variables from R's memory, type 

rm(list=ls()) 


#You can see that RStudio then empties the workspace window. 
#If you only want to remove the variable a, you can type 

rm(a) 



#**
  
#SCALARS, VECTORS AND MATRICES
  
#Like many other programs, R organizes numbers as:
      # scalars (a single number, 0-dimensional)
      # vectors (a row or column of numbers, 1-dimensional), and 
      # matrices (like a table, 2-dimensional). 

#The a we defined on the previous slide was a scalar. 

#To define a vector with the numbers 3, 4 and 5, we need the function c,
#which is short for concatenate (paste together). 

b=c(3,4,5) 
b

#***
  
#FUNCTIONS
#If you have a vector b=c(3,4,5) you may perform various operations
#with the elements of the vector b.

mean(x=b)			#mean of the elements of b (3,4,5)
mean(b)

sum(x=b)			#sum of the elements of b (3,4,5)
sum(b)

#There are tons of different functions in R Another example: let's 
#generate and plot 10 random numbers from the standard normal distribution
x <-round(rnorm(10, mean=0, sd=1), 3)		#round to 3 decimal digits
#On the x axis is the index of the number; on the y axis is the value of the number
plot(x)

#***
  
#GO TO SLIDES 13-14
  
#***

#VECTOR OPERATIONS
  
vec1 = c(1,4,6,8,10) 				#we've seen the c() command before
vec1 

vec1[5] 					          #getting the 5th element of vec1
vec1[3] = 12 				        #replacing the 3rd element (6) with 12
vec1 

#In vec2 below, elements are multiples of 0.25 from 0 to 1
vec2 = seq(from=0, to=1, by=0.25) 
vec2 
sum(vec1)				          	#sum of vec1 elements

vec1 + vec2 				      	#vec1 & vec2 both have 5 elements, can be added
vec3 <- vec1 + vec2         #assigning the value of vec3
vec3

#***
  
#MATRICES
#Matrices are like 2-dimensional vectors (or like data tables)
#The argument data specifies which numbers should be in the matrix. 
#We use either ncol to specify the number of columns or nrow to 
#specify the number of rows.
mat=matrix(data=c(9,2,3,4,5,6),ncol=3) 
mat

mat=matrix(data=c(9,2,3,4,5,6),nrow=2) 
mat

#Matrix-operations are similar to vector operations: 
mat[1,2]		           #element in 1st row, 2nd column
r2 <- mat[2,]		       #r2 is a vector which is the 2nd row of matrix
c2 <- mat[,2]		       #c2 is a vector which is the 2nd column of matrix
mean(mat)		           #mean of all 6 matrix elements (9, 3, 5, 2, 4, 6)
sum(mat)               #sum of all 6 matrix elements (9, 3, 5, 2, 4, 6)

#Matrix operations
mat1=matrix(data=c(9,2,3,4,5,6),ncol=3) 
mat2=matrix(data=c(7,6,5,3,2,8),nrow=2)

mat1
mat2

matsum=mat1+mat2
matsum

#In order to append 2+ matrices, number of columns has to be the    
#same; in order to merge, number of rows has to be the same 
merging=cbind(mat1,mat2)
merging

appending=rbind(mat1,mat2)
appending

trans=t(appending)		#transposing the matrix
dim(trans)	          #dimensions of the matrix trans (rows x columns)

#***
  
#GO TO SLIDE 18
  
#***

#DATA FRAMES

#A data frame is a matrix with names above the columns. 
#This is nice, because you can call and use one of the columns 
#without knowing in which position it is.
t = data.frame(x = c(11,12,14), y = c(19,20,21), z = c(10,9,7)) 
t

mean(t$z) 			   #takes mean of column z in dataframe t
mean(t[["z"]]) 	 	 #same as mean(t$z)

#***

#GO TO SLIDE 20

#***

#LISTS
  
#The main advantage of lists is that the "columns" (which aren't 
#really ordered in columns any more, but are more a collection of vectors) 
#don't have to be of the same length, unlike matrices and data frames.

#We won't be using them much in this course.
L = list(one=1, two=c(1,2), five=seq(0, 1, length=5)) 
L 
names(L) 		#what's in the list?
L$five + 10 		


#***

#GO TO SLIDE 21
  
#***
  
#READING AND WRITING DATA FILES
#First, let's set the working directory
setwd("D:/Dropbox/Documents/Work and School/Teaching/CPLN 671 - Statistics and Data Mining/Data/Lecture 2 - R")

d = data.frame(a = c(3,4,5), b = c(12,43,54))		#create a data frame d
d

#export data frame d as .txt file in working directory
write.table(d, file="tst0.txt", row.names=FALSE) 	

#import it back into an R data frame (and call it d2)
d2 = read.table(file="tst0.txt", header=TRUE) 		
d2 

#importing Dataset.csv file and calling it mydata
mydata <- read.csv("RegressionData.csv")			



#READING AND WRITING SHAPEFILES
#See https://www.nceas.ucsb.edu/scicomp/usecases/ReadWriteESRIShapeFiles
install.packages("sp")
install.packages("spatstat")
install.packages("sf")
install.packages("rgdal")
#install.packages("gpclib")
install.packages("maptools")


library(sp)
library(spatstat)
#library(gpclib)
#gpclibPermit()
library(maptools)
library(sf)
library(rgdal)
                   
#Reading Polygon Boundary.shp from the directory above
PhillyData <- readOGR("RegressionData.shp")
PhillyData      

#Plotting the shapefile (making the border red)
plot(PhillyData, border="red")
#Setting the main title for the plot (at the top) and a sub-title (at the bottom)
title(main="Philadelphia 2000 Data", sub="Census BGs")

#Making a fancy choropleth map of the variable MEDHHINC (which is one of the
#variables in the shapefile, and represents median household income of the 
#block group)
spplot(PhillyData, "MEDHHINC")


title(main = "Philadelphia Block Groups")

#Exporting back to Shapefile format 
#Shapefile Exported_PhillyDAta.shp will appear in your working directory

#Ignore the warning messages
writeOGR(PhillyData, ".", "Exported_PhillyData", driver="ESRI Shapefile")

#***

#MISSING DATA
  
#Imagine we want to import the following dataset  (called Missing Data.csv)
NAEx = read.csv("Missing Data.csv")
NAEx

#Let's try to compute the max of NAExample
max(NAEx)                   #no results due to NA values
max(NAEx, na.rm=TRUE)       #ignore NA values

#na.rm command tells R whether NA values should be stripped before the 
#computation proceeds. This or similar command exists in many R functions.

#Let's create a new variable d, which is c+1
NAEx$d = NAEx$c + 1
NAEx	                    #we see that NAEx has new column d

#***
  
#GO TO SLIDES 25 AND 26

#***
  
#To tell R that something is a character string, you should type the text 
#between apostrophes, otherwise R will start looking for a defined variable 
#with the same name.
m = "apples" 
m 

n = pears 
m + 2 		              #you cannot do computations with character strings

#***

#IF-ELSE STATEMENTS
w = 3 
if(w < 5 ) {d=2} else {d=10} 
d								            
#since w<5, d=2; otherwise it'd be 10

#For vectors, matrices and dataframes this won't work:
if (NAEx$c < 3) {NAEx$e=NAEx$d*5} else {NAEx$e=NAEx$c-1000}    
NAEx

#Try this instead:
#ifelse(condition, value if condition is true, value if condition is false)
NAEx$e <- ifelse(NAEx$c < 3, NAEx$d*5, NAEx$c-1000)
NAEx

#***

#WRITING YOUR OWN FUNCTIONS
#The function fun1 below has 2 arguments (inputs), arg1 and arg2.
#We want to generate a variable w, which is the square of the first argument (arg1).
#We then want the function to output the sum of w and arg2
fun1 = function(arg1, arg2) 
{
  w = arg1 ^ 2 		          #generate variable w
  return(arg2 + w) 		      #fun1 output is arg2 + w
} 

#w=3^2; return = arg2+w = 5+9 = 14
fun1(arg1 = 3, arg2 = 5) 

#***

#STATISTICAL FUNCTIONS
a = c(1,1,1,1,0,0,0,0,0,0,2,2,2,2,3,3,3,4)
mean(a)  			        #mean of elements in a
max(a)				        #maximum element in a
round(sd(a),2)				#standard deviation of a, rounded to 2 decimal places
sum(a)				        #sum the elements of a
summary(a)    				#summary statistics, median and quartiles
summary(factor(a))		#create a frequency table of a
#summary(mydata$a)		#same logic & functions work if a is a variable in dataset mydata

#***

#AVOIDING SCIENTIFIC NOTATION
options(scipen=999)   #useful to have at the beginning of every program