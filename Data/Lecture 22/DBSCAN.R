#DBSCAN

install.packages("dbscan")
install.packages("fpc")
install.packages("factoextra")

library(dbscan)
library(fpc)
library(factoextra)

# Load the data
data("iris")
iris <- as.matrix(iris[, 1:4])

dbscan::kNNdistplot(iris, k =  4)
abline(h = 0.4, lty = 2)

set.seed(123)
# fpc package
res.fpc <- fpc::dbscan(iris, eps = 0.4, MinPts = 4)
# dbscan package
res.db <- dbscan::dbscan(iris, 0.4, 4)

all(res.fpc$cluster == res.db)

fviz_cluster(res.fpc, iris, geom = "point")

??fviz_cluster

data("multishapes", package = "factoextra")
df <- multishapes[, 1:2]
The function dbscan() can be used as follow:
  
  library("fpc")
# Compute DBSCAN using fpc package
set.seed(123)
db <- fpc::dbscan(df, eps = 0.15, MinPts = 5)
# Plot DBSCAN results
plot(db, df, main = "DBSCAN", frame = FALSE)



It looks like the choose.vars argument is missing in your fviz_cluster() function. Try something like this:
  
iris.scaled <- scale(x = iris[, -5])

set.seed(123)
km.res <- kmeans(x = iris.scaled, centers = 3, nstart = 25)

fviz_cluster(object = km.res, data = iris.scaled, geom = "point",
             choose.vars = c("Sepal.Length", "Sepal.Width"), stand = FALSE, 
             ellipse.type = "norm") + theme_bw()
I also changed the frame.type argument since it is deprecated to ellipse.type.

Equivalent base R plot:
  
plot(x = iris$Sepal.Length, y = iris$Sepal.Width, col = km.res$cluster)
Update The author of the factoextra package, Alboukadel Kassambara, informed me that if you omit the choose.vars argument, the function fviz_cluster transforms the initial set of variables into a new set of variables through principal component analysis (PCA). This dimensionality reduction algorithm operates on the four variables and outputs two new variables (Dim1 and Dim2) that represent the original variables, a projection or "shadow" of the original data set. Each dimension represent a certain amount of the variation (i.e. information) contained in the original data set. In this example, Dim1 and Dim2 represent 73% and 22.9% respectively. When plotted, this lower-dimensional picture can be difficult to interpret. In exploratory data analysis, it is therefore perhaps more useful to purposefully select two variables at a time through the choose.vars argument, and then compare the plots.