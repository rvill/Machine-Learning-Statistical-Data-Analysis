#Clustering with Iris

Iris = read.csv ("/home/rvill/Desktop/iris.csv")
View(Iris)
Iris.features = Iris  #create a dataset called Iris.features taken from Iris
Iris.features$Species <- NULL  #delete the Species column from Iris.features
Iris.features$cluster <- NULL  #delete the cluster column from Iris.features
results <- kmeans(Iris.features, 3) #save output into a variable called 'results' with 3 clusters
results #summary of kmeans cluster results. Available components show further information, type results$size or result$centers to navigate
#results$size shows how many results are in each clusters (3 clusters)
results$size
plot(Iris[c("Petal.Length", "Petal.Width")], col=results$cluster) #plot the petal.length and petal.width on the x-y axis, color (col) the results output by cluster (cluster is from available components)

plot(Iris[c("Petal.Length", "Petal.Width")], col=Iris$Species) #the actual results, to compare how it did to our results plot
#####################

#Pharmacy data
Pharm = read.csv("/home/rvill/Desktop/MIT_Spring2014/15.077-StatData/pset_6/15.077(S14)HW6-Prob3-Pharmaceuticals.csv")
View(Pharm)
Pharm.features = Pharm
Pharm.features$Symbol <- NULL
Pharm.features$Name <- NULL
Pharm.features$Median_Recommendation <- NULL
Pharm.features$Location <- NULL
Pharm.features$Exchange <- NULL
View(Pharm.features)
results <- kmeans(Pharm.features,2)
results
results$size
plot(Pharm[c("Beta", "PE_Ratio")], col=results$cluster)


############# working solution ##############
#ref: http://stats.stackexchange.com/questions/31083/how-to-produce-a-pretty-plot-of-the-results-of-k-means-cluster-analysis
library(cluster)
library(fpc)
dat <- Pharm.features[,-5] #without known classification
#kmeans cluster analysis
clus <-kmeans(dat,centers=2)
clus

plotcluster(dat,clus$cluster)
#fancy complex plot:
clusplot(dat, clus$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)
##############################################








####TEST, using ref to explain plot###
#Ref:http://www.pik-potsdam.de/research/publications/pikreports/.files/pr126.pdf

## Partitioning iris-data (data frame) into 3 clusters, 
## and displaying the silhouette plot. 
## Moreover a 2-dimensional projection of the partitioning is given. 

library(cluster) # Load the package cluster 
data(iris) # Load the famous (Fisher’s or Anderson’s) iris-dataset 
View(iris)
iris.x <- iris[, 1:4] # Select the specific datacolumns: i.e. Sepal.Length, Sepal.Width, Petal.Length, Petal.Width #rv: keep columns 1:4 
#test
iris.xx <- iris[,2:4] # [r,c] [do stuff in this row e.g. exclude rows with : do all of this before the comma , do stuff in this column e.g. from columsn 2 to 4, or 2:4 gets rid of the of 1st and 4th column, there are 4 columns total in the iris dataset given]
View(iris.xx)
#end test

pr3 <- pam(iris.x, 3) # Perform the clustering by the PAM-method with 3 clusters 
si<-silhouette(pr3) # Compute the Silhouette information for the given clustering
plot(si, col = c("red", "green", "blue")) # draw a silhouette plot with clusterwise coloring 

clusplot(iris.x, pr3$clustering, shade=TRUE,color = TRUE, col.clus= c("red", 
                                                                      "green", "blue")) # draw a 2-dimensional clustering plot for the given clustering

#####END TEST


###TEST2 using the PAM method
library(cluster)
pr3 <- pam(Pharm.features, 2)
si <-silhouette(pr3)
plot (si, col=c("red","blue"))#,"green","orange"))
clusplot(Pharm.features, pr3$clustering, shade=TRUE, color=TRUE,col.clus=c("red","green"))#,"blue","orange"))





######







####CLUSTER ESTIMATION TESTS###########

#determining the number of clusters, ref: http://stackoverflow.com/questions/15376075/cluster-analysis-in-r-determine-the-optimal-number-of-clusters
library(cluster)
test <-  clusGap(Pharm.features, kmeans, 10, B = 100, verbose = interactive())
  clusGap(Pharm.features, kmeans, 10, B = 100, verbose = interactive())
plot(test)

#using affinity propogation to determin # of clusters
install.packages("apcluster")
library(apcluster)
Pharm.features.apclus <- apcluster(negDistMat(r=2), Pharm.features)
cat("affinity propogation optimal number of clusters:", length(Pharm.features.apclus@clusters), "\n")
# 4
heatmap(Pharm.features.apclus)
plot(Pharm.features.apclus, Pharm.features)




#using expectation maximization
# See http://www.jstatsoft.org/v18/i06/paper
# http://www.stat.washington.edu/fraley/mclust/tr504.pdf
#
install.packages("mclust")
library(mclust)
# Run the function to see how many clusters
# it finds to be optimal, set it to search for
# at least 1 model and up 20.
d_clust <- Mclust(as.matrix(Pharm.features), G=1:20)
m.best <- dim(d_clust$z)[2]
cat("model-based optimal number of clusters:", m.best, "\n")
# 4 clusters



### BEST PLOT TO DETERMINE NUMBER OF CLUSTERS SO FAR (but with some errors) #####
#estimation around the medoids to estimate # of clusters
install.packages("fpc")
library(fpc)
pamk.best <- pamk(Pharm.features)
cat("number of clusters estimated by optimum average silhoutte width =", pamk.best$nc, "\n")
plot(pam(Pharm.features, pamk.best$nc))

####Calinsky criterion: Another approach to diagnosing how many clusters suit the data. In this case we try 1 to 10 groups.
install.packages("vegan")
require(vegan)
fit <- cascadeKM(scale(Pharm.features, center = TRUE,  scale = TRUE), 1, 10, iter = 1000)
plot(fit, sortg = TRUE, grpmts.plot = TRUE)
calinski.best <- as.numeric(which.max(fit$results[2,]))
cat("Calinski criterion optimal number of clusters:", calinski.best, "\n")
# 5 clusters!
