## set your working directory
getwd()
setwd("C:\\Users\\vsm397\\Desktop\\Vir's Personal documents\\Pred Modeling (MSBA)")

## import the data and create data frame
green <- read.csv("greenbuildings.csv",header = T)

library(mosaic)
library(ggplot2)
library(LICORS)  # for kmeans++
library(foreach)


green_only = subset(green, green_rating==1)
not_green = subset(green, green_rating==0)

#the distributation of occupancy
hist(green_only$leasing_rate, 25)
hist(not_green$leasing_rate, 25)
#summary(lm(leasing_rate~.,data=green))


green_scaled <- scale(green, center=TRUE, scale=TRUE) 

# Form a pairwise distance matrix using the dist function
green_distance_matrix = dist(green_scaled, method='euclidean')


# Now run hierarchical clustering
hier_green = hclust(green_distance_matrix, method='average')


# Plot the dendrogram
plot(hier_green, cex=0.8)

# Cut the tree into 5 clusters
cluster1 = cutree(hier_green, k=5)
summary(factor(cluster1))

# Examine the cluster members
which(cluster1 == 1)
which(cluster1 == 2)
which(cluster1 == 3)




X = green
View(X)
X = scale(X, center=TRUE, scale=TRUE)
X = na.omit(X)

# Extract the centers and scales from the rescaled data (which are named attributes)
mu = attr(X,"scaled:center")
sigma = attr(X,"scaled:scale")

# Run k-means with 6 clusters and 25 starts
clust1 = kmeans(X, 6, nstart=25)

# What are the clusters?
clust1$center  # not super helpful
clust1$center[1,]*sigma + mu
clust1$center[2,]*sigma + mu
clust1$center[4,]*sigma + mu



# Which cars are in which clusters?
which(clust1$cluster == 1)
which(clust1$cluster == 2)
which(clust1$cluster == 3)
which(clust1$cluster == 4)
which(clust1$cluster == 5)

green2 = na.omit(green)
# A few plots with cluster membership shown
# qplot is in the ggplot2 library
qplot(size, Rent, data=green2, color=factor(clust1$cluster))
qplot(leasing_rate, Rent, data=green2, color=factor(clust1$cluster))
qplot(LEED, Rent, data=green2, color=factor(clust1$cluster))
qplot(LEED, leasing_rate, data=green2, color=factor(clust1$cluster))
qplot(LEED, size, data=green2, color=factor(clust1$cluster))
qplot(LEED, age, data=green2, color=factor(clust1$cluster))



clust2 = kmeanspp(X, k=6, nstart=25)
qplot(LEED, Rent, data=green2, color=factor(clust2$cluster))
qplot(LEED, leasing_rate, data=green2, color=factor(clust2$cluster))
qplot(LEED, size, data=green2, color=factor(clust2$cluster))
qplot(LEED, age, data=green2, color=factor(clust2$cluster))