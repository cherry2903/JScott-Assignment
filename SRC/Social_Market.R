library(corrplot)
#Load the data
social_market = read.csv('C:\\Users\\cherr\\STA380\\data\\social_marketing.csv', row.names=1)
#Selecting first few entries in the dataset
View(head(social_market))

#Filtering out columns = Chatter, Uncategorised, Spam and Adult. Anslyzing the data we can find that these few categories 
#might not be a good fit in most of the clusters
social_market <- social_market[,-c(1,5,35,36)]

#Check if any interest are correlated
corr_mat=cor(social_market,method="s")
corr_mat[1:32, 1:32]
corrplot(corr_mat)
# personal fitness and hesalth nutrition highly correlated - fitness enthusiast
# news and politics
# shopping and photo sharing
# parents, religion and sports_fandom

# Center/scale the data
social_market_scaled <- scale(social_market) 
View(head(social_market_scaled))

set.seed(99)

###Kmeans Clustering 
par(mfrow = c(1, 1))

# Scree plot for identifyin K

# Initialize total within sum of squares error: wss
wss <- 0

# Look over 1 to 15 possible clusters which gives minimum sum of squares
for (i in 1:15) {
  # Fit the model: km.out
  km.out <- kmeans(social_market_scaled, centers = i, nstart = 20, iter.max = 50)
  # Save the within cluster sum of squares
  wss[i] <- km.out$tot.withinss
}


# Produce a scree plot
plot(1:15, wss, type = "b", 
     xlab = "Number of Clusters", 
     ylab = "Within groups sum of squares")

# Taking optimum vale of K after which no improvement in SSE and fitting the model again
k <- 11

# Build model with k clusters: km.out
set.seed(99)
km.out <- kmeans(social_market_scaled, centers = k, nstart = 50, iter.max = 50)

# View the resulting model
km.out

#plotting the clusters (you can remove this, graph is too messy)
library(cluster)
clusplot(social_market_scaled, km.out$cluster, main='2D representation of the Cluster solution', 
         color=TRUE, shade=TRUE, labels=2, lines=0)

#identifying the groups in each cluster
for (i in 1:k){
  pie(colSums(social_market[km.out$cluster==i,]),cex=0.9)
}

### # Perform PCA 
pr.out = prcomp(social_market, scale = TRUE)

Cols=function(vec){cols=rainbow(length(unique(vec)));return(cols[as.numeric(as.factor(vec))])}

# plotting the first three components score vectors
par(mfrow=c(1,2))
plot(pr.out$x[,1:2], col=Cols(colnames(social_market)), pch=19,xlab="Z1",ylab="Z2")
plot(pr.out$x[,c(1,3)], col=Cols(colnames(social_market)), pch=19,xlab="Z1",ylab="Z3")

#Summary for PC output
summary(pr.out)

#Plot to explain the variance explained by first few PC
plot(pr.out)

#PVE for each PC(scree plot) and the cumulative PVE plot
par(mfrow=c(1,2))
plot(summary(pr.out)$importance[2,], type="o", ylab="PVE", xlab="Principal Component ",col="blue")
plot(summary(pr.out)$importance[3,], type="o", ylab="Cumulative PVE", xlab="Principal Component ", col="brown3")

#### # We can explain 80% of the variability with 16 principal components as can be seen from the graph

# Fit hierarchical clustering on these 16 PCs

# We use Correlation-based distance here(market segment problem)
res.cor <- cor(t(pr.out$x[,1:16]), method = "pearson")
d.cor <- as.dist(1 - res.cor)

# fitting hierarchical clustering
hc.out_single = hclust(d.cor, method = 'single')
# Visualization of hclust
plot(hc.out_single, labels = FALSE, hang = -1)

hc.out_complete = hclust(d.cor, method = 'complete')
# Visualization of hclust
plot(hc.out_complete, labels = FALSE, hang = -1)

hc.out_average = hclust(d.cor, method = 'average')
# Visualization of hclust
plot(hc.out_average, labels = FALSE, hang = -1)

#Average-link (or group average) clustering is a compromise between the sensitivity of 
#complete-link clustering to outliers and the tendency of single-link clustering to form 
#long chains that do not correspond to the intuitive notion of clusters as compact, spherical objects.


#Looking at the tree, we can cut the tree at the height of 0.9
hc_clust_avg = cutree(hc.out_average, h = 0.9)
table(hc_clust_avg)


##identifying the groups in each cluster
for (i in 1:12){
  pie(colSums(social_market[hc_clust_avg == i,]),cex=0.9)
}

