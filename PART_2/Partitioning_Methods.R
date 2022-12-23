# PARTITIONING METHODS

# Install Dependencies
install.packages("fpc")
install.packages("dbscan")
install.packages("factoextra")
install.packages("cluster")

library("fpc")
library("dbscan")
library("factoextra")
library("cluster")

library("ggplot2")

# Set seed for reproducibility
set.seed(1)
# Get preprocessed data
data <- readRDS("datasets/preprocessed.RDS")


######################################
#              K-MEANS               #
######################################

#Find the Optimal Number of Clusters

#Number of Clusters vs. the Total Within Sum of Squares
fviz_nbclust(data, kmeans, method = "wss")


#or this plot it appears that there is a bit of an elbow or “bend” at k = 3 clusters.


#Perform K-Means Clustering with Optimal K

#perform k-means clustering with k = 3 clusters
km <- kmeans(data, centers = 3, nstart = 25)
#view results
km


#plot results of final k-means model
fviz_cluster(km, data = data)




#find means of each cluster
aggregate(data, by=list(cluster=km$cluster), mean)



final_data <- cbind(data, cluster = km$cluster)


######################################
#             K-MEDOIDS              #
######################################

fviz_nbclust(data, pam, method = "wss")

#perform k-medoids clustering with k = 7 clusters
kmed <- pam(data, k = 7)

#view results
kmed

#plot results of final k-medoids model
fviz_cluster(kmed, data = data)

######################################
#               DBSCAN               #
######################################

# Knee Point Method
pdf("Knee_Point_DBSCAN.pdf") 
dbscan::kNNdistplot(data, k = 40)
abline(h = 2, lty = 2)
dev.off()

# Compute DBSCAN using fpc package
db <- fpc::dbscan(data, eps = 2, MinPts = 40)

# Plot DBSCAN results
fviz_cluster(db, data, stand = FALSE, ellipse = FALSE, geom = "point")

histogram=hist(db$cluster)

barplot(height=c(107, 1593), 
        col=c("black", "salmon"),
        names.arg=c("Noise", "Cluster 1"),
        border="white",
        xlab="Frequency",
        ylab="Members")
