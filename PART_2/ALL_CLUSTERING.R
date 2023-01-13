# PARTITIONING METHODS

# Install Dependencies
#install.packages("fpc")
#install.packages("dbscan")
#install.packages("factoextra")
#install.packages("cluster")

library("fpc")
library("dbscan")
library("factoextra")
library("cluster")
library(plyr)
library("ggplot2")

# Set seed for reproducibility
set.seed(1)
# Get preprocessed data
data <- readRDS("datasets/preprocessed.RDS")
class<- data$V122
data <- data[,!names(data) %in% 
               c('V122')]

############################################################################
#                           PARTITION CLUSTERING                           #
############################################################################


######################################
#              K-MEANS               #
######################################

#Find the Optimal Number of Clusters

#Number of Clusters vs. the Total Within Sum of Squares
fviz_nbclust(data, kmeans, method = "wss")


#or this plot it appears that there is a bit of an elbow or “bend” at k = 3 clusters.


#Perform K-Means Clustering with Optimal K

#perform k-means clustering with k = 3 clusters
km <- kmeans(data, centers = 2, nstart = 25)
#view results
km

#plot results of final k-means model
fviz_cluster(km, data = data)

{
  kmed_mad<-mapvalues(km$cluster, from = c(1, 2), to = c(0, 1))
  table(kmed_mad, class)
}

{
  kmed_mad<-mapvalues(km$cluster, from = c(1, 2), to = c(1, 0))
  table(kmed_mad, class)
}


#find means of each cluster
aggregate(data, by=list(cluster=km$cluster), mean)



final_data <- cbind(data, cluster = km$cluster)

######################################
#             K-MEDOIDS              #
######################################

fviz_nbclust(data, pam, method = "wss")

#perform k-medoids clustering with k = 7 clusters
kmed <- pam(data, k = 2)

#view results
kmed

#plot results of final k-medoids model
fviz_cluster(kmed, data = data)

{
  kmed_mad<-mapvalues(kmed$cluster, from = c(1, 2), to = c(0, 1))
  table(kmed_mad, class)
}

{
  kmed_mad<-mapvalues(kmed$cluster, from = c(2, 1), to = c(0, 1))
  table(kmed, class)
}

######################################
#               DBSCAN               #
######################################

# Knee Point Method
pdf("Knee_Point_DBSCAN.pdf") 
dbscan::kNNdistplot(data, k = 40)
abline(h = 2, lty = 2)
dev.off()

# Compute DBSCAN using fpc package
db <- fpc::dbscan(data, eps = 2, MinPts = 1000)

sum(db$cluster==0)

{
  table(db$cluster, class)
}

{
  kmed_mad<-mapvalues(db$cluster, from = c(0, 1), to = c(1, 0))
  table(kmed_mad, class)
}

hist(kmed_mad)

# Plot DBSCAN results
fviz_cluster(db, data, stand = FALSE, ellipse = FALSE, geom = "point")

histogram=hist(db$cluster)

barplot(height=c(107, 1593), 
        col=c("black", "salmon"),
        names.arg=c("Noise", "Cluster 1"),
        border="white",
        xlab="Frequency",
        ylab="Members")

############################################################################
#                           HIERARCHAL CLUSTERING                          #
############################################################################

d_data<-dist(scale(data))
hc_iris <- hclust(d_data, method = "ward.D2")

fviz_dend(hc_iris,k = 2,cex = 0.5,k_colors = c("#00AFBB","#E7B800","#FC4E07"),
          color_labels_by_k = TRUE, ggtheme = theme_minimal())

groups <- cutree(hc_iris, k = 2)

table(class,groups)
############################################################################

#install.packages(c("cluster", "factoextra","fpc"))
#install.packages('fossil')
library(cluster)
library(factoextra)
library(fossil)


data <- readRDS("datasets/preprocessed.RDS")
y <- data$V122
data$V122 <- NULL

# #See what the difference methods can do
# mycluster <- function(x, k) list(cluster=cutree(hclust(dist(x), method = "average"),k=k))
# gap_stat <- clusGap(data, FUN = mycluster, K.max = 8, B = 500)
# fviz_gap_stat(gap_stat) # "firstSEmax"

#New way
#aggolomerative
res.agnes <- agnes(x = data,
                   metric = "euclidean",
                   method = "average")
#Divide in two groups
grp <- cutree(res.agnes, k =2)

#Loop through everything
methods <- list("average", "single", "complete", "ward")
for (method in methods){
  res.agnes <- agnes(x = data,
                     metric = "euclidean",
                     method = method)
  grp <- cutree(res.agnes, k =2)
  rand_index <- rand.index(grp,y)
  print(method)
  print(rand_index)
}

#Divisive
res.diana <- diana(x = data,
                   metric = "euclidean")

grp <- cutree(res.diana, k =2)
rand_index <- rand.index(grp,y)


# #make dendogram
# fviz_dend(res.diana, cex = 0.6, k = 2)
# 
# #Make cluster plot
# fviz_cluster(list(data = data, cluster = grp), ellipse.type = "norm")
