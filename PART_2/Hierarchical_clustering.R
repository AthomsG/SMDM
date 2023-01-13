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
  print(table(mapvalues(grp, from = c(2, 1), to = c(0, 1)),y))
  print(table(mapvalues(grp, from = c(1, 2), to = c(0, 1)),y))
  rand_index <- rand.index(grp,y)
  print(method)
  print(rand_index)
}



#Divisive
res.diana <- diana(x = data,
                   metric = "euclidean")

grp <- cutree(res.diana, k =2)
rand_index <- rand.index(grp,y)
print(table(mapvalues(grp, from = c(2, 1), to = c(0, 1)),y))
print(table(mapvalues(grp, from = c(1, 2), to = c(0, 1)),y))

# #make dendogram
# fviz_dend(res.diana, cex = 0.6, k = 2)
# 
# #Make cluster plot
# fviz_cluster(list(data = data, cluster = grp), ellipse.type = "norm")
