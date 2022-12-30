#install.packages(c("cluster", "factoextra","fpc"))
#install.packages('fossil')
library(cluster)
library(factoextra)
library(fossil)


data <- readRDS("datasets/preprocessed.RDS")
y <- data$V122
data$V122 <- NULL

#See what the difference methods can do
# mycluster <- function(x, k) list(cluster=cutree(hclust(dist(x), method = "average"),k=k))
# gap_stat <- clusGap(data, FUN = mycluster, K.max = 8, B = 500) 
# fviz_gap_stat(gap_stat) # "firstSEmax"
# 
# #https://www.r-bloggers.com/2017/12/how-to-perform-hierarchical-clustering-using-r/
# h.out<-hclust(dist(data))
# plot(h.out, cex=0.6)
# rect.hclust(h.out, k = 2, border = 2:5)
# 
# sub_grp <- cutree(h.out, k = 2)
# fviz_cluster(list(data = data, cluster = sub_grp), ellipse.type = "norm")

#New way
methods <- list("average", "single", "complete", "ward")
#aggolomerative
res.agnes <- agnes(x = data,
                   metric = "euclidean",
                   method = "average")

#Divisive
res.diana <- diana(x = data,
                   metric = "euclidean")

#Divide in two groups
grp <- cutree(res.agnes, k =2)
table(grp)
#get the columns
# rownames(data)[grp == 1]
# rownames(data)[grp == 2]
# rand_index <- rand.index(grp-1,y)
# adj_rand_index <- adj.rand.index(grp-1,y)

#make dendogram
fviz_dend(res.diana, cex = 0.6, k = 2)

#Make cluster plot
fviz_cluster(list(data = data, cluster = grp), ellipse.type = "norm")


# for (method in methods){
#   res.agnes <- agnes(x = data,
#                      metric = "euclidean",
#                      method = method)
#   grp <- cutree(res.agnes, k =2)
  # rand_index <- rand.index(grp-1,y)
  # adj_rand_index <- adj.rand.index(grp-1,y)
#   print(grp)
#   fviz_cluster(list(data = data, cluster = grp), ellipse.type = "norm")
#   
# }


