install.packages(c("cluster", "factoextra","fpc"))
library(cluster)
library(factoextra)

data <- readRDS("datasets/preprocessed.RDS")
y <- data$V122
data$V122 <- NULL


mycluster <- function(x, k) list(cluster=cutree(hclust(dist(x), method = "average"),k=k))
gap_stat <- clusGap(data, FUN = mycluster, K.max = 3, B = 500) 
fviz_gap_stat(gap_stat) # "firstSEmax"

#https://www.r-bloggers.com/2017/12/how-to-perform-hierarchical-clustering-using-r/
h.out<-hclust(dist(data))
h.out
plot(h.out, cex=0.6)
rect.hclust(h.out, k = 2, border = 2:5)

sub_grp <- cutree(h.out, k = 2)
fviz_cluster(list(data = data, cluster = sub_grp), ellipse.type = "norm") 
y
