#install.packages(c("cluster", "factoextra","fpc"))
library(cluster)
library(factoextra)

data <- readRDS("datasets/preprocessed.RDS")
y <- data$V122
data$V122 <- NULL


# mycluster <- function(x, k) list(cluster=cutree(hclust(dist(x), method = "average"),k=k))
# gap_stat <- clusGap(data, FUN = mycluster, K.max = 8, B = 500) 
# fviz_gap_stat(gap_stat) # "firstSEmax"
# 
#https://www.r-bloggers.com/2017/12/how-to-perform-hierarchical-clustering-using-r/
h.out<-hclust(dist(data))
h.out
plot(h.out, cex=0.6)
rect.hclust(h.out, k = 2, border = 2:5)

sub_grp <- cutree(h.out, k = 2)
fviz_cluster(list(data = data, cluster = sub_grp), ellipse.type = "norm")
y


#New way
rest.dist <- dist(data)
res.hc <- hclust(d = rest.dist, method = "ward.D2")
res.coph <- cophenetic(res.hc)
cor(rest.dist, res.coph)
res.hc2 <- hclust(d = rest.dist, method = "average")
cor(rest.dist, cophenetic(res.hc2))

#Divide in two groups
grp <- cutree(res.hc2, k =2)
table(grp)
#get the columns
rownames(data)[grp == 1]

fviz_dend(res.hc2, k = 2, 
          cex= 0.5,
          k_colors = c('g', 'r'),
          color_labels_by_k = TRUE,
          rect = TRUE)

fviz_cluster(list(data = data, cluster = grp), ellipse.type = "norm")

#########
#aggolomerative
res.agnes <- agnes(x = data,
                   metric = "euclidean",
                   method = "ward")
res.coph <- cophenetic(res.agnes)
cor(rest.dist, res.coph)

fviz_dend(res.agnes, cex = 0.6, k = 2)
#Divisive
res.diana <- diana(x = data,
                   metric = "euclidean")