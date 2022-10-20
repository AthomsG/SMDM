# http://www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-heatmap-r-software-and-data-visualization

#DO CHI^2 TEST INSTEAD!!! VARIABLES ARE CATEGORICAL...

# LIBRARIES
#install.packages("ggplot2") # install the package
#install.packages("reshape2") # install the package
library(ggplot2)
library(reshape2)

data<-read.csv("datasets/MI.data") #Load Dataset

dim(data) #Check dimention

y<-data[c(122)] #122. Relapse of the myocardial infarction (REC_IM): Nominal Cases Fraction 

data<-data[-c(1, 95,102,105,112:124)] #Remove variables not useful for predition.

data[]<-lapply(data, function(x) as.numeric(as.character(x))) #Force all datatypes to be numeric

##Remove columns with more than 20% NA
data<-data[, which(colMeans(!is.na(data)) > 0.2)]

#Remove rows with NA's using na.omit()
data <- na.omit(data)
#Remove columns with standard deviation of zero
data<-Filter(function(x) sd(x) != 0, data)

#Get correlation matrix. Don't forget to ignore categorical variables in this process
cormat<-round(cor(data),2)

################### AUXILIARY FUNCTIONS ##########################

# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}
reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}

##################################################################

# Reorder the correlation matrix
cormat <- reorder_cormat(cormat)
upper_tri <- get_upper_tri(cormat)
# Melt the correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Create a ggheatmap
ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

#Adds correlation coefficients on the heatmap
ggheatmap<-ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))
print(ggheatmap)
ggsave(file="corr_matrix.pdf", width=40, height=40, dpi=300)


#Remove one variable of pair with high correlation.
#Do correlation with response variable (watch out because response variable is categorical)
