# LIBRARIES
#install.packages("ggplot2") # install the package
#install.packages("reshape2") # install the package
library(ggplot2)
library(reshape2)

data<-read.csv("datasets/MI.data") #Load Dataset

c_real<-c(2, 35, 36, 37, 38, 84, 86, 87, 88, 89, 90, 91)#Vector with continuos distributed data
data<-(data[c_real])

data[]<-lapply(data, function(x) as.numeric(as.character(x))) #Force all datatypes to be numeric

##Remove columns with more than 20% NA
data<-data[, which(colMeans(!is.na(data)) > 0.25)]

#Remove rows with NA's using na.omit()
data <- na.omit(data)
#Remove columns with standard deviation of zero
data<-Filter(function(x) sd(x) != 0, data)

#Get correlation matrix. Don't forget to ignore categorical variables in this process
cormat<-round(cor(data),2)

################### AUXILIARY FUNCTIONS ##########################
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}
##################################################################
upper_tri <- get_upper_tri(cormat)
#Remove one variable of pair with high correlation.
related_index = c()
for (line in 1:dim(upper_tri)[1]){
  for (col in 1:line){
    i = col+dim(upper_tri)[1]*(line-1)
    if (abs(upper_tri[i])>0.8 & abs(upper_tri[i])<1){
      related_index<-c(related_index, line)
    }
  }
}

data<-data[-related_index]
