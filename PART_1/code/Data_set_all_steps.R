library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
# # Real/numerical 2, 35, 36, 37, 38, 84, 86, 87, 88, 89. 90, 91  normalizen, anders te veel effect
# 

#Steps in order to fix the dataset
#Load Dataset
# data<-read.csv("datasets/MI.data") 
data <- read.table('datasets/MI.data', sep = ',')

data[]<-lapply(data, function(x) as.numeric(as.character(x))) #Force all datatypes to be numeric
y <- data[c(122)]#Output

#Remove variables that are useless
data <- data[-c(1, 95, 102, 105, 112:121, 123, 124)] 

#Split the data set in three seperate dataframes based on Binary, real, ordinal
binary_data <- Filter(function(x) all(x %in% c(0, 1, NA)), data)
real_data_only <- data[c('V2', 'V35', 'V36', 'V37', 'V38', 'V84', 'V86', 'V87', 'V88', 'V89' ,'V90', 'V91')]
without_ordinal <- cbind(binary_data, real_data_only)
ordinal_data <- data.frame(data[, -which(names(data) %in% names(without_ordinal))])

#Remove variables that have more than 25% missing values
binary_data<-binary_data[, which(colMeans(!is.na(binary_data)) > 0.75)]
real_data<-real_data_only[, which(colMeans(!is.na(real_data_only)) > 0.75)]
ordinal_data<-ordinal_data[, which(colMeans(!is.na(ordinal_data)) > 0.75)]

#Outlier replacement wit NA for the real values
outlierreplacement <- function(dataframe){
  dataframe %>%          
    map_if(is.numeric, ~ replace(.x, .x %in% boxplot.stats(.x)$out, NA)) %>%
    bind_cols 
}

real_data <- data.frame(outlierreplacement(real_data))

#Fix NaN values: Real variables: Mean, Binary variables: Mode
getmode <- function(v) { #Function for the mode
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#For binary and ordinal values, replace NA values with mode
for(i in 1:ncol(binary_data)){
  binary_data[ , i][is.na(binary_data[ , i])] <- getmode(binary_data[ , i])
}

for(i in 1:ncol(ordinal_data)){
  ordinal_data[ , i][is.na(ordinal_data[ , i])] <- getmode(ordinal_data[ , i])
}

#For real values, the mean
for (i in 1:ncol(real_data)){
  real_data[ , i][is.na(real_data[ , i])] <- mean(real_data[ , i], na.rm = TRUE)
}

#Normalization/standardization of real and ordinal values
real_ordinal_data <- cbind(real_data, ordinal_data)
min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}
real_ordinal_data <- as.data.frame(lapply(real_ordinal_data, min_max_norm))


#Remove variables with Chi-Squared test (Binary variables)
#Code for chi-squared correlations between dependent variable and the independent variables

#H0: The two variables are independent.
#H1: The two variables relate to each other.
x <- 1:length(binary_data)
result <- vector('list', length(x))
for(i in x){
  test <- chisq.test(binary_data[,colnames(binary_data[i])], y$V122)
  result[[i]] <- data.frame("X" = colnames(binary_data[i]), 
                            "Y" = colnames(y), 
                            "Chi.Square" = round(test$statistic,3),  
                            "df"= test$parameter,  
                            "p.value" = round(test$p.value, 3))
  
}
#print(result) # to see the p values 
#p values < 0.05: reject Null hypothesis : the selected variables are dependent, so can be used to predict the dependent variable

#Feature selection: Selection variables that can be removed 
x <- 1:length(binary_data)
Remove_vars <- c(rep(0,length(binary_data)))
for(i in x){
  if(result[[i]][[5]]> 0.05){
    Remove_vars[i] <- result[[i]][[1]]
  }
  
}

#Remove binary variables that have p-value > 0.05
data_final_binary <- binary_data[ , !(names(binary_data) %in% Remove_vars)]

#Removal of redundant variables real and ordinal data
#Get correlation matrix. Don't forget to ignore categorical variables in this process
cormat<-round(cor(real_ordinal_data),2)

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

if (is.null(related_index)){
  data_final_real <- real_ordinal_data
}else{
  data_final_real<-real_ordinal_data[-related_index]
}

#Merge, y variable is V122

final_data <- cbind(data_final_binary,data_final_real)
# 
saveRDS(final_data, file = "datasets/preprocessed.RDS")
