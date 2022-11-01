#Steps in order to fix the dataset
#Load Dataset
# data<-read.csv("datasets/MI.data") 
data <- read.table('datasets/MI.data', sep = ',')

data[]<-lapply(data, function(x) as.numeric(as.character(x))) #Force all datatypes to be numeric
y <- data[c(122)]#Output

#Remove variables that are useless
data <- data[-c(1, 95, 102, 105, 112:121, 123, 124)] 

#Remove variables that have more than 25% missing values
data<-data[, which(colMeans(!is.na(data)) > 0.25)]

#Fix NaN values: Real variables: Mean, Binary variables: Mode
getmode <- function(v) { #Function for the mode
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

data_na <- data
for(i in 1:ncol(data)) {
  if (is.na(getmode (data_na[ , i]))){ #If mode is NA, most likely that it is real value, mean is okay
    data_na[ , i][is.na(data_na[ , i])] <- mean(data_na[ , i], na.rm = TRUE)
  }
  else{ #Take for the ordinal and binary 
    data_na[ , i][is.na(data_na[ , i])] <- getmode(data_na[ , i])
  }
}

#Normalization/standardization of real values
min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}
data_norm <- as.data.frame(lapply(data_na, min_max_norm))


#Split the data set in two seperate dataframes based on Binary vs real/ordinal
binary_data <- Filter(function(x) all(x %in% c(0, 1)), data_norm)
real_data <- data.frame(data_norm[, -which(names(data_norm) %in% names(binary_data))])

#Remove variables with correlation matrix (real variables and ordinal data)


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
# print(result) # to see the p values
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
cormat<-round(cor(real_data),2)

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

data_final_real<-real_data[-related_index]

#Merge, y variable is V122
final_data <- cbind(data_final_binary,data_final_real)
                      
write.csv(final_data, "datasets/preprocessed.csv", row.names = FALSE)

#Optional
#One hot encoding of Ordinal values

#
