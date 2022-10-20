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

#Remove variables with correlation matrix (real variables)


#Remove variables with Chi-Squared test (categorical variables)


#Optional
#One hot encoding of Ordinal values


#