#Load Dataset
data<-read.csv("datasets/MI.data") 
data[]<-lapply(data, function(x) as.numeric(as.character(x))) #Force all datatypes to be numeric
y <- data[c(122)]#Output

#Remove irrelevant data columns
data <- data[-c(1, 95, 102, 105, 112:121, 123, 124)] 

#Remove columns with more than 25% NA
data<-data[, which(colMeans(!is.na(data)) > 0.25)]

#Change NA values 
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

#perform normalization on the data set
min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}
data_norm <- as.data.frame(lapply(data_na, min_max_norm))





