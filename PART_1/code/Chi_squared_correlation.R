data<-read.csv("datasets/MI.data") #Load Dataset

dim(data) #Check dimensions

y<-data[c(122)] #122. Relapse of the myocardial infarction (REC_IM): Nominal Cases Fraction 

data<-data[-c(1, 95,102,105,112:124)] #Remove variables not useful for prediction.

data[]<-lapply(data, function(x) as.numeric(as.character(x))) #Force all datatypes to be numeric


Ordinal_columns <- data[c(4:7,9,11,45:48,92:95)] #Variables that are categorical
Real_variables <- data[c(2,35,36,37,38,84,86,87,88,89,90,91)]

Binary_variables <- subset(data, select = -c(2,4:7,9,11,35,36,37,38,45:48,84,86,87,88,90,91,92:95))

colnames(Binary_variables)

#Code for chi-squared correlations between dependent variable and the independent variables

#H0: The two variables are independent.
#H1: The two variables relate to each other.


x <- 1:length(Binary_variables)
result <- vector('list', length(x))
for(i in x){
  test <- chisq.test(Binary_variables[,colnames(Binary_variables[i])], y$X0.88)
  result[[i]] <- data.frame("X" = colnames(Binary_variables[i]), 
                         "Y" = colnames(y), 
                         "Chi.Square" = round(test$statistic,3),  
                         "df"= test$parameter,  
                         "p.value" = round(test$p.value, 3))
  
}
print(result) # to see the p values 
#p values < 0.05: reject Null hypothesis : the selected variables are dependent, so can be used to predict the dependent variable

#Feature selection: Selection variables that can be removed 
x <- 1:length(Binary_variables)
Remove_vars <- c(rep(0,100))
for(i in x){
  if(result[[i]][[5]]> 0.05){
    Remove_vars[i] <- result[[i]][[1]]
  }
    
}

#Remove binary variables that have p-value > 0.05
data <- data[ , !(names(data) %in% Remove_vars)]
       