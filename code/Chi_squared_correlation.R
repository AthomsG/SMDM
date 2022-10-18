data<-read.csv("datasets/MI.data") #Load Dataset

dim(data) #Check dimensions

y<-data[c(122)] #122. Relapse of the myocardial infarction (REC_IM): Nominal Cases Fraction 

data<-data[-c(1, 95,102,105,112:124)] #Remove variables not useful for prediction.

data[]<-lapply(data, function(x) as.numeric(as.character(x))) #Force all datatypes to be numeric


Ordinal_columns <- data[c(4:7,9,11,45:48,92:95)] #Variables that are categorical

#Code for chi-squared correlations between dependent variable and the independent variables

#H0: The two variables are independent.
#H1: The two variables relate to each other.


x <- 1:length(Ordinal_columns)
result <- vector('list', length(x))
for(i in x){
  test <- chisq.test(Ordinal_columns[,colnames(Ordinal_columns[i])], y$X0.88)
  result[[i]] <- data.frame("X" = colnames(Ordinal_columns[i]), 
                         "Y" = colnames(y), 
                         "Chi.Square" = round(test$statistic,3),  
                         "df"= test$parameter,  
                         "p.value" = round(test$p.value, 3))
  
}
print(result) # to see the p values 
#p values < 0.05: reject Null hypothesis : the selected variables are dependent, so can be used to predict the dependent variable

#Feature selection

