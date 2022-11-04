#Exploratory data analysis

#Import libraries
library(ggplot2)
library(cowplot)

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


#Dependent variable
final_data$V122

colnames(final_data)[1]

EDA_dataset <- data_na[, colnames(final_data)]

summary(EDA_dataset)

Dependent_var <- table(data_na$V122)
barplot(Dependent_var, main="Myocarditis",
        xlab="No")

#Stacked bar plots of binary data
pdf(file="Stacked_bar_plots.pdf")

par(mfrow= c(3,3))

#V3
df <- data.frame(binary_data$V3, binary_data$V122)
data.frame(table(binary_data$v3, binary_data$v122))
dat <- data.frame(table(df$binary_data.V3,df$binary_data.V122))
names(dat) <- c("V3", "V122", "Count")

p1 <- ggplot(data=dat, aes(x=V3, y=Count, fill=V122)) + geom_bar(stat="identity")

#V20
df <- data.frame(binary_data$V20, binary_data$V122)
data.frame(table(binary_data$v20, binary_data$v122))
dat <- data.frame(table(df$binary_data.V20,df$binary_data.V122))
names(dat) <- c("V20", "V122", "Count")

p2 <- ggplot(data=dat, aes(x=V20, y=Count, fill=V122)) + geom_bar(stat="identity")

#V27
df <- data.frame(binary_data$V27, binary_data$V122)
data.frame(table(binary_data$v27, binary_data$v122))
dat <- data.frame(table(df$binary_data.V27,df$binary_data.V122))
names(dat) <- c("V27", "V122", "Count")

p3 <- ggplot(data=dat, aes(x=V27, y=Count, fill=V122)) + geom_bar(stat="identity")

#V30
df <- data.frame(binary_data$V30, binary_data$V122)
data.frame(table(binary_data$v30, binary_data$v122))
dat <- data.frame(table(df$binary_data.V30,df$binary_data.V122))
names(dat) <- c("V30", "V122", "Count")

p4 <- ggplot(data=dat, aes(x=V30, y=Count, fill=V122)) + geom_bar(stat="identity")

#V31
df <- data.frame(binary_data$V31, binary_data$V122)
data.frame(table(binary_data$v31, binary_data$v122))
dat <- data.frame(table(df$binary_data.V31,df$binary_data.V122))
names(dat) <- c("V31", "V122", "Count")

p5 <- ggplot(data=dat, aes(x=V31, y=Count, fill=V122)) + geom_bar(stat="identity")

#V43
df <- data.frame(binary_data$V43, binary_data$V122)
data.frame(table(binary_data$v43, binary_data$v122))
dat <- data.frame(table(df$binary_data.V43,df$binary_data.V122))
names(dat) <- c("V43", "V122", "Count")

p6 <- ggplot(data=dat, aes(x=V43, y=Count, fill=V122)) + geom_bar(stat="identity")

#V54
df <- data.frame(binary_data$V54, binary_data$V122)
data.frame(table(binary_data$v54, binary_data$v122))
dat <- data.frame(table(df$binary_data.V54,df$binary_data.V122))
names(dat) <- c("V54", "V122", "Count")

p7 <- ggplot(data=dat, aes(x=V54, y=Count, fill=V122)) + geom_bar(stat="identity")

#V99
df <- data.frame(binary_data$V99, binary_data$V122)
data.frame(table(binary_data$v99, binary_data$v122))
dat <- data.frame(table(df$binary_data.V99,df$binary_data.V122))
names(dat) <- c("V99", "V122", "Count")

p8 <- ggplot(data=dat, aes(x=V99, y=Count, fill=V122)) + geom_bar(stat="identity")

#V109
df <- data.frame(binary_data$V109, binary_data$V122)
data.frame(table(binary_data$v109, binary_data$v122))
dat <- data.frame(table(df$binary_data.V109,df$binary_data.V122))
names(dat) <- c("V109", "V122", "Count")

p9 <- ggplot(data=dat, aes(x=V109, y=Count, fill=V122)) + geom_bar(stat="identity")


plot_grid(p1, p2, p3, p4, p5, p6, p7, p8, p9)


