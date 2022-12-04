library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(cowplot)
#Steps in order to fix the dataset
#Load Dataset
# data<-read.csv("datasets/MI.data") 
data <- read.table('datasets/MI.data', sep = ',')

data[]<-lapply(data, function(x) as.numeric(as.character(x))) #Force all datatypes to be numeric
y <- data[c(122)]#Output

#Remove variables that are useless
data <- data[-c(1, 95, 102, 105, 112:121, 123, 124)] 

#Mkae plot for amount of NA data points
# na_count <-sapply(data, function(y) sum(length(which(is.na(y)))))
# hist(na_count, breaks = 10, main = 'Histogram of missing values', xlab = 'Number of outliers')

#Split the data set in three seperate dataframes based on Binary, real, ordinal
binary_data <- Filter(function(x) all(x %in% c(0, 1, NA)), data)
real_data_only <- data[c('V2', 'V35', 'V36', 'V37', 'V38', 'V84', 'V86', 'V87', 'V88', 'V89' ,'V90', 'V91')]
without_ordinal <- cbind(binary_data, real_data_only)
ordinal_data <- data.frame(data[, -which(names(data) %in% names(without_ordinal))])

#Remove variables that have more than 25% missing values
binary_data<-binary_data[, which(colMeans(!is.na(binary_data)) > 0.75)]
real_data<-real_data_only[, which(colMeans(!is.na(real_data_only)) > 0.75)]
ordinal_data<-ordinal_data[, which(colMeans(!is.na(ordinal_data)) > 0.75)]

#Make boxplots to indicate outliers
# for (i in colnames(real_data)){
#   boxplot(data[i], main = i)
# }

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

# Make plot of distribution of continuous variables
# pdf(file= "Histograms.pdf" )
# 
# # create a 3X3 grid
# par( mfrow= c(3,3) )
# 
# # dim(real_data)
# for (i in colnames(real_data)){
#   real_data %>% pull(i) %>% hist(main = i)
# }
# 

#Make plots for the real variables, where the distributions can be compared
# real_data_y <- cbind(real_data, y)
# real_data_split <- split(real_data_y, real_data_y$V122)
# real_data_split_0 <- real_data_split[[1]]
# real_data_split_1<- real_data_split[[2]]
# 
# pdf(file= "Histograms_y.pdf" )
# 
# # create a 2X2 grid
# par( mfrow= c(3,2) )
# # 
# for (i in colnames(real_data_split_0)){
#   if( i != 'V122'){
#     real_data_split_0 %>%  pull(i) %>% hist(main = paste(i, ", V122 is False", sep=""), col = 'coral1')
#     real_data_split_1 %>%  pull(i) %>% hist(main = paste(i, ", V122 is True", sep=""), col = 'cyan3')
#   }
# }

#Make Bar charts for the ordinal data
# pdf(file= "BarCharts_ordinal.pdf" )
# 
# # create a 2X2 grid
# par( mfrow= c(5,4) )
# 
# for (i in colnames(ordinal_data)){
#   barplot(table(ordinal_data[i]), main = i)
# }


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

#Make bar Charts of binary data after removal of features
# pdf(file= "BarCharts_binary.pdf" )
# 
# # create a 2X2 grid
# par( mfrow= c(5,2) )
# 
# for (i in colnames(data_final_binary)){
#   barplot(table(data_final_binary[i]), main = i)
# }


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



#V3
#df <- data.frame(binary_data$V3, binary_data$V122)
#data.frame(table(binary_data$v3, binary_data$v122))
#dat <- data.frame(table(df$binary_data.V3,df$binary_data.V122))
#names(dat) <- c("V3", "V122", "Count")

#p1 <- ggplot(data=dat, aes(x=V3, y=Count, fill=V122)) + geom_bar(stat="identity")

colnames(ordinal_data)
#Stacked bar plots of binary data
pdf(file="Stacked_Bar_plots_ordinal_1.pdf")

#par(mfrow= c(3,6))

#V4
df <- data.frame(ordinal_data$V4, y$V122)
dat <- data.frame(table(df$ordinal_data.V4, df$y.V122))
names(dat) <- c("V4", "V122", "Count")
p1 <- ggplot(data=dat, aes(x=V4, y=Count, fill=V122)) + geom_bar(stat="identity")

#V5
df <- data.frame(ordinal_data$V5, y$V122)
dat <- data.frame(table(df$ordinal_data.V5, df$y.V122))
names(dat) <- c("V5", "V122", "Count")
p2 <- ggplot(data=dat, aes(x=V5, y=Count, fill=V122)) + geom_bar(stat="identity")

#V6
df <- data.frame(ordinal_data$V6, y$V122)
dat <- data.frame(table(df$ordinal_data.V6, df$y.V122))
names(dat) <- c("V6", "V122", "Count")
p3 <- ggplot(data=dat, aes(x=V6, y=Count, fill=V122)) + geom_bar(stat="identity")

#V7
df <- data.frame(ordinal_data$V7, y$V122)
dat <- data.frame(table(df$ordinal_data.V7, df$y.V122))
names(dat) <- c("V7", "V122", "Count")
p4 <- ggplot(data=dat, aes(x=V7, y=Count, fill=V122)) + geom_bar(stat="identity")

#V9
df <- data.frame(ordinal_data$V9, y$V122)
dat <- data.frame(table(df$ordinal_data.V9, df$y.V122))
names(dat) <- c("V9", "V122", "Count")
p5 <- ggplot(data=dat, aes(x=V9, y=Count, fill=V122)) + geom_bar(stat="identity")

#V11
df <- data.frame(ordinal_data$V11, y$V122)
dat <- data.frame(table(df$ordinal_data.V11, df$y.V122))
names(dat) <- c("V11", "V122", "Count")
p6 <- ggplot(data=dat, aes(x=V11, y=Count, fill=V122)) + geom_bar(stat="identity")

#V12
df <- data.frame(ordinal_data$V12, y$V122)
dat <- data.frame(table(df$ordinal_data.V12, df$y.V122))
names(dat) <- c("V12", "V122", "Count")
p7 <- ggplot(data=dat, aes(x=V12, y=Count, fill=V122)) + geom_bar(stat="identity")

#V45
df <- data.frame(ordinal_data$V45, y$V122)
dat <- data.frame(table(df$ordinal_data.V45, df$y.V122))
names(dat) <- c("V45", "V122", "Count")
p8 <- ggplot(data=dat, aes(x=V45, y=Count, fill=V122)) + geom_bar(stat="identity")

#V46
df <- data.frame(ordinal_data$V46, y$V122)
dat <- data.frame(table(df$ordinal_data.V46, df$y.V122))
names(dat) <- c("V46", "V122", "Count")
p9 <- ggplot(data=dat, aes(x=V46, y=Count, fill=V122)) + geom_bar(stat="identity")

#V47
df <- data.frame(ordinal_data$V47, y$V122)
dat <- data.frame(table(df$ordinal_data.V47, df$y.V122))
names(dat) <- c("V47", "V122", "Count")
p10 <- ggplot(data=dat, aes(x=V47, y=Count, fill=V122)) + geom_bar(stat="identity")

#V48
df <- data.frame(ordinal_data$V48, y$V122)
dat <- data.frame(table(df$ordinal_data.V48, df$y.V122))
names(dat) <- c("V48", "V122", "Count")
p11 <- ggplot(data=dat, aes(x=V48, y=Count, fill=V122)) + geom_bar(stat="identity")

#V92
df <- data.frame(ordinal_data$V92, y$V122)
dat <- data.frame(table(df$ordinal_data.V92, df$y.V122))
names(dat) <- c("V92", "V122", "Count")
p12 <- ggplot(data=dat, aes(x=V92, y=Count, fill=V122)) + geom_bar(stat="identity")

#V93
df <- data.frame(ordinal_data$V93, y$V122)
dat <- data.frame(table(df$ordinal_data.V93, df$y.V122))
names(dat) <- c("V93", "V122", "Count")
p13 <- ggplot(data=dat, aes(x=V93, y=Count, fill=V122)) + geom_bar(stat="identity")

#V94
df <- data.frame(ordinal_data$V94, y$V122)
dat <- data.frame(table(df$ordinal_data.V94, df$y.V122))
names(dat) <- c("V94", "V122", "Count")
p14 <- ggplot(data=dat, aes(x=V94, y=Count, fill=V122)) + geom_bar(stat="identity")

#V100
df <- data.frame(ordinal_data$V100, y$V122)
dat <- data.frame(table(df$ordinal_data.V100, df$y.V122))
names(dat) <- c("V100", "V122", "Count")
p15 <- ggplot(data=dat, aes(x=V100, y=Count, fill=V122)) + geom_bar(stat="identity")

#V101
df <- data.frame(ordinal_data$V101, y$V122)
dat <- data.frame(table(df$ordinal_data.V101, df$y.V122))
names(dat) <- c("V101", "V122", "Count")
p16 <- ggplot(data=dat, aes(x=V101, y=Count, fill=V122)) + geom_bar(stat="identity")

#V103
df <- data.frame(ordinal_data$V103, y$V122)
dat <- data.frame(table(df$ordinal_data.V103, df$y.V122))
names(dat) <- c("V103", "V122", "Count")
p17 <- ggplot(data=dat, aes(x=V103, y=Count, fill=V122)) + geom_bar(stat="identity")

#V104
df <- data.frame(ordinal_data$V104, y$V122)
dat <- data.frame(table(df$ordinal_data.V104, df$y.V122))
names(dat) <- c("V104", "V122", "Count")
p18 <- ggplot(data=dat, aes(x=V104, y=Count, fill=V122)) + geom_bar(stat="identity")

plot_grid(p1, p2, p3, p4, p5, p6, p7, p8, p9)


pdf(file="Stacked_Bar_plots_ordinal_2.pdf")
plot_grid(p10, p11, p12, p13, p14, p15, p16, p17, p18)
