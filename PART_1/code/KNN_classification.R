# INSTALL LIBRARIES 
#install.packages('caret') 
#install.packages('kknn')
#install.packages('e1071')
#install.packages("caTools")
#install.packages("MLmetrics")

# CHECK TO INSTALL DMwR -> https://stackoverflow.com/questions/66923903/why-cant-i-install-the-dmwr-package
#install.packages(c("zoo","xts","quantmod", "abind")) ## FOR DMwR PACKAGE
#install.packages( PATH_TO_INTALLER, repos=NULL, type="source" )


# LOAD LIBRARIES 
library(caret)     # Split into train and test
library(kknn)      # KNN implementation
library(e1071)     # For outputting confusion matrix
library(caTools)   # 
library(MLmetrics) # Used for computing F1-Score metric
library(ggplot2)   # For plotting
library(DMwR)      # For SMOTE sampling

set.seed(40) # Set seed for reproducibility

### Get preprocessed data
##data <- readRDS("datasets/preprocessed.RDS")
##
### Separate into train and test sets
##indxTrain  <- createDataPartition(y = data$V122,p = 0.9, list = FALSE)
##data.train <-data[indxTrain,]
##data.test <- data[-indxTrain,] 
##
### CHECK WHAT DISTANCE TO USE!!!
##data.knn <- kknn(formula=formula(V122~.), train = data.train, test = data.test, k=13, distance = 1)
##
##fit <- fitted(data.knn)
##
##hist(fit)
##
##table(data.test$V122, round(fit))

##### - NEW
# Get preprocessed data
data <- readRDS("datasets/preprocessed.RDS")
class<- data$V122
data <- data[,!names(data) %in% 
           c('V122')]

data<-data.frame(data, class, stringsAsFactors=FALSE)
data$class<-as.factor(data$class)


# Separate into train and test sets
indxTrain  <- createDataPartition(y=data$class, p=0.8, list=FALSE)
data.train <-data[indxTrain,]
data.test  <- data[-indxTrain,]

rm(indxTrain)

# Upsampling Data
data.trainup<-upSample(x=data.train[,-ncol(data.train)],
                  y=data.train$class)
table(data.trainup$Class)

# Downsampling Data
data.traindown<-downSample(x=data.train[,-ncol(data.train)],
                           y=data.train$class)
table(data.traindown$Class)

# SMOTE
data.trainSMOTE <- SMOTE(class~.,data = data.train)
table(data.trainSMOTE$class)

f1 <- function(data, lev = NULL, model = NULL) {
  f1_val <- MLmetrics::F1_Score(y_pred = data$pred,
                                y_true = data$obs,
                                positive = "0")
  c(F1 = f1_val)
}

train_control <- trainControl(method = "cv", 
                              number=10,
                              summaryFunction = f1
                              )

knn_fit <- train(class~., 
                 data=data.train,
                 method="knn", 
                 tuneGrid = expand.grid(k = c(1:10)), 
                 metric ="F1", 
                 trControl=train_control
                 )

#knn_fit_up <- train(Class~., 
#                 data=data.trainup,
#                 method="knn", 
#                 tuneGrid = expand.grid(k = c(1:10)), 
#                 metric ="F1", 
#                 trControl=train_control
#                 )

knn_fit_down <- train(Class~., 
                    data=data.traindown,
                    method="knn", 
                    tuneGrid = expand.grid(k = c(1:20)), 
                    metric ="F1", 
                    trControl=train_control
                 )   

knn_fit_smote <- train(class~., 
                    data=data.trainSMOTE,
                    method="knn", 
                    tuneGrid = expand.grid(k = c(1:20)), 
                    metric ="F1", 
                    trControl=train_control
                 )

knn_fit
knn_fit_up
knn_fit_down
knn_fit_smote

######### SAMPLING TECHNIQUES REDUNDAT FOR KNN! THIS SERVES AS AN EXAMPLE FOR OTHER ML METHODS
###################################### PLOT F1 for class 1
pdf("plots/KNN_F1_1.pdf",
    width = 17, 
    height = 5)

#define plotting area
par(mfrow = c(1, 3))

# Plot Shows F1 Score - FOR 1
plot(knn_fit$results[,1], 
     knn_fit$results[,3],
     type="b",
     xlab="k",
     ylab="F1 Score",
     main="k-nearest neighbour")
grid(10,10)

plot(knn_fit_down$results[,1],
     knn_fit_down$results[,3],
     col="red",
     type="b",
     xlab="k",
     ylab="F1 Score",
     main="k-nearest neighbour - Downsampling")
grid(10,10)

plot(knn_fit_smote$results[,1],
     knn_fit_smote$results[,3],
     col="blue",
     type="b",
     xlab="k",
     ylab="F1 Score",
     main="k-nearest neighbour - SMOTE")
grid(10,10)

dev.off()

###################################### PLOT F1 for class 0
pdf("plots/KNN_F1_0.pdf",
    width = 17, 
    height = 5)

#define plotting area
par(mfrow = c(1, 3))

# Plot Shows F1 Score 
plot(knn_fit$results[,1], 
     knn_fit$results[,2],
     type="b",
     xlab="k",
     ylab="F1 Score",
     main="k-nearest neighbour")
grid(10,10)

plot(knn_fit_down$results[,1],
     knn_fit_down$results[,2],
     col="red",
     type="b",
     xlab="k",
     ylab="F1 Score",
     main="k-nearest neighbour - Downsampling")
grid(10,10)

plot(knn_fit_smote$results[,1],
     knn_fit_smote$results[,2],
     col="blue",
     type="b",
     xlab="k",
     ylab="F1 Score",
     main="k-nearest neighbour - SMOTE")
grid(10,10)

dev.off()

#CHOOSE BEST VALUE OF K ACCORDING TO F1 SCORE AND REPEAT!
predictions<-predict(knn_fit, newdata = data.test)
table(data.test$class, predictions)
