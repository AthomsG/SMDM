# INSTALL LIBRARIES 
#install.packages('caret') 
#install.packages('kknn')
#install.packages('e1071')
#install.packages("caTools")
#install.packages("MLmetrics")

# LOAD LIBRARIES 
library(caret) # Split into train and test
library(kknn)  # KNN implementation
library(e1071) # For outputting confusion matrix
library(caTools)
library(MLmetrics) # Used for computing F1-Score metric
library(ggplot2)

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
indxTrain  <- createDataPartition(y = data$class,p = 0.8, list = FALSE)
data.train <-data[indxTrain,]
data.test  <- data[-indxTrain,]

rm(indxTrain)

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
knn_fit

pdf("plots/KNN_F1_1.pdf",
    width = 5, 
    height = 5)
# Plot Shows F1 Score 
plot(knn_fit$results[,1], 
     knn_fit$results[,3],
     type="b",
     xlab="k",
     ylab="F1 Score",
     main="Cross-validation: k-nearest neighbour")
grid(10,10)
dev.off()

pdf("plots/KNN_F1_0.pdf",
    width = 5, 
    height = 5)
# Plot Shows F1 Score 
plot(knn_fit$results[,1], 
     knn_fit$results[,2],
     col='red',
     type="b",
     xlab="k",
     ylab="F1 Score",
     main="Cross-validation: k-nearest neighbour")
grid(10,10)
dev.off()

#CHOOSE BEST VALUE OF K ACCORDING TO F1 SCORE AND REPEAT!
predictions<-predict(knn_fit, newdata = data.test)
table(data.test$class, predictions)
