library(ggplot2)

data<-read.csv("datasets/MI.data") #Load Dataset

dim(data) #Check dimention

y<-data[c(122)] #122. Relapse of the myocardial infarction (REC_IM): Nominal Cases Fraction 

data<-data[-c(1, 95,102,105,112:124)] #Remove variables not useful for predition.

data[]<-lapply(data, function(x) as.numeric(as.character(x))) #Force all datatypes to be numeric

for(i in 1:ncol(data)) {       # for-loop over columns
  pdf(file=paste('plots/variable_distributions/variable_', i, '.pdf', sep=''))
  hist(data[, i],
       main=colnames(data)[i],
       xlab="Values",
       col="pink"
       #freq=FALSE 
       )
  dev.off()
}

pdf(file='plots/variable_distributions/response_variable.pdf')
hist(y,
     main='Response Variable',
     xlab="Values",
     col="pink"
     #freq=FALSE 
)
dev.off()
