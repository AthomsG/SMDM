# LIBRARIES
#install.packages("ggplot2") # install the package
library(ggplot2)

data<-read.csv("datasets/MI.data") #Load Dataset

dim(data) #Check dimention

y<-data[c(122)] #122. Relapse of the myocardial infarction (REC_IM): Nominal Cases Fraction 

data<-data[-c(1, 95,102,105,112:124)] #Remove variables not useful for predition.

data[]<-lapply(data, function(x) as.numeric(as.character(x))) #Force all datatypes to be numeric

missing<-(colMeans(is.na(data)))*100 #Get percentage of missing values of each col
missing = data.frame(as.list(missing))
missing <- as.data.frame(t(missing))

print(missing)

row.names(missing)

#Plot barplot with variables and percentage of missing values
ggplot(missing, aes(x=row.names(missing), y=missing[, "V1"])) + 
  geom_col(fill = "#0099f9") + coord_flip() + labs(x='Variable Name', y='Percentage Missing')

ggsave(file="missing_data.pdf", width=4, height=12, dpi=300)
