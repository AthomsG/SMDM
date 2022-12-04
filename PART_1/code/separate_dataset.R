data<-read.csv("datasets/MI.data") #Load Dataset

dim(data) #Check dimention

data<-data[-c(1, 95,102,105,112:124)] #Remove variables not useful for predition.

data[]<-lapply(data, function(x) as.numeric(as.character(x))) #Force all datatypes to be numeric

binary<-list()
real<-list()
ordinal<-list()

for (col in data){
  size=length(unique(col))
  if (size<4){
    binary<-c(binary, list(col))
  }
  if (size>4 & size<20){
    ordinal<-c(ordinal, list(col))
  }
  if (size>19){
    real<-c(real, list(col))
  }
}

binary<-data.frame(binary) #CHECKED
ordinal<-data.frame(ordinal) #CHECKED
real<-data.frame(real) #CHECKED
