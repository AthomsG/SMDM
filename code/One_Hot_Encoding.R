data<-read.csv("datasets/MI.data") #Load Dataset

dim(data) #Check dimention

y<-data[c(122)] #122. Relapse of the myocardial infarction (REC_IM): Nominal Cases Fraction 

data<-data[-c(1, 95,102,105,112:124)] #Remove variables not useful for predition.

data[]<-lapply(data, function(x) as.numeric(as.character(x))) #Force all datatypes to be numeric



#Code retrieved from https://stackoverflow.com/questions/48649443/how-to-one-hot-encode-several-categorical-variables-in-r
one_hot_encoding = function(df, columns="OHE"){ #OHE=column names to one hot encode
  # copy original dataset
  df = cbind(df)
  # convert the columns to vector in case it is a string
  columns = c(columns)
  # for each variable perform the OHE
  for (column in columns){
    unique_values = sort(unique(df[column])[,column])
    non_reference_values  = unique_values[c(-1)] # the first element is going 
    # to be the reference by default
    for (value in non_reference_values){
      # the new dummy column name
      new_col_name = paste0(column,'.',value)
      # create new dummy column for each value of the non_reference_values
      df[new_col_name] <- with(df, ifelse(df[,column] == value, 1, 0))
    }
    # delete the one hot encoded column
    df[column] = NULL
    
  }
  return(df)
}

#Apply to specific columns: 4-7,9,11,45-48,92-95
Ordinal_columns <- data[c(4:7,9,11,45:48,92:95)]
colnames(Ordinal_columns)

df_test <- one_hot_encoding(data, columns=colnames(Ordinal_columns))
colnames(df_test)
