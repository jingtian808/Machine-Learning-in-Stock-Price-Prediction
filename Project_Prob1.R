library(randomForest)
library(ranger)
library(caret)

#---------------- Set location--------------------
setwd("C:/Users/xieli/Desktop/850 project")
# read our sector CVS
data_sectors <- read.csv('GICS.csv',header=TRUE,stringsAsFactors=FALSE)
sector_names_list <- unique(data_sectors$Sector)

#------ load all models for different sector-----------------
model_1 = readRDS("Communication Services.rds")
model_2 = readRDS("Consumer Discretionary.rds")
model_3 = readRDS("Consumer Staples.rds")
model_4 = readRDS("Energy.rds")              
model_5 = readRDS("Financials.rds")              
model_6 = readRDS("Health Care.rds")
model_7 = readRDS("Industrials.rds")            
model_8 = readRDS("Information Technology.rds")
model_9 = readRDS("Materials.rds")
model_10 = readRDS("Real Estate.rds")           
model_11 = readRDS("Utilities.rds") 
model_other = readRDS("Other.rds")

# Communication_Services = readRDS("Communication Services.rds")
# Consumer_Discretionary = readRDS("Consumer Discretionary.rds")
# Consumer_Staples = readRDS("Consumer Staples.rds")
# Energy = readRDS("Energy.rds")              
# Financials = readRDS("Financials.rds")              
# Health_Care = readRDS("Health Care.rds")
# Industrials = readRDS("Industrials.rds")            
# Information_Technology = readRDS("Information Technology.rds")
# Materials = readRDS("Materials")
# Real_Estate = readRDS("Real Estate.rds")           
# Utilities = readRDS("Utilities.rds") 
# Other = readRDS("Other.rds")

sector_name1 =  data_sectors$Industry[data_sectors$Sector==sector_names_list[1]]
sector_name2 =  data_sectors$Industry[data_sectors$Sector==sector_names_list[2]]
sector_name3 =  data_sectors$Industry[data_sectors$Sector==sector_names_list[3]]
sector_name4 =  data_sectors$Industry[data_sectors$Sector==sector_names_list[4]]
sector_name5 =  data_sectors$Industry[data_sectors$Sector==sector_names_list[5]]
sector_name6 =  data_sectors$Industry[data_sectors$Sector==sector_names_list[6]]
sector_name7 =  data_sectors$Industry[data_sectors$Sector==sector_names_list[7]]
sector_name8 =  data_sectors$Industry[data_sectors$Sector==sector_names_list[8]]
sector_name9 =  data_sectors$Industry[data_sectors$Sector==sector_names_list[9]]
sector_name10 =  data_sectors$Industry[data_sectors$Sector==sector_names_list[10]]
sector_name11 =  data_sectors$Industry[data_sectors$Sector==sector_names_list[11]]

# ------------- Load 2018 data --------------
#data<-read.csv("testing_data.csv",header = TRUE)
#data<-read.csv("data_sec.csv",header = TRUE,stringsAsFactors=FALSE)
# 
data<-read.csv('mf850-finalproject-data.csv',header = TRUE)
# data= data[4000:5000,]

# data<-read.csv('test_data.csv',header = TRUE)
# data= data[6000:6020,]

# i=4;
# sector_name =  data_sectors$Industry[data_sectors$Sector==sector_names_list[i]]
# data<- data[data$Industry %in% sector_name,]



#------------------------------ Process data----------------------------------
unlist<-c("Date","compid","Industry","RETMONTH")
names_list <- which(!(names(data) %in% unlist))
r_month <- data$RETMONTH

r_month_spx <- data$retmonth_spx
industry_name <- data$Industry
my_data <- data[,names_list]
 
neutralize<-function(list){
  return ((list - mean(list))/(max(list)-min(list)))
}
my_data<-apply(my_data,2,neutralize)
my_data[is.na(my_data)]<-0

length = dim(my_data)[1]

#----------------------make predictions------------------------------------------
my_predictions <- c()
for (i in (1:length)){
  for (j in (1:11)){
    if (industry_name[i] %in% get(paste("sector_name",j,sep = ''))){
      # if this company's industry belongs to a sector, choose the corresponding model
      my_model = get(paste("model",j,sep="_"))
      break
      
    }else if(j==11 &(!industry_name[i] %in% sector_name11)){
      # if the industry does not belong to any existed sector, we are using a general model here
      my_model = model_other
      break
    }
  }
  
  if (i != length){
    res <- predict(my_model,data = my_data[i:(i+1),])
    my_predictions = c(my_predictions,res$predictions[1])
  }else {
    res <- predict(my_model,data = my_data[(i-1):i,])
    my_predictions = c(my_predictions,res$predictions[2])
  }
}

#######---my_predications is our predictions for monthly return-------



#----------------Calculate R2 and MSE-------------------------------

# SSE <- sum((my_predictions-r_month)^2)
# SSTO <- sum((r_month-mean(r_month))^2)
# print(paste("Test R2: ",1-SSE/SSTO))
# 
# print(sum(abs(my_predictions-r_month))/length(r_month))
# mse = sum((my_predictions-r_month)^2)/length(r_month)
# print(paste("MSE is:",mse))




