library(readr)
library(plotly)
library(stringr)
library(DataLoader)
library(forecast)
library(tseries)
library(zoo)
library(xts)
library(fts)
library(MASS)
library(caret)
library(e1071)
library(dplyr)
#library(h2o)        # Awesome ML Library
library(timetk)     # Toolkit for working with time series in R
library(tidyquant)

library(anomalyDetection)
library(TSMining)
#install.packages("devtools")

devtools::install_github("twitter/AnomalyDetection")
library(anomalyDetection)
packageVersion('plotly')
#devtools::install_github("tidyverse/ggplot2")
#devtools::install_github('hadley/ggplot2')
library(ggplot2)
# path to folder that holds multiple .csv files
#folder <- "C:/Users/USER/Documents/DP/Dataframe_Script/same_date/"      
folder <- "./"
file_list <- list.files(path=folder, pattern="*.csv") # create list of all .csv files in folder
# read in each .csv file in file_list and rbind them into a data frame called data1 
data1 <- 
  do.call("rbind", 
          lapply(file_list, 
                 function(x) 
                   read_csv(paste(folder, x, sep=''))))


#====================Preprocesing===============================
#chnge format of Date
data1$Date_Time <- data1$Date
data1$Date_Time <- do.call(paste,c(data1[c("Date_Time","Time")],sep = ""))
data1$Date_Time <- as.POSIXct(data1$Date_Time,format = "%d%m%y%H%M")
#data1$Date_Time <- as.Date(data1$Date_Time, "%d%m%y%H%M")
#data1$Date <- as.POSIXct(data1$Date,format = "%d%m%y")
data1<-data1[,c(ncol(data1),1:(ncol(data1)-1))]



#Add ":" in time
data1$Time <- sub("(.{2})(.*)", "\\1:\\2", data1$Time)


#Necessarily Columns as.factor
data1$CHANNEL <- as.numeric(data1$CHANNEL)
data1$LOCATION <- as.factor(data1$LOCATION)
data1$SITE <- as.factor(data1$SITE)
data1$FILENAME <- as.factor(data1$FILENAME)
data1$INSTRUMENT <- as.factor(data1$INSTRUMENT)
data1$HEADINGS <- as.factor(data1$HEADINGS)
#data1$Time <- as.numeric(data1$Time)
#set measured data as numeric
data1$SP1 = as.numeric(data1$SP1)
data1$SP2 = as.numeric(data1$SP2) 
data1$SP3 = as.numeric(data1$SP3) 
data1$SP4 = as.numeric(data1$SP4) 
data1$SP5 = as.numeric(data1$SP5) 
data1$SP6 = as.numeric(data1$SP6) 
data1$SP7 = as.numeric(data1$SP7)
data1$SP8 = as.numeric(data1$SP8) 
data1$SP9 = as.numeric(data1$SP9) 
data1$SP10 = as.numeric(data1$SP10) 
data1$SP11 = as.numeric(data1$SP11)
data1$SP12 = as.numeric(data1$SP12) 
data1$SP13 = as.numeric(data1$SP13) 
data1$SP14 = as.numeric(data1$SP14) 

data1$LN1 = as.numeric(data1$LN1)
data1$LN2 = as.numeric(data1$LN2)
data1$LN3 = as.numeric(data1$LN3)
data1$LN4 = as.numeric(data1$LN4)
data1$LN5 = as.numeric(data1$LN5)
data1$LN6 = as.numeric(data1$LN6)
data1$LN7 = as.numeric(data1$LN7)
data1$LN8 = as.numeric(data1$LN8)
data1$LN9 = as.numeric(data1$LN9)

data1$CS1 = as.numeric(data1$CS1)
data1$CS2 = as.numeric(data1$CS2)
data1$CS3 = as.numeric(data1$CS3)
data1$CS4 = as.numeric(data1$CS4)
data1$CS5 = as.numeric(data1$CS5)
data1$CS6 = as.numeric(data1$CS6)


#add column sum of cars in interval
data1$SUM<-rowSums(data1[,5:18])

#unfinished preparation I will explain what I need
splitH<-str_split_fixed(data1$HEADINGS, " ", 4)
splitH <- as.data.frame(splitH)
splitH <- unique(splitH)
data1$CHANNEL1 <- data1$CHANNEL
colnames(splitH) <- "CHANNEL1"
colnames(splitH)[2] <- "CHANNEL2"
colnames(splitH)[3] <- "CHANNEL3"
colnames(splitH)[4] <- "CHANNEL4"
splitH$rowNames <-row.names.data.frame(splitH)
#replace.if

# for (i in vector) {
#   data1$CHANNEL<-gsub("1", splitH[1,1], data1$CHANNEL)
#   data1$CHANNEL<-gsub("2", splitH[1,2], data1$CHANNEL)
#   data1$CHANNEL<-gsub("3", splitH[1,3], data1$CHANNEL)
#   data1$CHANNEL<-gsub("4", splitH[1,4], data1$CHANNEL)
# }


data1$CHANNEL <- do.call(paste,c(data1[c("CHANNEL1","CHANNEL")],sep = "_"))
data1$CHANNEL = as.factor(data1$CHANNEL)
data1$Time = as.factor(data1$Time)
data1$CHANNEL1 <- NULL


#===========================AGGREGATION==========================

filter<-filter(data1, CHANNEL == "PP",FILENAME == "SC1_MEJA")

df_XCV<-data1
abc <- df_XCV$Date_Time
df_XCV<-select_if(df_XCV, is.numeric)
df_XCV$Date_Time<- abc
df_XCV<-df_XCV[,c(ncol(df_XCV),1:(ncol(df_XCV)-1))]
#Create time series object
df_XCV_xts<-xts(df_XCV[, -1], order.by=as.POSIXct(df_XCV$Date_Time,tzone = Sys.getenv("TZ")))

ep <- endpoints(df_XCV_xts, on = "hours")
pac<-period.apply(df_XCV_xts[,(names(df_XCV_xts)) ], INDEX = ep, FUN = mean)
#Time series to DF
pac <-fortify(pac)
pac <-  select(pac, -c(PEAKINT, INTERVAL))
pac<-tk_augment_timeseries_signature(pac)
#source("Aggregation_Date.R")
#source("Aggregation_Date_Time.R")
#===========================PLOTS==========================
