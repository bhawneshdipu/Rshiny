library(readr)
library(ggplot2)
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
library(randomForest)
#install.packages("devtools")
#devtools::install_github("twitter/AnomalyDetection")
library(AnomalyDetection)
packageVersion('plotly')

browser()
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
data1$Date_Time <- as.Date(data1$Date_Time, "%d%m%y%H%M")
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
# replace.if
# 
# for (i in vector) {
# data1$CHANNEL<-gsub("1", splitH[1,1], data1$CHANNEL)
# data1$CHANNEL<-gsub("2", splitH[1,2], data1$CHANNEL)
# data1$CHANNEL<-gsub("3", splitH[1,3], data1$CHANNEL)
# data1$CHANNEL<-gsub("4", splitH[1,4], data1$CHANNEL)
# }


#data1$CHANNEL <- do.call(paste,c(data1[c("CHANNEL1","CHANNEL")],sep = "_"))
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

x <- ggplot(data1,aes(x = Date, y = SUM,col = CHANNEL))+
  geom_bar(stat = "identity",size = 3,alpha = 0.5,fill = "blue")

x<-ggplot(data1,aes(x = Date, y = SUM,col = CHANNEL))+
  geom_line(size = 0.5,alpha = 0.6)

p<-SUM_DATA %>%
  ggplot(aes(x = Index, y = SUM)) 
plot_ly(SUM_DATA, x = ~Index, y = ~SUM)%>%
  add_lines(alpha = 0.6)

ggplotly(x)  

ggplot(SUM_DATA,aes(x=Index,y=SUM))+
  # Training data
  geom_line(color = palette_light()[[1]]) +
  geom_point(color = palette_light()[[1]])+
  # Predictions
  geom_line(aes(y = value), color = palette_light()[[2]], data = predictions_tbl) +
  geom_point(aes(y = value), color = palette_light()[[2]], data = predictions_tbl)+ 
  # Actuals
  geom_line(color = palette_light()[[1]], data = actuals_tbl) +
  geom_point(color = palette_light()[[1]], data = actuals_tbl)+
  theme_tq() +
  labs(title = "Time series sum data")
#Plot of aggregation by SpeedBins
plot_ly(pac, x = ~Index, y = ~SP1 , name = 'Number of cars in time aggregated')%>%
  add_trace(y = ~SP1, name = '0-50km/h', mode = 'lines') %>%
  add_trace(y = ~SP2, name = '50-60km/h', mode = 'lines') %>%
  add_trace(y = ~SP3, name = '60-70km/h', mode = 'lines') %>%
  add_trace(y = ~SP4, name = '70-80km/h', mode = 'lines') %>%
  add_trace(y = ~SP5, name = '80-90km/h', mode = 'lines') %>%
  add_trace(y = ~SP6, name = '90-100km/h', mode = 'lines') %>%
  add_trace(y = ~SP7, name = '100-110km/h', mode = 'lines') %>%
  add_trace(y = ~SP8, name = '110-120km/h', mode = 'lines') %>%
  add_trace(y = ~SP9, name = '120-130km/h', mode = 'lines') %>%
  add_trace(y = ~SP10, name = '130-140km/h', mode = 'lines') %>%
  add_trace(y = ~SP11, name = '140-150km/h', mode = 'lines') %>%
  add_trace(y = ~SP12, name = '150-160km/h', mode = 'lines') %>%
  add_trace(y = ~SP13, name = '160-180km/h', mode = 'lines')%>%
  add_trace(y = ~SP14, name = '180-999km/h', mode = 'lines')%>%
  add_trace(y = ~SUM, name = 'TOTAL', mode = 'lines')

#Plot of aggregation by LengthBins
plot_ly(pac, x = ~Index, y = ~LN1 , name = 'Number of cars in time aggregated')%>%
  add_trace(y = ~LN1, name = '0-300 cm', mode = 'lines') %>%
  #add_trace(y = ~LN2, name = '300-470 cm', mode = 'lines') %>%
  add_trace(y = ~LN3, name = '470-550 cm', mode = 'lines') %>%
  add_trace(y = ~LN4, name = '550-600 cm', mode = 'lines') %>%
  add_trace(y = ~LN5, name = '600-1300 cm', mode = 'lines') %>%
  add_trace(y = ~LN6, name = '1300-1800 cm', mode = 'lines') %>%
  add_trace(y = ~LN7, name = '1800-2550 cm', mode = 'lines') %>%
  add_trace(y = ~LN8, name = '2550-3600 cm', mode = 'lines') %>%
  add_trace(y = ~LN9, name = '3600-9999 cm', mode = 'lines') 


#Plot of aggregation by WeigthBins
plot_ly(agg_CS, x = ~Date, y = ~CS1 , name = 'Number of cars in time aggregated')%>%
  add_trace(y = ~CS1, name = '0-2000 kg', mode = 'lines') %>%
  #add_trace(y = ~CS2, name = '2000-3501 kg', mode = 'lines') %>%
  add_trace(y = ~CS3, name = '3501-7497 kg', mode = 'lines') %>%
  add_trace(y = ~CS4, name = '7497-12002 kg', mode = 'lines') %>%
  add_trace(y = ~CS5, name = '12002-17998  kg', mode = 'lines') %>%
  add_trace(y = ~CS6, name = '17998-23999  kg', mode = 'lines') 
  

#Bar plot day of week
plot_ly(pac, x = ~wday.lbl, y = ~SP1, type = 'bar', name = "0-50km/h") %>%
  add_trace(y = ~SP2, name = '50-60km/h', mode = 'lines') %>%
  add_trace(y = ~SP3, name = '60-70km/h', mode = 'lines') %>%
  add_trace(y = ~SP4, name = '70-80km/h', mode = 'lines') %>%
  add_trace(y = ~SP5, name = '80-90km/h', mode = 'lines') %>%
  add_trace(y = ~SP6, name = '90-100km/h', mode = 'lines') %>%
  add_trace(y = ~SP7, name = '100-110km/h', mode = 'lines') %>%
  add_trace(y = ~SP8, name = '110-120km/h', mode = 'lines') %>%
  add_trace(y = ~SP9, name = '120-130km/h', mode = 'lines') %>%
  add_trace(y = ~SP10, name = '130-140km/h', mode = 'lines') %>%
  add_trace(y = ~SP11, name = '140-150km/h', mode = 'lines') %>%
  add_trace(y = ~SP12, name = '150-160km/h', mode = 'lines') %>%
  add_trace(y = ~SP13, name = '160-180km/h', mode = 'lines')%>%
  add_trace(y = ~SP14, name = '180-999km/h', mode = 'lines')%>%
  #add_trace(y = ~SUM, name = 'TOTAL', mode = 'lines')%>%
  layout(title = "Počty áuto podľa dni v týždni",yaxis = list(title = 'Count'), barmode = 'group')


#pie plot by day of week
  plot_ly(pac, labels = ~wday.lbl, values = ~SP10, type = 'pie',textposition = 'inside') %>%
  layout(title = 'Kol??ov? graf')
  
#boxplot
 plot_ly(pac,y = ~SP1, name = '0-50km/h',type = 'box')%>%
   add_trace(y = ~SP2, name = '50-60km/h', mode = 'lines') %>%
   add_trace(y = ~SP3, name = '60-70km/h', mode = 'lines') %>%
   add_trace(y = ~SP4, name = '70-80km/h', mode = 'lines') %>%
   add_trace(y = ~SP5, name = '80-90km/h', mode = 'lines') %>%
   add_trace(y = ~SP6, name = '90-100km/h', mode = 'lines') %>%
   add_trace(y = ~SP7, name = '100-110km/h', mode = 'lines') %>%
   add_trace(y = ~SP8, name = '110-120km/h', mode = 'lines') %>%
   add_trace(y = ~SP9, name = '120-130km/h', mode = 'lines') %>%
   add_trace(y = ~SP10, name = '130-140km/h', mode = 'lines') %>%
   add_trace(y = ~SP11, name = '140-150km/h', mode = 'lines') %>%
   add_trace(y = ~SP12, name = '150-160km/h', mode = 'lines') %>%
   add_trace(y = ~SP13, name = '160-180km/h', mode = 'lines')%>%
   add_trace(y = ~SP14, name = '180-999km/h', mode = 'lines')%>%
    layout(title = "Boxplot by day of week")
 
 plot_ly(data1,x = ~CHANNEL,y = ~SP11, color = ~wday.lbl,type = "box")%>%
   layout(boxmode = "group")

 #==================================Anomaly detection=======================================
View(pac)
 View(pac[,c(1,12)])
  SUM_DATA <- pac[,c(1,12)]
 abc <- as.numeric(rownames(SUM_DATA))
 ggplot(pac, aes(x=Index, y=SP11)) + geom_line()
 
 res = AnomalyDetectionTs(SUM_DATA, max_anoms=0.01, direction="pos", plot=TRUE, e_value = T)
 
 res = AnomalyDetectionVec(SUM_DATA[,2], max_anoms=0.01, period=2, direction='both',
                           only_last=FALSE, plot=TRUE)

 anomaly_table=res$anoms
 
 SUM_DATA$Index_row <- abc
 anomaly_table<-merge(SUM_DATA,anomaly_table,by.x = "Index_row",by.y = "index")
 
 
 
 
 plot_ly(SUM_DATA, x = ~Index, y = ~SP11)%>%
   add_trace(colors = "orange",name = "Po?etnos? ?ut v ?ase",mode = "lines")%>%
   add_trace(y = ~anoms, colors = "gray",name = "Anom?lie", mode = "markers", alpha = 1,data = anomaly_table)%>%
   layout(title = "Graf",
          xaxis = list(title = "Čas",
                       rangeslider = list(type = "date")),
          yaxis = list(title = "Početnosť áut"))
 #==================================Motif discovery=======================================
 

 res.wcc <- Func.motif(ts = pac$SP11,global.norm = F,local.norm = T,window.size = 10,overlap = 0,w = 6,a = 5,mask.size = 5,max.dist.ratio = 1.2,count.ratio.1 = 1.1,count.ratio.2 = 1.1)
 
 res.ahu <- Func.motif(ts = pac$SP12, global.norm = T, local.norm = F, window.size = 24, overlap = 0, w = 6, a = 5, mask.size = 5, max.dist.ratio = 1.2, count.ratio.1 = 1.1, count.ratio.2 = 1.1)
 
 
 #Visualization
 data.wcc <- Func.visual.SingleMotif(single.ts = pac$SP11, window.size = 24, motif.indices = res.wcc$Indices)
 data.ahu <- Func.visual.SingleMotif(single.ts = pac$SP12, window.size = 24, motif.indices = res.ahu$Indices)
 
 #Determine the total number of motifs discovered in the time series of WCC
 n <- length(unique(data.wcc$data.1$Y))
 #Make the plot
 p<-ggplot(data = data.wcc$data.1) +  
   geom_line(aes(x = 1:dim(data.wcc$data.1)[1], y = X)) +
   geom_point(aes(x = 1:dim(data.wcc$data.1)[1], y = X, color=Y, shape=Y))+
   scale_shape_manual(values = seq(from = 1, to = n)) +
   guides(shape=guide_legend(nrow = 2)) +
   xlab("Time (15-min)") + ylab("Počet áut") +
   theme(panel.background=element_rect(fill = "white", colour = "black"),
         legend.position="top",
         legend.title=element_blank())
 
 ggplotly(p)
#==================================PREDICTION=======================================
count_ts = ts(pac[, c('SUM')])
pac$clean_cnt = tsclean(count_ts)

pac$cnt_ma = ma(pac$SUM, order=7) # using the clean count with no outliers
pac$cnt_ma30 = ma(pac$SUM, order=30)


 l<-ggplot() +
  geom_line(data = pac, aes(x = Index, y = clean_cnt, colour = "Counts")) +
  geom_line(data = pac, aes(x = Index, y = cnt_ma,   colour = "Weekly Moving Average"))  +
  geom_line(data = pac, aes(x = Index, y = cnt_ma30, colour = "Monthly Moving Average"))  +
  ylab('Cars Count')
 
 ggplotly(l)

 
 
 
 
 count_ma = ts(na.omit(pac$SUM), frequency=3)
 decomp = stl(count_ma, s.window="periodic")
 deseasonal_cnt <- seasadj(decomp)
 k<-plot(decomp)

 adf.test(count_ma, alternative = "stationary")

 Acf(count_ma, main='') 

 Pacf(count_ma, main='')  
 
 
 count_d1 = diff(deseasonal_cnt, differences = 1)
 plot(count_d1)
 adf.test(count_d1, alternative = "stationary")

 
 Acf(count_d1, main='ACF for Differenced Series')
 Pacf(count_d1, main='PACF for Differenced Series') 
 
 
 auto.arima(deseasonal_cnt, seasonal=FALSE)

 fit<-auto.arima(deseasonal_cnt, seasonal=FALSE)
 tsdisplay(residuals(fit), lag.max=45, main='(1,1,1) Model Residuals') 

 fit2 = arima(deseasonal_cnt, order=c(1,1,7))
 
 fit2
 
 tsdisplay(residuals(fit2), lag.max=15, main='Seasonal Model Residuals') 

 fcast <- forecast(fit2, h=30)
 plot(fcast) 

 
 hold <- window(ts(deseasonal_cnt), start=700)
 
 fit_no_holdout = arima(ts(deseasonal_cnt[-c(700:725)]), order=c(1,1,7))
 
 fcast_no_holdout <- forecast(fit_no_holdout,h=25)
 plot(fcast_no_holdout, main=" ")
 lines(ts(deseasonal_cnt)) 

 
 fit_w_seasonality = auto.arima(deseasonal_cnt, seasonal=TRUE)
 fit_w_seasonality 
 seas_fcast <- forecast(fit_w_seasonality, h=30)
 plot(seas_fcast) 
 #=========================ARMA========================
 adf.test(diff(log(pac$SUM)), alternative="stationary", k=0)
 acf(log(pac$SUM))
 acf(diff(log(pac$SUM)))
 pacf(diff(log(pac$SUM)))
 (fit <- arima(log(pac$SUM), c(0, 1, 1),seasonal = list(order = c(0, 1, 1), period = 12)))
 pred <- predict(fit, n.ahead = 10*12)
 ts.plot(as.ts(pac$Index),2.718^pred$pred, log = "y", lty = c(1,3))
 #=========================MACHINE LEARNING========================
 
 # Shuffle row indices: rows
 rows <- sample(nrow(data1))
 
 # Randomly order data: Sonar
 data1 <- data1[rows,]
 
 # Identify row to split on: split
 split <- round(nrow(data1) * .60)
 
 # Create train
 train <- data1[1:split,]
 
 # Create test
 test <- data1[(split + 1):nrow(data1),]
#------------------------------------------
 
 # Run algorithms using 10-fold cross validation
 control <- trainControl(method="cv", number=10)
 #metric = ifelse(is.factor(y), "Accuracy", "RMSE")
 metric <- "Accuracy"
 validation_index <- createDataPartition(data1$CHANNEL, p=0.80, list=FALSE)
 # select 20% of the data for validation
 validation <- data1[-validation_index,]
 # use the remaining 80% of data to training and testing the models
 dataset <- data1[validation_index,]
 # a) linear algorithms
 set.seed(7)
 #fit.lda <- train(SP10 ~ Date_Time,data=data1, method="lda", metric=metric, trControl=control)
 fit.lda <- train(SP10 ~ Date_Time,data=data1, method="lda", metric=metric, trControl=control)
 fit.lda<-lda(SP10 ~ Date_Time,data=data1,metric=metric, trControl=control)
 
 # b) nonlinear algorithms
 # CART
 set.seed(7)
 fit.cart <- train(SP10 ~ Date_Time,data=data1, method="rpart", metric=metric, trControl=control)
 # kNN
 set.seed(7)
 fit.knn <- train(SP10 ~ Date_Time,data=data1, method="knn", metric=metric, trControl=control)
 # c) advanced algorithms
 # SVM
 set.seed(7)
 fit.svm <- train(SP10 ~ Date_Time,data=data1, method="svmRadial", metric=metric, trControl=control)
 # Random Forest
 set.seed(7)
 fit.rf <- train(SP10 ~ Date_Time,data=data1, method="rf", metric=metric, trControl=control)
 
 
 #summarize accuracy of models
 results <- resamples(list(lda=fit.lda, cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf))
 summary(results)
 dotplot(results)
 print(fit.lda)
 
 
 # estimate skill of LDA on the validation dataset
 predictions <- predict(fit.lda, validation)
 confusionMatrix(predictions, validation$CHANNEL)
 