## Aggregation ##



#===========================AGGREGATION==========================

function.MyAggregation<- function(data1,aggregater) {
  #message("Aggregation")
  #browser()
  df_XCV <- data1
  abc <- df_XCV$Date_Time
  df_XCV <- select_if(df_XCV, is.numeric)
  df_XCV$Date_Time <- abc
  df_XCV <- df_XCV[, c(ncol(df_XCV), 1:(ncol(df_XCV) - 1))]
  
  
  mytimezone<-Sys.getenv("TZ")
  if(mytimezone==""){
    Sys.setenv(TZ="GMT")
  }
  #Create time series object
  df_XCV_xts <-xts(df_XCV[,-1], order.by = as.POSIXct(df_XCV$Date_Time, tzone = Sys.getenv("TZ")))
  
  #ep <- endpoints(df_XCV_xts, on = "hours")
  ep <- endpoints(df_XCV_xts, on = paste0(aggregater))
  pac <-period.apply(df_XCV_xts[, (names(df_XCV_xts))], INDEX = ep, FUN = mean)
  #Time series to DF
  pac <- fortify(pac)
  pac <-  select(pac,-c(PEAKINT, INTERVAL))
  pac <- tk_augment_timeseries_signature(pac)
  #source("Aggregation_Date.R")
  #source("Aggregation_Date_Time.R")
  dipu.pac_data<<-pac
  pac
   
}