## Aggregation ##



#===========================AGGREGATION==========================

function.MyAggregation<- function(data1, channel, filename,aggregater) {
  
  filename<-paste0(filename,"_MEJA")
  heading="PP"
  if(channel=="CHANNEL3" || channel=="CHANNEL4"){
    heading="LM"
  }
  #filter <- filter(data1, CHANNEL == "PP", FILENAME == "SC1_MEJA")
  filter <- filter(data1, CHANNEL == heading, FILENAME == filename)
  df_XCV <- data1
  abc <- df_XCV$Date_Time
  df_XCV <- select_if(df_XCV, is.numeric)
  df_XCV$Date_Time <- abc
  df_XCV <- df_XCV[, c(ncol(df_XCV), 1:(ncol(df_XCV) - 1))]
  #Create time series object
  df_XCV_xts <-
    xts(df_XCV[,-1], order.by = as.POSIXct(df_XCV$Date_Time, tzone = Sys.getenv("TZ")))
  
  #ep <- endpoints(df_XCV_xts, on = "hours")
  ep <- endpoints(df_XCV_xts, on = aggregater)
  pac <-
    period.apply(df_XCV_xts[, (names(df_XCV_xts))], INDEX = ep, FUN = mean)
  #Time series to DF
  pac <- fortify(pac)
  pac <-  select(pac,-c(PEAKINT, INTERVAL))
  pac <- tk_augment_timeseries_signature(pac)
  #source("Aggregation_Date.R")
  #source("Aggregation_Date_Time.R")
   pac
}