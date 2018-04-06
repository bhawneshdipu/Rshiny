## Preprocessing ##


#====================Preprocesing===============================

function.MyPreprocessing<-function(data1) {
  #browser()
  
  #chnge format of Date
  data1$Date_Time <- data1$Date
  data1$Date_Time <-do.call(paste, c(data1[c("Date_Time", "Time")], sep = ""))
  data1$Date_Time <- as.POSIXct(data1$Date_Time, format = "%d%m%y%H%M")
  #data1$Date_Time <- as.Date(data1$Date_Time, "%d%m%y%H%M")
  data1$Date <- as.POSIXct(data1$Date,format = "%d%m%y")
  data1$Date <- as.Date(data1$Date,format = "%d%m%y")
  
  data1 <- data1[, c(ncol(data1), 1:(ncol(data1) - 1))]
  
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
  data1$SUM <- rowSums(data1[, 5:18])
  
  
   
  dipu.pre_data<<-data1
  #browser()
  data1
  
}

function.MyFilter<-function(data1,filename,daterange,channel){
  #browser()
  # filter by FILENAME
  if (filename == "all") {
    data1<-data1
  } else{
    data1 <- data1 %>% filter(FILENAME == paste0(filename, "_MEJA"))
  }
  
  #=====================DATE PREPROCESSING=============================
  # data1$Date_Time <- data1$Date
  # data1$Date_Time <- do.call(paste,c(data1[c("Date_Time","Time")],sep = ""))
  # data1$Date_Time <- as.POSIXct(data1$Date_Time,format = "%d%m%y%H%M")
  # data1$Date_Time <- as.Date(data1$Date_Time, "%d%m%y%H%M")
  # data1$Date <- as.POSIXct(data1$Date,format = "%d%m%y")
  # data1<-data1[,c(ncol(data1),1:(ncol(data1)-1))]
  
  #browser()
  #View(daterange)
  message(daterange)
  tryCatch({
    #filter by date
    fromdate<-as.Date(daterange[1],"%Y-%m-%d")
    todate<-as.Date(daterange[2],"%Y-%m-%d")
    message(todate)
    message(fromdate)
    data1<-data1[data1$Date>=fromdate & data1$Date<=todate,]
    head(data1)
  },error=function(cond){
    message("Date Range is not defined til now:")
  })
  
  #browser()
  #filter by channel
  if (channel == "all") {
    data1<-data1
  }else if(channel == "12"){
    data1 <- data1  %>% filter(CHANNEL %in%  c("1","2"))
  }else if(channel == "34"){
    data1 <- data1 %>% filter(CHANNEL %in%  c("3","4"))
  }else if(channel == "1"){
    data1 <- data1 %>% filter(CHANNEL == "1")
  }else if(channel == "2"){
    data1 <- data1 %>% filter(CHANNEL=="2")
  }else if(channel == "3"){
    data1 <- data1 %>% filter(CHANNEL == "3")
  }else if(channel == "4"){
    data1 <- data1 %>% filter(CHANNEL == "4")
  }else{
    data1<-data1
  }
  #browser()
  #unfinished preparation I will explain what I need
  splitH <- str_split_fixed(data1$HEADINGS, " ", 4)
  splitH <- as.data.frame(splitH)
  #splitH <- unique(splitH)
  data1$CHANNEL<-gsub("1", paste0("1_",splitH[1,1]), data1$CHANNEL)
  data1$CHANNEL<-gsub("2", paste0("2_",splitH[2,1]), data1$CHANNEL)
  data1$CHANNEL<-gsub("3", paste0("3_",splitH[3,1]), data1$CHANNEL)
  data1$CHANNEL<-gsub("4", paste0("4_",splitH[4,1]), data1$CHANNEL)
  
  data1$CHANNEL = as.factor(data1$CHANNEL)
  data1$Time = as.factor(data1$Time)
  dipu.fil_data<<-data1
  #browser()
  head(data1)
  data1
}
