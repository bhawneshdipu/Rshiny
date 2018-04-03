## MyPlots.R ##


function.MyPlotSpeedBins<-function(pac){
  renderPlotly({
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
    
  })
}

function.MyPlotLengthBins<-function(pac){
  renderPlotly({
    #Plot of aggregation by LengthBins
    plot_ly(pac, x = ~Index, y = ~LN1 , name = 'Number of cars in time aggregated')%>%
      add_trace(y = ~LN1, name = '0-300 cm', mode = 'lines') %>%
      add_trace(y = ~LN2, name = '300-470 cm', mode = 'lines') %>%
      add_trace(y = ~LN3, name = '470-550 cm', mode = 'lines') %>%
      add_trace(y = ~LN4, name = '550-600 cm', mode = 'lines') %>%
      add_trace(y = ~LN5, name = '600-1300 cm', mode = 'lines') %>%
      add_trace(y = ~LN6, name = '1300-1800 cm', mode = 'lines') %>%
      add_trace(y = ~LN7, name = '1800-2550 cm', mode = 'lines') %>%
      add_trace(y = ~LN8, name = '2550-3600 cm', mode = 'lines') %>%
      add_trace(y = ~LN9, name = '3600-9999 cm', mode = 'lines') 
  })
  
}

function.MyPlotWeightBins<-function(pac){
  renderPlotly({
    #Plot of aggregation by WeigthBins
    plot_ly(data1, x = ~Date, y = ~CS1 , name = 'Number of cars in time aggregated')%>%
      add_trace(y = ~CS1, name = '0-2000 kg', mode = 'lines') %>%
      add_trace(y = ~CS2, name = '2000-3501 kg', mode = 'lines') %>%
      add_trace(y = ~CS3, name = '3501-7497 kg', mode = 'lines') %>%
      add_trace(y = ~CS4, name = '7497-12002 kg', mode = 'lines') %>%
      add_trace(y = ~CS5, name = '12002-17998  kg', mode = 'lines') %>%
      add_trace(y = ~CS6, name = '17998-23999  kg', mode = 'lines') 
  })
}

function.MyBoxPlot1<-function(pac){
  renderPlotly({
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
})
  
}
function.MyBoxPlot2<-function(pac,x,y,color){
  renderPlotly({ 
    
   plot_ly(pac,x = ~get(x),y = ~get(y), color = ~get(color),type = "box")%>%
      layout(boxmode = "group")
    # plot_ly(pac,x = ~SUM,y = ~SP11, color = pac['wday.lbl'],type = "box")%>%
    #      layout(boxmode = "group")
  })
  
}

function.MyAnomalyDetection<-function(pac,myX,myY,Myperiod,MylastOnly){
  #==================================Anomaly detection=======================================
  
  View(c(myX,myY,Myperiod,MylastOnly,0))
  #SUM_DATA <- pac[,c(1,12)]
  
  myX<-paste0(myX)
  myY<-paste0(myY)
  Myperiod<-2
  MylastOnly<-FALSE
  SUM_DATA <- pac[,c(grep(paste0(myX),colnames(pac)),grep(paste0(myY), colnames(pac)))]
  #View(SUM_DATA)
  View(c(myX,myY,Myperiod,MylastOnly,1))
  abc <- as.numeric(rownames(SUM_DATA))
  ggplot(pac, aes(x=myX, y=myY)) + geom_line()
  
  #ggplot(pac, aes(x=Index, y=SP11)) + geom_line()
  #res = AnomalyDetectionTs(SUM_DATA, max_anoms=0.01, direction="pos", plot=TRUE, e_value = T)
  #res = AnomalyDetectionVec(SUM_DATA[,2], max_anoms=0.01, period=96, direction='both',
  #                          only_last=FALSE, plot=TRUE)
  #View(SUM_DATA)
  
  res = AnomalyDetectionVec(SUM_DATA[,2], max_anoms=0.01, period=Myperiod, direction='both',
                            only_last=MylastOnly, plot=TRUE)
  anomaly_table=res$anoms
  
  #SUM_DATA$Index_row <- abc
  
  SUM_DATA[[paste0(myX,"_row")]] <- abc
  
  #anomaly_table<-merge(SUM_DATA,anomaly_table,by.x = "Index_row",by.y = "index")
  View(anomaly_table)
  View(SUM_DATA)
  View(c(dim(anomaly_table)))
  if(is.null(anomaly_table)){
    anomaly_table<-anomaly_table
  }else{
    anomaly_table<-
      
                    tryCatch({
                                merge(SUM_DATA,anomaly_table,by.x = paste0(myX,"_row"),by.y = "index")
                                
                                },error=function(cond){
                                  message("Error in anomaly_table")
                                  anomaly_table
                                  
                                },warning=function(cond){
                                  message("Warning in anomaly_table")
                                  anomaly_table
                                },finally={
                                  message("Warning of anomaly_table")
                              }
                    )
  }
  
  renderPlotly({
  plot_ly(SUM_DATA, x = ~get(myX), y = ~get(myY))%>%
    add_trace(colors = "orange",name = "Po?etnos? ?ut v ?ase",mode = "lines")%>%
    add_trace(y = ~anoms, colors = "gray",name = "Anom?lie", mode = "markers", alpha = 1,data = anomaly_table)%>%
    layout(title = "Graf",
           xaxis = list(title = "Čas",
                        rangeslider = list(type = "date")),
           yaxis = list(title = "Početnosť áut"))
  })
}