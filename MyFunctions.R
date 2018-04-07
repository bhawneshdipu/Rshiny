## MyFunctions ##

source("MyPlots.R")
dipu.dashboard<-function(input,output,session,data1){
  #browser()
  dipu.fil_data<<-function.MyFilter(dipu.pre_data,input$dashboardselectfile,input$dashboardselectdaterange,input$dashboardselectchannel)
  dipu.pac_data<<-function.MyAggregation(dipu.fil_data,input$dashboardselectdataaggregation)
  pac<-dipu.pac_data
  
  hide("plot11")
  hide("plot12")
  hide("plot13")
  speed      <- "speed"       %in% input$dashboardselectbins
  length      <- "length"       %in% input$dashboardselectbins
  weight      <- "weight"       %in% input$dashboardselectbins
  
  if(speed & length & weight){
    output$plot11 <- function.MyPlotSpeedBins(pac)
    show("plot11")
    output$plot12 <- function.MyPlotLengthBins(pac)
    show("plot12")
    output$plot13 <- function.MyPlotWeightBins(dipu.fil_data)
    show("plot13")
  }else if(speed & length){
    output$plot11 <- function.MyPlotSpeedBins(pac)
    show("plot11")
    output$plot12 <- function.MyPlotLengthBins(pac)
    show("plot12")
    
  }else if(length & weight){
    
    output$plot12 <- function.MyPlotLengthBins(pac)
    show("plot12")
    output$plot13 <- function.MyPlotWeightBins(dipu.fil_data)
    show("plot13")
    
  }else if(speed & weight){
    output$plot11 <- function.MyPlotSpeedBins(pac)
    show("plot11")
    
    output$plot13 <- function.MyPlotWeightBins(dipu.fil_data)
    show("plot13")
    
  }else if(length){
    output$plot12 <- function.MyPlotLengthBins(pac)
    show("plot12")
    
  }else if(speed){
    output$plot11 <- function.MyPlotSpeedBins(pac)
    show("plot11")
    
  }else if(weight){
    output$plot13 <- function.MyPlotWeightBins(dipu.fil_data)
    show("plot13")
    
  }
  
  

}
dipu.boxplot<-function(input,output,session){
  #browser()
  dipu.fil_data<<-function.MyFilter(dipu.pre_data,input$boxplotselectfile,input$boxplotselectdaterange,input$boxplotselectchannel)
  dipu.pac_data<<-function.MyAggregation(dipu.fil_data,input$boxplotselectdataaggregation)
  pac<-dipu.pac_data
  
  hide("plot21")
  hide("plot22")
  hide("plot23")
  #hide("plot24")
  speed      <- "speed"       %in% input$boxplotselectbins
  length      <- "length"       %in% input$boxplotselectbins
  weight      <- "weight"       %in% input$boxplotselectbins
  
  if(speed & length & weight){
    output$plot21 <- function.MyBoxPlotspeed(pac)
    show("plot21")
    output$plot22 <- function.MyBoxPlotlength(pac)
    show("plot22")
    output$plot23 <- function.MyBoxPlotweight(pac)
    show("plot23")
    
  }else if(speed & length){
    output$plot21 <- function.MyBoxPlotspeed(pac)
    show("plot21")
    output$plot22 <- function.MyBoxPlotlength(pac)
    show("plot22")
    
  }else if(length & weight){
    output$plot22 <- function.MyBoxPlotlength(pac)
    show("plot22")
    output$plot23 <- function.MyBoxPlotweight(pac)
    show("plot23")
    
    
  }else if(speed & weight){
    output$plot21 <- function.MyBoxPlotspeed(pac)
    show("plot21")
    
    output$plot23 <- function.MyBoxPlotweight(pac)
    show("plot23")
    
  }else if(length){
    output$plot22 <- function.MyBoxPlotlength(pac)
    show("plot22")
    
  }else if(speed){
    output$plot21 <- function.MyBoxPlotspeed(pac)
    show("plot21")
    
  }else if(weight){
    output$plot23 <- function.MyBoxPlotweight(pac)
    show("plot23")
    
  }
  output$plot24 <- function.MyBoxPlot2(pac,input$boxplotselectx,input$boxplotselecty,input$boxplotselectcolor)
  
}

dipu.barplot<-function(input,output,session){
  #browser()
  dipu.fil_data<<-function.MyFilter(dipu.pre_data,input$barplotselectfile,input$barplotselectdaterange,input$barplotselectchannel)
  dipu.pac_data<<-function.MyAggregation(dipu.fil_data,input$barplotselectdataaggregation)
  pac<-dipu.pac_data
  
  hide("plot81")
  hide("plot82")
  hide("plot83")
  #hide("plot84")
  speed      <- "speed"       %in% input$barplotselectbins
  length      <- "length"       %in% input$barplotselectbins
  weight      <- "weight"       %in% input$barplotselectbins
  
  if(speed & length & weight){
    output$plot81 <- function.MyBarPlotspeed(pac)
    show("plot81")
    output$plot82 <- function.MyBarPlotlength(pac)
    show("plot82")
    output$plot83 <- function.MyBarPlotweight(pac)
    show("plot83")
    
  }else if(speed & length){
    output$plot81 <- function.MyBarPlotspeed(pac)
    show("plot81")
    output$plot82 <- function.MyBarPlotlength(pac)
    show("plot82")
    
  }else if(length & weight){
    output$plot82 <- function.MyBarPlotlength(pac)
    show("plot82")
    output$plot83 <- function.MyBarPlotweight(pac)
    show("plot83")
    
    
  }else if(speed & weight){
    output$plot81 <- function.MyBarPlotspeed(pac)
    show("plot81")
    
    output$plot83 <- function.MyBarPlotweight(pac)
    show("plot83")
    
  }else if(length){
    output$plot82 <- function.MyBarPlotlength(pac)
    show("plot82")
    
  }else if(speed){
    output$plot81 <- function.MyBarPlotspeed(pac)
    show("plot81")
    
  }else if(weight){
    output$plot83 <- function.MyBarPlotweight(pac)
    show("plot83")
    
  }
  output$plot84 <- function.MyBarPlot2(pac,input$barplotselectx,input$barplotselecty,input$barplotselectcolor)
  
}
dipu.anomalydetection<-function(input,output,session){
  
  dipu.fil_data<<-function.MyFilter(dipu.pre_data,input$anomalyselectfile,input$anomalyselectdaterange,input$anomalyselectchannel)
  dipu.pac_data<<-function.MyAggregation(dipu.fil_data,input$anomalyselectdataaggregation)
  anomaly_pac<-dipu.pac_data
  function.MyAnomalyDetection(anomaly_pac,input$anomalyselectx,input$anomalyselecty,input$anomalyselectperiod,input$anomalyselectlastonly,output)
}

dipu.motifdiscovery<-function(input,output,session){
  
  dipu.fil_data<<-function.MyFilter(dipu.pre_data,input$motifselectfile,input$motifselectdaterange,input$motifselectchannel)
  dipu.pac_data<<-function.MyAggregation(dipu.fil_data,input$motifselectdataaggregation)
  motif_pac<-dipu.pac_data
  function.MyMotifDiscovery(motif_pac,input$motifselectx,input$motifselecty,input$motifselectwindowsize,output)
}
dipu.machinelearning<-function(input,output,session){
  
  dipu.fil_data<<-function.MyFilter(dipu.pre_data,input$machinelearningselectfile,input$machinelearningselectdaterange,input$machinelearningselectchannel)
  dipu.pac_data<<-function.MyAggregation(dipu.fil_data,input$machinelearningselectdataaggregation)
  machine_pac<-dipu.pac_data

  dataplot<-function.MyMachineLearning(machine_pac,input$machinelearningselecttrack,input$machinelearningmaxruntime,output)
}
dipu.linearregression<-function(input,output,session){
  dipu.fil_data<<-function.MyFilter(dipu.pre_data,input$linearregressionselectfile,input$linearregressionselectdaterange,input$linearregressionselectchannel)
  dipu.pac_data<<-function.MyAggregation(dipu.fil_data,input$linearregressionselectdataaggregation)
  linear_pac<-dipu.pac_data
  dataplot<-function.MyLinearRegression(linear_pac,input$linearregressionselecttrack,output)
  
}
dipu.arima<-function(input,output,session){
  dipu.fil_data<<-function.MyFilter(dipu.pre_data,input$arimaselectfile,input$arimaselectdaterange,input$arimaselectchannel)
  dipu.pac_data<<-function.MyAggregation(dipu.fil_data,input$arimaselectdataaggregation)
  arima_pac<-dipu.pac_data
  dataplot<-function.MyArima(arima_pac,input$arimaselecttrack,output)
  
}
