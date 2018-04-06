## MyFunctions ##

source("MyPlots.R")
dipu.dashboard<-function(input,output,session,data1){
  #print(input$selectfile)
  # data1<-function.getRawData()
  # data1<-function.MyPreprocessing(data1)
  #browser()
  dipu.fil_data<<-function.MyFilter(dipu.pre_data,input$selectfile,input$selectdaterange,input$selectchannel)
  dipu.pac_data<<-function.MyAggregation(dipu.fil_data,input$selectdataaggregation)
  #View(dipu.fil_data)
  #View(dipu.pac_data)
  #View(dipu.fil_data)
  pac<-dipu.pac_data
  
  if(input$selectdataset=="speed"){
    output$plot11 <- function.MyPlotSpeedBins(pac)
    
  }else if(input$selectdataset=="length"){
    output$plot11 <- function.MyPlotLengthBins(pac)
    
  }else {
    output$plot11 <- function.MyPlotWeightBins(dipu.fil_data)
  }
}
dipu.boxplot<-function(input,output,session){
  #browser()
  #data1<-function.getRawData()
  #data1<-function.MyPreprocessAndFilter(data1,input$selectfile,input$selectdaterange,input$selectchannel)
  pac<-function.MyAggregation(dipu.fil_data,input$selectdataaggregation)
  
  output$plot21 <- function.MyBoxPlot1(pac)
  output$plot22 <- function.MyBoxPlot2(pac,input$selectboxplotx,input$selectboxploty,input$selectcolor)
  
}
dipu.anomalydetection<-function(input,output,session){
  #browser()
  #data1<-function.getRawData()
  #data1<-function.MyPreprocessAndFilter(data1,input$selectfile,input$selectdaterange,input$selectchannel)
  
  dipu.pac_data<<-function.MyAggregation(dipu.fil_data,input$selectdataaggregation)
  output$plot31<-function.MyAnomalyDetection(dipu.pac_data,input$selectanomalyx,input$selectanomalyy,input$selectperiod,input$selectlastonly)
}

dipu.motifdiscovery<-function(input,output,session){
  #browser()
  #data1<-function.getRawData()
  #data1<-function.MyPreprocessAndFilter(data1,input$selectfile,input$selectdaterange,input$selectchannel)
  
  
# no non-missing arguments to max; returning -Inf
# Warning: Error in {: task 1 failed - "subscript out of bounds"
# Stack trace (innermost first):
# 72: <Anonymous>
# 71: stop
# 70: e$fun
# 69: %do%
# 68: Func.motif
# 67: function.MyMotifDiscovery [MyPlots.R#167]
# 66: dipu.motifdiscovery [MyFunctions.R#51]
# 65: observeEventHandler [/home/dipu/fiverr/Rshiny/server.R#105]
# 1: runApp
  
  #dipu.pac_data<<-function.MyAggregation(dipu.fil_data,input$selectdataaggregation)
  #output$plot41<-function.MyMotifDiscovery(dipu.pac_data,input$selectmotifx,input$selectmotify)
}
dipu.machinelearning<-function(input,output,session){
  #browser()
  #data1<-function.getRawData()
  #data1<-function.MyPreprocessAndFilter(data1,input$selectfile,input$selectdaterange,input$selectchannel)
  #pac<-function.MyAggregation(dipu.fil_data,input$selectdataaggregation)
  

  dataplot<-function.MyMachineLearning(dipu.pac_data,input$machinelearningselectchannel,output)
  #output$plot51<-renderPlotly(dataplot[0])
  #output$plot52<-renderPlotly(dataplot[1])
  #output$table53<-DT::renderDataTable({dataplot[2]})
}
dipu.linearregression<-function(input,output,session){
  #browser()
  #data1<-function.getRawData()
  #data1<-function.MyPreprocessAndFilter(data1,input$selectfile,input$selectdaterange,input$selectchannel)
  dataplot<-function.MyLinearRegression(dipu.pac_data,input$linearregressionselectchannel,output)
  
}
dipu.arima<-function(input,output,session){
  #browser()
  #data1<-function.getRawData()
  #data1<-function.MyPreprocessAndFilter(data1,input$selectfile,input$selectdaterange,input$selectchannel)
  dataplot<-function.MyArima(dipu.pac_data,input$arimaselectchannel,output)
  
}
