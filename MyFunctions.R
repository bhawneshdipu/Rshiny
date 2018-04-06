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
  
  
  # commented due to error 
  # Warning: Error in resamples: object 'fit.lda' not found
  # Stack trace (innermost first):
  # 68: resamples
  # 67: function.MyMachineLearning [MyPlots.R#309]
  # 66: dipu.machinelearning [MyFunctions.R#58]
  # 65: observeEventHandler [/home/dipu/fiverr/Rshiny/server.R#114]
  # 1: runApp
  
  
  dataplot<-function.MyMachineLearning(dipu.pac_data,input$machinelearningselectchannel,output)
  #output$plot51<-renderPlotly(dataplot[0])
  #output$plot52<-renderPlotly(dataplot[1])
  #output$table53<-DT::renderDataTable({dataplot[2]})
}
dipu.linearregressionandarima<-function(input,output,session){
  #browser()
  #data1<-function.getRawData()
  #data1<-function.MyPreprocessAndFilter(data1,input$selectfile,input$selectdaterange,input$selectchannel)
  
  
#commented due to
# Warning in predict.lm(fit_lm, newdata = select(new_data_tbl, -c(index, diff))) :
# prediction from a rank-deficient fit may be misleading
# Warning: Error in UseMethod: no applicable method for 'margin' applied to an object of class "NULL"
# Stack trace (innermost first):
# 77: margin
# 76: structure
# 75: element_text
# 74: mget
# 73: find_args
# 72: theme
# 71: inherits
# 70: is.theme
# 69: %+replace%
# 68: theme_tq
# 67: function.MyLinearRegressionAndArima [MyPlots.R#516]
# 66: dipu.linearregressionandarima [MyFunctions.R#94]
# 65: observeEventHandler [/home/dipu/fiverr/Rshiny/server.R#131]
# 1: runApp
  
  #dipu.pac_data<-function.MyAggregation(dipu.fil_data,input$selectdataaggregation)
  #dataplot<-function.MyLinearRegressionAndArima(dipu.pac_data,input$linerregresionselectchannel,output)
  
}
