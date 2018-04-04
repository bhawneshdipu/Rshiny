## MyFunctions ##

dipu.dashboard<-function(input,output){
  #print(input$selectfile)
  data1<-function.getMyData(input$selectfile)
  #View(data1)
  data1<-function.MyPreprocessing(data1,input$selectchannel,input$selectdaterange)
  pac<-function.MyAggregation(data1,input$selectchannel,input$selectfile,input$selectdataaggregation)
  
  if(input$selectdataset=="speed"){
    output$plot11 <- function.MyPlotSpeedBins(pac)
    
  }else if(input$selectdataset=="length"){
    output$plot11 <- function.MyPlotLengthBins(pac)
    
  }else {
    output$plot11 <- function.MyPlotWeightBins(pac)
  }
}
dipu.boxplot<-function(input,output,session){
  data1<-function.getMyData(input$selectfile)
  #View(data1)
  data1<-function.MyPreprocessing(data1,input$selectchannel,input$selectdaterange)
  pac<-function.MyAggregation(data1,input$selectchannel,input$selectfile,input$selectdataaggregation)
  
  output$plot21 <- function.MyBoxPlot1(pac)
  output$plot22 <- function.MyBoxPlot2(pac,input$selectboxplotx,input$selectboxploty,input$selectcolor)
  
}
dipu.anomalydetection<-function(input,output,session){
  
  output$plot31<-function.MyAnomalyDetection(pac,input$selectanomalyx,input$selectanomalyy,input$selectperiod,input$selectlastonly)
}

dipu.motifdiscovery<-function(input,output,session){
  output$plot41<-function.MyMotifDiscovery(pac,input$selectmotifx,input$selectmotify)
}
dipu.machinelearning<-function(input,output,session){
  
  dataplot<-function.MyMachineLearning(data1)
  output$plot51<-renderPlotly(dataplot[0])
  output$table52<-DT::renderDataTable({inspect(dataplot[1])})
}
dipu.linearregression<-function(input,output,session){
  dataplot<-function.MyLinearRegression(pac)
  output$plot61<-renderPlotly(dataplot[0])
  output$table62<-DT::renderDataTable({inspect(dataplot[1])})
}
dipu.arima<-function(input,output,session){
  dataplot<-function.MyArima(pac)
  output$plot71<-renderPlotly(dataplot[0])
  output$table72<-DT::renderDataTable({inspect(dataplot[1])})
}