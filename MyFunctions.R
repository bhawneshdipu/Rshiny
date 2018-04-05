## MyFunctions ##

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
    output$plot11 <- function.MyPlotWeightBins(pac)
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
  
  pac<-function.MyAggregation(dipu.fil_data,input$selectdataaggregation)
  output$plot31<-function.MyAnomalyDetection(pac,input$selectanomalyx,input$selectanomalyy,input$selectperiod,input$selectlastonly)
}

dipu.motifdiscovery<-function(input,output,session){
  #browser()
  #data1<-function.getRawData()
  #data1<-function.MyPreprocessAndFilter(data1,input$selectfile,input$selectdaterange,input$selectchannel)
  
  pac<-function.MyAggregation(dipu.fil_data,input$selectdataaggregation)
  
  output$plot41<-function.MyMotifDiscovery(pac,input$selectmotifx,input$selectmotify)
}
dipu.machinelearning<-function(input,output,session){
  #browser()
  #data1<-function.getRawData()
  #data1<-function.MyPreprocessAndFilter(data1,input$selectfile,input$selectdaterange,input$selectchannel)
  
  pac<-function.MyAggregation(dipu.fil_data,input$selectdataaggregation)
  
  dataplot<-function.MyMachineLearning(data1)
  output$plot51<-renderPlotly(dataplot[0])
  output$table52<-DT::renderDataTable({dataplot[1]})
}
dipu.linearregression<-function(input,output,session){
  #browser()
  #data1<-function.getRawData()
  #data1<-function.MyPreprocessAndFilter(data1,input$selectfile,input$selectdaterange,input$selectchannel)
  
  pac<-function.MyAggregation(dipu.fil_data,input$selectdataaggregation)
  
  dataplot<-function.MyLinearRegression(pac)
  output$plot61<-renderPlotly(dataplot[0])
  output$table62<-DT::renderDataTable({dataplot[1]})
}
dipu.arima<-function(input,output,session){
  #browser()
  #data1<-function.getRawData()
  #data1<-function.MyPreprocessAndFilter(data1,input$selectfile,input$selectdaterange,input$selectchannel)
  
  pac<-function.MyAggregation(dipu.fil_data,input$selectdataaggregation)
  
  dataplot<-function.MyArima(pac)
  output$plot71<-renderPlotly(dataplot[0])
  output$table72<-DT::renderDataTable({dataplot[1]})
}