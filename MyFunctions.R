## MyFunctions ##

dipu.dashboard<-function(input,output){
  #print(input$selectfile)
  data1<-function.getMyData(input$selectfile)
  #View(data1)
  data1<-function.MyPreprocessing(data1,input$selectchannel)
  pac<-function.MyAggregation(data1,input$selectchannel,input$selectfile,input$selectdataaggregation)
  
  if(input$selectdataset=="speed"){
    output$plot11 <- function.MyPlotSpeedBins(pac)
    
  }else if(input$selectdataset=="length"){
    output$plot11 <- function.MyPlotLengthBins(pac)
    
  }else {
    output$plot11 <- function.MyPlotWeightBins(pac)
  }
}