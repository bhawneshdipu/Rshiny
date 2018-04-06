source("global.R")
source("MyFunctions.R")
server <- function(input, output,session) {
  message("Server ")
  ##browser()
  output$sectortable <- renderTable({
    message("rander table")
    ##browser()
      data<-c("SC1_MEJA k. Mengusovce - k. PP Západ",
              "SC2_MEJA k. PP Západ - k. Vysoké Tatry",
              "SC3_MEJA k. Vysoké Tatry - k. PP Východ",
              "SC4_MEJA k. PP Východ - k. Spišský Štvrtok",
              "SC5_JAJA k. Spišský Štvrtok - k. Levoča"
      )
      matrix <- matrix(data,nrow=5,ncol=1)
      colnames(matrix) <- c('Files')
      matrix
    },striped = TRUE,hover = TRUE,bordered = TRUE,spreing = c("l"),width = "auto"
  )
  
  
  graphics.off()
  pdf(NULL)
  ##=================== Dashboard======================== ##
  observeEvent(
    input$selectfile,{
      dipu.dashboard(input,output,session)
    },ignoreInit = FALSE)
  
  observeEvent(
    input$selectchannel,{
      dipu.dashboard(input,output,session)
    },ignoreInit = TRUE)
  observeEvent(
    input$selectdataset,{
      dipu.dashboard(input,output,session)
    },ignoreInit = TRUE)
  observeEvent(
    input$selectdataaggregation,{
      dipu.dashboard(input,output,session)
    },ignoreInit = TRUE)
  
  observeEvent(
    input$daterange,{
      dipu.dashboard(input,output,session)
    },ignoreInit = TRUE)
  
  
  choices = setNames(colnames(dipu.pre_data),colnames(dipu.pre_data))
  updateSelectInput(session,"selectboxploty","Select Y:",choices=choices)
  updateSelectInput(session,"selectboxplotx","Select X:",choices=choices)
  
  
  
  observeEvent(
    input$selectcolor,{
      dipu.boxplot(input,output,session)
    },ignoreInit = TRUE)
  observeEvent(
    input$selectboxploty,{
      dipu.boxplot(input,output,session)
    },ignoreInit = TRUE)
  observeEvent(
    input$selectboxplotx,{
      dipu.boxplot(input,output,session)
    },ignoreInit = TRUE)
  
  ##=============================== Anomaly Detection========================= ##
  choices = setNames(colnames(dipu.pre_data),colnames(dipu.pre_data))
  updateSelectInput(session,"selectanomalyx","Select X:",choices=choices,selected="SP11")
  updateSelectInput(session,"selectanomalyy","Select Y:",choices=choices,selected="SP12")
  
  observeEvent(
    input$selectlastonly,{
      dipu.anomalydetection(input,output,session)
    },ignoreInit = TRUE)
  
  observeEvent(
    input$selectanomalyx,{
      dipu.anomalydetection(input,output,session)
    },ignoreInit = TRUE)
  
  observeEvent(
    input$selectanomalyy,{
      dipu.anomalydetection(input,output,session)
    },ignoreInit = TRUE)
  
  observeEvent(
    input$selectperiod,{
      dipu.anomalydetection(input,output,session)
    },ignoreInit = TRUE)
  
  #==================================Motif discovery=======================================
  choices = setNames(colnames(dipu.pre_data),colnames(dipu.pre_data))
  updateSelectInput(session,"selectmotifx","Select X:",choices=choices,selected="SP11")
  updateSelectInput(session,"selectmotify","Select Y:",choices=choices,selected="SP12")
  # 
  observeEvent(
    input$selectmotifx,{
      dipu.motifdiscovery(input,output,session)
    },ignoreInit = TRUE)

  observeEvent(
    input$selectmotify,{
      dipu.motifdiscovery(input,output,session)
    },ignoreInit = TRUE)
  # 
  #=========================MACHINE LEARNING========================
  choices = setNames(colnames(dipu.pre_data),colnames(dipu.pre_data))
  updateSelectInput(session,"machinelearningselectchannel","Select Track:",choices=choices,selected="SUM")
  
  observeEvent(
    input$machinelearningselectchannel,{
      dipu.machinelearning(input,output,session)
    },ignoreInit = TRUE)


  
  #====================================timetk + linear regression: MAPE = 4.3% (timetk demo)==================================
  choices = setNames(colnames(dipu.pac_data),colnames(dipu.pac_data))
  updateSelectInput(session,"linearregressionselectchannel","Select Track:",choices=choices,selected="SUM")
  
  observeEvent(
    input$linearregressionselectchannel,{
      dipu.linearregression(input,output,session)
    },ignoreInit = FALSE)
  
  
  
  updateSelectInput(session,"arimaselectchannel","Select Track:",choices=choices,selected="SUM")
  

  observeEvent(
    input$arimaselectchannel,{
      dipu.arima(input,output,session)
    },ignoreInit = FALSE)

  }
