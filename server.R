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
  sp <- c("SP1","SP2","SP3","SP4","SP5","SP6","SP7","SP8","SP9","SP10","SP11","SP12","SP13","SP14")
  ln <- c("LN1","LN2","LN3","LN4","LN5","LN6","LN7","LN8","LN9")
  cs <- c("CS1","CS2","CS3","CS4","CS5","CS6","SUM")
  dt<-c("Index", "Date_Time")
  lst<-c(sp,ln,cs)
  
  ##=================== Dashboard======================== ##
  
  observeEvent(
    input$dashboardaction,{
      dipu.dashboard(input,output,session)
    },ignoreInit = FALSE)
  
  
  ##=======================Box PLot ========================= ##
  choices=setNames(lst,lst)
  
  observeEvent(
    input$boxplotaction,{
      dipu.boxplot(input,output,session)
    },ignoreInit = TRUE)
  
  ##=======================Bar PLot ========================= ##
  
  observeEvent(
    input$barplotaction,{
      dipu.barplot(input,output,session)
    },ignoreInit = TRUE)
  
  ##=============================== Anomaly Detection========================= ##
  # choicex = setNames(dt,dt)
  # choicey=setNames(lst,lst)
  # updateSelectInput(session,"anomalyselectx","Select X:",choices=choicex,selected="Index")
  # updateSelectInput(session,"anomalyselecty","Select Y:",choices=choicey,selected="SP12")
  # 
  observeEvent(
    input$anomalyaction,{
      dipu.anomalydetection(input,output,session)
    },ignoreInit = TRUE)
  
  #==================================Motif discovery=======================================
   
  observeEvent(
    input$motifaction,{
      dipu.motifdiscovery(input,output,session)
    },ignoreInit = TRUE)

  #=========================MACHINE LEARNING========================
  
  observeEvent(
    input$machinelearningaction,{
      dipu.machinelearning(input,output,session)
    },ignoreInit = TRUE)


  
  #====================================timetk + linear regression: MAPE = 4.3% (timetk demo)==================================
  
  observeEvent(
    input$linearregressionaction,{
      dipu.linearregression(input,output,session)
    },ignoreInit = FALSE)
  
  
  
  

  observeEvent(
    input$arimaaction,{
      dipu.arima(input,output,session)
    },ignoreInit = FALSE)

  }
