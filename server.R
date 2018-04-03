#1. GetMyData.R -> filename ==> data1
#2. Preprocessing -> data,channel ==> data1
#3. Aggregation ->data,channel,filename,aggregater ==> pac
source("Lib.R")
source("GetMyData.R")
source("Preprocessing.R")
source("Aggregation.R")
source("MyPlots.R")
source("MyFunctions.R")

server <- function(input, output,session) {
  output$sectortable <- renderTable({
    
      data<-c("SC1_MEJA k. Mengusovce - k. PP Západ",
              "SC2_MEJA k. PP Západ - k. Vysoké Tatry",
              "SC3_MEJA k. Vysoké Tatry - k. PP Východ",
              "SC4_MEJA k. PP Východ - k. Spišský Štvrtok",
              "SC5_JAJA k. Spišský Štvrtok - k. Levoča"
      )
      matrix <- matrix(data,nrow=5,ncol=1)
      colnames(matrix) <- c('Files')
      matrix
    },striped = TRUE,hover = TRUE,bordered = TRUE,spacing = c("l"),width = "auto"
  )
  
  
  graphics.off()
  pdf(NULL)
  ##=================== Dashboard======================== ##
  observeEvent(
    input$selectfile,{
      dipu.dashboard(input,output)
    },ignoreInit = TRUE)
  
  observeEvent(
    input$selectchannel,{
      dipu.dashboard(input,output)
    },ignoreInit = TRUE)
  observeEvent(
    input$selectdataset,{
      dipu.dashboard(input,output)
    },ignoreInit = TRUE)
  observeEvent(
    input$selectdataaggregation,{
      dipu.dashboard(input,output)
    },ignoreInit = TRUE)
  observeEvent(
    input$daterange,{
      dipu.dashboard(input,output)
    },ignoreInit = TRUE)
  
  ##======================= Box Plot ===============================##
  choices = setNames(colnames(pac),colnames(pac))
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
  choices = setNames(colnames(pac),colnames(pac))
  updateSelectInput(session,"selectanomalyx","Select X:",choices=choices,selected="Index")
  updateSelectInput(session,"selectanomalyy","Select Y:",choices=choices,selected="SP11")
  
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
  choices = setNames(colnames(pac),colnames(pac))
  updateSelectInput(session,"selectmotifx","Select X:",choices=choices,selected="SP12")
  updateSelectInput(session,"selectmotify","Select Y:",choices=choices,selected="SP11")
  
  observeEvent(
    input$selectmotifx,{
      dipu.motifdiscovery(input,output,session)
    },ignoreInit = TRUE)
  
  observeEvent(
    input$selectmotify,{
      dipu.motifdiscovery(input,output,session)
    },ignoreInit = TRUE)
  
  #=========================MACHINE LEARNING========================
  observeEvent(
    input$machinelearningslider,{
      dipu.machinelearning(input,output,session)
    },ignoreInit = FALSE)
  
  
  
  #====================================timetk + linear regression: MAPE = 4.3% (timetk demo)==================================
  
  observeEvent(
    input$linearregressionslider,{
      dipu.linearregression(input,output,session)
    },ignoreInit = FALSE)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
    # output$plot11 <- renderPlotly({
    #   
    #   
    #   SUM_DATA <- pac[,c(1,31)]
    #   
    #   SUM_DATA %>%
    #     tk_index() %>%
    #     tk_get_timeseries_summary() %>%
    #     glimpse()
    #   
    #   beer_sales_tbl_aug <- SUM_DATA %>%
    #     tk_augment_timeseries_signature()
    #   
    #   beer_sales_tbl_aug
    #   beer_sales_tbl_aug<-na.omit(beer_sales_tbl_aug)
    #   
    #   (l <- sapply(beer_sales_tbl_aug, function(x) is.factor(x)))
    #   m <- beer_sales_tbl_aug[, l]
    #   ifelse(n <- sapply(m, function(x) length(levels(x))) == 1, "DROP", "NODROP")
    #   
    #   fit_lm <- lm(SUM~ ., data = select(beer_sales_tbl_aug, -c(Index, diff,month.lbl)))
    #   
    #   summary(fit_lm)
    #   #na.omit(fit_lm)
    #   
    #   beer_sales_idx <- SUM_DATA %>%
    #     tk_index()
    #   
    #   tail(beer_sales_idx)
    #   
    #   # Make future index
    #   future_idx <- beer_sales_idx %>%
    #     tk_make_future_timeseries(n_future =10)
    #   
    #   future_idx
    #   
    #   new_data_tbl <- future_idx %>%
    #     tk_get_timeseries_signature()
    #   
    #   new_data_tbl
    #   
    #   # Make predictions
    #   pred <- predict(fit_lm, newdata = select(new_data_tbl, -c(index, diff)))
    #   predictions_tbl <- tibble(
    #     Index  = future_idx,
    #     value = pred
    #   )
    #   
    #   predictions_tbl
    #   
    #   
    #   split <- round(nrow(SUM_DATA) * .90)
    #   datat_to <- SUM_DATA[1:split,]
    #   actuals_tbl <- SUM_DATA[(split + 1):nrow(SUM_DATA),]
    #   #colnames(actuals_tbl)[2] <- "value"
    #   
    #   
    #   p<-ggplot(SUM_DATA,aes(x=Index,y=SUM))+
    #     # Training data
    #     geom_line(color = palette_light()[[1]]) +
    #     geom_point(color = palette_light()[[1]])+
    #     # Predictions
    #     geom_line(aes(y = value), color = palette_light()[[4]], data = predictions_tbl) +
    #     geom_point(aes(y = value), color = palette_light()[[4]], data = predictions_tbl)+ 
    #     # Actuals
    #     geom_line(aes(y = SUM),color = palette_light()[[3]], data = actuals_tbl) +
    #     geom_point(aes(y = SUM),color = palette_light()[[3]], data = actuals_tbl)+
    #     theme_tq() +
    #     labs(title = "Time series sum data")
    #   ggplotly(p) 
    #   
    #   
    # })
    # 
  # output$plot1 <- renderPlotly({
  #   img <- plot_ly(SUM_DATA, x = ~Index, y = ~SUM)%>%
  #     add_trace(colors = "orange",name = "Početnosť áut v čase",mode = "lines")%>%
  #     add_trace(y = ~pred, colors = "gray",name = "Predikované hodnoty", mode = "lines+markers", alpha = 1,data = error_tbl)%>%
  #     layout(title = "Graf",
  #            xaxis = list(title = "Čas",
  #                         rangeslider = list(type = "date")),
  #            yaxis = list(title = "Početnosť áut"))
  #   (img)
  #   
  # })
  # output$plot2 <- renderPlotly({
  #   #pie plot by day of week
  #   plot_ly(pac, labels = ~wday.lbl, values = ~SP10, type = 'pie',textposition = 'inside')%>%
  #     layout(title = 'Kol??ov? graf')
  # })
  # 
  # output$plot3 <- renderPlot({
  #   x <- ggplot(data1,aes(x = Date, y = data1$SUM,col = CHANNEL))+
  #     geom_bar(stat = "identity",size = 3,alpha = 0.5,fill = "blue")
  #   #ggplotly(x)
  #   (x)
  # })
  # output$plot21 <- renderPlotly({
  #   #Plot of aggregation by SpeedBins
  #   plot_ly(pac, x = ~Index, y = ~SP1 , name = 'Number of cars in time aggregated')%>%
  #     add_trace(y = ~SP1, name = '0-50km/h', mode = 'lines') %>%
  #     add_trace(y = ~SP2, name = '50-60km/h', mode = 'lines') %>%
  #     add_trace(y = ~SP3, name = '60-70km/h', mode = 'lines') %>%
  #     add_trace(y = ~SP4, name = '70-80km/h', mode = 'lines') %>%
  #     add_trace(y = ~SP5, name = '80-90km/h', mode = 'lines') %>%
  #     add_trace(y = ~SP6, name = '90-100km/h', mode = 'lines') %>%
  #     add_trace(y = ~SP7, name = '100-110km/h', mode = 'lines') %>%
  #     add_trace(y = ~SP8, name = '110-120km/h', mode = 'lines') %>%
  #     add_trace(y = ~SP9, name = '120-130km/h', mode = 'lines') %>%
  #     add_trace(y = ~SP10, name = '130-140km/h', mode = 'lines') %>%
  #     add_trace(y = ~SP11, name = '140-150km/h', mode = 'lines') %>%
  #     add_trace(y = ~SP12, name = '150-160km/h', mode = 'lines') %>%
  #     add_trace(y = ~SP13, name = '160-180km/h', mode = 'lines')%>%
  #     add_trace(y = ~SP14, name = '180-999km/h', mode = 'lines')%>%
  #     add_trace(y = ~SUM, name = 'TOTAL', mode = 'lines')
  #   
  # })
  # output$plot22 <- renderPlotly({
  #   #Plot of aggregation by LengthBins
  #   plot_ly(pac, x = ~Index, y = ~LN1 , name = 'Number of cars in time aggregated')%>%
  #     add_trace(y = ~LN1, name = '0-300 cm', mode = 'lines') %>%
  #     #add_trace(y = ~LN2, name = '300-470 cm', mode = 'lines') %>%
  #     add_trace(y = ~LN3, name = '470-550 cm', mode = 'lines') %>%
  #     add_trace(y = ~LN4, name = '550-600 cm', mode = 'lines') %>%
  #     add_trace(y = ~LN5, name = '600-1300 cm', mode = 'lines') %>%
  #     add_trace(y = ~LN6, name = '1300-1800 cm', mode = 'lines') %>%
  #     add_trace(y = ~LN7, name = '1800-2550 cm', mode = 'lines') %>%
  #     add_trace(y = ~LN8, name = '2550-3600 cm', mode = 'lines') %>%
  #     add_trace(y = ~LN9, name = '3600-9999 cm', mode = 'lines') 
  #   
  # })
  # output$plot31 <- renderPlotly({
  #   
  #   
  #   SUM_DATA <- pac[,c(1,31)]
  #   
  #   SUM_DATA %>%
  #     tk_index() %>%
  #     tk_get_timeseries_summary() %>%
  #     glimpse()
  #   
  #   beer_sales_tbl_aug <- SUM_DATA %>%
  #     tk_augment_timeseries_signature()
  #   
  #   beer_sales_tbl_aug
  #   beer_sales_tbl_aug<-na.omit(beer_sales_tbl_aug)
  #   
  #   (l <- sapply(beer_sales_tbl_aug, function(x) is.factor(x)))
  #   m <- beer_sales_tbl_aug[, l]
  #   ifelse(n <- sapply(m, function(x) length(levels(x))) == 1, "DROP", "NODROP")
  #   
  #   fit_lm <- lm(SUM~ ., data = select(beer_sales_tbl_aug, -c(Index, diff,month.lbl)))
  #   
  #   summary(fit_lm)
  #   #na.omit(fit_lm)
  #   
  #   beer_sales_idx <- SUM_DATA %>%
  #     tk_index()
  #   
  #   tail(beer_sales_idx)
  #   
  #   # Make future index
  #   future_idx <- beer_sales_idx %>%
  #     tk_make_future_timeseries(n_future =10)
  #   
  #   future_idx
  #   
  #   new_data_tbl <- future_idx %>%
  #     tk_get_timeseries_signature()
  #   
  #   new_data_tbl
  #   
  #   # Make predictions
  #   pred <- predict(fit_lm, newdata = select(new_data_tbl, -c(index, diff)))
  #   predictions_tbl <- tibble(
  #     Index  = future_idx,
  #     value = pred
  #   )
  #   
  #   predictions_tbl
  #   
  #   
  #   split <- round(nrow(SUM_DATA) * .90)
  #   datat_to <- SUM_DATA[1:split,]
  #   actuals_tbl <- SUM_DATA[(split + 1):nrow(SUM_DATA),]
  #   #colnames(actuals_tbl)[2] <- "value"
  #   
  #   
  #   p<-ggplot(SUM_DATA,aes(x=Index,y=SUM))+
  #     # Training data
  #     geom_line(color = palette_light()[[1]]) +
  #     geom_point(color = palette_light()[[1]])+
  #     # Predictions
  #     geom_line(aes(y = value), color = palette_light()[[4]], data = predictions_tbl) +
  #     geom_point(aes(y = value), color = palette_light()[[4]], data = predictions_tbl)+ 
  #     # Actuals
  #     geom_line(aes(y = SUM),color = palette_light()[[3]], data = actuals_tbl) +
  #     geom_point(aes(y = SUM),color = palette_light()[[3]], data = actuals_tbl)+
  #     theme_tq() +
  #     labs(title = "Time series sum data")
  #   ggplotly(p) 
  #   
  #   
  # })
  # output$plot32 <- renderPlotly({
  #   #Bar plot day of week
  #   plot_ly(pac, x = ~wday.lbl, y = ~SP1, type = 'bar', name = "0-50km/h") %>%
  #     add_trace(y = ~SP2, name = '50-60km/h', mode = 'lines') %>%
  #     add_trace(y = ~SP3, name = '60-70km/h', mode = 'lines') %>%
  #     add_trace(y = ~SP4, name = '70-80km/h', mode = 'lines') %>%
  #     add_trace(y = ~SP5, name = '80-90km/h', mode = 'lines') %>%
  #     add_trace(y = ~SP6, name = '90-100km/h', mode = 'lines') %>%
  #     add_trace(y = ~SP7, name = '100-110km/h', mode = 'lines') %>%
  #     add_trace(y = ~SP8, name = '110-120km/h', mode = 'lines') %>%
  #     add_trace(y = ~SP9, name = '120-130km/h', mode = 'lines') %>%
  #     add_trace(y = ~SP10, name = '130-140km/h', mode = 'lines') %>%
  #     add_trace(y = ~SP11, name = '140-150km/h', mode = 'lines') %>%
  #     add_trace(y = ~SP12, name = '150-160km/h', mode = 'lines') %>%
  #     add_trace(y = ~SP13, name = '160-180km/h', mode = 'lines')%>%
  #     add_trace(y = ~SP14, name = '180-999km/h', mode = 'lines')%>%
  #     #add_trace(y = ~SUM, name = 'TOTAL', mode = 'lines')%>%
  #     layout(title = "Počty áuto podľa dni v týždni",yaxis = list(title = 'Count'), barmode = 'group')
  #   
  #   
  # })
   
}
