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


function.MyMotifDiscovery<-function(pac,myX,myY){
  #==================================Motif discovery=======================================
  
  res.wcc <- Func.motif(
    ts = pac[paste0(myX)],
    global.norm = T,
    local.norm = F,
    window.size = 24,
    overlap = 0,
    w = 6,
    a = 5,
    mask.size = 5,
    max.dist.ratio = 1.2,
    count.ratio.1 = 1.1,
    count.ratio.2 = 1.1
    )
  #res.wcc <- Func.motif(ts = pac$SP11, global.norm = T, local.norm = F, window.size = 24, overlap = 0, w = 6, a = 5, mask.size = 5, max.dist.ratio = 1.2, count.ratio.1 = 1.1, count.ratio.2 = 1.1)
  
  #res.ahu <- Func.motif(ts = pac$SP12, global.norm = T, local.norm = F, window.size = 24, overlap = 0, w = 6, a = 5, mask.size = 5, max.dist.ratio = 1.2, count.ratio.1 = 1.1, count.ratio.2 = 1.1)
  res.ahu <- Func.motif(ts = pac[paste0(myY)], global.norm = T, local.norm = F, window.size = 24, overlap = 0, w = 6, a = 5, mask.size = 5, max.dist.ratio = 1.2, count.ratio.1 = 1.1, count.ratio.2 = 1.1)
  
  
  #Visualization
  data.wcc <- Func.visual.SingleMotif(single.ts = pac[paste0(myY)], window.size = 24, motif.indices = res.wcc$Indices)
  data.ahu <- Func.visual.SingleMotif(single.ts = pac[paste0(myY)], window.size = 24, motif.indices = res.ahu$Indices)
  
  #Determine the total number of motifs discovered in the time series of WCC
  n <- length(unique(data.wcc$data.1$Y))
  #Make the plot
  p<-ggplot(data = data.wcc$data.1) +  
    geom_line(aes(x = 1:dim(data.wcc$data.1)[1], y = X)) +
    geom_point(aes(x = 1:dim(data.wcc$data.1)[1], y = X, color=Y, shape=Y))+
    scale_shape_manual(values = seq(from = 1, to = n)) +
    guides(shape=guide_legend(nrow = 2)) +
    xlab("Time (15-min)") + ylab("Počet áut") +
    theme(panel.background=element_rect(fill = "white", colour = "black"),
          legend.position="top",
          legend.title=element_blank())
  
  ggplotly(p)
}

#=========================MACHINE LEARNING========================

function.MyMachineLearning<-function(data1){

# Shuffle row indices: rows
rows <- sample(nrow(data1))

# Randomly order data: Sonar
data1 <- data1[rows,]

# Identify row to split on: split
split <- round(nrow(data1) * .60)

# Create train
train <- data1[1:split,]

# Create test
test <- data1[(split + 1):nrow(data1),]
#------------------------------------------

# Run algorithms using 10-fold cross validation
control <- trainControl(method="cv", number=10)
metric <- "Accuracy"
validation_index <- createDataPartition(data1$CHANNEL, p=0.80, list=FALSE)
# select 20% of the data for validation
validation <- data1[-validation_index,]
# use the remaining 80% of data to training and testing the models
dataset <- data1[validation_index,]
# a) linear algorithms
set.seed(7)
fit.lda <- train(SP10 ~ Date_Time,data=data1, method="lda", metric=metric, trControl=control)
# b) nonlinear algorithms
# CART
set.seed(7)
fit.cart <- train(SP10 ~ Date_Time,data=data1, method="rpart", metric=metric, trControl=control)
# kNN
set.seed(7)
fit.knn <- train(SP10 ~ Date_Time,data=data1, method="knn", metric=metric, trControl=control)
# c) advanced algorithms
# SVM
set.seed(7)
fit.svm <- train(SP10 ~ Date_Time,data=data1, method="svmRadial", metric=metric, trControl=control)
# Random Forest
set.seed(7)
fit.rf <- train(SP10 ~ Date_Time,data=data1, method="rf", metric=metric, trControl=control)


#summarize accuracy of models
results <- resamples(list(lda=fit.lda, cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf))
summary(results)
dotplot(results)
print(fit.lda)


# estimate skill of LDA on the validation dataset
predictions <- predict(fit.lda, validation)
confusionMatrix(predictions, validation$CHANNEL)
}

function.MyLinearRegression<-function(pac){
  
  #====================================timetk + linear regression: MAPE = 4.3% (timetk demo)==================================
  
  
  SUM_DATA <- pac[,c(1,31)]
  
  SUM_DATA %>%
    tk_index() %>%
    tk_get_timeseries_summary() %>%
    glimpse()
  
  beer_sales_tbl_aug <- SUM_DATA %>%
    tk_augment_timeseries_signature()
  
  beer_sales_tbl_aug
  beer_sales_tbl_aug<-na.omit(beer_sales_tbl_aug)
  
  (l <- sapply(beer_sales_tbl_aug, function(x) is.factor(x)))
  m <- beer_sales_tbl_aug[, l]
  ifelse(n <- sapply(m, function(x) length(levels(x))) == 1, "DROP", "NODROP")
  
  fit_lm <- lm(SUM~ ., data = select(beer_sales_tbl_aug, -c(Index, diff,month.lbl)))
  
  summary(fit_lm)
  #na.omit(fit_lm)
  
  beer_sales_idx <- SUM_DATA %>%
    tk_index()
  
  tail(beer_sales_idx)
  
  # Make future index
  future_idx <- beer_sales_idx %>%
    tk_make_future_timeseries(n_future =10)
  
  future_idx
  
  new_data_tbl <- future_idx %>%
    tk_get_timeseries_signature()
  
  new_data_tbl
  
  # Make predictions
  pred <- predict(fit_lm, newdata = select(new_data_tbl, -c(index, diff)))
  predictions_tbl <- tibble(
    Index  = future_idx,
    value = pred
  )
  
  predictions_tbl
  
  
  split <- round(nrow(SUM_DATA) * .90)
  datat_to <- SUM_DATA[1:split,]
  actuals_tbl <- SUM_DATA[(split + 1):nrow(SUM_DATA),]
  #colnames(actuals_tbl)[2] <- "value"
  
  
  p<-ggplot(SUM_DATA,aes(x=Index,y=SUM))+
    # Training data
    geom_line(color = palette_light()[[1]]) +
    geom_point(color = palette_light()[[1]])+
    # Predictions
    geom_line(aes(y = value), color = palette_light()[[4]], data = predictions_tbl) +
    geom_point(aes(y = value), color = palette_light()[[4]], data = predictions_tbl)+ 
    # Actuals
    geom_line(aes(y = SUM),color = palette_light()[[3]], data = actuals_tbl) +
    geom_point(aes(y = SUM),color = palette_light()[[3]], data = actuals_tbl)+
    theme_tq() +
    labs(title = "Time series sum data")
  return (ggplotly(p)) 
  
  
  
  error_tbl <- left_join(actuals_tbl, predictions_tbl) %>%
    rename(actual = SUM, pred = value) %>%
    mutate(
      error     = actual - pred,
      error_pct = error / actual
    ) 
  error_tbl
  
  # Calculating test error metrics
  test_residuals <- error_tbl$error
  test_error_pct <- error_tbl$error_pct * 100 # Percentage error
  
  me   <- mean(test_residuals, na.rm=TRUE)
  rmse <- mean(test_residuals^2, na.rm=TRUE)^0.5
  mae  <- mean(abs(test_residuals), na.rm=TRUE)
  mape <- mean(abs(test_error_pct), na.rm=TRUE)
  mpe  <- mean(test_error_pct, na.rm=TRUE)
  
  tibble(me, rmse, mae, mape, mpe) %>% glimpse()
  
  
  
  # Coerce to xts
  beer_sales_xts <- tk_xts(SUM_DATA) 
  
  # Show the first six rows of the xts object
  beer_sales_xts %>%
    head()
  tk_tbl(beer_sales_xts, rename_index = "date")
  
  # Coerce to ts
  beer_sales_ts <- tk_ts(SUM_DATA)
  
  # Show the calendar-printout
  beer_sales_ts
  tk_tbl(beer_sales_ts, rename_index = "date")
  
  has_timetk_idx(beer_sales_ts)
  
  # If timetk_idx is present, can get original dates back 
  tk_tbl(beer_sales_ts, timetk_idx = TRUE, rename_index = "date")
  
  
}

#====================================ARIMA + sweep: MAPE = 4.3% (sweep demo)==================================


function.MyArima<-function(pac){
  
  
  SUM_DATA <- pac[,c(1,31)]
  SUM_DATA %>%
    ggplot(aes(Index, SUM)) +
    geom_line(col = palette_light()[1]) +
    geom_point(col = palette_light()[1]) +
    geom_ma(ma_fun = SMA, n = 12, size = 1) +
    theme_tq() +
    labs(title = "Beer Sales: 2007 through 2016")
  
  
  beer_sales_ts <- tk_ts(SUM_DATA)
  beer_sales_ts
  has_timetk_idx(beer_sales_ts)
  fit_arima <- auto.arima(beer_sales_ts)
  
  fit_arima
  # sw_tidy - Get model coefficients
  sw_tidy(fit_arima)
  # sw_glance - Get model description and training set accuracy measures
  sw_glance(fit_arima) %>%
    glimpse()
  
  # sw_augment - get model residuals
  sw_augment(fit_arima, timetk_idx = TRUE)
  
  sw_augment(fit_arima, timetk_idx = TRUE) %>%
    ggplot(aes(x = index, y = .resid)) +
    geom_point() + 
    geom_hline(yintercept = 0, color = "red") + 
    labs(title = "Residual diagnostic") +
    theme_tq()
  
  # Forecast next 12 months
  fcast_arima <- forecast(fit_arima, h = 100)
  class(fcast_arima)
  
  # Check if object has timetk index 
  has_timetk_idx(fcast_arima)
  
  # sw_sweep - tidies forecast output
  fcast_tbl <- sw_sweep(fcast_arima, timetk_idx = TRUE)
  
  fcast_tbl
  
  
  
  
  # Visualize the forecast with ggplot
  fcast_tbl %>%
    ggplot(aes(x = index, y = SUM, color = key)) +
    # 95% CI
    geom_ribbon(aes(ymin = lo.95, ymax = hi.95), 
                fill = "#D5DBFF", color = NA, size = 0) +
    # 80% CI
    geom_ribbon(aes(ymin = lo.80, ymax = hi.80, fill = key), 
                fill = "#596DD5", color = NA, size = 0, alpha = 0.8)+
    # Prediction
    geom_line() +
    geom_point() +
    # Actuals
    geom_line(aes(x = Index, y = SUM), color = palette_light()[[1]], data = actuals_tbl) +
    geom_point(aes(x = Index, y = SUM), color = palette_light()[[1]], data = actuals_tbl)+ 
    # Aesthetics
    labs(title = "Beer Sales Forecast: ARIMA", x = "", y = "Thousands of Tons",
         subtitle = "sw_sweep tidies the auto.arima() forecast output") +
    scale_color_tq() +
    scale_fill_tq() +
    theme_tq()
  
  
  
  
  
  rror_tbl <- left_join(actuals_tbl, fcast_tbl, by = c("Index" = "index"))%>% 
    rename(actual = SUM.x, pred = SUM.y) %>%
    select(Index, actual, pred) %>%
    mutate(
      error     = actual - pred,
      error_pct = error / actual
    ) 
  error_tbl
  na.omit(error_tbl)
  # Calculate test error metrics
  test_residuals <- error_tbl$error
  test_error_pct <- error_tbl$error_pct * 100 # Percentage error
  
  me   <- mean(test_residuals, na.rm=TRUE)
  rmse <- mean(test_residuals^2, na.rm=TRUE)^0.5
  mae  <- mean(abs(test_residuals), na.rm=TRUE)
  mape <- mean(abs(test_error_pct), na.rm=TRUE)
  mpe  <- mean(test_error_pct, na.rm=TRUE)
  
  tibble(me, rmse, mae, mape, mpe) %>% glimpse()
  
  
}