## MyPlots.R ##
source("Lib.R")

function.MyPlotSpeedBins <- function(pac) {
  renderPlotly({
    #Plot of aggregation by SpeedBins
    plot_ly(pac,
            x = ~ Index,
            y = ~ SP1 ,
            name = 'Number of cars in time aggregated') %>%
      add_trace(y = ~ SP1,
                name = '0-50km/h',
                mode = 'lines') %>%
      add_trace(y = ~ SP2,
                name = '50-60km/h',
                mode = 'lines') %>%
      add_trace(y = ~ SP3,
                name = '60-70km/h',
                mode = 'lines') %>%
      add_trace(y = ~ SP4,
                name = '70-80km/h',
                mode = 'lines') %>%
      add_trace(y = ~ SP5,
                name = '80-90km/h',
                mode = 'lines') %>%
      add_trace(y = ~ SP6,
                name = '90-100km/h',
                mode = 'lines') %>%
      add_trace(y = ~ SP7,
                name = '100-110km/h',
                mode = 'lines') %>%
      add_trace(y = ~ SP8,
                name = '110-120km/h',
                mode = 'lines') %>%
      add_trace(y = ~ SP9,
                name = '120-130km/h',
                mode = 'lines') %>%
      add_trace(y = ~ SP10,
                name = '130-140km/h',
                mode = 'lines') %>%
      add_trace(y = ~ SP11,
                name = '140-150km/h',
                mode = 'lines') %>%
      add_trace(y = ~ SP12,
                name = '150-160km/h',
                mode = 'lines') %>%
      add_trace(y = ~ SP13,
                name = '160-180km/h',
                mode = 'lines') %>%
      add_trace(y = ~ SP14,
                name = '180-999km/h',
                mode = 'lines') %>%
      add_trace(y = ~ SUM,
                name = 'TOTAL',
                mode = 'lines')
    
  })
}

function.MyPlotLengthBins <- function(pac) {
  renderPlotly({
    #Plot of aggregation by LengthBins
    plot_ly(pac,
            x = ~ Index,
            y = ~ LN1 ,
            name = 'Number of cars in time aggregated') %>%
      add_trace(y = ~ LN1,
                name = '0-300 cm',
                mode = 'lines') %>%
      add_trace(y = ~ LN2,
                name = '300-470 cm',
                mode = 'lines') %>%
      add_trace(y = ~ LN3,
                name = '470-550 cm',
                mode = 'lines') %>%
      add_trace(y = ~ LN4,
                name = '550-600 cm',
                mode = 'lines') %>%
      add_trace(y = ~ LN5,
                name = '600-1300 cm',
                mode = 'lines') %>%
      add_trace(y = ~ LN6,
                name = '1300-1800 cm',
                mode = 'lines') %>%
      add_trace(y = ~ LN7,
                name = '1800-2550 cm',
                mode = 'lines') %>%
      add_trace(y = ~ LN8,
                name = '2550-3600 cm',
                mode = 'lines') %>%
      add_trace(y = ~ LN9,
                name = '3600-9999 cm',
                mode = 'lines')
  })
  
}

function.MyPlotWeightBins <- function(pac) {
  renderPlotly({
    #Plot of aggregation by WeigthBins
    plot_ly(pac,
            x = ~ Date,
            y = ~ CS1 ,
            name = 'Number of cars in time aggregated') %>%
      add_trace(y = ~ CS1,
                name = '0-2000 kg',
                mode = 'lines') %>%
      add_trace(y = ~ CS2,
                name = '2000-3501 kg',
                mode = 'lines') %>%
      add_trace(y = ~ CS3,
                name = '3501-7497 kg',
                mode = 'lines') %>%
      add_trace(y = ~ CS4,
                name = '7497-12002 kg',
                mode = 'lines') %>%
      add_trace(y = ~ CS5,
                name = '12002-17998  kg',
                mode = 'lines') %>%
      add_trace(y = ~ CS6,
                name = '17998-23999  kg',
                mode = 'lines')
  })
}

function.MyBoxPlot1 <- function(pac) {
  renderPlotly({
    #boxplot
    plot_ly(pac,
            y = ~ SP1,
            name = '0-50km/h',
            type = 'box') %>%
      add_trace(y = ~ SP2,
                name = '50-60km/h',
                mode = 'lines') %>%
      add_trace(y = ~ SP3,
                name = '60-70km/h',
                mode = 'lines') %>%
      add_trace(y = ~ SP4,
                name = '70-80km/h',
                mode = 'lines') %>%
      add_trace(y = ~ SP5,
                name = '80-90km/h',
                mode = 'lines') %>%
      add_trace(y = ~ SP6,
                name = '90-100km/h',
                mode = 'lines') %>%
      add_trace(y = ~ SP7,
                name = '100-110km/h',
                mode = 'lines') %>%
      add_trace(y = ~ SP8,
                name = '110-120km/h',
                mode = 'lines') %>%
      add_trace(y = ~ SP9,
                name = '120-130km/h',
                mode = 'lines') %>%
      add_trace(y = ~ SP10,
                name = '130-140km/h',
                mode = 'lines') %>%
      add_trace(y = ~ SP11,
                name = '140-150km/h',
                mode = 'lines') %>%
      add_trace(y = ~ SP12,
                name = '150-160km/h',
                mode = 'lines') %>%
      add_trace(y = ~ SP13,
                name = '160-180km/h',
                mode = 'lines') %>%
      add_trace(y = ~ SP14,
                name = '180-999km/h',
                mode = 'lines') %>%
      layout(title = "Boxplot by day of week")
  })
  
}
function.MyBoxPlot2 <- function(pac, x, y, color) {
  renderPlotly({
    plot_ly(
      pac,
      x = ~ get(x),
      y = ~ get(y),
      color = ~ get(color),
      type = "box"
    ) %>%
      layout(boxmode = "group")
    # plot_ly(pac,x = ~SUM,y = ~SP11, color = pac['wday.lbl'],type = "box")%>%
    #      layout(boxmode = "group")
  })
  
}
#==================================Anomaly detection=======================================

function.MyAnomalyDetection <-
  function(pac, myX, myY, Myperiod, MylastOnly) {
    #View(c(myX,myY,Myperiod,MylastOnly,0))
    #SUM_DATA <- pac[,c(1,12)]
    
    # myX<-paste0(myX)
    # myY<-paste0(myY)
    # Myperiod<-2
    # MylastOnly<-FALSE
    SUM_DATA <-
      pac[, c(grep(paste0(myX), colnames(pac)), grep(paste0(myY), colnames(pac)))]
    #View(SUM_DATA)
    #View(c(myX,myY,Myperiod,MylastOnly,1))
    abc <- as.numeric(rownames(SUM_DATA))
    ggplot(pac, aes(x = myX, y = myY)) + geom_line()
    
    #ggplot(pac, aes(x=Index, y=SP11)) + geom_line()
    #res = AnomalyDetectionTs(SUM_DATA, max_anoms=0.01, direction="pos", plot=TRUE, e_value = T)
    #res = AnomalyDetectionVec(SUM_DATA[,2], max_anoms=0.01, period=96, direction='both',
    #                          only_last=FALSE, plot=TRUE)
    ##View(SUM_DATA)
    
    res = AnomalyDetectionVec(
      SUM_DATA[, 2],
      max_anoms = 0.01,
      period = Myperiod,
      direction = 'both',
      only_last = MylastOnly,
      plot = TRUE
    )
    
    anomaly_table = res$anoms
    #SUM_DATA$Index_row <- abc
    
    SUM_DATA[[paste0(myX, "_row")]] <- abc
    
    #anomaly_table<-merge(SUM_DATA,anomaly_table,by.x = "Index_row",by.y = "index")
    #View(anomaly_table)
    #View(SUM_DATA)
    #View(c(dim(anomaly_table)))
    if (is.null(anomaly_table)) {
      anomaly_table <- anomaly_table
    } else{
      tryCatch({
        anomaly_table <-
          merge(SUM_DATA,
                anomaly_table,
                by.x = paste0(myX, "_row"),
                by.y = "index")
        
      }, error = function(cond) {
        message("Error in anomaly_table")
        anomaly_table
        
      }, warning = function(cond) {
        message("Warning in anomaly_table")
        anomaly_table
      }, finally = {
        message("Warning of anomaly_table")
      })
    }
    
    renderPlotly({
      plot_ly(SUM_DATA, x = ~ get(myX), y = ~ get(myY)) %>%
        add_trace(colors = "orange",
                  name = "Po?etnos? ?ut v ?ase",
                  mode = "lines") %>%
        add_trace(
          y = ~ anoms,
          colors = "gray",
          name = "Anom?lie",
          mode = "markers",
          alpha = 1,
          data = anomaly_table
        ) %>%
        layout(
          title = "Graf",
          xaxis = list(title = "Čas",
                       rangeslider = list(type = "date")),
          yaxis = list(title = "Početnosť áut")
        )
    })
  }



#==================================Motif discovery=======================================

function.MyMotifDiscovery <- function(pac, myX, myY,output) {
  #mx <- as.numeric(unlist(pac[paste0(myX)]))
  #my <- as.numeric(unlist(pac[paste0(myY)]))

  if(is.null(myX) || myX==""){
    myX="SP11"
  }
  if(is.null(myY) || myY==""){
    myY="SP12"
  }
    browser()  
  tsvalx <-
    pac %>% select(paste0(myX)) %>% unlist(use.names = FALSE)
  tsvaly <-
    pac %>% select(paste0(myX)) %>% unlist(use.names = FALSE)
  
  typeof(tsvalx)
  
  tryCatch({
    res.wcc <- Func.motif(
      ts = pac[[paste0(myX)]],
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
    
  },warning=function(cond){
    output$errormsg40 <-
      renderUI(tagList(
        tags$b("Exception in motif's discovery", paste0(cond), style = "color:red")
      ))
    
  },error=function(cond){
    output$errormsg40 <-
      renderUI(tagList(
        tags$b("Exception in motif's discovery ", paste0(cond), style = "color:red")
      ))
    
  })
  
  #res.wcc <- Func.motif(ts = pac$SP11, global.norm = T, local.norm = F, window.size = 24, overlap = 0, w = 6, a = 5, mask.size = 5, max.dist.ratio = 1.2, count.ratio.1 = 1.1, count.ratio.2 = 1.1)
  #res.ahu <- Func.motif(ts = pac$SP12, global.norm = T, local.norm = F, window.size = 24, overlap = 0, w = 6, a = 5, mask.size = 5, max.dist.ratio = 1.2, count.ratio.1 = 1.1, count.ratio.2 = 1.1)
  tryCatch({
    res.ahu <-
      Func.motif(
        ts =  pac[[paste0(myX)]],
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
    
    
  },warning=function(cond){
    output$errormsg40 <-
      renderUI(tagList(
        tags$b("Exception in motif's discovery", paste0(cond), style = "color:red")
      ))
    
  },error=function(cond){
    output$errormsg40 <-
      renderUI(tagList(
        tags$b("Exception in motif's discovery ", paste0(cond), style = "color:red")
      ))
    
  })
  
  
  tryCatch({
    #Visualization
    data.wcc <-
      Func.visual.SingleMotif(
        single.ts =  pac[[paste0(myX)]],
        window.size = 24,
        motif.indices = res.wcc$Indices
      )  
    
  },warning=function(cond){
    output$errormsg40 <-
      renderUI(tagList(
        tags$b("Exception in motif's discovery", paste0(cond), style = "color:red")
      ))
    
  },error=function(cond){
    output$errormsg40 <-
      renderUI(tagList(
        tags$b("Exception in motif's discovery ", paste0(cond), style = "color:red")
      ))
    
  })
  
  
  tryCatch({
    
  data.ahu <-
    Func.visual.SingleMotif(
      single.ts =  pac[[paste0(myY)]],
      window.size = 24,
      motif.indices = res.ahu$Indices
    )
  
},warning=function(cond){
  output$errormsg40 <-
    renderUI(tagList(
      tags$b("Exception in motif's discovery", paste0(cond), style = "color:red")
    ))
  
},error=function(cond){
  output$errormsg40 <-
    renderUI(tagList(
      tags$b("Exception in motif's discovery ", paste0(cond), style = "color:red")
    ))
  
})
  #Determine the total number of motifs discovered in the time series of WCC
  tryCatch({
    n <- length(unique(data.wcc$data.1$Y))
    
  },warning=function(cond){
    output$errormsg40 <-
      renderUI(tagList(
        tags$b("Exception in motif's discovery", paste0(cond), style = "color:red")
      ))
    
  },error=function(cond){
    output$errormsg40 <-
      renderUI(tagList(
        tags$b("Exception in motif's discovery ", paste0(cond), style = "color:red")
      ))
    
  })
  #Make the plot
  tryCatch({
    p <- ggplot(data = data.wcc$data.1) +
      geom_line(aes(x = 1:dim(data.wcc$data.1)[1], y = X)) +
      geom_point(aes(
        x = 1:dim(data.wcc$data.1)[1],
        y = X,
        color = Y,
        shape = Y
      )) +
      scale_shape_manual(values = seq(from = 1, to = n)) +
      guides(shape = guide_legend(nrow = 2)) +
      xlab("Time (15-min)") + ylab("Počet áut") +
      theme(
        panel.background = element_rect(fill = "white", colour = "black"),
        legend.position = "top",
        legend.title = element_blank()
      )
    
  },warning=function(cond){
    output$errormsg40 <-
      renderUI(tagList(
        tags$b("Exception in motif's discovery", paste0(cond), style = "color:red")
      ))
    
  },error=function(cond){
    output$errormsg40 <-
      renderUI(tagList(
        tags$b("Exception in motif's discovery ", paste0(cond), style = "color:red")
      ))
    
  })  
  
  output$plot41 <- renderPlotly(ggplotly(p))
}

#=========================MACHINE LEARNING========================

function.MyMachineLearning <- function(pac, my_track, output) {
  #browser()
  if (my_track == "") {
    my_track = "SUM"
  }
  SUM_DATA <- pac[, c(1, grep(paste0(my_track), colnames(pac)))]
  
  #SUM_DATA <- pac[,c(1,31)]
  
  SUM_DATA %>% glimpse()
  beer_sales_tbl_aug <- SUM_DATA %>%
    tk_augment_timeseries_signature()
  
  beer_sales_tbl_aug %>% glimpse()
  
  beer_sales_tbl_clean <- beer_sales_tbl_aug %>%
    select_if( ~ !is.POSIXct(.)) %>%
    select_if( ~ !any(is.na(.))) %>%
    mutate_if(is.ordered, ~ as.character(.) %>% as.factor)
  
  beer_sales_tbl_clean %>% glimpse()
  #browser()
  beer_sales_tbl_clean[, 1] <-
    as.integer(beer_sales_tbl_clean %>% select(paste0(my_track)) %>% unlist(use.names =
                                                                              FALSE))
  
  #beer_sales_tbl_clean[,1] <- as.integer(beer_sales_tbl_clean$SUM)
  
  #beer_sales_tbl_clean$month.lbl <- NULL
  #beer_sales_tbl_clean$wday.lbl <- NULL
  split <- round(nrow(beer_sales_tbl_clean) * .70)
  datat_to <- beer_sales_tbl_clean[1:split, ]
  actuals_tbl <-
    beer_sales_tbl_clean[(split + 1):nrow(beer_sales_tbl_clean), ]
  split2 <- round(nrow(actuals_tbl) * .66)
  data_beet <- actuals_tbl[1:split2, ]
  data_end <- actuals_tbl[(split2 + 1):nrow(actuals_tbl), ]
  
  
  
  train_tbl <- datat_to
  valid_tbl <- data_beet
  test_tbl  <- data_end
  
  
  #h2o.init()
  #h2o.no_progress()
  train_h2o <- as.h2o(train_tbl)
  valid_h2o <- as.h2o(valid_tbl)
  test_h2o  <- as.h2o(test_tbl)
  
  #y <- "SUM"
  y <- paste0(my_track)
  x <- setdiff(names(train_h2o), y)
  
  tryCatch({
    # linear regression model used, but can use any model
    automl_models_h2o <- h2o.automl(
      x = x,
      y = y,
      training_frame = train_h2o,
      validation_frame = valid_h2o,
      leaderboard_frame = test_h2o,
      max_runtime_secs = 60,
      stopping_metric = "deviance"
    )
    
    automl_leader <- automl_models_h2o@leader
    
    pred_h2o <- h2o.predict(automl_leader, newdata = test_h2o)
    
    h2o.performance(automl_leader, newdata = test_h2o)
    
    
    
    
    error_tbl <- SUM_DATA %>%
      tail(SUM_DATA, n = sum(nrow(data_end))) %>%
      add_column(pred = pred_h2o %>% as.tibble() %>% pull(predict)) %>%
      rename_(actual = paste0(my_track)) %>%
      mutate(error     = actual - pred,
             error_pct = error / actual)
    output$table53 <- DT::renderDataTable({
      error_tbl
    })
    
    
    error_tbl %>%
      summarise(
        me   = mean(error),
        rmse = mean(error ^ 2) ^ 0.5,
        mae  = mean(abs(error)),
        mape = mean(abs(error_pct)),
        mpe  = mean(error_pct)
      ) %>%
      glimpse()
    
    
    
    
    
    
    p <- SUM_DATA %>%
      #ggplot(aes(x = Index, y = SUM)) +
      ggplot(aes_string(x = paste0("Index"), y = paste0(my_track))) +
      # Data - Spooky Orange
      geom_point(
        size = 2,
        color = "gray",
        alpha = 0.5,
        shape = 21,
        fill = "orange"
      ) +
      geom_line(color = "orange", size = 0.5) +
      geom_ma(n = 1, color = "black") +
      # Predictions - Spooky Purple
      geom_point(
        aes(y = pred),
        size = 2,
        color = "gray",
        alpha = 1,
        shape = 21,
        fill = "purple",
        data = error_tbl
      ) +
      geom_line(aes(y = pred),
                color = "purple",
                size = 0.5,
                data = error_tbl) +
      # Aesthetics
      
      labs(title = "Graf",
           subtitle = "Algotimus H2O mal najvyššiu prestnosť MAPE:9,6%")
    output$plot51 <- renderPlotly(ggplotly(p))
    
    output$plot52 <- renderPlotly(
      plot_ly(SUM_DATA, x = ~ Index, y = ~ get(my_track)) %>%
        add_trace(
          colors = "orange",
          name = "Početnosť áut v čase",
          mode = "lines"
        ) %>%
        add_trace(
          y = ~ pred,
          colors = "gray",
          name = "Predikované hodnoty",
          mode = "lines+markers",
          alpha = 1,
          data = error_tbl
        ) %>%
        layout(
          title = "Graf",
          xaxis = list(title = "Čas",
                       rangeslider = list(type = "date")),
          yaxis = list(title = "Početnosť áut")
        )
    )
    
  }, error = function(cond) {
    output$errormsg50 <-
      renderUI(tagList(
        tags$b("Exception in model building", paste0(cond), style = "color:red")
      ))
    
  }, warning = function(cond) {
    output$errormsg50 <-
      renderUI(tagList(
        tags$b("Exception in model building", paste0(cond), style = "color:red")
      ))
    
  })
  
  #return(list(myggplot,myplotlyplot,my_error_table))
}

#====================================timetk + linear regression: MAPE = 4.3% (timetk demo)==================================

## Linear Regression ##
#====================================timetk + linear regression: MAPE = 4.3% (timetk demo)==================================
function.MyLinearRegression <- function(pac, my_track, output) {
  #browser()
  tryCatch({
    #SUM_DATA <- pac[, c(1, 31)]
    message(my_track)
    if (is.null(my_track) || my_track == "") {
      my_track = "SUM"
    }
    SUM_DATA <- pac[, c(1, grep(paste0(my_track), colnames(pac)))]
    
    SUM_DATA %>%
      tk_index() %>%
      tk_get_timeseries_summary() %>%
      glimpse()
    
    beer_sales_tbl_aug <- SUM_DATA %>%
      tk_augment_timeseries_signature()
    
    beer_sales_tbl_aug
    beer_sales_tbl_aug <- na.omit(beer_sales_tbl_aug)
    
    (l <- sapply(beer_sales_tbl_aug, function(x)
      is.factor(x)))
    m <- beer_sales_tbl_aug[, l]
    ifelse(n <-
             sapply(m, function(x)
               length(levels(x))) == 1, "DROP", "NODROP")
    
    #beer_sales_tbl_aug %>% select(paste0("Index")) %>% unlist(use.names = FALSE)
    #fit_lm <-lm(SUM ~ ., data = select(beer_sales_tbl_aug, -c(Index, diff, month.lbl)))
    lmx = as.formula(paste0(my_track, " ~ ", paste(".", collapse = "+")))
    fit_lm <-
      lm(lmx, data = select(beer_sales_tbl_aug,-c(Index, diff, month.lbl)))
    tryCatch({
      summary(fit_lm)
      #na.omit(fit_lm)
      
    }, warning = function(cond) {
      message(paste(cond))
    }, error = function(cond) {
      message(paste(cond))
    })
    
    beer_sales_idx <- SUM_DATA %>% tk_index()
    
    tail(beer_sales_idx)
    
    Sys.setenv(TZ = "America/Toronto")
    # Make future index
    future_idx <- beer_sales_idx %>%
      tk_make_future_timeseries(n_future = 10)
    
    future_idx
    
    new_data_tbl <- future_idx %>%
      tk_get_timeseries_signature()
    
    new_data_tbl
    
    tryCatch({
      # Make predictions
      
      pred <-
        predict(fit_lm, newdata = select(new_data_tbl,-c(index, diff)))
      
    }, warning = function(cond) {
      message(paste(cond))
    }, error = function(cond) {
      message(paste(cond))
    })
    
    tryCatch({
      predictions_tbl <- tibble(Index  = future_idx,
                                value = pred)
      
    }, warning = function(cond) {
      message(paste(cond))
    }, error = function(cond) {
      message(paste(cond))
    })
    
    predictions_tbl
    
    
    split <- round(nrow(SUM_DATA) * .90)
    datat_to <- SUM_DATA[1:split, ]
    actuals_tbl <- SUM_DATA[(split + 1):nrow(SUM_DATA), ]
    #colnames(actuals_tbl)[2] <- "value"
    
    
    p <-
      ggplot(SUM_DATA, aes_string(x = paste0("Index"), y = paste0(my_track))) +
      # Training data
      geom_line(color = palette_light()[[1]]) +
      geom_point(color = palette_light()[[1]]) +
      # Predictions
      geom_line(aes(y = value), color = palette_light()[[4]], data = predictions_tbl) +
      geom_point(aes(y = value), color = palette_light()[[4]], data = predictions_tbl) +
      # Actuals
      geom_line(aes_string(y = paste0(my_track)),
                color = palette_light()[[3]],
                data = actuals_tbl) +
      geom_point(aes_string(y = paste0(my_track)),
                 color = palette_light()[[3]],
                 data = actuals_tbl) +
      #theme_tq() +
      labs(title = "Time series sum data")
    ggplotly(p)
    output$plot61 <- renderPlotly(ggplotly(p))
    
    #
    # error_tbl <- left_join(actuals_tbl, predictions_tbl) %>%
    #   rename_(actual = paste0(my_track), pred = paste0(value)) %>%
    #   mutate(error     = actual - pred,
    #          error_pct = error / actual)
    
    error_tbl <- left_join(actuals_tbl, predictions_tbl) %>%
      rename_(actual = paste0(my_track)) %>%
      mutate(error     = actual - pred,
             error_pct = error / actual)
    
    error_tbl
    
    
    
    output$table62 <- DT::renderDataTable(datatable(error_tbl))
    
    # Calculating test error metrics
    test_residuals <- error_tbl$error
    test_error_pct <- error_tbl$error_pct * 100 # Percentage error
    
    me   <- mean(test_residuals, na.rm = TRUE)
    rmse <- mean(test_residuals ^ 2, na.rm = TRUE) ^ 0.5
    mae  <- mean(abs(test_residuals), na.rm = TRUE)
    mape <- mean(abs(test_error_pct), na.rm = TRUE)
    mpe  <- mean(test_error_pct, na.rm = TRUE)
    
    tibble(me, rmse, mae, mape, mpe) %>% glimpse()
    
    #Warning message:
    #In tk_xts_.data.frame(ret, select = select, silent = silent) :
    #Non-numeric columns being dropped: Index
    tryCatch({
      # Coerce to xts
      beer_sales_xts <- tk_xts(SUM_DATA)
    }, warning = function(cond) {
      message(paste(cond))
    })
    
    # Show the first six rows of the xts object
    beer_sales_xts %>%
      head()
    tk_tbl(beer_sales_xts, rename_index = "date")
    
    tryCatch({
      # Coerce to ts
      beer_sales_ts <- tk_ts(SUM_DATA)
      
    }, warning = function(cond) {
      message(paste(cond))
    })
    
    # Show the calendar-printout
    beer_sales_ts
    tk_tbl(beer_sales_ts, rename_index = "date")
    
    has_timetk_idx(beer_sales_ts)
    
    # If timetk_idx is present, can get original dates back
    tk_tbl(beer_sales_ts,
           timetk_idx = TRUE,
           rename_index = "date")
  }, error = function(cond) {
    message(paste(cond))
    output$errormsg60 <-
      renderUI(tagList(
        tags$b("Exception in model building", paste0(cond), style = "color:red")
      ))
  }, warning = function(cond) {
    output$errormsg60 <-
      renderUI(tagList(
        tags$b("Exception in model building", paste0(cond), style = "color:red")
      ))
  })
  
}
#====================================ARIMA + sweep: MAPE = 4.3% (sweep demo)==================================

function.MyArima <- function(pac, my_track, output) {
  #browser()
  tryCatch({
    if (is.null(my_track) || my_track == "") {
      my_track = "SUM"
    }
    SUM_DATA <- pac[, c(1, grep(paste0(my_track), colnames(pac)))]
    
    #SUM_DATA <- pac[, c(1, 31)]
    tryCatch({
      p <- SUM_DATA %>%
        ggplot(aes_string("Index", paste0(my_track))) +
        geom_line(col = palette_light()[1]) +
        geom_point(col = palette_light()[1]) +
        geom_ma(ma_fun = SMA,
                n = 12,
                size = 1) +
        theme_tq() +
        labs(title = "Beer Sales: 2007 through 2016")
      
    }, error = function(cond) {
      message("Exception ", paste(cond))
    }, warning = function(cond) {
      message("Exception ", paste(cond))
    })
    output$plot71 <- renderPlotly(ggplotly(p))
    tryCatch({
      beer_sales_ts <- tk_ts(SUM_DATA)
      beer_sales_ts
      has_timetk_idx(beer_sales_ts)
      
    }, error = function(cond) {
      message("Exception ", paste(cond))
    }, warning = function(cond) {
      message("Exception ", paste(cond))
    })
    tryCatch({
      fit_arima <- auto.arima(beer_sales_ts)
      
      fit_arima
      
    }, error = function(cond) {
      message("Exception ", paste(cond))
    }, warning = function(cond) {
      message("Exception ", paste(cond))
    })
    
    
    # sw_tidy - Get model coefficients
    sw_tidy(fit_arima)
    # sw_glance - Get model description and training set accuracy measures
    sw_glance(fit_arima) %>%
      glimpse()
    
    # sw_augment - get model residuals
    sw_augment(fit_arima, timetk_idx = TRUE)
    
    tryCatch({
      p <- sw_augment(fit_arima, timetk_idx = TRUE) %>%
        ggplot(aes(x = index, y = .resid)) +
        geom_point() +
        geom_hline(yintercept = 0, color = "red") +
        labs(title = "Residual diagnostic") +
        theme_tq()
      
      
      }, error = function(cond) {
      message("Exception ", paste(cond))
    }, warning = function(cond) {
      message("Exception ", paste(cond))
    })
    output$plot72 <- renderPlotly(ggplotly(p))
    
    #browser()
    # Forecast next 12 months
    fcast_arima <- forecast(fit_arima, h = 100)
    class(fcast_arima)
    
    # Check if object has timetk index
    has_timetk_idx(fcast_arima)
    
    # sw_sweep - tidies forecast output
    fcast_tbl <- sw_sweep(fcast_arima, timetk_idx = TRUE)
    
    fcast_tbl
    
    
    
    tryCatch({
      # Visualize the forecast with ggplot
      p <- fcast_tbl %>%
        ggplot(aes_string(x = "index", y = paste0(my_track), color = paste0("key"))) +
        # 95% CI
        geom_ribbon(
          aes(ymin = lo.95, ymax = hi.95),
          fill = "#D5DBFF",
          color = NA,
          size = 0
        ) +
        # 80% CI
        geom_ribbon(
          aes(
            ymin = lo.80,
            ymax = hi.80,
            fill = key
          ),
          fill = "#596DD5",
          color = NA,
          size = 0,
          alpha = 0.8
        ) +
        # Prediction
        geom_line() +
        geom_point() +
        # Actuals
        geom_line(aes_string(x = "Index", y = paste0(my_track)),
                  color = palette_light()[[1]],
                  data = actuals_tbl) +
        geom_point(aes_string(x = "Index", y = paste0(my_track)),
                   color = palette_light()[[1]],
                   data = actuals_tbl) +
        # Aesthetics
        labs(
          title = "Beer Sales Forecast: ARIMA",
          x = "",
          y = "Thousands of Tons",
          subtitle = "sw_sweep tidies the auto.arima() forecast output"
        ) +
        scale_color_tq() +
        scale_fill_tq() +
        theme_tq()
    }, error = function(cond) {
      message("Exception ", paste(cond))
    }, warning = function(cond) {
      message("Exception ", paste(cond))
    })
    
    #ggplotly(p)
    output$plot73 <- renderPlotly(ggplotly(p))
    
    
    
    tryCatch({
      # error_tbl <-
      #   left_join(actuals_tbl, fcast_tbl, by = c("Index" = "index")) %>%
      #   rename_(actual = SUM.x, pred = SUM.y) %>%
      #   select(Index, actual, pred) %>%
      #   mutate(error     = actual - pred,
      #          error_pct = error / actual)
      # error_tbl
      error_tbl <-
        left_join(actuals_tbl, fcast_tbl, by = c("Index" = "index")) %>%
        rename_(actual = paste0(my_track,".","x")) %>%
        select_("Index", "actual", "pred") %>%
        mutate(error     = actual - pred,
               error_pct = error / actual)
      
    }, error = function(cond) {
      message("Exception ", paste(cond))
    }, warning = function(cond) {
      message("Exception ", paste(cond))
    })
    error_tbl
    
    output$table74 <- DT::renderDataTable(error_tbl)
    
    
    na.omit(error_tbl)
    
    
    # Calculate test error metrics
    test_residuals <- error_tbl$error
    test_error_pct <- error_tbl$error_pct * 100 # Percentage error
    
    me   <- mean(test_residuals, na.rm = TRUE)
    rmse <- mean(test_residuals ^ 2, na.rm = TRUE) ^ 0.5
    mae  <- mean(abs(test_residuals), na.rm = TRUE)
    mape <- mean(abs(test_error_pct), na.rm = TRUE)
    mpe  <- mean(test_error_pct, na.rm = TRUE)
    
    tibble(me, rmse, mae, mape, mpe) %>% glimpse()
  }, error = function(cond) {
    message(paste(cond))
    output$errormsg70 <-
      renderUI(tagList(
        tags$b("Exception in model building", paste0(cond), style = "color:red")
      ))
  }, warning = function(cond) {
    output$errormsg70 <-
      renderUI(tagList(
        tags$b("Exception in model building", paste0(cond), style = "color:red")
      ))
  })
  
  
  
}