## Lib.R ##

library(DT)
library(readr)
library(plotly)
library(stringr)
library(DataLoader)
library(forecast)
library(tseries)
library(zoo)
library(xts)
library(fts)
library(MASS)
library(caret)
library(e1071)
library(dplyr)
library(h2o)        # Awesome ML Library
library(timetk)     # Toolkit for working with time series in R
library(tidyquant)
library(anomalyDetection)
library(TSMining)
library(randomForest)
#install.packages("devtools")
#devtools::install_github("twitter/AnomalyDetection")
library(AnomalyDetection)
packageVersion('plotly')
#devtools::install_github("tidyverse/ggplot2")
#devtools::install_github('hadley/ggplot2')
library(ggplot2)




## Machine Learning ##


#====================================timetk + h2o: MAPE = 3.9% (This demo)==================================





#beer_sales_tbl <- tq_get("S4248SM144NCEN", get = "economic.data", from = "2010-01-01", to = "2017-10-27")

SUM_DATA <- pac[,c(1,31)]

SUM_DATA %>% glimpse()
beer_sales_tbl_aug <- SUM_DATA %>%
  tk_augment_timeseries_signature()

beer_sales_tbl_aug %>% glimpse()

beer_sales_tbl_clean <- beer_sales_tbl_aug %>%
  select_if(~ !is.POSIXct(.)) %>%
  select_if(~ !any(is.na(.))) %>%
  mutate_if(is.ordered, ~ as.character(.) %>% as.factor)

beer_sales_tbl_clean %>% glimpse()
beer_sales_tbl_clean[,1] <- as.integer(beer_sales_tbl_clean$SUM)
#beer_sales_tbl_clean$month.lbl <- NULL
#beer_sales_tbl_clean$wday.lbl <- NULL
split <- round(nrow(beer_sales_tbl_clean) * .70)
datat_to <- beer_sales_tbl_clean[1:split,]
actuals_tbl <- beer_sales_tbl_clean[(split + 1):nrow(beer_sales_tbl_clean),]
split2 <- round(nrow(actuals_tbl) * .66)
data_beet <- actuals_tbl[1:split2,]
data_end <- actuals_tbl[(split2 + 1):nrow(actuals_tbl),]



train_tbl <- datat_to
valid_tbl <- data_beet
test_tbl  <- data_end


h2o.init()
#h2o.no_progress()
train_h2o <- as.h2o(train_tbl)
valid_h2o <- as.h2o(valid_tbl)
test_h2o  <- as.h2o(test_tbl)

y <- "SUM"
x <- setdiff(names(train_h2o), y)

# linear regression model used, but can use any model
automl_models_h2o <- h2o.automl(
  x = x, 
  y = y, 
  training_frame = train_h2o, 
  validation_frame = valid_h2o, 
  leaderboard_frame = test_h2o, 
  max_runtime_secs = 60, 
  stopping_metric = "deviance")

automl_leader <- automl_models_h2o@leader

pred_h2o <- h2o.predict(automl_leader, newdata = test_h2o)

h2o.performance(automl_leader, newdata = test_h2o)




error_tbl <- SUM_DATA %>% 
  tail(SUM_DATA,n=sum(nrow(data_end)))%>% 
  add_column(pred = pred_h2o %>% as.tibble() %>% pull(predict)) %>%
  rename(actual = SUM) %>%
  mutate(
    error     = actual - pred,
    error_pct = error / actual
  ) 
error_tbl


error_tbl %>%
  summarise(
    me   = mean(error),
    rmse = mean(error^2)^0.5,
    mae  = mean(abs(error)),
    mape = mean(abs(error_pct)),
    mpe  = mean(error_pct)
  ) %>%
  glimpse()






p<-SUM_DATA %>%
  ggplot(aes(x = Index, y = SUM)) +
  # Data - Spooky Orange
  geom_point(size = 2, color = "gray", alpha = 0.5, shape = 21, fill = "orange") +
  geom_line(color = "orange", size = 0.5) +
  geom_ma(n = 1, color = "black")+ 
  # Predictions - Spooky Purple
  geom_point(aes(y = pred), size = 2, color = "gray", alpha = 1, shape = 21, fill = "purple", data = error_tbl) +
  geom_line(aes(y = pred), color = "purple", size = 0.5, data = error_tbl) +
  # Aesthetics
  
  labs(
    title = "Graf",
    subtitle = "Algotimus H2O mal najvyššiu prestnosť MAPE:9,6%"
  )
ggplotly(p)

plot_ly(SUM_DATA, x = ~Index, y = ~SUM)%>%
  add_trace(colors = "orange",name = "Početnosť áut v čase",mode = "lines")%>%
  add_trace(y = ~pred, colors = "gray",name = "Predikované hodnoty", mode = "lines+markers", alpha = 1,data = error_tbl)%>%
  layout(title = "Graf",
         xaxis = list(title = "Čas",
                      rangeslider = list(type = "date")),
         yaxis = list(title = "Početnosť áut"))





