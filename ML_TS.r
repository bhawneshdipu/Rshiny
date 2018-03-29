library(readr)
library(ggplot2)
library(plotly)
library(stringr)
library(DataLoader)
library(ggplot2)
library(forecast)
library(tseries)
library(zoo)
library(xts)
library(fts)
library(MASS)
library(caret)
library(e1071)
library(dplyr)
#library(h2o)        # Awesome ML Library
#library(timetk)     # Toolkit for working with time series in R
library(tidyquant)
library(sweep) 
library(lubridate)




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
h2o.no_progress()
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
ggplotly(p) 

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


#====================================ARIMA + sweep: MAPE = 4.3% (sweep demo)==================================



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

