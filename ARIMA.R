## ARIMA ##



#====================================ARIMA + sweep: MAPE = 4.3% (sweep demo)==================================



SUM_DATA <- pac[,c(1,31)]
p<-SUM_DATA %>%
  ggplot(aes(Index, SUM)) +
  geom_line(col = palette_light()[1]) +
  geom_point(col = palette_light()[1]) +
  geom_ma(ma_fun = SMA, n = 12, size = 1) +
  theme_tq() %>%
  labs(title = "Beer Sales: 2007 through 2016")
ggplotly(p)

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

p<-sw_augment(fit_arima, timetk_idx = TRUE) %>%
  ggplot(aes(x = index, y = .resid)) +
  geom_point() + 
  geom_hline(yintercept = 0, color = "red") + 
  labs(title = "Residual diagnostic") +
  theme_tq()
ggplotly(p)
# Forecast next 12 months
fcast_arima <- forecast(fit_arima, h = 100)
class(fcast_arima)

# Check if object has timetk index 
has_timetk_idx(fcast_arima)

# sw_sweep - tidies forecast output
fcast_tbl <- sw_sweep(fcast_arima, timetk_idx = TRUE)

fcast_tbl




# Visualize the forecast with ggplot
p<-fcast_tbl %>%
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



ggplotly(p)

error_tbl <- left_join(actuals_tbl, fcast_tbl, by = c("Index" = "index"))%>% 
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

