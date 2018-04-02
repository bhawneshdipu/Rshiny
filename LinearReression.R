## Linear Regression ##


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

Sys.setenv(TZ = "America/Toronto")
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
