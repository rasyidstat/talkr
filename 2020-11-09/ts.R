# install.packages("gtrendsR")

library(forecast)
library(xts)
library(tidyverse)
library(gtrendsR)
library(lubridate)
library(plotly)


# Get the data
obj <- gtrends(keyword = "baju", geo = "ID", time = "all")
ts_tbl <- obj$interest_over_time
p <- ts_tbl %>%
  ggplot(aes(date, hits)) +
  geom_line() +
  theme_minimal() +
  labs(x = "Month",
       y = "Hits")
ggplotly(p)


# Take only upto 2016
ts_tbl <- ts_tbl %>%
  filter(year(date) <= 2016)


# Split train and test
train <- ts_tbl %>%
  filter(year(date) < 2016)
test <- ts_tbl %>%
  filter(year(date) >= 2016)


# Build time series object
train_ts <- train %>%
  select(hits) %>%
  ts(start = c(year(min(train$date)), month(min(train$date))),
     end = c(year(max(train$date)), month(max(train$date))),
     frequency = 12)
train_ts
test_ts <- test %>%
  select(hits) %>%
  ts(start = c(year(min(test$date)), month(min(test$date))),
     end = c(year(max(test$date)), month(max(test$date))),
     frequency = 12)


# Model building
snaive_mdl <- snaive(train_ts, h = 12)
arima_mdl <- auto.arima(train_ts)
arima_mdl
ets_mdl <- ets(train_ts)
ets_mdl


# Model evaluation
forecast_to_eval <- function(mdl, model_name = "model_name", h = 12) {
  eval <- forecast(mdl, h = h) %>%
    accuracy(test_ts) %>%
    as.data.frame() %>%
    rownames_to_column(var = "dataset") %>%
    mutate(model = model_name) %>%
    select(model, dataset, RMSE, MAE, MAPE, MASE)
  eval
}
forecast_to_tbl <- function(mdl, model_name = "model_name", h = 12) {
  data.frame(date = seq.Date(as.Date(min(test$date)),
                             as.Date(max(test$date)),
                             "month")) %>%
      mutate(hits = forecast(mdl, h = h)$mean,
             cat = model_name)
}

bind_rows(
  forecast_to_eval(snaive_mdl, "sNaive"),
  forecast_to_eval(arima_mdl, "ARIMA"),
  forecast_to_eval(ets_mdl, "ETS")
) %>%
  filter(dataset == "Test set")

pred <- train %>%
  mutate(cat = "Training set") %>%
  bind_rows(
    test %>%
      mutate(cat = "Test set")
  ) %>%
  select(date, hits, cat) %>%
  bind_rows(
    forecast_to_tbl(snaive_mdl, "sNaive"),
    forecast_to_tbl(arima_mdl, "ARIMA"),
    forecast_to_tbl(ets_mdl, "ETS")
  )
p <- pred %>%
  ggplot(aes(date, hits, color = cat)) +
  geom_line() +
  theme_minimal() +
  labs(x = "Month",
       y = "Hits")
ggplotly(p)
