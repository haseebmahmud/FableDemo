# Topic: Forecasting using "fable"
# Haseeb Mahmud
# R Coffee Break: Wiesbaden R User Group

# As a start, you may have a look here [1] to see the list of time series related 
# packages available by category. This list is maintained by 
# Prof. Rob Hyndman.


# The example is copied from Mitchell O'Hara-Wild [3] and package vignettes.


# Loading packages ----

library(fable)
library(tidyverse)
library(tsibble)

# DATA ----

glimpse(tourism)

tourism_aus <- tourism %>% 
  summarise(Trips = sum(Trips))

tourism_aus

# Plot the time series ==== 

tourism_aus %>% 
  forecast::autoplot(Trips)

# SINGLE MODEL ----

# Let's use ARIMA to demonstrate our first example using a single series,

# Fitting the model ====

fit_single <- tourism_aus %>% 
  model(auto_arima = ARIMA(Trips))
fit_single

# Report the model fit ====
report(fit_single)

# Generate forecasts ====

fc_single <- fit_single %>% 
  forecast(h = "2 years")
fc_single

# Accuracy measures ====

accuracy(fit_single)

# Plot forecasts ====

fc_single %>% 
  forecast::autoplot(tourism_aus)

# MULTIPLE MODEL ----

# Fitting the model ====

fit_multi <- tourism_aus %>% 
  model(
    ets      = ETS(Trips),
    arima    = ARIMA(Trips),
    theta    = THETA(Trips),
#    nnetar   = NNETAR(Trips),
    snaive   = SNAIVE(Trips),
    lm       = TSLM(Trips ~ trend() + season())
  )

# Generate forecasts ====

fit_multi %>% 
  forecast(h = "2 years") %>% 
  autoplot(tourism_aus, level = 80, alpha = 0.5)

# Accuracy measures (in-sample) ====

accuracy(fit_multi)

# Accuracy measures (out-of-sample) ====

fc_multi_out <- tourism_aus %>% 
  # Withhold the last 3 years before fitting the model
  filter(Quarter < yearquarter("2015 Q1")) %>% 
  # Estimate the models on the training data (1998-2014)
  model(
    ets      = ETS(Trips),
    arima    = ARIMA(Trips),
    theta    = THETA(Trips),
    # nnetar   = NNETAR(Trips),
    snaive   = SNAIVE(Trips),
    lm       = TSLM(Trips ~ trend() + season())
  ) %>% 
  # Forecast the witheld time peroid (2015-2017)
  forecast(h = "3 years")

fc_multi_out %>% 
  # Compute accuracy of the forecasts relative to the actual data 
  accuracy(tourism_aus)

# COMBINATION OF FORECASTS

# Fitting the model ====

fit_comb <- tourism_aus %>% 
  model(
    ets = ETS(Trips),
    arima = ARIMA(Trips)
  ) %>% 
  mutate(
    average = (ets + arima) / 2
  )

# Generate forecasts ====

fit_comb %>% 
  forecast(h = "2 years") %>% 
  autoplot(tourism_aus, level = 80, alpha = 0.5)

# FORECASTING AT SCALE ----

# Getting the data ready ====

tourism_state <- tourism %>% 
  group_by(State) %>% 
  summarise(Trips = sum(Trips))
tourism_state

# Plot the time series ====

tourism_state %>% 
  autoplot(Trips)

# Fitting the model ====

fit <- tourism_state %>% 
  model(
    ets = ETS(Trips),
    arima = ARIMA(Trips)
  ) %>% 
  mutate(
    average = (ets + arima)/2
  )
fit

# Generating forecasts ====

fit %>% 
  forecast(h = "2 years") %>% 
  autoplot(tourism_state, level = NULL)

# References ----

# [1] https://cran.r-project.org/web/views/TimeSeries.html
# [2] https://robjhyndman.com/
# [3] https://www.mitchelloharawild.com/blog/fable/