# Topic: Forecasting using "fable"
# Haseeb Mahmud
# R Coffee Break: Wiesbaden R User Group
# Code: https://github.com/haseebmahmud/FableDemo

# As a start, you may have a look here [1] to see the list of time series related 
# packages available by category. This list is maintained by 
# Prof. Rob Hyndman.

# The example is copied from Mitchell O'Hara-Wild [3] and package vignettes.

# Loading packages ----

library(fable)
library(tidyverse)
library(tsibble)

# DATA ----
# Tourism data from tsibble package [4]

glimpse(tourism)

tourism_aus <- tourism %>% 
  summarise(Trips = sum(Trips))

tourism_aus

# Plot the time series ==== 

tourism_aus %>% 
  forecast::autoplot(Trips)


# SINGLE MODEL: ARIMA ----

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

# Accuracy measures ====

accuracy(fit_multi)

# COMBINATION OF FORECASTS ----

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

# Accuracy measures ====

accuracy(fit_comb)

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

fit_scale <- tourism_state %>% 
  model(
    ets = ETS(Trips),
    arima = ARIMA(Trips)
  ) %>% 
  mutate(
    average = (ets + arima)/2
  )
fit_scale

# Generating forecasts ====

fit_scale %>% 
  forecast(h = "2 years") %>% 
  autoplot(tourism_state, level = NULL)

# Accuracy measures ====

accuracy(fit_scale)

# Advantages and weak points ----
# Extensions: fabletools [5]
# Extensions: fable.prophet [6]
# Ensemble methods
# ML methods(?)

# References ----

# [1] https://cran.r-project.org/web/views/TimeSeries.html
# [2] https://robjhyndman.com/
# [3] https://www.mitchelloharawild.com/blog/fable/
# [4] https://github.com/tidyverts/tsibble
# [5] https://cran.r-project.org/web/packages/fabletools/index.html
# [6] https://cran.r-project.org/web/packages/fable.prophet/index.html