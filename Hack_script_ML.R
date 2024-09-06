install.packages("lubridate")
install.packages("zoo")
library(lubridate)
library(zoo)

#EDA analysis


#next step 
install.packages("forecast")
library(forecast)

# Fit the SARIMA model


# Assuming Training_data is your dataframe
# First, ensure the dataframe is sorted by year and month
Training_data <- Training_data[order(Training_data$Jahr, Training_data$Monat),]

# Now, create a univariate time series object from the 'n' column
# The frequency is 12 because data is monthly
ts_data <- ts(Training_data$n, start = c(min(Training_data$Jahr), min(Training_data$Monat)), frequency = 12)

# Now you can apply auto.arima on the time series object
library(forecast)
model <- auto.arima(ts_data, seasonal = TRUE)

# Check the summary of the fitted model
summary(model)

#Forecast the values

# Load the forecast library
install.packages("forecast")
library(forecast)

# Forecast the next 12 months using the model you've fitted
future_forecasts <- forecast(model, h = 12)

# Print the forecasted values
print(future_forecasts)

# Plot the forecasts
plot(future_forecasts)


#TRY WITH TRAINING DATA2 (WITHOUT 2023)

# Assuming Training_data2 is your dataframe
# First, ensure the dataframe is sorted by year and month
Training_data2 <- Training_data2[order(Training_data2$Jahr, Training_data2$Monat),]

# Now, create a univariate time series object from the 'n' column
# The frequency is 12 because data is monthly
ts_data2 <- ts(Training_data2$n, start = c(min(Training_data2$Jahr), min(Training_data2$Monat)), frequency = 12)

# Now you can apply auto.arima on the time series object
library(forecast)
model2 <- auto.arima(ts_data2, seasonal = TRUE)

# Check the summary of the fitted model
summary(model)

#Forecast the values

# Load the forecast library
install.packages("forecast")
library(forecast)

# Forecast the next 12 months using the model you've fitted
future_forecasts <- forecast(model2, h = 12)

# Print the forecasted values
print(future_forecasts)

# Plot the forecasts
plot(future_forecasts)
