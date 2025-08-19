---
title: "Forecast daily bike rental demand using time series models"
date: "`r Sys.Date(22-Jul-2025)`"
output: html_document
author: "Shivam Srivastava"
---

# About Data Analysis Report

This RMarkdown file contains the report of the data analysis done for the project on forecasting daily bike rental demand using time series models in R. It contains analysis such as data exploration, summary statistics and building the time series models. The final report was completed on Tue Jul 22 15:46:04 2025.
**Data Description:**

This dataset contains the daily count of rental bike transactions between years 2011 and 2012 in Capital bikeshare system with the corresponding weather and seasonal information.

**Data Source:** https://archive.ics.uci.edu/ml/datasets/bike+sharing+dataset

**Relevant Paper:** 

Fanaee-T, Hadi, and Gama, Joao. Event labeling combining ensemble detectors and background knowledge, Progress in Artificial Intelligence (2013): pp. 1-15, Springer Berlin Heidelberg



# Task One: Load and explore the data

## Load data and install packages


## Import required packages
```{r}
install.packages("tidyverse")
install.packages("forecast")
install.packages("lubridate")
install.packages("ggplot2")
install.packages("timetk")
```
# Load required packages
```{r}
library(tidyverse)
library(forecast)
library(lubridate)
library(ggplot2)
library(timetk)
```
#Load dataset
```{r}
read_csv("bike_sharing_daily.csv")
```


## Describe and explore the data

```{r}
names(bike_sharing_daily)

view(bike_sharing_daily)

str(bike_sharing_daily)
```
# Convert the date column to proper Date format       

```{r}
bike_sharing_daily$dteday <- ymd(bike_sharing_daily$dteday)
```
# Check structure to confirm it's working
```{r}
glimpse(bike_sharing_daily)



```



# Task Two: Create interactive time series plots
                                                                                            
# Time series line plot of daily rentals
```{r}
library(readr) 

ggplot(bike_sharing_daily, aes(x = dteday, y = cnt)) +
  geom_line(color = "steelblue") +
  labs(title = "Daily Bike Rentals Over Time",
       x = "Date", y = "Total Rentals") +
  theme_minimal()

```
# What Each Part Does:
 
- ymd(bike_sharing_daily$dteday) # Converts date column to Date class.
- cnt #The total count of rentals (target variable).
- geom_line()# Creates the time series line.
- theme_minimal() # A clean, modern theme.

# Task Three: Smooth time series data

```{r}
ggplot(bike_sharing_daily, aes(x = dteday)) +
  geom_line(aes(y = cnt), color = "red", alpha = 0.5) +
  geom_line(aes(y = cnt_clean), color = "blue") +
  labs(title = "Original vs Cleaned Time Series",
       y = "Bike Rentals", x = "Date") +
  theme_minimal()
```

# Apply simple exponential smoothing
```{r}
ses_model <- HoltWinters(bike_ts_clean, beta = FALSE, gamma = FALSE)
```
# Forecast next 30 days
```{r}
ses_forecast <- forecast(ses_model, h = 30)
```
# Plot forecast
```{r}
autoplot(ses_forecast) +
  labs(title = "Simple Exponential Smoothing Forecast",
        y = "Rentals", x = "Day")

```
# Calculate 10-day Simple Moving Average
```{r}
bike_sharing_daily$sma_10 <- SMA(bike_sharing_daily$cnt_clean, n = 10)
```
# Plot Moving Average
```{r}
ggplot(bike_sharing_daily, aes(x = dteday)) +
  geom_line(aes(y = cnt_clean), color = "gray", alpha = 0.4) +
  geom_line(aes(y = sma_10), color = "darkgreen", size = 1) +
  labs(title = "10-Day Simple Moving Average",
       y = "Smoothed Rentals", x = "Date") +
  theme_minimal()
```
# Task Four: Decompose and assess the stationarity of time series data

```{r}
library(tidyverse)
library(lubridate)
library(forecast)
library(tseries)     # For adf.test()

```

# Create time series again (cleaned version from previous step)
```{r}
bike_ts_clean <- ts(bike_sharing_daily$cnt_clean, frequency = 365)
```
# Plot the decomposition
```{r}
autoplot(decomp) +
  labs(title = "STL Decomposition of Bike Rentals Time Series")

```
# Extract seasonally adjusted data
```{r}
seasonally_adjusted <- seasadj(decomp)

```
# Plot adjusted vs original
```{r}
autoplot(cbind(Original = bike_ts_clean, Adjusted = seasonally_adjusted)) +
  labs(title = "Original vs Seasonally Adjusted Series")

```
# ADF Test: Null hypothesis = NOT stationary
```{r}
adf_test_result <- adf.test(seasonally_adjusted)
print(adf_test_result)

```
#### Interpretation:

- **p-value > 0.05** → Data is **non-stationary**
- **p-value < 0.05** → Data is **stationary**

# Apply first differencing
```{r}
diff_1 <- diff(seasonally_adjusted, differences = 1)

```
# Plot ACF and PACF
```{r}
par(mfrow = c(1, 2))
acf(diff_1, main = "ACF of Differenced Series")
pacf(diff_1, main = "PACF of Differenced Series")
par(mfrow = c(1, 1))

```
# ADF test after differencing
```{r}
adf.test(diff_1)

```

# Task Five: Fit and forecast time series data using ARIMA models

```{r}
library(forecast)
library(tseries)

```
# Fit auto ARIMA
```{r}
auto_fit <- auto.arima(seasonally_adjusted)

```
# Summary and AIC/BIC
```{r}
summary(auto_fit)

manual_fit <- arima(seasonally_adjusted, order = c(1, 1, 1))

```
# Summary and AIC/BIC
```{r}
summary(manual_fit)
```
# Residual diagnostics
```{r}
checkresiduals(auto_fit) # includes ACF and Ljung-Box test
```
# Shapiro-Wilk Normality Test
```{r}
shapiro.test(residuals(auto_fit))
```
# Forecast next 25 days
```{r}
auto_forecast <- forecast(auto_fit, h = 25)
manual_forecast <- forecast(manual_fit, h = 25)
```
# Plot both
```{r}
autoplot(auto_forecast) + labs(title = "Auto ARIMA Forecast (25 days)")
autoplot(manual_forecast) + labs(title = "Manual ARIMA Forecast (25 days)")

```
# **Task Six: Findings and Conclusions**

## Findings and Conclusions

Throughout this project, I analyzed daily bike sharing rental data to understand patterns, smooth the time series, and build accurate forecasts.

## What I Did

* Explored daily rental trends and visualized patterns using time series plots.

* Cleaned the data using the tsclean() function to handle outliers and missing values.

* Applied Simple Moving Average and Simple Exponential Smoothing to smooth fluctuations.

* Decomposed the time series using STL to observe trend and seasonal components.

* Tested for stationarity using the Augmented Dickey-Fuller test, ACF, and PACF plots.

* Used differencing to make the series stationary when necessary.

* Built and compared Auto ARIMA and Manual ARIMA models.

* Generated 25-day forecasts and evaluated model performance based on residuals and AIC.

# **Takeaways**
* Time series decomposition is essential to understand the underlying structure of the data.

* Proper cleaning and transformation (seasonal adjustment, differencing) is necessary before modeling.

* Auto ARIMA is a powerful tool for quick and effective forecasting, but manual tuning gives more control.

* The final models provide realistic short-term forecasts that could help in planning inventory, workforce, and promotions for bike-sharing services.
