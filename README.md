# BTC Data Analysis
A data science project analyzing Bitcoin price trends using R, including time series analysis, moving averages, returns, and visual
library(tidyverse)
library(lubridate)
library(zoo)
library(quantmod)
library(ggplot2)
library(readr)
library(dplyr)
library(vroom)
library(xts)
library(TTR)
library(patchwork)

#---------------------Importing CSV files--------------------------------
btc <- readr::read_csv("C:/Users/Hp/Downloads/BTC_clean.csv")

#data handling and data manipulation
head(btc)
str(btc)
summary(btc)
btc <- btc |>select(timestamp, open, high, low, close, volume) |>mutate(timestamp = ymd_hms(timestamp))|>arrange(timestamp)
colnames(btc)


#-------------------Candlestick Chart-------------------------------------

#************how price moved during a period,zoomed-in details********
btc_xts <- xts(
  btc[, c("open", "high", "low", "close")],
  order.by = btc$timestamp
)
chartSeries(
  btc_xts,
  theme = chartTheme("white"),
  name = "BTC-USD Candlestick Chart",
  type = "candlesticks"
)

#-----------------------Line charts-----------------------------------
#***************how price moved during a period,clean overview.**************
btc$timestamp <- as.Date(btc$timestamp)

ggplot(btc, aes(x = timestamp, y = close)) +
  geom_line(linewidth = 0.7, color = "blue") +
  labs(
    title = "BTC Closing Price Trend",
    x = "Date",
    y = "Closing Price (USD)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

#----------------Calculate moving averages---------------------------
#SMA20 = average price of the last 20 days
#SMA50 = average price of the last 50 days
btc <- btc |>
  mutate(
    SMA20 = SMA(close, 20),
    SMA50 = SMA(close, 50)
  )

# Plot
ggplot(btc,(aes(x = timestamp))) +
  geom_line(aes(y = close), color = "blue", linewidth = 0.7) +
  geom_line(aes(y = SMA20), color = "red", linewidth = 0.7) +
  geom_line(aes(y = SMA50), color = "green", linewidth = 0.7) +
  labs(
    title = "BTC Price with SMA20 & SMA50",
    x = "Date",
    y = "Price (USD)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

#------------------Calculate daily returns-----------------------------

btc <- btc |>
  mutate(Return = (close / lag(close) - 1) * 100)

# Plot daily returns
ggplot(btc, aes(x = timestamp, y = Return)) +
  geom_line(linewidth = 0.7, color = "purple") +
  labs(
    title = "BTC Daily Returns (%)",
    x = "Date",
    y = "Daily Return (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

  mutate(Return = (close / lag(close) - 1) * 100)

#--------------Histogram of BTC daily returns---------------------
  #Histogram visually shows volatility
  
ggplot(btc, aes(x = Return)) +
  geom_histogram(bins = 60, fill = "skyblue", color = "black") +
  labs(
    title = "Distribution of BTC Daily Returns",
    x = "Daily Return (%)",
    y = "Frequency"
    ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold")
    )


#------------------Price vs Volume (Correlation Scatter Plot)**-------------
  #It makes a scatter plot showing the relationship between:x-axis: Trading volume,y-axis: BTC closing price
  #Each point = one day of data.

ggplot(btc, aes(x = volume, y = close)) +
  geom_point(alpha = 0.5, color = "darkblue") +
  labs(
    title = "BTC: Price vs Volume",
    x = "Volume",
    y = "Closing Price (USD)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold")
  )
  
#-------------------***Plotting all of the charts***-----------------
p1 <- ggplot(btc, aes(x = timestamp, y = close)) +
  geom_line(linewidth = 0.7, color = "blue") +
  labs(title = "BTC Closing Price") +
  theme_minimal()

p2 <- ggplot(btc, aes(x = timestamp)) +
  geom_line(aes(y = close), color = "blue", linewidth = 0.7) +
  geom_line(aes(y = SMA20), color = "red", linewidth = 0.7) +
  geom_line(aes(y = SMA50), color = "green", linewidth = 0.7) +
  labs(title = "BTC with SMA20 & SMA50") +
  theme_minimal()

p3 <- ggplot(btc, aes(x = timestamp, y = Return)) +
  geom_line(linewidth = 0.7, color = "purple") +
  labs(title = "BTC Daily Returns (%)") +
  theme_minimal()

p4 <- ggplot(btc, aes(x = Return)) +
  geom_histogram(bins = 60, fill = "skyblue", color = "black") +
  labs(title = "Distribution of BTC Returns") +
  theme_minimal()

p5 <- ggplot(btc, aes(x = volume, y = close)) +
  geom_point(alpha = 0.5, color = "darkblue") +
  labs(title = "Price vs Volume") +
  theme_minimal()

print((p1 | p2) / (p3 | p4) / p5)
