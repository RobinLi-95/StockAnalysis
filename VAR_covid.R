# VAR in R using AEX Data
# Build VAR Model for stock price and corresponding macroscopic economic indexes/Covid-19 Data 

# Load required packages for running VAR

library(urca)
library(vars)
library(mFilter)
library(tseries)
library(forecast)
library(tidyverse)
library(ggplot2)
library(ggfortify)
library(lubridate)
library(dplyr)
library(tseries)
library(zoo)
library(scales)
library(TSstudio)

# Load the DataSet

SimRet <- function(data){
  sr <- diff(data)/na.omit(lag(data, k=-1))
  return(sr)
}

## Load the Crude Oil Prices
OilPrice <- read_csv("Crude-Oil-Price-USD-Nasdaq.csv")
OilPrice$newdate <- strptime(as.character(OilPrice$Date),"%m/%d/%Y")
format(OilPrice$newdate, "%Y-%m-%d")

OilPrice$quarter <- floor_date(OilPrice$newdate, "quarter")
df <- OilPrice %>%
  group_by(quarter) %>%
  summarize(mean = mean(`Close/Last`))

QuarterlyPrice <- data.frame(date = as.Date(df$quarter), price = df$mean)
quarter.r <- SimRet(QuarterlyPrice$price)
# oilprice <- ts(QuarterlyPrice$price, start = c(2014,1), end = c(2019,4),frequency = 4)
oilprice <- ts(quarter.r, start = c(2014,1), end = c(2019,4),frequency = 4)
ts.plot(oilprice, gpars=list(xlab="Year", ylab="Quarterly Oil Price (USD)"))

## Load Gold Prices
GoldPrice <- read_csv("Gold-price-USD-INVESTING.csv")
GoldPrice$newdate <- strptime(as.character(GoldPrice$Date),"%b %d,%Y")
format(GoldPrice$newdate, "%Y-%m-%d")
GoldPrice$quarter <- floor_date(GoldPrice$newdate, "quarter")
df <- GoldPrice %>%
  group_by(quarter) %>%
  summarize(mean = mean(Price))

GoldQuarterlyPrice <- data.frame(date = as.Date(df$quarter), price = df$mean)
gold.r <- SimRet(GoldQuarterlyPrice$price)
goldprice <- ts(gold.r, start = c(2014,1), end = c(2019,4), frequency = 4)
# goldprice <- ts(GoldQuarterlyPrice$price, start = c(2014,1), end = c(2019,4), frequency = 4)
ts.plot(goldprice, gpars=list(xlab="Year", ylab="Quarterly Gold Price (USD)"))

## Load Stock Index
AEX <- read_csv("荷兰AEX指数历史数据.csv")
AEX$newdate <- strptime(as.character(AEX$日期),"%Y年%m月%d日")
format(AEX$newdate, "%Y-%m-%d")
AEX$quarter <- floor_date(AEX$newdate, "quarter")
df <- AEX %>%
  group_by(quarter) %>%
  summarize(mean = mean(收盘))

AEXPrice <- data.frame(date = as.Date(df$quarter), price = df$mean)
aex.r <- SimRet(AEXPrice$price)
aex <- ts(aex.r, start = c(2014,1), end = c(2019,4), frequency = 4)
# FTSE <- ts(FTSEPrice$price, start = c(2014,1), end = c(2019,4), frequency = 4)
ts.plot(aex, gpars=list(xlab="Year", ylab="AEX"))

## Load CPI Data
CPI <- read_csv("CPI_MONTHLY.csv")
CPI$quarter <- floor_date(CPI$...1, "quarter")
df <- CPI %>%
  group_by(quarter) %>%
  summarize(mean = mean(Netherlands))

CPI.r <- data.frame(date = as.Date(df$quarter), price = df$mean)
cpi <- ts(CPI.r$price, start = c(2014,1), end = c(2019,4), frequency = 4)
ts.plot(cpi, gpars=list(xlab="Year", ylab="Custormer Price Index"))

## Load GDP Data
GDP <- read_csv("GDP_Q.csv")
GDP.r <- data.frame(date = as.Date(as.yearqtr(GDP$Date,format="%YQ%q")), price = GDP$Netherlands)
gdp.r <- SimRet(GDP.r$price)
gdp <- ts(gdp.r, start = c(2014,1), end = c(2019,4), frequency = 4)
ts.plot(gdp, gpars=list(xlab="Year", ylab="Seasonally Adjusted GDP"))

## Load US Covid data
#USCovid <- read_csv("US_Covid19.csv")
#USCovid$month <- floor_date(USCovid$`# Date_reported`, "month")
#df <- USCovid %>%
#  group_by(month) %>%
#  summarize(mean = mean(New_cases))
#NewCase <- data.frame(date = as.Date(df$month), price = df$mean)
#newcase <- ts(NewCase$price, start = c(2020,1), end = c(2021,11), frequency = 12)

# Determine the persistence of the model
acf(oilprice, main = "ACF for Oil Price")
pacf(oilprice, main = "PACF for Oil Price")

acf(goldprice, main = "ACF for Gold Price")
pacf(oilprice, main = "PACF for Gold Price")

acf(aex, main = "ACF for AEX Price")
pacf(aex, main = "PACF for AEX Price")

acf(cpi, main = "ACF for CPI")
pacf(cpi, main = "PACF for CPI")

acf(gdp, main = "ACF for GDP")
pacf(gdp, main = "PACF for GDP")

#acf(newcase, main = "ACF for New Cases")
#pacf(newcase, main = "PACF for New Cases")

# Doing the Augmented Dickey-Fuller Test
adf.test(oilprice)
adf.test(goldprice)
adf.test(aex)
adf.test(cpi)
adf.test(gdp)
#adf.test(newcase)

# Finding the Optimal Lags

macro.bv <- cbind(oilprice,goldprice,gdp,cpi,aex)
colnames(macro.bv) <- cbind("Oil Price","Gold Price","GDP","CPI","AEX")
lagselect <- VARselect(macro.bv, lag.max = 15, type = "const")
lagselect$selection

# Build the Model
Model <- VAR(macro.bv, p = 2, type = "const", season = NULL, exog = NULL)
summary(Model)

# Diagnosing the VAR

# Serial Correlation
Serial1 <- serial.test(Model, lags.pt = 15, type = "PT.asymptotic")
Serial1

# Heteroscedasticity

Arch1 <- arch.test(Model, lags.multi = 3, multivariate.only = TRUE)
Arch1

# Normal Distribution of the Residuals

Norm1 <- normality.test(Model, multivariate.only = TRUE)
Norm1

# Testing for Structural Breaks in the Residuals
Stability1 <- stability(Model, type="OLS-CUSUM")
plot(Stability1)

# Granger Causality
Granger <- causality(Model, cause = "AEX")
Granger

Granger <- causality(Model, cause = "CPI")
Granger

Granger <- causality(Model, cause = "GDP")
Granger

Granger <- causality(Model, cause = "Oil.Price")
Granger

Granger <- causality(Model, cause = "Gold.Price")
Granger

# Impulse Response Functions

irf <- irf(Model, impulse = "GDP", response = "AEX", n.ahead = 20, boot =TRUE)
plot(irf, ylab ="AEX Index", main="Shock from GDP")

irf <- irf(Model, impulse = "CPI", response = "AEX", n.ahead = 20, boot =TRUE)
plot(irf, ylab ="AEX Index", main="Shock from CPI")

irf <- irf(Model, impulse = "Gold.Price", response = "AEX", n.ahead = 20, boot =TRUE)
plot(irf, ylab ="AEX Index", main="Shock from Gold.Price")

irf <- irf(Model, impulse = "Oil.Price", response = "AEX", n.ahead = 20, boot =TRUE)
plot(irf, ylab ="AEX Index", main="Shock from Gold.Price")

# Variance Decomposition
FEVD <- fevd(Model, n.ahead = 10)
FEVD
plot(FEVD)

# VAR Forecasting

forecast <- predict(Model, n.ahead = 4, ci = 0.95)
forecast
fanchart(forecast, names ="AEX")
