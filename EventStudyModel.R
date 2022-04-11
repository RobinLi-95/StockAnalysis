# Calculate the CAR for 6 months after Covid-19 outbreak for American Retailing Stock

library(tidyverse)

#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

# Load the DataSet

SimRet <- function(data){
  sr <- diff(data)/na.omit(lag(data, k=-1))
  return(sr)
}

## Load S&P 500 Index
MI <- read_csv("XXX.csv")
MI$newdate <- strptime(as.character(MI$Date),"%Y-%m-%d")
format(MI$newdate, "%Y-%m-%d")

MIPrice <- data.frame(date = as.Date(MI$newdate), price = MI$Close)
mi.r <- SimRet(MIPrice$price)
mi <- ts(mi.r, start = c(2015,1), end = c(2020,1), frequency = 365)
# sp500 <- ts(SP500Price$price, start = c(2014,1), end = c(2019,4), frequency = 4)
#ts.plot(sp500, gpars=list(xlab="Year", ylab="S&P 500 Index (USD)"))
eventmi <- ts(mi.r, start = c(2020,32), end = c(2021,62), frequency = 365)

## Load Stock Price
ST <- read_csv(args[1])
ST$newdate <- strptime(as.character(ST$Date),"%Y-%m-%d")
format(ST$newdate, "%Y-%m-%d")
STPrice <- data.frame(date = as.Date(ST$newdate), price = ST$Close)
st.r <- SimRet(STPrice$price)
st <- ts(st.r,start = c(2015,1), end = c(2020,1), frequency = 365)
#ts.plot(st, gpars=list(xlab="Year", ylab="AAPL"))
eventst <- ts(st.r, start = c(2020,32), end = c(2021,62), frequency = 365)

## Linear Fit
newdata <- cbind(mi,st)
lmST <- lm(mi~st,data = newdata)
summary(lmST)
intercept <- summary(lmST)$coefficients[1,1]
slope <- summary(lmST)$coefficients[2,1]

## Deal with the residue
sj <- lmST$residuals - mean(lmST$residuals)
sj <- sum(sj**2)
Tdays <- length(st)
sj2 <- sj / (Tdays - 1)
sj2

## Calculate the SAR
AR <- eventst - (eventmi * slope + intercept)
Rm <- mean(mi)
Rmi <- (st - Rm)**2
sum1 <- sum(Rmi)

ratio <- sj2 * (1 + 1/Tdays + ((eventmi - Rm)**2/sum1))
SAR <- AR / (ratio ** (0.5))
SAR

## Calculate CAR
m <- length(SAR)
CAR <- 1/(m**0.5) * sum(SAR)

CAR
