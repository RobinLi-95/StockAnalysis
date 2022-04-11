# This code is to read the companys' name from excel and then search for the related symbol in the Yahoo Financial and download the price

import pandas as pd
import yahooquery as yq
import yfinance as yf
import os
import sys
import numpy as np

# Read Data
df = pd.read_excel('New_DataBase/StockList.xls', header=7)

# Read the dataframe (which is a table downloaded from S&P Global Database)
rows = len(df.index)
CompanyName = []
ExchangeTicker = []
Exchange = []
StockTicker = []

for i in range(rows):
    CompanyName.append(df.at[i,'Company Name'])
    ExchangeTicker.append(df.at[i,'Exchange:Ticker'])
    
for i in range(len(ExchangeTicker)):
    if ExchangeTicker[i] != '-':
        Exchange.append(ExchangeTicker[i].split(':')[0])
        StockTicker.append(ExchangeTicker[i].split(':')[1])
        
# Filter the company
FinalList = []

for i in range(len(CompanyName)):
    if ':' in CompanyName[i]:
        FinalList.append(['('.join(CompanyName[i].split('(')[:-1]),CompanyName[i].split('(')[-1].split(':')[0]])
        
# Search the symbol of the company in the final list
def get_symbol(query, preferred_exchange):
    try:
        data = yq.search(query)
    except ValueError: # Will catch JSONDecodeError
        print(query)
    else:
        quotes = data['quotes']
        if len(quotes) == 0:
            return 'No Symbol Found'

        symbol = quotes[0]['symbol']
        for quote in quotes:
            if quote['exchange'] == preferred_exchange:
                symbol = quote['symbol']
                break
        return symbol
      
      
companies =  [i[0] for i in FinalList]
exchanges = [i[1] for i in FinalList]
df = pd.DataFrame({'Company name': companies})
df.insert(1, "Exchange", exchanges, True)
df['Company symbol'] = df.apply(lambda x: get_symbol(x['Company name'],x['Exchange']), axis=1)


# Define a function to download the stock price
def Downloader(Stock):
    stock = yf.Ticker(Stock)
    name = Stock + '.xlsx'
    hist = stock.history(period="max")
    hist.to_excel(name)
    
ticker = []

rows = len(df.index)

for i in range(rows):
    if df.at[i,'Company symbol'] != 'No Symbol Found' and df.at[i,'Company symbol'] != None:
        ticker.append(df2.at[i,'Company symbol'])
        
        
name = []

rows = len(df2.index)

for i in range(rows):
    if df2.at[i,'Company symbol'] != 'No Symbol Found' and df2.at[i,'Company symbol'] != None:
        name.append(df2.at[i,'Company name'])
