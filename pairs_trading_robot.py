import websocket
import json
import pprint 
#import config 
import numpy as np
import pandas as pd
#import talib
from binance.client import Client
from binance.enums import *
from sklearn import linear_model
import statsmodels.api as sm
import time

#file to write price data to
eth_ltc = 'C://mr_roboto2//eth_ltc.csv'
#accessing api keys
api_keys = 'C://mr_roboto2//binance_keys.txt'
#opens file containing api keys
with open(api_keys, 'r') as FILE:
    #splits each line in text file by comma (keys are separated by commas)
    keys=[i for line in FILE for i in line.split(',')]
#first line is api key    
api_key = keys[0]
#second line is api secret
api_secret = keys[1]
#socket to stream minute by minute data
socket = "wss://stream.binance.com:9443/ws/ethusdt@kline_1m/ltcusdt@kline_1m"
#accessing binance client
client = Client(api_key, api_secret, tld='us')
#list to append closed values
eth_close = []
ltc_close = []
#Trading Threshold
Threshold = 1.5
#1st Step: Engle-Granger two-step method
#Step 1 Regress one series on another 
def reg(x,y):
    regr = linear_model.LinearRegression
    x_constant = pd.concat([x, pd.Series([1]*len(x), index = x.index)], axis = 1)
    regr.fit(x_constant, y)
    beta = regr.coef_[0]
    alpha = regr.intercept_
    spread = y - x*beta - alpha
    return spread
#Function to create orders on binance
def order(symbol, quantity, side, order_type=ORDER_TYPE_MARKET):
    try:
        print("Sending Order")
        order = client.create_order(symbol=symbol,
        side = side,
        type = order_type,
        quantity = quantity)
    except Exception as e:
        return False

    return True

#prints when web socket is open
def on_open(ws):
    print('Wake Up Neo')
#sleeping program
time.sleep(5)
#prints when web socket is closed
def on_close(ws):
    print('Closed Connection')
#prints the message received when accessed (aka JSON data)
def on_message(ws, message):

    print('Received Message')
    #json.loads converts serialized JSON data to python dictionary
    json_message = json.loads(message)
    #using pretty print to print message in new lines
    pprint.pprint(json_message)
    #candle accesses dictionary within dictionary of 'k'[price data]
    candle = json_message['k']
    #looking for boolean 'True' (closes on the minute)
    is_candle_closed = candle['x']
    #accesses ticker symbol from python dictionary
    ticker = json_message['s']
    print(ticker)
    #saves variable close from dict within dict of closing price
    close = candle['c']
    #for each respective ticker being analyzed it prints the closing price, as well as prices to append to df
    if  ticker == 'ETHUSDT' and is_candle_closed:
        print("candle closed at {}".format(close))
        eth_close.append(float(close))
        print("eth closes")
        print(eth_close)
    if  ticker == 'LTCUSDT' and is_candle_closed:
        print("candle closed at {}".format(close))
        ltc_close.append(float(close))
        print("ltc closes")
        print(ltc_close)
        #create a pandas df of price data
        df1 = pd.DataFrame()
        #appending respective closes as a pandas Series (allows you to run numpy functions on)
        df1['eth close'] = pd.Series(eth_close)
        df1['ltc close'] = pd.Series(ltc_close)
        #creating two columns which include the log transformed price
        df1['log eth'] = np.log(df1['eth close'])
        df1['log ltc'] = np.log(df1['ltc close'])
        #writing values to a csv file
        df1.to_csv(eth_ltc)

        if len(eth_close) and len(ltc_close) == 250:
            x = df1['log eth']
            y = df1['log ltc']
            spread = reg(x,y)
            #Step 2 Engel Granger Method
            adf = sm.tsa.stattools.adfuller(spread, maxlag=1)
            print('ADF Test Statistic:%.2f' % adf[0])
            print('p-value: %.3f' % adf[1])
            mean = np.mean(spread)
            std = np.std(spread)
            ratio = df1['eth close'] / df1['ltc close']
            df1['mean spread'] = mean
            df1['std spread'] = std
            df1['ratio'] = ratio

        #if spread[-1] > mean + Threshold * std:
            #buy/sell logic here

        #elif spread[-1] < mean +Threshold * std:
            #inverted buy sell logic here

        #else:
            #liquidate logic here 

#websocket for:
#Ethereum + Litecoin
ws = websocket.WebSocketApp(socket, on_open=on_open,
                            on_close=on_close, on_message=on_message)
ws.run_forever()






