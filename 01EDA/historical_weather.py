# -*- coding: utf-8 -*-
"""
Created on Sun Feb 10 14:36:11 2019

@author: smler
"""
import requests
import pandas
import datetime
import numpy as np

cities_df = pandas.read_csv('cities_for_weather.csv')
print(cities_df)

cities_df['date2'] = pandas.to_datetime(cities_df['Review_Date2'])
cities_df['epoch_date'] = (cities_df['date2']-datetime.datetime(1970,1,1))
cities_df['epoch_date'] = cities_df['epoch_date'].astype('timedelta64[s]')

'''
Amsterdam = 52.3680° N, 4.9036° E
London = 51.5074° N, 0.1278° W
Paris = 48.8566° N, 2.3522° E
Barcelona = 41.3851° N, 2.1734° E
Milan = 45.4642° N, 9.1900° E
Vienna = 48.2082° N, 16.3738° E
'''

conditions = [
    (cities_df['Hotel_City'] == 'Amsterdam'),
    (cities_df['Hotel_City'] == 'London'),
    (cities_df['Hotel_City'] == 'Paris'),
    (cities_df['Hotel_City'] == 'Barcelona'),
    (cities_df['Hotel_City'] == 'Milan'),
    (cities_df['Hotel_City'] == 'Vienna')]
choices = [52.3680,  51.5074, 48.8566, 41.3851, 45.4642, 48.2082]
cities_df['lat'] = np.select(conditions, choices, default=0)

choices = [4.9036, -0.1278, 2.3522, 2.1734, 9.1900, 16.3738]
cities_df['lon'] = np.select(conditions, choices, default=0)

file = open("../00Data/key.txt", "r") 
key = file.read() 

def get_weather(x):
    '''function that gets historical weather data for hotel review date/city
     through Dark Sky API'''
    lat = cities_df['lat'][x]
    lon = cities_df['lon'][x]
    offset = cities_df['epoch_date'][x]
    city = cities_df['Hotel_City'][x]
    date = cities_df['Review_Date2'][x]
    
    r = requests.get("https://api.darksky.net/forecast/" + key + "/" + str(lat) + "," + str(lon) + "," + str(offset.astype(np.int64)) + "?exclude=currently,flags,units=us")
    data = r.json()
    weather = []
    tempHigh = data["daily"]["data"][0]['temperatureMax']
    tempLow = data["daily"]["data"][0]['temperatureMin']
    summary = data["daily"]["data"][0]['summary']
    print(city, date, tempHigh, tempLow, summary)
    weather.append(city)
    weather.append(date)
    weather.append(tempHigh)
    weather.append(tempLow)
    weather.append(summary)
    return weather

weather_master = pandas.DataFrame(columns = ["City", "Date", "TempHigh", "TempLow", "Summary"])
<<<<<<< HEAD
for i in range(4350,4386):
    weather = get_weather(i)
    print(weather)
    weather_master = weather_master.append({"City": weather[0], "Date": weather[1], 'TempHigh': weather[2], 'TempLow': weather[3], 'Summary': weather[4]}, ignore_index = True)
print(weather_master)

weather_master.to_csv("../00Data/hist_weather6.csv")
