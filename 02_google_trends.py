# -*- coding: utf-8 -*-
"""
@date: 02-12-2025
@author: sap218
"""

import logging
import time
from pytrends.request import TrendReq
import pandas as pd

timeperiods = {
    "before":['2018-01-05', '2019-04-08'], # to account for week sunday starts 
    #"during":['2020-01-01', '2021-05-01'],
    "during":['2020-01-05', '2021-04-04'], ## 66 rows
    "after":['2022-01-05', '2023-04-08'], # to account for week sunday starts 
    }

#to_look_at = "before"
#to_look_at = "during"
to_look_at = "after"

logging.basicConfig(
    filename=f"output/02_{to_look_at}.log",
    encoding="utf-8",
    filemode="a",
    format="{asctime} - {levelname} - {message}",
    style="{",
    datefmt="%Y-%m-%d %H:%M",
    level=logging.INFO,
    force=True
    )
logging.info(f"Starting script for 02 Google Trends for {to_look_at}")


timeperiod = timeperiods[to_look_at]


start, end = pd.to_datetime(timeperiod)
#daterows = pd.date_range(start=start, end=end, freq='ME') # MS ?
daterows = pd.date_range(start=start, end=end, freq='7D') # W ?

dreamDF = pd.DataFrame({"date":daterows,
                        "month": daterows.strftime('%b-%Y')})
del start, end, daterows


if to_look_at == "before":
    timeframeSTR = f"{timeperiod[0]} 2019-04-04" # fix for trends
elif to_look_at == "after":
    timeframeSTR = f"{timeperiod[0]} 2023-04-04" # fix for trends
else:
    timeframeSTR = f"{timeperiod[0]} {timeperiod[1]}"
del timeperiod


keysearch = [
    "happy dream",

    "great dream",
    "pleasant dream",
    "good dream",
    "nice dream",
    
    "silly dream",
    "confusing dream",
    "wild dream",
    "crazy dream",
    
    "strange dream",
    "weird dream",
    
    "intense dream",
    "vivid dream",
    "lucid dream",
    
    "sad dream",
    
    "bad dream",
    "scary dream",
    
    "nightmare",
    ]


# to test
#keysearch = ["nightmare"]

if to_look_at == "during":
    keysearch.append("COVID")


pytrend = TrendReq(hl='en-GB', tz=0, 
                   requests_args={'verify':True,
                                  'headers': {'User-Agent': 'Mozilla/5.0'}
                        },
                   )

## weekly index of relative popularity

terms_no_covid_data = []

for term in keysearch:
    t = term.split(" ")[0]    
    
    while t not in list(dreamDF) and t not in terms_no_covid_data:
    
        #time.sleep(35)
        time.sleep(6)
        #time.sleep(2)
    
        try:
            
            pytrend.build_payload(
                [term],
                cat=0, # 45=medical | 0=all
                timeframe=timeframeSTR,
                geo='GB', # -ENG
                gprop='' # 'images' ?
            )
        
            df = pytrend.interest_over_time()
        
            if len(df) == 0:
                print(f"No results available for:\t{t}")
                terms_no_covid_data.append(t)
                logging.warning(f"No results available for:\t{t}")
            
            else:
                if 'isPartial' in df.columns: df = df.drop(columns=['isPartial'])
                
                #df = df.resample('ME').sum() # monthly
                df = df.resample('W').sum() # weekly
                       
                
                #df = df.iloc[1:]
                
                # normalising
                df[term] = (df[term] - df[term].min()) / (df[term].max() - df[term].min())
                
                dreamDF[t] = list(df[term])
            
                print(f"Success for:\t{t}")
                logging.info(f"Success for:\t{t}")
            
        except Exception as e:
            print(f"Error:\t{t} -- {e}")
    

dreamDF.to_csv(f"output/02_{to_look_at}_trends.csv", index=False)

# End
