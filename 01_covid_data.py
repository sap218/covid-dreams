# -*- coding: utf-8 -*-
"""
@date: 03-12-2025
@author: sap218
"""

import pandas as pd

cols_of_interest = [
    "country",
    "date",
    "new_cases",
    ]

data = pd.read_csv("input/COVID-2019 - ECDC (2020).csv", usecols=cols_of_interest)

df = data[data["country"] == "United Kingdom"].copy()
del data
del df["country"]
df = df.dropna()

# Below coverts weekly/daily to monthly counts
df.set_index('date', inplace=True)
df.index = pd.to_datetime(df.index)
df = df.loc['2019-10-01':'2021-04-01']
#df = df.resample('ME').sum() # monthly
df = df.resample('W').sum() # weekly

df['date'] = df.index
df['month'] = df['date'].dt.strftime('%b-%Y')

df.to_csv("output/01_covid.csv", index=False)

# End
