# -*- coding: utf-8 -*-
"""
@date: 03-12-2025
@author: sap218
"""

import pandas as pd

to_look_at = [
    "before",
    "during",
    "after",
    ]

cols = []

for d in to_look_at:
    f = pd.read_csv(f"output/02_{d}_trends.csv")
    cols.append(list(f))
del d, f

common_cols = list(set(cols[0]) & set(cols[1]) )# & set(cols[2]))
del cols

dream_groups = {
    "Positive": ["Happy","Great","Good","Nice"], # Pleasant
    "Negative": ["Sad","Bad","Scary","Nightmare"],
    "Oddity": ["Wild","Crazy","Strange","Weird"], # Silly, Confusing
    "Intensity": ["Vivid","Lucid"], # Intense
    }

#to_look_at = to_look_at[0]
#to_look_at = to_look_at[1]
to_look_at = to_look_at[2]


if to_look_at == "during":
    common_cols.append("COVID")
    dream_groups["COVID"] = ["Covid"]


covid = pd.read_csv("output/01_covid.csv")
google = pd.read_csv(f"output/02_{to_look_at}_trends.csv", usecols=common_cols)

google["cases"] = covid["new_cases"]
df = google.copy()
del covid, google

df.columns = [x.capitalize() for x in list(df)]
cols_other = ["Date","Month","Cases",]
cols_dreams = [x for x in list(df) if x not in cols_other]

# normalising against case counts
normalised = df[ [col for col in df.columns if col in cols_other ] ].copy()
for d in cols_dreams:
    normalised[d] = df[d] * max(df['Cases'])
del d, df

# averges across groups
ave_df = normalised[cols_other].copy()

for sentiment, listCols in dream_groups.items():
    n_filter = normalised[listCols]
    
    averages = []
    for index, row in n_filter.iterrows():
        averages.append( sum(row) / len(row) )
    ave_df[sentiment] = averages
del index, row, sentiment, listCols, n_filter, averages
del normalised

ave_df.to_csv(f"output/03_{to_look_at}_merged.csv", index=False)

monthly = ave_df.copy()
del ave_df
del monthly["Month"]
monthly.set_index('Date', inplace=True)
monthly.index = pd.to_datetime(monthly.index)
monthly = monthly.resample('ME').sum() # monthly
monthly['Date'] = monthly.index
monthly['Month'] = monthly['Date'].dt.strftime('%b-%Y')
monthly.to_csv(f"output/03_{to_look_at}_monthly.csv", index=False)

# End
