import pandas as pd

ps = pd.read_csv("prices-split-adjusted.csv")

ps.head()
wltw = ps.loc[ps["symbol"] == "WLTW"]
wltw.to_csv("wltw.csv")