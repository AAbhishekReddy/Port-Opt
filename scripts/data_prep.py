# Import the data handleinig package
import pandas as pd

# Reading the raw data set
ps = pd.read_csv("prices-split-adjusted.csv")

# Creting a vector with all the unique names of the companies
names = ps["symbol"]
names.values.tolist()
names = list(set(names))
names.sort()

# Creating a dataset for every company listed and writing them in to a file
for i in names:
    ps.loc[ps["symbol"] == i].to_csv(i + ".csv")


ps.head()
# wltw = ps.loc[ps["symbol"] == "WLTW"]
# wltw.to_csv("wltw.csv")

# a = ps.loc[ps["symbol"] == "A"]
# a.to_csv("a.csv")

# aal = ps.loc[ps["symbol"] == "AAL"]
# aal.to_csv("aal.csv")

# aap = ps.loc[ps["symbol"] == "AAP"]
# aap.to_csv("aap.csv")

# aapl = ps.loc[ps["symbol"] == "AAPL"]
# aapl.to_csv("aapl.csv")

# abc = ps.loc[ps["symbol"] == 'ABC']
# abc.to_csv("abc.csv")

# abt = ps.loc[ps["symbol"] == 'ABT']
# abt.to_csv("abt.csv")


#%%
print "hello"

# %%
import pandas as pd


# %%
ps = pd.read_csv("/home/ghost/Desktop/Port-Opt/new_york/prices-split-adjusted.csv/")

# %%
