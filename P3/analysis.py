import pandas as pd
import numpy as np

pd.options.mode.chained_assignment = None  # default='warn'

fn = "./data/2008.csv.bz2"

#read first million lines
data = pd.io.parsers.read_csv(fn, compression="bz2", nrows=1000000)

#Problem 1: Find the IATA code that is in the Origin column, but is NOT is the Dest column.
result = list(set(data['Dest']) - set(data['Origin']))
print(result)

#Problem 2:  Which airport, as defined by the IATA code, has at least 10,000 flights and 
# had the lowest probability for a delayed flight in the data?

#get column names
#sorted(list(data.columns.values))

#extract related columns
df = data[['Origin', 'Dest', 'ArrDelay', 'DepDelay']]
df.insert(4, "ArrDelay2", np.nan)
df.insert(5, 'DepDelay2', np.nan)

row_idx = data['ArrDelay'] > 0 
df.loc[row_idx , "ArrDelay2"] = 1
df.loc[(data['ArrDelay'] <= 0) , "ArrDelay2"] = 0

df.loc[(data['DepDelay'] > 0) , "DepDelay2"] = 1
df.loc[(data['DepDelay'] <= 0) , "DepDelay2"] = 0

#drop 
del df['ArrDelay']
del df['DepDelay']
df = df.rename(columns={'ArrDelay2':'ArrDelay', 'DepDelay2':'DepDelay'})


#get unique iata code from Origin and Dest columns
iatas = list(pd.concat([df['Origin'], df['Dest']]).unique())

#create empty result data frame
resultDf = pd.DataFrame(columns = ('iata', 'totalFlight', 'totalDelay', 'delayProb'))

#for each iata 
for iata in iatas:	
	#get the subset of corrsponding data frame
	temp = df.loc[(data['Origin'] == iata) | (data['Dest'] == iata), ['ArrDelay', 'DepDelay']]
	totalFlight = len(temp.index)
	totalDelay = pd.DataFrame.sum(temp).sum()
	delayProb = totalDelay / totalFlight
	resultDf = resultDf.append({'iata':iata, 'totalFlight':totalFlight, 'totalDelay':totalDelay, 'delayProb':delayProb}, ignore_index=True)	

#look for iata has at least 10,000 flights and had the lowest probability for a delayed flight
print (resultDf.loc[(resultDf['totalFlight'] >=10000)].sort(['delayProb']).head(1))
#iata  totalFlight  totalDelay  delayProb
# HNL        17626        8846   0.501872

# prepare a new data frame
df2 = data[['UniqueCarrier', 'DayOfWeek', 'ArrDelay', 'DepDelay', 'DepTime']]

df2.insert(5, 'DayOfWeek2', np.nan)
df2.insert(6, 'DepTime2', np.nan)
df2.insert(7, 'Delay', np.nan)

df2.loc[(df2['DayOfWeek'] >= 1) &  (df2['DayOfWeek'] <= 5), "DayOfWeek2"] = "Weekday"
df2.loc[(df2['DayOfWeek'] >= 6) &  (df2['DayOfWeek'] <= 7), "DayOfWeek2"] = "Weekend"

df2.loc[(df2['DepTime'] >= 501) &  (df2['DepTime'] <= 1700), "DepTime2"] = "Day Time"
df2.loc[(df2['DepTime'] >= 1701) &  (df2['DepTime'] <= 2400), "DepTime2"] = "Night Time"
df2.loc[(df2['DepTime'] >= 0) &  (df2['DepTime'] <= 500), "DepTime2"] = "Red Eye"

df2.loc[(df2['ArrDelay'] > 0) | (df2['DepDelay'] > 0) , "Delay"] = 1
df2.loc[(df2['ArrDelay'] <= 0) & (df2['DepDelay'] <= 0) , "Delay"] = 0

del df2['DayOfWeek']
del df2['DepTime']
del df2['ArrDelay']
del df2['DepDelay']

df2 = df2.rename(columns={'DayOfWeek2':'DayOfWeek', 'DepTime2':'DepTime'})

#the historical probability of flight delay in each of the twenty carriers based off of two characteristics.
resultSeries = df2.groupby(['UniqueCarrier', 'DayOfWeek', 'DepTime']).Delay.sum() / df2.groupby(['UniqueCarrier', 'DayOfWeek', 'DepTime']).Delay.count()

#write to file
resultSeries.to_csv('output_py.csv')