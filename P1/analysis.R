setwd("C:/[DATA]/DataLiterate/P1")

#read the file
df = read.csv("bike_trip_data.csv", stringsAsFactor=FALSE)

#checking
dim(df) #dimension of the data
summary(df)

#What was the average total time (in minutes) used by a bicycle in the data?
#Duration column is in seconds
#any(is.na(df$Duration)) - no null
avgTime = mean(df$Duration) / 60


#What was the most popular day by trip frequency in this dataset?
# => I consider which Start.Date has the most trip frequency

#create a new data frame with the day only
t = as.data.frame(as.Date(df$Start.Date, "%m/%d/%y"))
names(t)[1] = "day"
idx = which.max(table(t))
table(t)[idx]


# Assuming there are 30 bikes per station, 
# find what date the bikes FIRST need to be rebalanced. 
# As in, there are 0 bikes at a terminal for a customer to rent. 
# Do this ignoring "Start.Date" and "End.Date" columns.

getRebalanceDate = function(df, id){
  #create data frame - out
  outDF = as.data.frame(df[which(df$Start.Terminal == id), c("Start.Date")])
  outDF$state = -1
  names(outDF)[1] = "time"
  #create data frame - in
  inDF = as.data.frame(df[which(df$End.Terminal == id), c("End.Date")])
  inDF$state = 1
  names(inDF)[1] = "time"
  
  #combine two data frame
  DF = rbind(outDF, inDF)
  #sort by date time
  DF = DF[order(as.POSIXlt(DF$time, format="%m/%d/%y %H:%M")), ]
  #at the begining with 30 bicycle
  DF[1, "state"] = DF[1, "state"] + 30
  #calculate the acculated sum
  csum = cumsum(DF$state)
  #check which index get the value 0
  idxs = which(csum == 0)
  #get the date time
  as.character(DF[idxs[1], "time"])
}

#create a record data frame
record = as.data.frame(as.character(unique(df$Start.Station)))
record$id = as.character(unique(df$Start.Terminal)) #get the ids

#get the rebalance time
l = unlist(lapply(record$id, function(x){
        getRebalanceDate(df, x)
      }))
#update the record
record$time = l

#sort the data with respect to the time
record = record[order(as.POSIXlt(record$time, format="%m/%d/%y %H:%M")), ]

#need to rebalance FIRST
record[1, ]