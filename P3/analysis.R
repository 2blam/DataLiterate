setwd("C:/Users/User/Documents/GitHub/DataLiterate/P3")

data = read.csv("./data/2008.csv.bz2", nrows=1000000)

#change all columns to lowercase
colnames(data) = tolower(colnames(data))

#find the IATA code that is in the origin column, 
# but is NOT in Dest column
# Wrong question? should be 
# in the dest column, but not in origin column
setdiff(levels(data$dest), levels(data$origin))



#Which airport, as defined by the IATA code, 
# has at least 10,000 flights and had the lowest probability
# for a delayed flight in the data?
#prepare the new data frame with 4 columns only
df = data[, c("origin", "dest", "depdelay", "arrdelay")]
df[which(temp$depdelay>0), "depdelay"] = 1
df[which(temp$depdelay<0), "depdelay"] = 0

df[which(temp$arrdelay>0), "arrdelay"] = 1
df[which(temp$arrdelay<0), "arrdelay"] = 0

getProbDelay = function(df, iataCode){
  #get the total number of flight
  totalFlight = sum(temp$origin == iataCode) + sum(temp$dest == iataCode)
  #get the total number of delay
  totalDelay = sum(temp[which(temp$origin == iataCode), "depdelay"], na.rm = TRUE) + 
    sum(temp[which(temp$origin == iataCode), "arrdelay"], na.rm = TRUE) 
  probDelay = totalDelay / totalFlight
  c(iataCode, totalFlight, probDelay)
}

#get all the iata code
iataCode = sort(intersect(levels(data$origin), levels(data$dest)))

#get the result list
resultList = unlist(lapply(iataCode, function (x){getProbDelay(df, x)}))
resultDf = data.frame(matrix(resultList, nrow=length(iataCode), byrow=TRUE))
colnames(resultDf) = c("iata", "numofflight", "probdelay")

resultDf$numofflight = as.numeric(as.character(resultDf$numofflight))
resultDf$probdelay = as.numeric(as.character(resultDf$probdelay))

#order by probdelay
resultDf = resultDf[order(resultDf$probdelay), ]
#get the iata code has at least 10,000 flights and had the lowest probability
resultDf[resultDf$numofflight > 10000, ][1,]


#
# create a new data frame
#df = data[, c("dayofweek", "deptime", "uniquecarrier", "arrdelay", "depdelay")]
df = data[, c("dayofweek", "deptime", "uniquecarrier", "carrierdelay")]
temp=df
temp$dayofweek2 = NA
temp$deptime2 = NA
#temp$delay = NA

temp[temp$dayofweek <= 5, "dayofweek2"] = "Weekday"
temp[temp$dayofweek >= 6, "dayofweek2"] = "Weedend"

temp[which(temp$deptime >= 501 & temp$deptime <= 1700), "deptime2"] = "Day Time"
temp[which(temp$deptime >= 1701 & temp$deptime <= 2400), "deptime2"] = "Night Time"
temp[which(temp$deptime >= 0000 & temp$deptime <= 500), "deptime2"] = "Red Eye"

temp[which(temp$carrierdelay > 0), "carrierdelay"] = 1
#temp[which(temp$arrdelay > 0 | temp$depdelay >0), "delay"] = 1

temp$dayofweek = NULL
temp$deptime = NULL
#temp$arrdelay = NULL
#temp$depdelay = NULL

colnames(temp)[3] = "dayofweek"
colnames(temp)[4] = "deptime"

#drop NA in deptime
temp = temp[-which(is.na(temp$deptime)), ]

AA = temp[temp$uniquecarrier == "AA", ]
AA[is.na(AA$carrierdelay), "carrierdelay"] = 0
t = melt(AA)
dcast(t, uniquecarrier + dayofweek+deptime ~ variable, fun.aggregate=length)
dcast(t, uniquecarrier + dayofweek+deptime ~ variable, fun.aggregate=sum(carrierdelay))

library(plyr)
ddply(temp, c("uniquecarrier", "dayofweek", "deptime"), summarise, N = sum(!is.na(uniquecarrier)), delay = sum(!is.na(delay)))
)
