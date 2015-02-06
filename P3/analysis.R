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
df$depdelay2 = NA
df$arrdelay2 = NA
df[which(df$depdelay>0), "depdelay2"] = 1
df[which(df$depdelay<=0), "depdelay2"] = 0

df[which(df$arrdelay>0), "arrdelay2"] = 1
df[which(df$arrdelay<=0), "arrdelay2"] = 0

#drop columns
df$depdelay = NULL
df$arrdelay = NULL

colnames(df) = c("origin", "dest", "depdeplay", "arrdelay")

getProbDelay = function(df, iataCode){
  #filter the related iata data
  temp = df[df$origin == iataCode | df$dest == iataCode, ]
  #get the total number of flight
  totalFlight = nrow(temp)
  #get the total number of delay
  totalDelay = sum(temp$depdeplay, na.rm = TRUE) + sum(temp$arrdelay, na.rm = TRUE) 
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
df = data[, c("dayofweek", "deptime", "uniquecarrier", "arrdelay", "depdelay")]
df$dayofweek2 = NA
df$deptime2 = NA

df[df$dayofweek <= 5, "dayofweek2"] = "Weekday"
df[df$dayofweek >= 6, "dayofweek2"] = "Weekend"

df[which(df$deptime >= 501 & df$deptime <= 1700), "deptime2"] = "Day Time"
df[which(df$deptime >= 1701 & df$deptime <= 2400), "deptime2"] = "Night Time"
df[which(df$deptime >= 0000 & df$deptime <= 500), "deptime2"] = "Red Eye"

df[which(df$arrdelay > 0 | df$depdelay >0), "delay"] = 1
df[which(df$arrdelay <= 0 & df$depdelay <= 0), "delay"] = 0

df$dayofweek = NULL
df$deptime = NULL
df$arrdelay = NULL
df$depdelay = NULL

colnames(df)[2] = "dayofweek"
colnames(df)[3] = "deptime"

#drop NA
df = df[-which(is.na(df$deptime)), ]
df = df[-which(is.na(df$delay)), ]

#t = melt(df)
#dcast(t, uniquecarrier + dayofweek+deptime ~ variable, fun.aggregate=length)
#dcast(t, uniquecarrier + dayofweek+deptime ~ variable, fun.aggregate=sum, na.rm=T)

library(plyr)

#ddply(temp, c("uniquecarrier", "dayofweek", "deptime"), summarise, totalFlight = sum(!is.na(uniquecarrier)), totalDelay = sum(delay), prob = sum(delay) / sum(!is.na(uniquecarrier)))
result = ddply(df, c("uniquecarrier", "dayofweek", "deptime"), summarise, probdelay = sum(delay) / sum(!is.na(uniquecarrier)))
write.csv(result, file="output_R.csv", row.names=F)