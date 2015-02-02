library(ggplot2)
library(plyr)

#set working directory
setwd("C:/[DATA]/DataLiterate/P2")

#set the csv directory
csvDir = paste0(getwd(), "/csv")

#get list of csv
csvFiles = list.files(path=csvDir, full.names=TRUE)

#combine all csv data into single data frame
allData = do.call(rbind.fill, lapply(csvFiles, read.csv))

#change all column names as lowercase
colnames(allData) = tolower(colnames(allData))

#check the variables
variableNames = unique(allData$variable)

#found that there is some \n in the variable name
#replace all "\n" in variable names, replace some empty space, convert to lowercase
allData$variable = gsub("\n ", "", allData$variable)
allData$variable = gsub("  ", " ", allData$variable)
allData$variable = tolower(allData$variable)

#correct the date format
allData$date = as.Date(gsub("/14$", "/2014", allData$date), "%m/%d/%Y")


#show all unique variable names
allVariable = unique(allData$variable)
#allVariable

#there is a column named x, but only 3 rows have data
#idx = !is.na(allData$x)
#allData[idx, ] 
#they are not relevant, remove the whole column
allData$x = NULL # remove this column


totalDeaths = allData[allData$variable == "total death/s in confirmed cases", ]
#drop variable column
totalDeaths$variable = NULL


library(reshape2)
library(RColorBrewer)

t = melt(totalDeaths, id.vars="date")
colnames(t)[2] = "location" 

colorCount = length(unique(t$location))
getPalette = colorRampPalette(brewer.pal(12, "Paired"))

lp1 = ggplot(data=t, aes(x=date, y=value, group=location, color=location)) + geom_point() + geom_line(data=t[!is.na(t$value),], size=2)

lp1 + scale_colour_manual(name = "Location",
                          values=getPalette(colorCount),
                          breaks=c("national", "bomi.county", "bong.county",
                                   "grand.kru", "lofa.county", "margibi.county",
                                   "maryland.county", "montserrado.county", "nimba.county",
                                   "river.gee.county", "rivercess.county", "sinoe.county",
                                   "gbarpolu.county", "grand.bassa", "grand.cape.mount",
                                   "grand.gedeh"),
                          labels=c("National", "Bomi County", "Bong County",
                                   "Grand Kru County", "Lofa County", "Margibi County",
                                   "Maryland County", "Montserrado County", "Nimba County",
                                   "River Gee County", "Rivercess County", "Sinoe County",
                                   "Gbarpolu County", "Grand Bassa County", "Grand Cape Mount County",
                                   "Grand Gedeh County")) +
  xlab("Date") +
  ylab("Total Number of Deaths")  + 
  ggtitle("Total death/s in confirmed cases")


newCasesProb = allData[allData$variable == "new case/s (probable)", ]
#remove the column about variable
newCasesProb$variable = NULL
t = melt(newCasesProb, id.vars="date")

lp1 = ggplot(data=t, aes(x=date, y=value, group=variable, color=variable)) + geom_point() + geom_line(data=t[!is.na(t$value),], size=2)

lp1 + scale_colour_manual(name = "Location",
                          values=getPalette(colorCount),
                          breaks=c("national", "bomi.county", "bong.county",
                                   "grand.kru", "lofa.county", "margibi.county",
                                   "maryland.county", "montserrado.county", "nimba.county",
                                   "river.gee.county", "rivercess.county", "sinoe.county",
                                   "gbarpolu.county", "grand.bassa", "grand.cape.mount",
                                   "grand.gedeh"),
                          labels=c("National", "Bomi County", "Bong County",
                                   "Grand Kru County", "Lofa County", "Margibi County",
                                   "Maryland County", "Montserrado County", "Nimba County",
                                   "River Gee County", "Rivercess County", "Sinoe County",
                                   "Gbarpolu County", "Grand Bassa County", "Grand Cape Mount County",
                                   "Grand Gedeh County"))  +
  xlab("Date") +
  ylab("Number of New Cases (Probable)")  + 
  ggtitle("New Case/s (Probable)")

totalConfirmedCases = allData[allData$variable == "total confirmed cases", ]
#remove the column about variable
totalConfirmedCases$variable = NULL

library(zoo)

removeInconsistent = function(values){
  orgValues = values
  #forward check
  idx = which(diff(na.locf(values, na.rm=FALSE))<0) + 1
  if(length(idx) != 0){
    values[idx] = NA  
  }
  
  #backward check
  idx = which(rev(diff(na.locf(rev(values), na.rm=FALSE)) > 0))
  if(length(idx) != 0){
    values[idx] = NA  
  }
  values
}

#for each 2nd column till the last column, check
for (i in 2:ncol(totalConfirmedCases)){
  totalConfirmedCases[,i] = removeInconsistent(totalConfirmedCases[, i])  
}

t = melt(totalConfirmedCases, id.vars="date")

lp1 = ggplot(data=t, aes(x=date, y=value, group=variable, color=variable)) + geom_point() + geom_line(data=t[!is.na(t$value),], size=2)

lp1 + scale_colour_manual(name = "Location",
                          values=getPalette(colorCount),
                          breaks=c("national", "bomi.county", "bong.county",
                                   "grand.kru", "lofa.county", "margibi.county",
                                   "maryland.county", "montserrado.county", "nimba.county",
                                   "river.gee.county", "rivercess.county", "sinoe.county",
                                   "gbarpolu.county", "grand.bassa", "grand.cape.mount",
                                   "grand.gedeh"),
                          labels=c("National", "Bomi County", "Bong County",
                                   "Grand Kru County", "Lofa County", "Margibi County",
                                   "Maryland County", "Montserrado County", "Nimba County",
                                   "River Gee County", "Rivercess County", "Sinoe County",
                                   "Gbarpolu County", "Grand Bassa County", "Grand Cape Mount County",
                                   "Grand Gedeh County")) +
  xlab("Date") +
  ylab("Total Number of Confirmed Cases")  + 
  ggtitle("Total Number of Confirmed Cases")




#extract the new cases
newCases = allData[grepl("new case*", allData$variable), ]
#drop variable, national column
newCases$variable = NULL
newCases$national = NULL

#convert the date to store month value only
newCases$date = unlist(lapply(newCases$date, function(x){format(x, "%m")}))

#calculate the total number of new cases for each month
newCases = ddply(newCases, .(date), function(x) colSums(x[,-1], na.rm = TRUE))

newCases = melt(newCases, id.vars="date")

#mapping for the map
#replace the name
mapping = data.frame(shortForm = c("bomi.county", "bong.county",
                                   "grand.kru", "lofa.county", "margibi.county",
                                   "maryland.county", "montserrado.county", "nimba.county",
                                   "river.gee.county", "rivercess.county", "sinoe.county",
                                   "gbarpolu.county", "grand.bassa", "grand.cape.mount",
                                   "grand.gedeh"),
                     fullName = c("BOMI", "BONG", 
                                  "GRANDKRU", "LOFA", "MARGIBI",
                                  "MARYLAND", "MONTSERRADO", "NIMBA",
                                  "RIVER GEE", "RIVER CESS", "SINOE",
                                  "GBAPOLU", "GRANDBASSA", "GRAND CAPE MOUNT", 
                                  "GRANDGEDEH"))

#update the county name
newCases$variable = factor(newCases$variable, levels=mapping$shortForm, labels=mapping$fullName)

library(scales)
library(rgeos)
library(maptools)

lbr_dist <- readShapeSpatial("./LBR_adm/LBR_adm1.shp") #admin boundary level 1
lbr_dist <- fortify(lbr_dist, region = "NAME_1") # get the name of county for plotting
lbr_dist$id <- toupper(lbr_dist$id) #change the id to uppercase
#calculate the center
distcenters <- ddply(lbr_dist, .(id), summarize, clat = mean(lat), clong = mean(long))

jun = newCases[newCases$date == "06", c("variable", "value")]
p1 = ggplot() + geom_map(data =jun, aes(map_id = variable, fill = value), 
                         map = lbr_dist) + expand_limits(x = lbr_dist$long, y = lbr_dist$lat) + 
  scale_fill_gradient2(low = muted("blue"), mid="orange", midpoint = 400, high = muted("red"), limits = c(0, 800)) + 
  geom_text(data = distcenters, aes(x = clong, y = clat, label = id, size = 0.2)) +
  xlab("")+ ylab("") +
  ggtitle("The Number of New Ebola Cases in Liberia\nJun 2014")


jul = newCases[newCases$date == "07", c("variable", "value")]
p2 = ggplot() + geom_map(data =jul, aes(map_id = variable, fill = value), 
                         map = lbr_dist) + expand_limits(x = lbr_dist$long, y = lbr_dist$lat) + 
  scale_fill_gradient2(low = muted("blue"), mid="orange", midpoint = 400, high = muted("red"), limits = c(0, 800)) + 
  geom_text(data = distcenters, aes(x = clong, y = clat, label = id, size = 0.2)) +
  xlab("")+ ylab("") +
  ggtitle("The Number of New Ebola Cases in Liberia\nJul 2014")

aug = newCases[newCases$date == "08", c("variable", "value")]
p3 = ggplot() + geom_map(data =aug, aes(map_id = variable, fill = value), 
                         map = lbr_dist) + expand_limits(x = lbr_dist$long, y = lbr_dist$lat) + 
  scale_fill_gradient2(low = muted("blue"), mid="orange", midpoint = 400, high = muted("red"), limits = c(0, 800)) + 
  geom_text(data = distcenters, aes(x = clong, y = clat, label = id, size = 0.2)) +
  xlab("")+ ylab("") +
  ggtitle("The Number of New Ebola Cases in Liberia\nAug 2014")

sept = newCases[newCases$date == "09", c("variable", "value")]
p4 = ggplot() + geom_map(data =sept, aes(map_id = variable, fill = value), 
                         map = lbr_dist) + expand_limits(x = lbr_dist$long, y = lbr_dist$lat) + 
  scale_fill_gradient2(low = muted("blue"), mid="orange", midpoint = 400, high = muted("red"), limits = c(0, 800)) + 
  geom_text(data = distcenters, aes(x = clong, y = clat, label = id, size = 0.2)) +
  xlab("")+ ylab("") +
  ggtitle("The Number of New Ebola Cases in Liberia\nSept 2014")

oct = newCases[newCases$date == "10", c("variable", "value")]
p5 = ggplot() + geom_map(data =oct, aes(map_id = variable, fill = value), 
                         map = lbr_dist) + expand_limits(x = lbr_dist$long, y = lbr_dist$lat) + 
  scale_fill_gradient2(low = muted("blue"), mid="orange", midpoint = 400, high = muted("red"), limits = c(0, 800)) + 
  geom_text(data = distcenters, aes(x = clong, y = clat, label = id, size = 0.2)) +
  xlab("")+ ylab("") +
  ggtitle("The Number of New Ebola Cases in Liberia\nOct 2014")

nov = newCases[newCases$date == "11", c("variable", "value")]
p6 = ggplot() + geom_map(data =nov, aes(map_id = variable, fill = value), 
                         map = lbr_dist) + expand_limits(x = lbr_dist$long, y = lbr_dist$lat) + 
  scale_fill_gradient2(low = muted("blue"), mid="orange", midpoint = 400, high = muted("red"), limits = c(0, 800)) + 
  geom_text(data = distcenters, aes(x = clong, y = clat, label = id, size = 0.2)) +
  xlab("")+ ylab("") +
  ggtitle("The Number of New Ebola Cases in Liberia\nNov 2014")

dec = newCases[newCases$date == "12", c("variable", "value")]
p7 = ggplot() + geom_map(data =dec, aes(map_id = variable, fill = value), 
                         map = lbr_dist) + expand_limits(x = lbr_dist$long, y = lbr_dist$lat) + 
  scale_fill_gradient2(low = muted("blue"), mid="orange", midpoint = 400, high = muted("red"), limits = c(0, 800)) + 
  geom_text(data = distcenters, aes(x = clong, y = clat, label = id, size = 0.2)) +
  xlab("")+ ylab("") +
  ggtitle("The Number of New Ebola Cases in Liberia\nDec 2014")


# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
# from: http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

multiplot(p1, p3, p5, p7, p2, p4, p6, cols=2)
