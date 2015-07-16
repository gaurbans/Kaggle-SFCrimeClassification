setwd("~/SFcrime/")

library(caret)
library(lubridate)

#Load in data.
traindata <- read.csv("train.csv")
testdata <- read.csv("test.csv")

#Store names of Category variables. This will come in handy later!
trainnames <- sort(unique(as.character(traindata$Category)))

#Reorganize data.
traindata$Category <- make.names(traindata$Category)
traindata$Category <- as.factor(traindata$Category)
traindata$Dates <- strptime(traindata$Dates, "%Y-%m-%d %H:%M:%S", tz="GMT")
traindata$Years <- as.factor(year(traindata$Dates))
traindata$Months <- as.factor(months(traindata$Dates))
traindata$Hours <- as.factor(hour(traindata$Dates))
traindata <- traindata[, -c(3,6)]

testdata$Dates <- strptime(testdata$Dates, "%Y-%m-%d %H:%M:%S", tz="GMT")
testdata$Years <- as.factor(year(testdata$Dates))
testdata$Months <- as.factor(months(testdata$Dates))
testdata$Hours <- as.factor(hour(testdata$Dates))

#Create as smaller dataset (2% of traindata) to build a model on. 
set.seed(50)
trainsmall <- traindata[sample(1:nrow(traindata), round(nrow(traindata)/50, 0)),]

outersect <- function(x, y) {
  sort(c(setdiff(x, y),
         setdiff(y, x)))
}

##See which Category variables have not been included and add them if necessary. 
outersect(unique(trainsmall$Category), unique(traindata$Category))

##"TREA" is missing, so add it. 
trainsmall <- rbind(trainsmall, traindata[head(which(traindata$Category=="TREA"),1),])
trainsmall <- trainsmall[order(trainsmall$Dates),]
trainsmall <- droplevels(trainsmall)
trainsmall <- trainsmall[,-5]

#Build random forst model.
set.seed(1234)
Sys.time()
fit <- train(Category ~ PdDistrict + Years + Months + DayOfWeek + Hours + X + Y, method="rf", data=trainsmall)
Sys.time()

solution <- predict(fit, testdata, type="prob")
solution <- cbind(Id=testdata$Id, solution)

#Check to see if solution has all the Category variables as traindata (or trainnames) 
outersect(names(solution), make.names(trainnames))

names(solution) <- c("Id", trainnames)

write.csv(solution, "solution.csv", row.names=F)



