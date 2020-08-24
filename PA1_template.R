setwd("E:/ljubi/edukacija/Coursera/John Hopkins - Data Science Specialization/Reproducible research")
list.files()
rawData <- read.csv("activity.csv")
summary(rawData)
#around 14.5% of NA values, pretty significant
str(rawData)
year(rawData$date)
#when reading data use na.rm=TRUE, for example when grouping or summarizing
#by mean or sum
#convert dates into posixct or the formula year/month/day
library(lubridate)
library(dplyr)
library(plyr)
#1. Code for reading in the dataset and/or processing the data
#this is solved by read.csv , dataframe is pretty normal and tidy,
#just needs tidying by data and managment of NA values
colnames(rawData)
#2. Histogram of the total number of steps taken each day
library(ggplot2)
rawData$date <- as.Date(rawData$date)
sumsteps <- aggregate(rawData$steps,by=list(rawData$date),FUN=sum,na.rm=TRUE)
colnames(sumsteps) <- c("dates","steps")
hist(sumsteps$steps, main="Total number of steps",xlab="steps")
# 3. Mean and median number of steps taken each day
meansteps <- aggregate(rawData$steps,by=list(rawData$date),FUN=mean,na.rm=TRUE)
colnames(meansteps) <- c("dates","mean")

meansteps
mediansteps <- aggregate(rawData$steps,by=list(rawData$date),FUN=median,na.rm=TRUE)
colnames(mediansteps) <- c("dates","median")
# 4. Time series plot of the average number of steps taken
meansteps$dates <- as.Date(meansteps$dates)
ggplot(data=subset(meansteps, !is.na(mean)), aes(x=dates,y=mean)) + geom_line(color="#00AFBB", size =1) + stat_smooth() 
summary(meansteps$mean)
#5. The 5-minute interval that, on average, contains the maximum number of steps
maxmeaninterval <- rawData %>% group_by(interval)
meaninterval <- maxmeaninterval %>% summarise_all(funs(mean(.,na.rm=TRUE)))
meaninterval
newdata <- meaninterval[order(-meaninterval$steps),]
#need to remove dots from ranking steps!
newdata
#6. Code to describe and show a strategy for imputing missing data
#my strategy is to fulfill the missing values by the mean for that interval
#which is calculated from other days but at the same time(interval)
colnames(meaninterval)[2] <- c("average")
meaninterval
testData <- rawData
#df$A <- ifelse(is.na(df$A), df$B, df$A)
#joinati po intervalima mean u posebnom stupcu pa gore iskoristiti ifeelse
colnames(meaninterval)[2] <- c("averageint")
meaninterval
summary(meaninterval)
testData <- join(meaninterval, rawData, by=c("interval"))
str(testData)
#join baca 47 stepsa za cetvrti dan=!=!=
testData
meaninterval
testData <- cbind(meaninterval$averageint,rawData,by=c("interval"))
head(testData3)
colnames(testData)[1] <- c("averageSteps")
View(testData)
rawData
testdataifelse <- ifelse(is.na(testData$steps),testData$averageSteps,testData$steps)
#pogledati gdje su NA vrijednosti pa gdje je zamijenjeno meanom
testdataifelse
head(testdataifelse)
forgedData <- cbind(testdataifelse,rawData)
colnames(forgedData)[1] <- c("newvalues")
View(forgedData)
meaninterval
#meaninterval
#7. Histogram of the total number of steps taken
#each day after missing values are imputed
testingHist <- forgedData %>% group_by(date) %>% summarize(sum2=sum(newvalues))
testingHist <- data.frame(date=unique(forgedData$date),sum2=tapply(forgedData$newvalues,forgedData$date,FUN=sum))
head(testingHist)
#sum by summarize bylist=dates, use
hist(testingHist$sum2)
#this probably doesn't make sense, the question of histogram
#making a plot for number of steps for each day, it obviously refers to 
#a plot, because histogram is used for measuring frequency, not time
#episodes, but I will leave the code just in case
testingHist
plot(testingHist$date,testingHist$sum2)

#we take the mean from the new values, but by dates we select weekdays
#and weekends
base_plot <- ggplot(data = testingHist) +
    geom_line(aes(x = date, y = sum2), 
              color = "#09557f",
              alpha = 0.6,
              size = 0.6) +
    labs(x = "Date", 
         y = "Number of steps",
         title = "Number of steps taken each day") + theme_minimal()
base_plot
testingHist
#the values might be adjusted or rescaled due to extreme values which
#reconfigure the plot on its own, just a thought

##8. Panel plot comparing the average number of steps
#taken per 5-minute interval across weekdays and weekends

#weekdays and weekend , we should split the data into weekdays and weekends
week <- as.data.frame(weekdays(testingHist$date))
week
testingHist2 <- testingHist
testingHist2 <- cbind(testingHist,week)
colnames(testingHist2)[3] <- c("dayofweek")
testingHist2
weekend <- c("Saturday","Sunday")
dataweekend <- testingHist2[testingHist2$dayofweek %in% weekend,]
dataweekend
weekend
testingHist2$dayofweek
workdays <- testingHist2[!testingHist2$dayofweek %in% weekend,]
workdays
testingHist2
# weekend<-c("Saturday","Sunday") then, subset using this: 
#     new_data_weekends<-
# df1[df1$weekday %in% weekend,] and for weekdays:
# new_data_weekdays<-df1[df1$weekday !%in% weekend,]
testingHist
forgedData
#use forgedData(the imouted NA's data ) with summarizing mean
newDatamean <- data.frame(date=unique(forgedData$date),mean2=tapply(forgedData$newvalues,forgedData$date,FUN=mean))
#testingHist <- data.frame(date=unique(forgedData$date),sum2=tapply(forgedData$newvalues,forgedData$date,FUN=sum))
newDatameanDays <- cbind(newDatamean,week)
colnames(newDatameanDays)[3] <- c("days")
dataweekendMean <- newDatameanDays[newDatameanDays$days %in% weekend,]
dataweekendMean
str(dataweekendMean)
dataworkdaysMean <- newDatameanDays[!newDatameanDays$days %in% weekend,]
head(dataworkdaysMean)
head(dataweekendMean)
dataworkdaysMean$days <- as.character(dataworkdaysMean$days)
dataworkdaysMean
dataweekendMean$days <- as.character(dataweekendMean$days)
par(mfrow=c(1,2),mar=c(4,4,2,1))
plot(dataworkdaysMean$days,dataworkdaysMean$mean2)
rng <- range(dataweekendMean$mean2,dataworkdaysMean$mean2,na.rm=TRUE)
plot(dataweekendMean$days,dataweekendMean$mean2,pch=20,ylim=rng,main="Weekend",ylab="average steps")
abline(h=median(dataweekendMean$mean2,na.rm=TRUE))
plot(dataworkdaysMean$days,dataworkdaysMean$mean2,pch=20,ylim=rng,main="Monday-Friday",ylab="average steps")
abline(h=median(dataworkdaysMean$mean2,na.rm=TRUE))
dev.off()
plot(dataweekendMean$days,dataweekendMean$mean2)
dataweekendMean$days <- as.factor(dataweekendMean$days)
str(dataweekendMean$days)

dataweekendMean$days <- as.factor(as.character(dataweekendMean$days))
str(dataweekendMean$days)
dataweekendMean$days[3]
junk$alpha[junk$alpha == "B"] <- "b"
dataweekendMean$days[dataweekendMean$days=="3"] <- c("Saturday")
dataweekendMean
plot(dataweekendMean$days,dataweekendMean$mean2,pch=20,ylim=rng,main="Weekend",ylab="average steps",type="l")
abline(h=median(dataweekendMean$mean2,na.rm=TRUE))
str(dataweekendMean)
str(dataweekendMean)
par(mfrow=c(1,2),mar=c(4,4,2,1))
base_plot1 <- ggplot(data = dataweekendMean) +
    geom_line(aes(x = date, y = mean2), 
              color = "#09557f",
              alpha = 0.6,
              size = 0.6) +
    labs(x = "Date", 
         y = "Number of steps",
         title = "Number of steps - weekend") + theme_minimal()
base_plot1
base_plot2 <- ggplot(data = dataworkdaysMean) +
    geom_line(aes(x = date, y = mean2), 
              color = "#09557f",
              alpha = 0.6,
              size = 0.6) +
    labs(x = "Date", 
         y = "Number of steps",
         title = "Number of steps- workdays") + theme_minimal()
base_plot1
base_plot2
attach(dataworkdaysMean)
rng <- range(dataweekendMean$mean2,dataworkdaysMean$mean2,na.rm=TRUE)
plot(date, mean2, main="Workdays",
     xlab="Car Weight ", ylab="Miles Per Gallon ", pch=19,ylim=rng) 
abline(h=median(dataworkdaysMean$mean2,na.rm=TRUE))
attach(dataweekendMean)
plot(date, mean2, main="Weekend",
     xlab="Car Weight ", ylab="Miles Per Gallon ", pch=19,ylim=rng) 
abline(h=median(dataweekendMean$mean2,na.rm=TRUE))
summary(dataweekendMean)
summary(dataworkdaysMean)
