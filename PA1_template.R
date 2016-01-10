setwd("C:/Users/Derek/Desktop/datasciencecoursera/data")

# Required Libraries
library("knitr")
library("dplyr")
library("lubridate")
library("ggplot2")
act<-read.csv("activity.csv")

#Question #1
paces<-tbl_df(na.omit(act))
paces<-select(act, steps, date)%>%group_by(date)%>%summarize(steps=sum(steps))
hist(paces$steps, main= "Total # of Steps Per Day ex-NAs \n(10/1/12 through 11/30/12)", 
     xlab="Steps", col="white")
paces<-tapply(paces$steps,paces$date,sum)
mean(paces, na.rm=TRUE)
median(paces, na.rm=TRUE)

#Question #2
apaces<-select(act, steps, date, interval)%>%group_by(interval)%>%summarize(steps=mean(steps,na.rm=TRUE))
plot(x=apaces$interval,y=apaces$steps,type="l", xlab="5-Minute Interval", ylab="Average # of Steps", 
     main="Time Series of Average Steps in 5-Minute Intervals \nAcross All Days (10/1/12 -11/30/12)")
max_interval<-apaces[which.max(apaces$steps),1]
max_interval

# Question #3

# Calculate and report the number of missing values (e.g., NAs)
no_value<-which(is.na(act$step))
length(no_value)

# Complete the dataset by inserting the mean for the specific 5-minute interval
mfile<-subset(act,is.na(steps),select=c("steps","date","interval"))
minterval<-select(act, steps, date, interval)%>%group_by(interval)%>%summarize(steps=mean(steps,na.rm=TRUE))
missing<-merge(mfile,minterval, by="interval")
good_file<-subset(act,!is.na(steps),select=c("steps","date","interval"))
missing<-select(missing, -steps.x)
colnames(missing)<-c("interval","date","steps")
missing<-select(missing,steps,date,interval)

# This is the new data Set
new_file<-rbind(missing,good_file)

# This section constructs the histogram with the completed new dataset
n_file<-select(new_file, steps, date)%>%group_by(date)%>%summarize(steps=sum(steps,na.rm=TRUE))
hist(n_file$steps, main= "Total # of Steps Per Day incl. Imputted Data \n(10/1/12 through 11/30/12)", 
     xlab="Steps", col="white")

# This section computes and report the mean and median with using the new dataset
n_file<-tapply(n_file$steps,n_file$date,sum)
mean(n_file,na.rm=TRUE)
median(n_file,na.rm=TRUE)

# Question #4
# Create weekend, weekday factor
new_file$date <-wday(new_file$date, label=TRUE)
wdays<-c("Mon","Tues","Wed","Thurs","Fri")
business<-new_file$date%in%wdays
new_file<-cbind(new_file,business)
new_file$business<-as.factor(new_file$business)
levels(new_file$business)<-list(weekend="FALSE", weekday="TRUE")

# Compute average number of steps taken over the 5-minute interval across the days
# and constructs a panel plot containing a time series plot.
activity<-select(new_file, steps, date, business, interval)%>%group_by(interval,business)%>%summarize(steps=mean(steps,na.rm=TRUE))
ggplot(activity, aes(x=interval,y=steps))+geom_line()+facet_wrap(~business,nrow=2)+ylab("Average Number of Steps")




