# Jingwei Feng
# Data analytics
# Lab1 

rm(list=ls())
# setwd("D:/RPI Grad Courses/Data_analytics")
# install.packages("readxl")
#library("readxl")
# my_data <- read_excel("GPW3_GRUMP_SummaryInformation_2010.xls",header=T,skip=17)
# my_data <- read_excel("2010EPI_data.xls",sheet="EPI2010_onlyEPIcountries")

# my_data <- read.csv("GPW3_GRUMP_SummaryInformation_2010.csv",header=T)
# my_data$PopulationPerUnit
my_epi <- read.csv("2010EPI_data.csv",header=T,skip=1)
attach(my_epi)
# fix(my_epi)
head(my_epi)
EPI <- my_epi$EPI
na <- is.na(EPI)
EPI <- EPI[!na]
summary(EPI)
# na.rm is all NaN are droped before compute the statistics
fivenum(EPI,na.rm=TRUE)
stem(EPI)
hist(EPI)
hist(EPI,seq(30.,95.,1.0),prob=TRUE)
lines(density(EPI,na.rm=TRUE,bw=1.))
lines(density(EPI,na.rm=TRUE,bw=SJ))
rug(EPI)
# exercise 1
plot(ecdf(EPI),do.points=FALSE,verticals=TRUE)
par(oty="s")
qqnorm(EPI)
qqline(EPI)
qqplot(qt(ppoints(250),df=5),x,xlab="Q-Q plot for t dsn")
qqline(x)

daly <- my_epi$DALY
daly <-daly[!is.na(daly)]
summary(daly)
fivenum(daly,na.rm=TRUE)
stem(daly)
hist(daly)
hist(daly,seq(30,95,1.0),prob=TRUE)
lines(density(daly,na.rm=TRUE,bw=1.))
rug(daly)

plot(ecdf(daly),do.points=FALSE,verticals=TRUE)
par(oty="s")
qqnorm(daly)
qqline(daly)
qqplot(qt(ppoints(250),df=5),x,xlab="Q-Q plot for t dsn")
qqline(x)

water_h <- my-epi$WATER_H
daly <-daly[!is.na(water_h)]
summary(water_h)
fivenum(water_h,na.rm=TRUE)
stem(water_h)
hist(water_h)
hist(water_h,seq(30,95,1.0),prob=TRUE)
lines(density(water_h,na.rm=TRUE,bw=1.))
rug(water_h)

plot(ecdf(water_h),do.points=FALSE,verticals=TRUE)
par(oty="s")
qqnorm(water_h)
qqline(water_h)
qqplot(qt(ppoints(250),df=5),x,xlab="Q-Q plot for t dsn")
qqline(x)

boxplot(EPI,DALY)
qqplot(EPI,DALY)

EPIland <- EPI[!Landlock]
Eland <- EPIland[!is.na(EPIland)]
his(Eland)
hist(Eland,seq(30,95,1.0),prob=TRUE)
plot(ecdf(Eland),do.points=FALSE,verticals=TRUE)
par(oty="s")
qqnorm(Eland)
qqline(Eland)
qqplot(qt(ppoints(250),df=5),x,xlab="Q-Q plot for t dsn")
qqline(x)

my_data <- read.csv("GPW3_GRUMP_SummaryInformation_2010.csv",header=T)
pp <- my_data$PopulationPerUnit
pp <-pp[!is.na(pp)]
summary(pp)
fivenum(pp,na.rm=TRUE)
stem(pp)
hist(pp)
hist(pp,seq(30,95,1.0),prob=TRUE)
lines(density(pp,na.rm=TRUE,bw=1.))
rug(pp)

plot(ecdf(pp),do.points=FALSE,verticals=TRUE)
par(oty="s")
qqnorm()
qqline(pp)
qqplot(qt(ppoints(250),df=5),x,xlab="Q-Q plot for t dsn")
qqline(x)

my_data1 <- read.csv("water_treatment.csv",header=T)
x <- my_data1$PH-D
x <-x[!is.na(x)]
summary(x)
fivenum(x,na.rm=TRUE)
stem(x)
hist(x)
hist(x,seq(30,95,1.0),prob=TRUE)
lines(density(x,na.rm=TRUE,bw=1.))
rug(x)

plot(ecdf(x),do.points=FALSE,verticals=TRUE)
par(oty="s")
qqnorm()
qqline(x)
qqplot(qt(ppoints(250),df=5),x,xlab="Q-Q plot for t dsn")
qqline(x)
