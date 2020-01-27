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
EPI_new <- EPI[!na]
EPI_new
summary(EPI)
# na.rm is all NaN are droped before compute the statistics
fivenum(EPI,na.rm=TRUE)
stem(EPI)
hist(EPI)
hist(EPI,seq(30.,95.,1.0),prob=TRUE)
lines(density(EPI,na.rm=TRUE,bw=1.))
lines(density(EPI,na.rm=TRUE,bw=SJ))
rug(EPI)
plot(ecdf(EPI),do.points=FALSE,verticals=TRUE)
par(oty="s")
qqnorm(EPI)
qqline(EPI)
qqplot(qt(ppoints(250),df=5),x,xlab="Q-Q plot for t dsn")
qqline(x)
boxplot(EPI,DALY)