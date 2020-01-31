# Jingwei Feng
# lab 1 part2
# 1/30/2020

rm(list=ls())

my_epi <- read.csv("2010EPI_data.csv",skip=1)
epi <- EPI[!is.na(my_epi$EPI)]
plot(ecdf(epi),do.points=FALSE,verticals=TRUE)
qqnorm(epi)
qqline(epi)

x <- seq(30,95,1)
qqplot(qt(ppoints(250),df=5),x,xlab="Q-Q plot for t dsn")

qqline(x)

plot(ecdf(epi),do.points=TRUE,verticals=TRUE)
par(pty="s")

qqnorm(my_epi$EPI)
qqline(my_epi$EPI)

daly <- my_epi$DALY
plot(ecdf(daly),do.points=FALSE,verticals=TRUE)
water <- my_epi$WATER_H
plot(ecdf(water),do.points=FALSE,verticals=TRUE)

qqplot(EPI,DALY)
boxplot(my_epi$EPI,my_epi$DALY)

multivariate <- read.csv("multivariate.csv")
attach(multivariate)
mm <- lm(Homeowners~Immigrant)
mm
summary(mm)$coef
# the inercept is beta
# variable coefficient is immigrant estimate
plot(Homeowners~Immigrant)
# help(abline)
abline(mm)
abline(mm,col="blue",lwd=3,lty=3)

n_immigrant <- data.frame(Immigrant=c(0,20))
# mm %>% predict(n_immigrant)
attributes(mm)
mm$coefficients

plot(mtcars$wt,mtcars$mpg)
library(ggplot2)
qplot(mtcars$wt,mtcars$mpg)
qplot(wt,mpg,data=mtcars)
ggplot(mtcars,aes(x=wt,y=mpg))+geom_point()
plot(pressure$temperature,pressure$pressure,type="l")
points(pressure$temperature,pressure$pressure)

lines(pressure$temperature,pressure$pressure/2,col="red")
points(pressure$temperature,pressure$pressure/2,col="blue")
qplot(pressure$temperature,pressure$pressure,geom="line")
qplot(temperature,pressure,data=pressure,geom="line")
ggplot(pressure,aes(x=temperature,y=pressure))+geom_line()+geom_point()

barplot(BOD$demand,names.arg=BOD$Time)
hist(mtcars$mpg)
