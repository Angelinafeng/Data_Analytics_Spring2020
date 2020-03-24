#Jingwei Feng
# lab from thursday before spring break
# Group 2 labs
rm(list=ls())
set.seed(12345)
par(mar=rep(0.2,4))
data_Matrix <- matrix(rnorm(400),nrow=40)
image(1:10,1:40,t(data_Matrix)[,nrow(data_Matrix):1])
heatmap(data_Matrix)

set.seed(678910)
for(i in 1:40){
  coin_Flip <- rbinom(1,size=1,prob=0.5)
  if(coin_Flip){
    data_Matrix[i,] <- data_Matrix[i,]+rep(c(0,3),each=5)
  }
}

par(mar=rep(0.2,4))
image(1:10,1:40,t(data_Matrix)[,nrow(data_Matrix):1])
par(mar=rep(0.2,4))
heatmap(data_Matrix)

hh <- hclust(dist(data_Matrix))
data_Matrix_Ordered <- data_Matrix[hh$order,]
par(mfrow=c(1,3))
image(t(data_Matrix_Ordered)[,nrow(data_Matrix_Ordered):1])
plot(rowMeans(data_Matrix_Ordered),40:1, xlab='The Row Mean',ylab='Row',pch=19)
plot(colMeans(data_Matrix_Ordered),xlab='Column',ylab='Column Mean',pch=19)

rm(list=ls())
# install.packages("titanic")
library(titanic)
# rpart
library(rpart)
fit_rpart <- rpart(Survived~.,data=titanic_train)
summary(fit_rpart)
titanic_train_filter <- titanic_train[titanic_train$Embarked != "",-c(1,4,9,11)]
filter_rpart1 <- rpart(Survived~.,data=titanic_train_filter)
summary(filter_rpart1)
printcp(filter_rpart1)
par(mfrow=c(1,2)) 
rsq.rpart(filter_rpart1)

#ctree
require(party)
fit2 <- ctree(Survived~.,data=na.omit(titanic_train_filter))
summary(fit_rpart)
plot(fit_ctree)

#hclust
library(dummies)
hh1 <- hclust(dist(na.omit(titanic_train_filter)))
hh1_Ordered <- titanic_train_filter.dummy[hh$order,]
par(mfrow=c(1,3))
image(t(hh1_Ordered)[,nrow(hh1_Orderedd):1])
plot(rowMeans(hh1_Ordered),40:1, xlab='The Row Mean',ylab='Row',pch=19)
plot(colMeans(hh1_Ordered),xlab='Column',ylab='Column Mean',pch=19)

# randomForest
library(randomForest)
fit3 <- randomForest(Survived ~ ., data = hh1_Ordered)
plot(fit3)