library(ggplot2)
library(reshape)
library(zoo)
rm(list=ls())
setwd("/home/erpreciso/Documents/school/titanic")
train <- read.csv("data/train.csv")
test <- read.csv("data/test.csv")

## simple prevision based on sex and age
# proportion table for sex
prop.table(table(train$Sex, train$Survived), 1)
# table for sex and ages in categories
train$AgeSimulated <- na.approx(train$Age) # simulate NA in Age in train
train$AgeCluster <- cut(train$AgeSimulated, seq(0,80,5))
# aggregate(Survived ~ Sex + AgeCluster, data=train, FUN=sum)
qplot(AgeCluster,   # visualize
      data=train,
      fill=as.factor(Survived),
      facets= . ~ Sex,
      geom="histogram")


simple.model <- lm(Survived ~ Sex + AgeSimulated, data=train)
res <- data.frame(Sex=test$Sex, Age=test$Age, PassengerId=test$PassengerId)
res$AgeSimulated <- na.approx(res$Age, na.rm=FALSE) # simulate NAs
res$AgeSimulated <- na.locf(res$AgeSimulated) # simulate trailing NAs
res$Survived <- round(predict(simple.model, res))
res <- res[,c("PassengerId", "Survived")]
# check of no missing values
paste("Missing value #: ", sum(is.na(res$Survived)))
write.csv(res, "submissions/genderagelm.csv", row.names=FALSE)
