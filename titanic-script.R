library(ggplot2)
library(reshape)
library(zoo)
rm(list=ls())
setwd("/home/erpreciso/Documents/school/titanic")
train <- read.csv("data/train.csv")
test <- read.csv("data/test.csv")

## simple prevision based on sex and age
train[is.na(train$Age), "Age"] <- median(train$Age, na.rm=TRUE)
simple.model <- lm(Survived ~ Sex + Age, data=train)
test[is.na(test$Age), "Age"] <- median(test$Age, na.rm=TRUE)
res <- data.frame(Sex=test$Sex, Age=test$Age, PassengerId=test$PassengerId)
res$Survived <- round(predict(simple.model, res))
res <- res[,c("PassengerId", "Survived")]
# check of no missing values
paste("Missing value #: ", sum(is.na(res$Survived)))
write.csv(res, "submissions/genderagelm.csv", row.names=FALSE)
