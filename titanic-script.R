library(ggplot2)
library(reshape)
library(zoo)
rm(list=ls())
setwd("/home/erpreciso/Documents/school/titanic")
train <- read.csv("data/train.csv")
test <- read.csv("data/test.csv")

# evaluate fare paid

##  prevision based on sex and fare
titanic.model <- lm(Survived ~ Sex + Fare, data=train)
test[is.na(test$Fare), "Fare"] <- median(test$Fare, na.rm=TRUE)
test$Survived <- round(predict(titanic.model, test))
submission <- test[,c("PassengerId", "Survived")]

# check of no missing values
paste("Missing value #: ", sum(is.na(submission$Survived)))

# export for submission
write.csv(submission, "submissions/genderfarelm.csv", row.names=FALSE)
