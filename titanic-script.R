library(ggplot2)
library(reshape)
library(zoo)
rm(list=ls())
setwd("/home/erpreciso/Documents/school/titanic")
train <- read.csv("data/train.csv")
test <- read.csv("data/test.csv")

# evaluate cabin placement
train[grep("A",train$Cabin),"cabin.class"] <- "A"
train[grep("B",train$Cabin),"cabin.class"] <- "B"
train[grep("C",train$Cabin),"cabin.class"] <- "C"
train[grep("D",train$Cabin),"cabin.class"] <- "D"
train[grep("E",train$Cabin),"cabin.class"] <- "E"
train[train$Cabin=="", "cabin.class"] <- "Other"

##  prevision based on sex, age, cabin.class and Pclass
train[is.na(train$Age), "Age"] <- median(train$Age, na.rm=TRUE)
titanic.model <- lm(Survived ~ Sex + Age + Pclass + cabin.class, data=train)

test[grep("A",test$Cabin),"cabin.class"] <- "A"
test[grep("B",test$Cabin),"cabin.class"] <- "B"
test[grep("C",test$Cabin),"cabin.class"] <- "C"
test[grep("D",test$Cabin),"cabin.class"] <- "D"
test[grep("E",test$Cabin),"cabin.class"] <- "E"
test[test$Cabin=="", "cabin.class"] <- "Other"
test[is.na(test$cabin.class), "cabin.class"] <- "Other"
test[is.na(test$Age), "Age"] <- median(test$Age, na.rm=TRUE)
test$Survived <- round(predict(titanic.model, test))
submission <- test[,c("PassengerId", "Survived")]

# check of no missing values
paste("Missing value #: ", sum(is.na(submission$Survived)))
# export for submission
write.csv(submission, "submissions/genderageclasslm.csv", row.names=FALSE)
