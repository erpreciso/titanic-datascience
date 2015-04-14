library(ggplot2)
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
rm(list=ls())
setwd("/home/erpreciso/Documents/school/titanic")
train <- read.csv("data/train.csv")
test <- read.csv("data/test.csv")

##  decision tree
fit <- rpart(Survived ~ Pclass + Sex + Age  + Fare + Embarked,
             data=train, method="class")
fancyRpartPlot(fit) # show tree

test$Survived <- predict(fit, test, type="class")

# format for submission, check for missing values and export
submission <- test[,c("PassengerId", "Survived")]
paste("Missing value #: ", sum(is.na(submission$Survived)))
write.csv(submission, "submissions/decisiontreelm.csv", row.names=FALSE)
