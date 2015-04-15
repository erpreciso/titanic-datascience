library(ggplot2)
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
rm(list=ls())
setwd("/home/erpreciso/Documents/school/titanic")
train <- read.csv("data/train.csv")
test <- read.csv("data/test.csv")

# aggregate to simplify engineering
test$Survived <- NA
combi <- rbind(train, test)

# create title variable
combi$title <- sapply(combi$Name, FUN=function(x){
    strsplit(as.character(x), "[,.]")[[1]][2]})
combi$title <- sub(" ", "", combi$title)
table(combi$title)

# clean it
combi$title[combi$title %in% c("Ms","Mrs", "Miss", "Mlle","Mme")] <- "Miss"
combi$title[combi$title %in% c("Capt","Don", "Major", "Col", "Sir", "Dr")] <- "Sir"
combi$title[combi$title %in% c("Lady","the Countess", "Jonkheer")] <- "Lady"
combi$title <- as.factor(combi$title)

# add family size
combi$family.size <- combi$SibSp + combi$Parch

# combine by family
combi$surname <- sapply(combi$Name, FUN=function(x){
    strsplit(as.character(x), "[,.]")[[1]][1]})
combi$surname <- sub(" ", "", combi$surname)
combi$family.ID <- paste(as.character(combi$family.size), combi$surname)

# remove small families
combi$family.ID[combi$family.size<=2] <- "Small"
family.frequencies <- data.frame(table(combi$family.ID))
family.frequencies <- family.frequencies[family.frequencies$Freq<=2,]
combi$family.ID[combi$family.ID %in% family.frequencies$Var1] <- "Small"
combi$family.ID <- as.factor(combi$family.ID)

# rebreak train and test
train <- combi[1:891,]
test <- combi[892:1309,]

##  decision tree
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare +
                 Embarked + title + family.size + family.ID,
             data=train, method="class")
fancyRpartPlot(fit) # show tree

test$Survived <- predict(fit, test, type="class")

# format for submission, check for missing values and export
submission <- test[,c("PassengerId", "Survived")]
paste("Missing value #: ", sum(is.na(submission$Survived)))
write.csv(submission, "submissions/engineeredvars.csv", row.names=FALSE)
