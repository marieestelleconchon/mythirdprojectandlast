# Set working directory
setwd("C:/Users/m-conchon/Documents/DS training/Phase 2/Kaggle TITANIC")
library(verification)
library(ROCR)

# Assign the training set
train <- read.csv("train.csv")
# Assign the test set
test <- read.csv("test.csv")

print(train)
print(test)
summary(train)
summary(test)
length(train$Survived)

# First statistics

table(train$Survived)
prop.table(table(train$Survived))

table(train$Sex, train$Survived)
prop.table(table(train$Sex, train$Survived),1)

# Information about the age

train$Child <- NA
train$Child[train$Age < 18] <- 1
train$Child[train$Age >= 18] <- 0

test$Child <- NA
test$Child[test$Age < 18] <- 1
test$Child[test$Age >= 18] <- 0

prop.table(table(train$Child, train$Survived),1)

# Prediction based on gender 
test_one <- test
test_one$Survived <- 0
test_one$Survived[test$Sex == 'female'] <- 1
my_solution <- data.frame(PassengerId = test_one$PassengerId, Survived = test_one$Survived)
write.table(my_solution, file = "pred_test1.csv", sep=",", row.names = FALSE)

cor(train)

# Logistic regression

reglog1 <- glm(Survived ~ Sex * Pclass, data = train, family = "binomial", na.action=na.omit)
print(summary(reglog1))
?glm
anova(reglog1,test="Chisq")

pred_reglog1 <- predict.glm(reglog1, newdata = train, type="response")

roc.area(train$Survived, pred_reglog1)
?roc.area

roc.plot(train$Survived, pred_reglog1, binormal = TRUE)

test$Survived <- predict.glm(reglog1, newdata = test, type="response", na.action=na.omit)
test$Survived <- round(test$Survived, digits=0)
my_solution <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)

write.table(my_solution, file = "pred_reglogtest1.csv", sep=",", row.names = FALSE)
?write.table
