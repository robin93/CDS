#read the csv files for the tatanic datasets
titanicTrain = read.csv("train.csv",header = TRUE)
titanicTest = read.csv("test.csv",header = TRUE)

titanicTrain

library(rpart)

#Build a decision tree using method "class"
titanicFormula <- Survived~Pclass+Sex+Age+SibSp+Parch+Fare+Embarked
#survived is a funcition of the other variables and it is the formula
titanicFormula
titanicFit <- rpart(titanicFormula,data=titanicTrain,method='class')