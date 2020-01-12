library(tidyverse)
library(naivebayes)
library(e1071)
library(class)
# set working directory
path_loc <- "C:/repo/IntObl/Zadania05"
setwd(path_loc)
# reading in the data
df <- read_csv("diabetes.csv") ; df
str(df)

df$`pregnant-times` <- as.numeric(df$`pregnant-times`)
df$`glucose-concentr` <- as.numeric(df$`glucose-concentr`)
df$`blood-pressure` <- as.numeric(df$`blood-pressure`)
df$`skin-thickness` <- as.numeric(df$`skin-thickness`)
df$insulin <- as.numeric(df$insulin)
df$`mass-index` <- as.numeric(df$`mass-index`)
df$`pedigree-func` <- as.numeric(df$`pedigree-func`)
df$age <- as.numeric(df$age)

# taking a quick look
glimpse(df)

# a) zbiór treningowy (67%) i testowy (33%).

set.seed(1234) #To get reproducible result
ind <- sample(2,nrow(df), replace=TRUE, prob=c(0.67,0.33))
trainData <- df[ind==1,]
testData <- df[ind==2,]

# b)Uruchom każdy z klasyfikatorów wykorzystując paczki i dokonaj ewaluacji na
# zbiorze testowym wyświetlając procentową dokładność i macierz błędu

#Fitting the Naive Bayes model
Naive_Bayes_Model=naiveBayes(class ~., data=testData)
#Print the model summary
Naive_Bayes_Model

#naiveBayes.default(x = X, y = Y, laplace = laplace)
#glimpse(data(HouseVotes84, package = "mlbench"))

predict(Naive_Bayes_Model, testData[1:8,])
predict(Naive_Bayes_Model, testData[1:8,], type = "raw")

pred <- predict(Naive_Bayes_Model, testData)
table(pred, testData$class)


m <- naiveBayes(testData[,-5], testData[,5])
table(predict(m, testData), testData[,5])






model <- naiveBayes(class ~ ., data = testData)
predict(model, testData[1:9,])
predict(model, testData[1:10,], type = "raw")

pred <- predict(model, testData)
table(pred, testData$class)
pred <-predict(model, as.data.frame(testData))



data(HouseVotes84, package = "mlbench")
model <- naiveBayes(Class ~ ., data = HouseVotes84)
predict(model, HouseVotes84[1:10,])
predict(model, HouseVotes84[1:10,], type = "raw")


naiveBayes.formula(formula = class ~ ., data = testData)


m4 <- naiveBayes(as.factor(class)~., data=testData[1:10,], method="class")
p4<-predict(m4, testData[11:15,]) 
p4

table(testData[11:15,], predicted=p4)
table(testData[101:150,5], predicted=p4)
table(testData[11:15,], predicted=p4)


#kNN
#knn(trainData, testData, cl, k = 1, l = 0, prob = FALSE, use.all = TRUE)

#cl <- factor(c(rep("s",25), rep("c",25)))
#knn(trainData, testData, cl, k = 2, prob=TRUE)
#attributes(.Last.value)


##the normalization function is created
nor <-function(x) { (x -min(x))/(max(x)-min(x))   }

##Run nomalization on first 4 coulumns of dataset because they are the predictors
data_norm <- as.data.frame(lapply(testData[,c(2,3,4,5,6,7,8)], nor))

summary(data_norm)

ran <- sample(1:nrow(df), 0.67 * nrow(df)) 

##extract training set
data_train <- data_norm[ran,] 
##extract testing set
data_test <- data_norm[-ran,] 

#data_train$glucose.concentr <- as.double(data_train$glucose.concentr)

##extract 5th column of train dataset because it will be used as 'cl' argument in knn function.
data_target_category <- df[ran,9]
##extract 5th column if test dataset to measure the accuracy
data_test_category <- df[-ran,9]
##load the package class
library(class)
##run knn function
pr <- knn(trainData,testData,cl=data_target_category,k=13)

knn(data_train, data_test, data_train$class)

knn(trainData[, -1], testData[, -1], trainData[, 1])

##create confusion matrix
tab <- table(pr,data_test_category)

##this function divides the correct predictions by total number of predictions that tell us how accurate teh model is.

accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(tab)

# e) Wyświetl macierz błędu (confusion matrix) dla tej ewaluacji. Wyjaśnij jakie błędy
# popełniał klasyfikator wskazując na liczby w macierzy błędu.


