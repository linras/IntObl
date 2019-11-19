# ZADANIE 2

library(party)
str(iris)

# a) zb treningowy 70%, zb testowy 30%

set.seed(1234) #To get reproducible result
ind <- sample(2,nrow(iris), replace=TRUE, prob=c(0.7,0.3))
trainData <- iris[ind==1,]
testData <- iris[ind==2,]

# b) Wytrenuj drzewo decyzyjne na zbiorze treningowym.

myFormula <- Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width
iris_ctree <- ctree(myFormula, data=trainData)

# c) Wyświetl drzewo w formie tekstowej i w formie graficznej.

print(iris_ctree)
plot(iris_ctree)

# d)  Dokonaj ewaluacji klasyfikatora: sprawdź jak drzewo poradzi sobie z rekordami ze zbioru testowego. 

#train_predict <- predict(iris_ctree)
train_predict <- predict(iris_ctree,testData,type="response")

table(train_predict,testData$Species)

# Wyświetl procent poprawnych odpowiedzi.
100 - mean(train_predict != testData$Species) * 100


# e) Wyświetl macierz błędu (confusion matrix) dla tej ewaluacji. Wyjaśnij jakie błędy
# popełniał klasyfikator wskazując na liczby w macierzy błędu.

table(train_predict,testData$Species)