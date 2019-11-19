# ZADANIE 1
library(tidyverse)
data(iris)
#head(iris, 3)

versicolors <- (iris %>%
          filter(Species=='versicolor'))

setosas <- (iris %>%
              filter(Species=='setosa'))

virginicas <- (iris %>%
              filter(Species=='virginica'))

myPredictRow <- function(sl,sw,pl,pw) {
  pwAv <- mean(iris$Sepal.Length)/mean(iris$Petal.Width)
  vir <- (sl - mean(virginicas$Sepal.Length))+(sw - mean(virginicas$Sepal.Width))+3*(pl-mean(virginicas$Petal.Length))+pwAv*(pw -mean(virginicas$Petal.Width));
  set <- (sl - mean(setosas$Sepal.Length))+(sw - mean(setosas$Sepal.Width))+3*(pl-mean(setosas$Petal.Length))+pwAv*(pw -mean(setosas$Petal.Width));
  ver <- (sl - mean(versicolors$Sepal.Length))+(sw - mean(versicolors$Sepal.Width))+3*(pl-mean(versicolors$Petal.Length))+pwAv*(pw -mean(versicolors$Petal.Width));
  
  if (abs(set) < abs(vir) & abs(set) < abs(ver)) {
    return('setosa')
  } else {
    if (abs(vir) < abs(set) & abs(vir)< abs(ver)) {
      return('virginica')
    } else {
      return('versicolor')
    }
  }
}

#myPredictRow(5.1,3.5,1.4,0.2)

rows = function(x) lapply(seq_len(nrow(x)), function(i) lapply(x,"[",i))

myPredict <- function() {
  i <- 0;
  for (flower in rows(iris)) {
    #glimpse(myPredictRow(flower$Sepal.Length, flower$Sepal.Width,flower$Petal.Length,flower$Petal.Width))
    if(myPredictRow(flower$Sepal.Length, flower$Sepal.Width,flower$Petal.Length,flower$Petal.Width)==flower$Species)
      i <- i+1;
  }
  i
}
# Wyświetl procent poprawnych odpowiedzi:
myPredict()/1.5


mean(versicolors$Petal.Width)
mean(setosas$Petal.Width)
mean(virginicas$Petal.Width)

mean(versicolors$Petal.Length)
mean(setosas$Petal.Length)
mean(virginicas$Petal.Length)

mean(versicolors$Sepal.Width)
mean(setosas$Sepal.Width)
mean(virginicas$Sepal.Width)

mean(versicolors$Sepal.Length)
mean(setosas$Sepal.Length)
mean(virginicas$Sepal.Length)
