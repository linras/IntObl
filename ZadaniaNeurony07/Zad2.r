#install.packages("neuralnet")
library(neuralnet)

# HELPERS

set.seed(1000)

normalize <- function(x) {
  (x - min(x)) / ((max(x)) - min(x))
}

my.max.col <- function(x, y, z) {
  if (x == max(x, y, z)) {
    return ('virginica')
  }
  if (y == max(x, y, z)) {
    return ('versicolor')
  }
  else
    return('setosa')
}

# MAIN

iris.norm <- normalize(iris[, 1:4])

virginica.offset <- c(rep("virginica", nrow(iris.norm)))
versicolor.offset <- c(rep("versicolor", nrow(iris.norm)))
setosa.offset <- c(rep("setosa", nrow(iris.norm)))

virginica.offset <- matrix(match(iris[, 5], virginica.offset, nomatch = FALSE, incomparables = NULL))
versicolor.offset <- matrix(match(iris[, 5], versicolor.offset, nomatch = FALSE, incomparables = NULL))
setosa.offset <- matrix(match(iris[, 5], setosa.offset, nomatch = FALSE, incomparables = NULL))

colnames(virginica.offset)[1] <- "Virginica"
colnames(versicolor.offset)[1] <- "Versicolor"
colnames(setosa.offset)[1] <- "Setosa"

iris.types <-
  cbind(virginica.offset, versicolor.offset, setosa.offset)
iris.norm <- cbind(iris.norm, iris.types)

ind <- sample(2,
              nrow(iris.norm),
              replace = TRUE,
              prob = c(0.67, 0.33))
iris.norm.training <- iris.norm[ind == 1, 1:7]
iris.norm.test <- iris.norm[ind == 2, 1:7]

iris.network <- neuralnet(
  Virginica + Versicolor + Setosa
  ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
  iris.norm.training,
  hidden = 4,
  stepmax = 1000000
)
iris.result <-
  compute(iris.network, iris.norm.test[1:4], rep = 1)$net.result

first.results <- c()
second.results <- c()

for (i in 1:nrow(iris.result)) {
  first.results <-
    c(first.results,
      my.max.col(iris.result[i, 1], iris.result[i, 2], iris.result[i, 3]))
  second.results <-
    c(second.results,
      my.max.col(iris.norm.test[i, 5], iris.norm.test[i, 6], iris.norm.test[i, 7]))
}

confMatrix <- table(first.results, second.results)
accuracy <- sum(diag(confMatrix)) / sum(confMatrix)
