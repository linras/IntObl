iris.log <- log(iris[, 1:4])
iris.stand <- scale(iris.log, center = TRUE)
iris.pca <- prcomp(iris.stand)
iris.final <- predict(iris.pca)[, 1:2]
cl <- kmeans(iris.final, 3)
plot(iris.final, col = cl$cluster)
points(cl$centers, col = 1:5, pch = 8)
plot(iris.final, col = iris$Species)
