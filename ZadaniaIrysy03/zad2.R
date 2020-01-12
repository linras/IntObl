tmp <- iris;
for(i in 1:4) {
  tmp[,i] <- log(tmp[,i])
};
iris.log <- tmp;
iris.log;

for(i in 1:4) {
  tmp[,i] <- scale(tmp[,i])
};
iris.log.scale <- tmp;
iris.log.scale;

iris.log.scale <- iris.log.scale[,-5];
iris.log.scale;
iris.pca <- prcomp(iris.log.scale);
iris.pca[1];
iris.pca[2];
iris.pca[2]$rotation;
iris.predict <- predict(iris.pca);
iris.predict;

iris.predict <- iris.predict[,c(-3, -4)];
iris.predict <- data.frame(iris.predict);
iris.predict$NAMES <- iris[,5];
iris.predict;
x <- iris.predict[,1];
y <- iris.predict[,2];
n <- iris.predict[,3];
plot(x, y, col=c("red", "green", "blue")[n], pch=19, xlab="PC1", ylab="PC2");
legend(x="topleft", legend=unique(n), pch=19, col=c("red", "green", "blue"), lty=1);