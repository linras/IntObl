plot(-plecakGenAlg$best, ylim=c(0, 1600), type="l", xlab="pokolenie", ylab="fitness (ocena)", main="Gen alg", col="red")
lines(-plecakGenAlg$mean, col="blue")
legend("bottomright", legend=c("srednia", "maksymalne"), col=c("blue", "red"), lty=1)