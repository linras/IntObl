#install.packages("genalg")

library(genalg)
plecakdb <- data.frame(przedmiot=c("kanapka", "telewizor", "statua wolnosci", "tablet", "gqozdzie", "lampa", "srebrny wazon", "porcelana", "figura z brazu", "torebka", "odkurzacz"), 
                          wartosc = c(100, 300, 200, 40, 500, 70, 100, 250, 300, 280, 300), waga = c(7, 7, 6, 2, 5, 6, 1, 3, 10, 3, 15))
plecaklimit <- 25
chromosome = c(0, 0, 0, 1, 1, 0, 0, 1, 0, 0, 1)
plecakdb[chromosome == 1, ]
cat(chromosome %*% plecakdb$wartosc)
fitnessFunc <- function(chr) {
     calkowita_wartosc_chr <- chr %*% plecakdb$wartosc
     calkowita_waga_chr <- chr %*% plecakdb$waga
     if (calkowita_waga_chr > plecaklimit)
         return (0) else return (-calkowita_wartosc_chr)
}
plecakGenAlg <- rbga.bin(size = 11, popSize = 200, iters = 100, mutationChance = 0.05, elitism = T, evalFunc = fitnessFunc)
summary(plecakGenAlg, echo=TRUE)


#zadanie 2



plot(-plecakGenAlg$best, ylim=c(0, 1600), type="l", xlab="pokolenie", ylab="fitness (ocena)", main="Gen alg", col="red")
lines(-plecakGenAlg$mean, col="blue")
legend("bottomright", legend=c("srednia", "maksymalne"), col=c("blue", "red"), lty=1)