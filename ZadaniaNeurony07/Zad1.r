fct.act <- function(x) {
  return (1 / (1 + exp(-x)))
}

forwardPass <- function(wiek, waga, wzrost) {
  hidden1 <-
    fct.act((-0.46122 * wiek) + (0.97314 * waga) + (-0.39203 * wzrost) + 0.80109)
  hidden2 <-
    fct.act((0.78548 * wiek) + (2.10584 * waga) + (-0.57847 * wzrost) + 0.43529)
  output <- ((-0.81546 * hidden1) + (1.03775 * hidden2) - 0.2368)
  return (output)
}

forwardPass(23, 75, 176)
forwardPass(28, 120, 175)
forwardPass(48, 97, 178)
forwardPass(46, 70, 187)

