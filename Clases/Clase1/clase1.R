# Secuencia de valores de theta
theta <- seq(0, 1, length=1000)

# Función densidad beta
beta_density <- function(a, b) {
  dbeta(theta, a, b)
}

# Graficar prior
plot(theta, beta_density(5,5), type="l", lwd=2, col="black",
     xlab="theta", ylab="Densidad",
     main="Actualización Bayesiana (Beta-Bernoulli)")

# Agregar posteriors con colores
lines(theta, beta_density(6,14), col="red", lwd=2)
lines(theta, beta_density(10,10), col="blue", lwd=2)
lines(theta, beta_density(12,8), col="green", lwd=2)
lines(theta, beta_density(55,55), col="purple", lwd=2)
lines(theta, beta_density(105,905), col="orange", lwd=2)
lines(theta, beta_density(500, 500), col ="yellow")

# Leyenda
legend("topright",
       legend=c("Prior Beta(5,5)",
                "1/10",
                "5/10",
                "7/10",
                "50/100",
                "100/1000"),
       col=c("black","red","blue","green","purple","orange"),
       lwd=2)



########Ejercicio manejo#############

int1 <- qbeta(c(0.025, 0.975), 16, 14)

int2 <- qbeta(c(0.025, 0.975), 509, 511)

int3 <- qbeta(c(0.025, 0.975), 79, 41)

