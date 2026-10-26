# Leemos los datos del ejercicio
df <- read.csv("datos_4323.csv")

#Estimador ingenuo
mu_ingenuo <- mean(df$Y[df$A == 1])

#OR con regresión lineal ( Y ~ X)
modelo_y <- lm(Y ~ X, data = subset(df, A == 1))
mu_or  <- mean(predict(modelo_y, newdata = df))

#OR con regresión lineal (Y ~ X + X^2)
modelo_y2 <- lm(Y ~ X + I(X^2), data = subset(df, A == 1))
mu_or2  <- mean(predict(modelo_y2, newdata = df))


#HT con propensity logístico
modelo_a <- glm(A ~ X, data = df, family = binomial(link = "logit"))
pi_x <- predict(modelo_a, type = "response")


mu_ht_log <- mean(ifelse(df$A == 1, df$Y / pi_x, 0))

# HT con propensity promedio

prom <- mean(df$A)
mu_ht_mean <- mean(ifelse(df$A == 1, df$Y / prom,0))


#DR con regresión lineal y propensity logistico



