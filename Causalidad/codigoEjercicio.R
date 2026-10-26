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
m_x   <- predict(modelo_y, newdata = df)
y_obs <- ifelse(is.na(df$Y), 0, df$Y)
mu_dr <- mean(m_x + (df$A / pi_x) * (y_obs - m_x))


#DR con regresión cuadrática y propensity constante
m_x2 <- predict(modelo_y2, newdata = df)
y_obs <- ifelse(is.na(df$Y), 0, df$Y)
mu_dr <- mean(m_x2 + (df$A / prom) * (y_obs - m_x2))


#DR con regresión lineal y propensity constante

mu_dr3 <- mean(m_x + (df$A / prom) * (y_obs - m_x))



