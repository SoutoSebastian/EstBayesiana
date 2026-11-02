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
mu_dr1 <- mean(m_x + (df$A / pi_x) * (y_obs - m_x))


#DR con regresión cuadrática y propensity constante
m_x2 <- predict(modelo_y2, newdata = df)
y_obs <- ifelse(is.na(df$Y), 0, df$Y)
mu_dr2 <- mean(m_x2 + (df$A / prom) * (y_obs - m_x2))


#DR con regresión lineal y propensity constante

mu_dr3 <- mean(m_x + (df$A / prom) * (y_obs - m_x))

# ==============================================================================
# Estimador de Nadaraya-Watson exactamente como en el script JS del Cuestionario
# Kernel Gaussiano: K(u) = exp(-0.5 * u^2)
# Ventana fija: h = 0.3
# ==============================================================================

H_FIXED <- 0.3

nw <- function(x0, Xs, Zs, h = H_FIXED) {
  # x0: punto o vector de puntos donde evaluar
  # Xs: vector de características X de entrenamiento
  # Zs: vector de respuestas (Y o A) de entrenamiento
  # h:  ancho de banda (fijo en 0.3)
  
  sapply(x0, function(x) {
    u <- (x - Xs) / h
    k <- exp(-0.5 * u * u)
    num <- sum(k * Zs)
    den <- sum(k)
    if (den == 0) return(NA)
    return(num / den)
  })
}

# ------------------------------------------------------------------------------
# Preprocesamiento de datos
# ------------------------------------------------------------------------------
# Filtrar la muestra donde Y está observada (A == 1)
df_obs <- subset(df, A == 1)

# Vector de Y rellenando NA con 0 para las fórmulas de HT y DR
Y0 <- ifelse(is.na(df$Y), 0, df$Y)

# ------------------------------------------------------------------------------
# 1. Nadaraya–Watson para E(Y | X = 0)
# Evaluado en x = 0 sobre los casos observados (df_obs)
# ------------------------------------------------------------------------------
nw_m0 <- nw(x0 = 0, Xs = df_obs$X, Zs = df_obs$Y, h = H_FIXED)

# ------------------------------------------------------------------------------
# 2. Nadaraya–Watson para P(A = 1 | X = -1.5)
# Evaluado en x = -1.5 sobre toda la muestra usando A
# ------------------------------------------------------------------------------
nw_p15 <- nw(x0 = -1.5, Xs = df$X, Zs = df$A, h = H_FIXED)

# ------------------------------------------------------------------------------
# 3. Predicciones no paramétricas para todas las 500 filas
# ------------------------------------------------------------------------------
# m_np(X_i): predicción de Y dado X_i
mNp <- nw(x0 = df$X, Xs = df_obs$X, Zs = df_obs$Y, h = H_FIXED)

# p_np(X_i): predicción de A dado X_i (Propensity Score)
pNp <- nw(x0 = df$X, Xs = df$X, Zs = df$A, h = H_FIXED)

# ------------------------------------------------------------------------------
# 4. Estimadores globales de E(Y)
# ------------------------------------------------------------------------------

# A. Outcome Regression (OR) no paramétrico
or_np <- mean(mNp)

# B. Horvitz-Thompson (HT / IPW) no paramétrico
ht_np <- mean(df$A * Y0 / pNp)

# C. Doblemente Robusto (DR / AIPW) no paramétrico
dr_np <- mean(mNp + df$A * (Y0 - mNp) / pNp)

# ==============================================================================
# Imprimir Resultados Redondeados a 3 Decimales
# ==============================================================================
cat("=== RESULTADOS NADARAYA-WATSON (h = 0.3) ===\n")
cat("1. E(Y | X = 0) NW:         ", round(nw_m0, 3), "\n")
cat("2. P(A = 1 | X = -1.5) NW:  ", round(nw_p15, 3), "\n")
cat("3. OR no paramétrico:       ", round(or_np, 3), "\n")
cat("4. HT no paramétrico:       ", round(ht_np, 3), "\n")
cat("5. DR no paramétrico:       ", round(dr_np, 3), "\n")



# ==============================================================================
# Simulación
# ==============================================================================


# ==============================================================================
# Simulación de Monte Carlo (R = 500, n = 500, h = 0.3) para calcular el ECM
# ==============================================================================

set.seed(4323) # Para reproducibilidad
R <- 500      # Número de réplicas
N <- 500      # Tamaño muestral por réplica
H <- 0.3      # Bandwidth fijo para Nadaraya-Watson

# Kernel Gaussiano y estimador Nadaraya-Watson
nw <- function(x0, Xs, Zs, h = H) {
  sapply(x0, function(x) {
    u <- (x - Xs) / h
    k <- exp(-0.5 * u * u)
    den <- sum(k)
    if (den == 0) return(NA)
    sum(k * Zs) / den
  })
}

# Matriz para almacenar las estimaciones de cada réplica r = 1, ..., R
estimaciones <- matrix(NA, nrow = R, ncol = 11)
colnames(estimaciones) <- c(
  "naive", "or_lin", "or_quad", "or_np", 
  "ht_log", "ht_const", "ht_np", 
  "dr_lin_log", "dr_quad_const", "dr_lin_const", "dr_np"
)

cat("Iniciando simulación de R =", R, "réplicas...\n")

for (r in 1:R) {
  # 1. Generar muestra del modelo poblacional
  X <- rnorm(N, mean = 0, sd = 1)
  prob_A <- 1 / (1 + exp(-(0.3 + X)))
  A <- rbinom(N, size = 1, prob = prob_A)
  eps <- rnorm(N, mean = 0, sd = 1)
  Y_obs <- 1 + X + X^2 + eps
  
  # Y es observado solo cuando A == 1
  Y0 <- ifelse(A == 1, Y_obs, 0) # Relleno con 0 para términos HT/DR
  
  # Subconjunto observado
  idx_obs <-  A == 1
  Xo <- X[idx_obs]
  Yo <- Y_obs[idx_obs]
  
  # --- A. Regresiones Paramétricas (MCO) ---
  mod_lin  <- lm(Yo ~ Xo)
  mod_quad <- lm(Yo ~ Xo + I(Xo^2))
  mLin  <- predict(mod_lin, newdata = data.frame(Xo = X))
  mQuad <- predict(mod_quad, newdata = data.frame(Xo = X))
  
  # --- B. Propensity Scores Paramétricos ---
  mod_log <- glm(A ~ X, family = binomial(link = "logit"))
  pLog   <- predict(mod_log, type = "response")
  pConst <- rep(mean(A), N)
  
  # --- C. Nadaraya-Watson (No Paramétricos con h = 0.3) ---
  mNp <- nw(x0 = X, Xs = Xo, Zs = Yo, h = H)
  pNp <- nw(x0 = X, Xs = X,  Zs = A,  h = H)
  
  # --- D. Cálculo de las 11 Estimaciones de E(Y) ---
  
  # 1. Naive
  e_naive <- mean(Yo)
  
  # 2. Outcome Regressions (OR)
  e_or_lin  <- mean(mLin)
  e_or_quad <- mean(mQuad)
  e_or_np   <- mean(mNp)
  
  # 3. Horvitz-Thompson (HT)
  e_ht_log   <- mean(A * Y0 / pLog)
  e_ht_const <- mean(A * Y0 / pConst)
  e_ht_np    <- mean(A * Y0 / pNp)
  
  # 4. Doblemente Robustos (DR)
  e_dr_lin_log   <- mean(mLin + A * (Y0 - mLin) / pLog)
  e_dr_quad_const <- mean(mQuad + A * (Y0 - mQuad) / pConst)
  e_dr_lin_const <- mean(mLin + A * (Y0 - mLin) / pConst)
  e_dr_np        <- mean(mNp + A * (Y0 - mNp) / pNp)
  
  # Guardar fila en la matriz
  estimaciones[r, ] <- c(
    e_naive, e_or_lin, e_or_quad, e_or_np,
    e_ht_log, e_ht_const, e_ht_np,
    e_dr_lin_log, e_dr_quad_const, e_dr_lin_const, e_dr_np
  )
}

# ==============================================================================
# Cálculo del Error Cuadrático Medio (ECM) respecto a mu = 2
# ECM = (1/R) * sum( (mu_hat - 2)^2 )
# ==============================================================================

ecm_vector <- colMeans((estimaciones - 2)^2)

# Crear DataFrame con los resultados formateados a 4 decimales
tabla_ecm <- data.frame(
  Estimador = names(ecm_vector),
  ECM = sprintf("%.4f", ecm_vector)
)

print(tabla_ecm)