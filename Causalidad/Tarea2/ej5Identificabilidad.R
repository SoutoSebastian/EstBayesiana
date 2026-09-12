
# Ejercicio 5: Función 'cotas' para calcular los límites del ATE sin identificación


cotas <- function(p) {
  # Validación básica de entrada
  if (length(p) != 4) {
    stop("El vector p debe tener exactamente 4 elementos: c(q1, q2, q3, q4).")
  }
  if (abs(sum(p) - 1) > 1e-6) {
    warning("La suma de los elementos del vector p no da 1. Se procede con el cálculo.")
  }
  
  # Asignación de componentes de la distribución observada (T, Y)
  # q1 = P(T=t, Y=0)
  # q2 = P(T=t, Y=1)
  # q3 = P(T=c, Y=0)
  # q4 = P(T=c, Y=1)
  q1 <- p[1]
  q2 <- p[2]
  q3 <- p[3]
  q4 <- p[4]
  
  # Cálculo de las cotas inferior y superior
  minimo_ATE <- - q1 - q4
  maximo_ATE <-  q2 + q3
  
  # Retornar los resultados etiquetados
  resultado <- c(Minimo = minimo_ATE, Maximo = maximo_ATE)
  return(resultado)
}


# Prueba de verificación con los datos del Ejercicio 3


# Vector q del Ejercicio 3: c(12/20, 3/20, 3/20, 2/20)
q_ejercicio3 <- c(12/20, 3/20, 3/20, 2/20)

# Ejecución de la función
resultado_ej3 <- cotas(q_ejercicio3)
print(resultado_ej3)
