#####Ejercicio Clase 2


#Leemos los datos
datos <-read.csv("datos.csv")


#Transformamos los datos del tratamiento a binario y lo mismo con noche
datos$cafe <- ifelse(datos$cafe == "Toma café", 1, 0)
datos$noche <- ifelse(datos$noche == "Noche", 1, 0)


#Calculamos promedio 
E_1_dia <- mean(datos$prod[datos$cafe == 1 & datos$noche == 0])
E_0_dia <- mean(datos$prod[datos$cafe == 0 & datos$noche == 0])

E_1_noche <- mean(datos$prod[datos$cafe == 1 & datos$noche == 1])
E_0_noche <- mean(datos$prod[datos$cafe == 0 & datos$noche == 1])

#Calculamos las esperanzas pesando lo anterior por las proporciones de la variable noche.
EY_X1_T <- E_1_dia * mean(datos$noche == 0) + E_1_noche * mean(datos$noche == 1)

EY_X0_T <- E_0_dia * mean(datos$noche == 0) + E_0_noche * mean(datos$noche == 1)


#Ahora que ya tenemos los dos términos calculamos la diferencia
ATE_est <- EY_X1_T - EY_X0_T

  
#Ahora vamos a hacer muestras bootstrap sobre nuestra muestra original
  
  
  

#primero vamos a definir una función para caluclar el ATE

calcular_ATE <- function(datos) {
  
  p_dia <- mean(datos$noche == 0)
  p_noche <- mean(datos$noche == 1)
  
  EY_X1_T <- mean(datos$prod[datos$cafe == 1 & datos$noche == 0]) * p_dia +
    mean(datos$prod[datos$cafe == 1 & datos$noche == 1]) * p_noche
  
  EY_X0_T <- mean(datos$prod[datos$cafe == 0 & datos$noche == 0]) * p_dia +
    mean(datos$prod[datos$cafe == 0 & datos$noche == 1]) * p_noche
  
  return(EY_X1_T - EY_X0_T)
}

B <- 500

Ates <- numeric(B)


for(i in 1:500){
  
  n <- nrow(datos)
  
  indices <- sample(1:n, size = n, replace = TRUE)
  
  muestra_bootstrap <- datos[indices, ]
  
  Ates[i] <- calcular_ATE(muestra_bootstrap)
  
}


Int_Ate <- quantile(Ates, c(0.025, 0.975))



