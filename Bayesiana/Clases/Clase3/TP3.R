library(MASS)
library(BayesFactor)

#######EJ 3 ##########

data <- iris

summary(anovaBF(Sepal.Length ~ Species, data = data ))


#########EJ 4##############

tabla <- matrix(c(
  15, 8, 7,
  10, 12, 8,
  12, 14, 9,
  8, 10, 7
), nrow = 4, byrow = TRUE)

colnames(tabla) <- c("Privado", "Publico", "Prepaga")
rownames(tabla) <- c("Hipocalorica", "Hipercalorica", "Vegetariana", "Proteica")

tabla

#Enfoque frecuentista

chisq.test(tabla)


#Enfoque bayesiano

contingencyTableBF(tabla, sampleType = "indepMulti", fixedMargin = "rows")


#####EJ 5 ######

tabla <- matrix(c(
  3, 1,
  1, 3
  
), nrow = 2, byrow = TRUE)

colnames(tabla) <- c("Muriel dice Te", "Muriel dice Leche")
rownames(tabla) <- c("te", "Leche")

tabla

#Frecuentista:

fisher.test(tabla)

#Bayesiano:
contingencyTableBF(tabla, sampleType = "hypergeom")



