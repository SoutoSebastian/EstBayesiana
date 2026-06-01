## Practica GLM

#### EJ 1 ####  

library(ggplot2)
library(dplyr)

#a

data("diamonds")

ggplot(diamonds, aes(x = carat, y = price)) +
  geom_point(alpha = 0.2) +
  labs(
    x = "Quilates (carat)",
    y = "Precio",
    title = "Precio vs. peso en quilates"
  ) +
  theme_minimal()

#b

ggplot(diamonds, aes(x = carat, y = price)) +
  geom_point(alpha = 0.2) +
  scale_x_log10() + 
  scale_y_log10() +  
  labs(
    x = "Quilates (carat)",
    y = "Precio",
    title = "Precio vs. peso en quilates"
  ) +
  theme_minimal()

#Se adquiere una forma lineal. Tiene sentido porque linealiza la relaci?n ?

#c
diamonds <- diamonds %>%
  mutate(
    across(
      c(cut, color, clarity),
      ~ factor(.x, ordered = FALSE)
    )
  )


#d

modelo <- lm(
  log(price) ~ log(carat) + cut + color + clarity,
  data = diamonds
)

summary(modelo)

#e

#Intercept describe un diamente para el cual log(carat) = 0, osea carat = 1 y que las otras
#categorias sean las base: cut = Fair, color = D, clarity = I1.

# EL log(price) aumenta B1*log(2) -> el precio aumenta 2^B1

# cutIdeal: e^0.16. Osea e^(estimado)

#se verifica

