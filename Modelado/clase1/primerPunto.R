####Ejercicio Batting

library(Lahman)
library(dplyr)
library(MASS)
library(ggplot2)

data <- Batting

help(Batting)

#a


batting_totales <- Batting %>%
  group_by(playerID) %>%
  summarise(
    total_hits = sum(H, na.rm = TRUE),
    total_AB = sum(AB, na.rm = TRUE)
  )

#b
#Eligiria a los que tienen promedio 1 porque quiere decir que todas las que intentaron le pegaron.



#c
#jugadores con mas de 1000

batters_1000 <- batting_totales |>
  filter(total_AB > 1000) |>
  mutate(AVG = total_hits / total_AB)

# Ajuste Beta
fit_beta <- fitdistr(
  batters_1000$AVG,
  densfun = "beta",
  start = list(shape1 = 10, shape2 = 30)
)

fit_beta

#d
#tenemos bernoullis y betas, entonces tenemos conjugadas, donde tita|x ~ Beta(sum(xi) + a, n - sum(xi) + b)

#el estimador de bayes es la media posterior: (sum(xi) + a)/n+a+b

alpha <- 65.114427
beta  <- 183.890403

bayes_est <- batting_totales |>
  mutate(
    p_mle = total_hits / total_AB,
    p_bayes = (total_hits + alpha) / (total_AB + alpha + beta)
  ) |>
  arrange(desc(p_bayes))

head(bayes_est)

#e

ggplot(bayes_est,
       aes(x = p_mle, y = p_bayes)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  geom_abline(slope = 1, intercept = 0,
              color = "red", linetype = "dashed") +
  labs(
    title = "Frecuentista vs Empirical Bayes",
    x = "Estimación frecuentista (H/AB)",
    y = "Estimación Empirical Bayes"
  ) +
  theme_minimal()


#g



ranking_prob <- batting_totales |>
  filter(total_AB > 0) |>
  mutate(
    alpha_post = total_hits + alpha,
    beta_post  = total_AB - total_hits + beta,
    
    p_gt_0.3 = 1 - pbeta(0.3, alpha_post, beta_post)
  ) |>
  mutate(
    p_mle = total_hits / total_AB,
    p_bayes = (total_hits + alpha) / (total_AB + alpha + beta)
  ) |>
  arrange(desc(p_gt_0.3))

head(ranking_prob, 20)


ggplot(ranking_prob,
       aes(x = p_gt_0.3, y = p_bayes)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  geom_abline(slope = 1, intercept = 0,
              color = "red", linetype = "dashed") +
  labs(
    title = "Frecuentista vs Empirical Bayes",
    x = "Estimación P(p_i > 0.3 | datos)",
    y = "Estimación Empirical Bayes"
  ) +
  theme_minimal()



