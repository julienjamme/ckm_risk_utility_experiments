library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)
library(latex2exp)

# seuil de confidentialité
k <- 0.8

# Fonction donnant les bornes de l'IC en fonction des paramètres
ic_g_rho <- function(rho, n, sigma_nu, sigma_eps, error = 0.05) {
  q_val <- qnorm(1-error/2)
  upper <- q_val * sqrt(rho^(2*n) * sigma_nu^2 + sigma_eps^2)
  lower <- -upper
  return(tibble(icgl = lower, icgu = upper))
}

n_optim <- function(rho, gamma, k, error){
  q_val <- qnorm(1-error/2)
  return((log(gamma) - log(q))/log(k))
}

# rhos à tester
rho_grid <- seq(0, 1, length.out = 101)


# Jeu de paramètres à tester
params <- expand_grid(
  rho = rho_grid,
  n = c(0, 1, 2, 3, 4, 5, 6),
  sigma_nu = c(0.05, 0.1, 0.25, 0.5, 1),
  sigma_eps = c(0, 0.01, 0.05)
)

ic_g_res <- bind_cols(params, params |> pmap(ic_g_rho) |> list_rbind())

# Visualisation

label_eps <- function(eps) {
  paste0("σε  = ", eps)
}
label_nu <- function(nu) {
  paste0("σν = ", nu)
}
label_n <- function(n) {
  paste0("n = ", n)
}

ic_g_res |>
  filter(sigma_eps > 0, sigma_nu > 0, n > 0) |>
  ggplot(aes(x = rho, ymin = icgl, ymax = icgu, fill = as.factor(sigma_nu))) +
  geom_ribbon(alpha = 0.2, aes(group = as.factor(sigma_nu))) +
  facet_grid(n~sigma_eps, labeller = labeller(n = label_n, sigma_eps = label_eps)) +
  geom_line(aes(y = icgu, group = as.factor(sigma_nu), color = as.factor(sigma_nu)), alpha = 0.7) +
  geom_line(aes(y = icgl, group = as.factor(sigma_nu), color = as.factor(sigma_nu)), alpha = 0.7) +
  labs(
    title = TeX("Intervalle de Confiance à 95% de $g(\\rho)$ en fonction de $\\rho$"),
    x = expression(rho),
    y = "Intervalle de confiance",
    color = TeX("$\\sigma_\\nu$"),
    fill = TeX("$\\sigma_\\nu$")
  ) +
  scale_x_continuous("") +
  geom_vline(aes(xintercept=k), linetype = "dashed", color = "grey35") +
  theme_minimal()


ic_g_res |>
  filter(sigma_eps > 0, sigma_nu > 0, n > 0) |>
  ggplot(aes(x = rho, ymin = icgl, ymax = icgu, fill = as.factor(n))) +
  geom_ribbon(alpha = 0.2, aes(group = as.factor(n))) +
  facet_grid(sigma_eps~sigma_nu, labeller = labeller(sigma_nu = label_nu, sigma_eps = label_eps)) +
  geom_line(aes(y = icgu, group = as.factor(n), color = as.factor(n)), alpha = 0.7) +
  geom_line(aes(y = icgl, group = as.factor(n), color = as.factor(n)), alpha = 0.7) +
  labs(
    title = TeX("Intervalle de Confiance à 95% de $g(\\rho)$ en fonction de $\\rho$"),
    x = expression(rho),
    y = "Perte d'information (en %)",
    color = TeX("$n$"),
    fill = TeX("$n$")
  ) +
  scale_x_continuous(breaks = seq(0,1,0.2)) + 
  scale_y_continuous(breaks = seq(-2,2,0.5), labels = seq(-2,2,0.5)*100) +
  geom_vline(aes(xintercept=k), linetype = "dashed", color = "grey35") +
  theme_minimal()
