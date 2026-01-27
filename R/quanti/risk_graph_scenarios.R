#

library(dplyr)
library(purrr)
library(ggplot2)
library(latex2exp)

source("R/quanti/risk_assessment_scenarios.R")

# seuil de dominance
k <- 0.8

# rhos à tester
rho_grid <- seq(0, 1, length.out = 101)

# Jeu de paramètres à tester
params <- tidyr::expand_grid(
  k = k,
  rho = rho_grid,
  n = c(0, 1, 2, 3, 4, 5, 6),
  sigma_nu = c(0.05, 0.1, 0.25, 0.5, 1),
  sigma_eps = c(0, 0.01, 0.05)
)

scenarioI_risk_res <- bind_cols(params, tibble(muI = params |> pmap(mu_I) |> list_c()))

label_eps <- function(eps) {
  paste0("σε  = ", eps)
}
label_nu <- function(nu) {
  paste0("σν = ", nu)
}
label_n <- function(n) {
  paste0("n = ", n)
}

scenarioI_risk_res |>
  filter(sigma_eps > 0, sigma_nu > 0, n %in% c(1,3,5)) |>
  ggplot(aes(x = rho, y = muI, color = as.factor(sigma_nu))) +
  geom_line(aes(group = sigma_nu), size = 0.8) +
  facet_grid(sigma_eps ~ n, labeller = labeller(n = label_n, sigma_eps = label_eps)) +
  labs(
    title = TeX("Risque $\\mu_I$ en fonction de $\\rho$ pour différents paramètres"),
    x = expression(rho),
    y = TeX("Mesure de risque $\\mu_I$"),
    color = TeX("$\\sigma_\\nu$")
  ) +
  scale_x_continuous(breaks = seq(0,1,0.2)) + 
  scale_y_continuous(breaks = seq(0,1,0.2)) +
  geom_vline(aes(xintercept=k), linetype = "dashed", color = "grey35") +
  theme_minimal() +
  theme(legend.position = "bottom")



# Scénario 3
scenarioIII_risk_res <- bind_cols(params, tibble(muIII = params |> pmap(mu_III) |> list_c()))

scenarioIII_risk_res |>
  filter(sigma_eps > 0, sigma_nu > 0, n %in% c(1,3,5)) |>
  ggplot(aes(x = rho, y = muIII, color = as.factor(sigma_nu))) +
  geom_line(aes(group = sigma_nu), size = 0.8) +
  facet_grid(sigma_eps ~ n, labeller = labeller(n = label_n, sigma_eps = label_eps)) +
  labs(
    title = TeX("Risque $\\mu_{III}$ en fonction de $\\rho$ pour différents paramètres"),
    x = expression(rho),
    y = TeX("Mesure de risque $\\mu_{III}$"),
    color = TeX("$\\sigma_\\nu$")
  ) +
  scale_x_continuous(breaks = seq(0,1,0.2)) + 
  scale_y_continuous(breaks = seq(0,1,0.2)) +
  geom_vline(aes(xintercept=k), linetype = "dashed", color = "grey35") +
  theme_minimal() +
  theme(legend.position = "bottom")


# Scénario 2
# Jeu de paramètres à tester
params2 <- expand_grid(
  k = k,
  rho = rho_grid,
  rho2 = 1,
  n = c(0, 1, 2, 3, 4, 5, 6),
  sigma_nu = c(0.05, 0.1, 0.25, 0.5, 1),
  sigma_eps = c(0, 0.01, 0.05)
) |> 
  mutate(rho = rho/2, rho2=rho)

scenarioII_risk_res <- bind_cols(params2, tibble(muII = params2 |> pmap(mu_II) |> list_c()))

scenarioII_risk_res |>
  filter(sigma_eps > 0, sigma_nu > 0, n %in% c(1,3,5)) |>
  ggplot(aes(x = rho, y = muII, color = as.factor(sigma_nu))) +
  geom_line(aes(group = sigma_nu), size = 0.8) +
  facet_grid(sigma_eps ~ n, labeller = labeller(n = label_n, sigma_eps = label_eps)) +
  labs(
    title = TeX("Risque $\\mu_{II}$ en fonction de $\\rho$ pour différents paramètres"),
    x = expression(rho),
    y = TeX("Mesure de risque $\\mu_{II}$"),
    color = TeX("$\\sigma_\\nu$")
  ) +
  scale_x_continuous(breaks = seq(0,1,0.2)) + 
  scale_y_continuous(breaks = seq(0,1,0.2)) +
  geom_vline(aes(xintercept=k), linetype = "dashed", color = "grey35") +
  theme_minimal() +
  theme(legend.position = "bottom")

