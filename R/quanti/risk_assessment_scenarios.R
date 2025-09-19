
# Fonction calculant sigma de Z selon rho, n, sigma_nu, sigma_eps
sigma_Z <- function(rho, n, sigma_nu, sigma_eps) {
  sqrt(rho^(2*n)*sigma_nu^2 + sigma_eps^2)
}

# Mesure de risque mu_I(k)
mu_I <- function(k, rho, n, sigma_nu, sigma_eps) {
  beta <- 1 - k
  sigma <- sigma_Z(rho, n, sigma_nu, sigma_eps)
  lower <- (1 - beta) * rho - 1
  upper <- (1 + beta) * rho - 1
  pnorm(upper, mean = 0, sd = sigma) - pnorm(lower, mean = 0, sd = sigma)
}

# Mesure de risque mu_II(k) avec rho2 = part second plus grand contributeur
mu_II <- function(k, rho, rho2, n, sigma_nu, sigma_eps) {
  beta <- 1 - k
  sigma <- sigma_Z(rho, n, sigma_nu, sigma_eps)
  lower <- (1 - beta) * rho + rho2 - 1
  upper <- (1 + beta) * rho + rho2 - 1
  pnorm(upper, mean = 0, sd = sigma) - pnorm(lower, mean = 0, sd = sigma)
}

# Mesure de risque mu_III(k)
mu_III <- function(k, rho, n, sigma_nu, sigma_eps) {
  beta <- 1 - k
  sigma <- sigma_Z(rho, n, sigma_nu, sigma_eps)
  upper <- beta * rho
  lower <- -beta * rho
  pnorm(upper, mean = 0, sd = sigma) - pnorm(lower, mean = 0, sd = sigma)
}

# Exemple d'utilisation
k <- 0.8
rho <- 0.6
rho2 <- 0.2
n <- 2
sigma_nu <- 0.1
sigma_eps <- 0.05

mu_I(k, rho, n, sigma_nu, sigma_eps)
mu_II(k, rho, rho2, n, sigma_nu, sigma_eps)
mu_III(k, rho, n, sigma_nu, sigma_eps)
