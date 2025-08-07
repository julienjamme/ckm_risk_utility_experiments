library(ckm)
library(dplyr)

params <- data.frame(
  D = 10,
  V = 10,
  js = 3,
  s = 11,
  lambda  = 5
)




trans <- purrr::pmap(params |> select(D,V,js), ckm::create_transition_matrix)

frequencies <- data.frame(
  i = 0:50,
  N = NA
) |>
  mutate(p_hat = dpois(i, params$lambda))

ckm::assess_risk(trans[[1]], frequencies, I = 1:(params$s-1), J=(params$js+1):(params$s-1))






