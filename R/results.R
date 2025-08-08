library(ckm)
library(dplyr)
library(purrr)

source("R/risk_assessment.R")
source("R/utility_assessment.R")

s = 11 # confidentiality threshold

params <- read.csv(
  file = file.path("data", paste0("params",".csv")),
)

rep_data <- "data"
frequencies_list <- list(
  "freq_census_tab1" =  list(
    prior_pi = "custom",
    freq = read.csv(file.path(rep_data, "freq_census_tab1.csv"))
    ),
  "freq_census_tab2" = list(
    prior_pi = "custom",
    freq = read.csv(file.path(rep_data, "freq_census_tab2.csv"))
    ),
  "poisson5" = list(
    prior_pi = "custom",
    freq = data.frame(i = 0:100) |> mutate(p_hat = dpois(i, 5)) #98.7% de cases < 11
  ),
  "poisson10" = list(
    prior_pi = "custom",
    freq = data.frame(i = 0:100) |> mutate(p_hat = dpois(i, 10)) #58.3% de cases < 11
  ),
  "poisson15" = list(
    prior_pi = "custom",
    freq = data.frame(i = 0:100) |> mutate(p_hat = dpois(i, 15)) #11.9% de cases < 11
  ),
  "poisson20" = list(
    prior_pi = "custom",
    freq = data.frame(i = 0:100) |> mutate(p_hat = dpois(i, 20)) #1.1% de cases < 11
  )
)

all_results_l <- purrr::imap(
  frequencies_list,
  \(fr,na){
    cat("\n=========== ", na, " =================\n")
    utility <- purrr::pmap(
      params |> select(D,V=Vf,js) |> unique(),
      utility_assessment,
      freq = fr$freq,
      freq_name = na,
      precision = 5,
      .progress = TRUE
    ) |>
      purrr::list_rbind()
    
    risk <- purrr::pmap(
      params |> select(D,V=Vf,js,s) |> unique(),
      risk_assessment,
      prior_pi = fr$prior_pi,
      freq = fr$freq,
      freq_name = na,
      I = 1:(s-1), 
      J = 1:(s-1),
      .progress = TRUE
    ) |>
      purrr::list_rbind()
    
    risk1 <- risk |>
      filter(i == j) |>
      group_by(D,V,js,tab,s) |>
      summarise(R1 = sum(qij), .groups = "drop")
    
    
    risk2 <- risk |>
      group_by(D,V,js,tab,s) |>
      slice(n()) |>
      summarise(R2 = qij, .groups = "drop")
    
    
    RU <- utility |>
      full_join(
        full_join(risk1, risk2, by = c("D","V","js","tab","s")),
        by = c("D","V","js","tab")
      ) |> 
      unique()
  }
)

all_RUs <- all_results_l |> purrr::list_rbind()

save(all_RUs, file = "all_RUs.RData")


library(ggplot2)

ggplot(RU) +
  geom_point(aes(x=1-U1, y=R1, color = as.factor(js), size = V)) +
  geom_line(aes(x=1-U1, y=R1, group = as.factor(js),  color = as.factor(js))) +
  facet_wrap(~D)

ggplot(RU) +
  geom_point(aes(x=1-U1, y=R1, color = V, shape = as.factor(D))) +
  geom_line(aes(x=1-U1, y=R1, group = V,  color = V)) +
  facet_wrap(~js, label="label_both")

ggplot(RU) +
  geom_point(aes(x=1-U1, y=R2, color = V, shape = as.factor(D))) +
  geom_line(aes(x=1-U1, y=R2, group = V,  color = V)) +
  facet_wrap(~js, label="label_both")


ggplot(RU) +
  geom_point(aes(x=U2, y=R2, color = V, shape = as.factor(D))) +
  geom_line(aes(x=U2, y=R2, group = V,  color = V)) +
  facet_wrap(~js, label="label_both")


ggplot(RU) +
  geom_point(aes(x=U2, y=R2, color = V)) +
  geom_line(aes(x=U2, y=R2, group = as.factor(js), linetype =as.factor(js))) +
  facet_wrap(~D, label="label_both")



ggplot(RU) +
  geom_point(aes(x=U2, y=R1, color = V)) +
  geom_line(aes(x=U2, y=R1, group = as.factor(js), linetype =as.factor(js))) +
  facet_wrap(~D, label="label_both")


ggplot(RU) +
  geom_point(aes(x=1-U3, y=R1, color = V)) +
  geom_line(aes(x=1-U3, y=R1, group = as.factor(js), linetype =as.factor(js))) +
  facet_wrap(~D, label="label_both")



