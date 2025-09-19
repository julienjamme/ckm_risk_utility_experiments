library(dplyr)
library(purrr)
library(ggplot2)

source("R/risk_assessment.R")


params1 <- data.frame(
  D = rep(5, 12),
  V = sort(rep(c(2.5, 5, 10, 15), 3)),
  js = rep(c(0, 2, 4),4),
  s=5
) |>
  filter(D > js)

params2 <- data.frame(
  D = rep(10, 12),
  V = sort(rep(c(2.5, 5, 10, 15), 3)),
  js = rep(c(0, 2, 4),4),
  s=5
) |>
  filter(D > js)
params <- bind_rows(list(params1,params2)) |> unique() 

res_uniform <- params |>
  purrr::pmap(
    risk_assessment,
    prior_pi = "uniform", 
    Ncell = 100,
    I = 1:4, J=1:5,
    .progress = TRUE
  )

# res_uniform2 <- params |>
#   purrr::pmap(
#     risk_assessment,
#     prior_pi = "uniform", 
#     Ncell = 1e6,
#     I = 1:4, J=1:5,
#     .progress = TRUE
#   )
# le résultat est indépendant du nombre de cellules !

census_tab1 = read.csv(file.path("data", "freq_census_tab1.csv")) 
census_tab1 |> filter(i > 0, i < 5) |> summarise(sum(p_hat)) #37.7% de cases < 5
census_tab1 |> filter(i > 20) |> summarise(sum(p_hat)) #7.5%
census_tab1 |> filter(i == 0) |> summarise(sum(p_hat)) #36.2%

res_tab1 <- params |> 
  purrr::pmap(
    risk_assessment,
    prior_pi = "custom", 
    freq = census_tab1,
    I = 1:4, J=1:5,
    .progress = TRUE
  )


census_tab2 = read.csv(file.path("data", "freq_census_tab2.csv")) 
census_tab2 |> filter(i > 0, i < 5) |> summarise(sum(p_hat)) #1.3% de cases < 5 et 97.2% de cases > 20
census_tab2 |> filter(i > 20) |> summarise(sum(p_hat))

res_tab2 <- params |> 
  purrr::pmap(
    risk_assessment,
    prior_pi = "custom", 
    freq = census_tab2,
    I = 1:4, J=1:5,
    .progress = TRUE
  )

res_all <- res_uniform |> 
  purrr::map(
    \(t) if(!is.null(t)) t |> tail(1) |> select(D, V, js, s, I=i, J=j, risk = qij) |> mutate(type="uniform") else NULL
  ) |> 
  purrr::compact() |>
  bind_rows(
    res_tab1 |> 
      purrr::map(
        \(t) if(!is.null(t)) t |> tail(1) |> select(D, V, js, s, I=i, J=j, risk = qij) |> mutate(type="tab1") else NULL
      ) |> 
      purrr::compact()
  ) |>
  bind_rows(
    res_tab2 |> 
      purrr::map(
        \(t) if(!is.null(t)) t |> tail(1) |> select(D, V, js, s, I=i, J=j, risk = qij) |> mutate(type="tab2") else NULL
      ) |> 
      purrr::compact()
  )

library(ggplot2)
source("R/theme_for_ggplots.R")

res_all |> filter(type %in% c("uniform","tab1")) |> 
  tidyr::pivot_wider(names_from = type, values_from = risk) |>
  mutate(js_num = as.numeric(as.factor(js)),
         V_jitter = V + (js_num - mean(js_num)) * 0.5) |>
  ggplot(aes(y = V_jitter, color = as.factor(js))) +
  geom_segment(aes(x = uniform, xend = tab1, yend = V_jitter), size = 1) +  
  geom_point(aes(x = uniform, shape = "uniform"), fill = "white", size=6) +              
  geom_point(aes(x = tab1, shape = "tab1"), fill = "white", size=6) + 
  geom_hline(yintercept = 0, color = "grey25") +
  geom_vline(xintercept = 0, color = "grey25") +
  facet_wrap(~ D, scales = "fixed", labeller = "label_both") +                                 
  labs(x = "Risk Interval", y = "V", title = "Range of risk assessment", subtitle = "Comparison of uniform prior and empirical frequencies (highly ventilated)") +
  scale_y_continuous("V", breaks = c(2.5, 5, 10, 15), labels = c(2.5, 5, 10, 15), expand = c(0,0), limits = c(0,16)) +
  scale_x_continuous("Risk assessment", breaks = seq(0,1,0.25), labels = seq(0,1,0.25), expand = c(0,0), limits = c(0,1)) +
  scale_color_manual("js", values = c("0" = "#377eb8", "2" = "orangered", "4" = "grey15")) + #type="qual", palette=6) +
  scale_shape_manual(name = "Prior", values = c(uniform = 21, tab1 = 22), labels = c(uniform="Uniform", tab1="Frequency")) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank()) 

ggsave("risk_assessment_tab1.pdf", device = "pdf", units="cm",width = 24,height=15)

res_all |> filter(type %in% c("uniform","tab2")) |> 
  tidyr::pivot_wider(names_from = type, values_from = risk) |>
  mutate(js_num = as.numeric(as.factor(js)),
         V_jitter = V + (js_num - mean(js_num)) * 0.5) |>
  ggplot(aes(y = V_jitter, color = as.factor(js))) +
  geom_segment(aes(x = uniform, xend = tab2, yend = V_jitter), size = 1) +  
  geom_point(aes(x = uniform, shape = "uniform"), fill = "white", size=6) +              
  geom_point(aes(x = tab2, shape = "tab2"), fill = "white", size=6) + 
  geom_hline(yintercept = 0, color = "grey25") +
  geom_vline(xintercept = 0, color = "grey25") +
  facet_wrap(~ D, scales = "fixed", labeller = "label_both") +                                 
  labs(x = "Risk Interval", y = "V", title = "Range of risk assessment", subtitle = "Comparison of uniform prior and empirical frequencies (highly aggregated)") +
  scale_y_continuous("V", breaks = c(2.5, 5, 10, 15), labels = c(2.5, 5, 10, 15), expand = c(0,0), limits = c(0,16)) +
  scale_x_continuous("Risk assessment", breaks = seq(0,1,0.25), labels = seq(0,1,0.25), expand = c(0,0), limits = c(0,1)) +
  scale_color_manual("js", values = c("0" = "#377eb8", "2" = "orangered", "4" = "grey15")) + #type="qual", palette=6) +
  scale_shape_manual(name = "Prior", values = c(uniform = 21, tab2 = 22), labels = c(uniform="Uniform", tab2="Frequency")) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank()) 

ggsave("risk_assessment_tab2.pdf", device = "pdf", units="cm",width = 24,height=15)
