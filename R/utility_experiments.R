
source("R/utility_assessment.R")


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

census_tab1 = read.csv(file.path("data", "freq_census_tab1.csv")) 
census_tab1 |> filter(i > 0, i < 5) |> summarise(sum(p_hat)) #37.7% de cases < 5
census_tab1 |> filter(i > 20) |> summarise(sum(p_hat)) #7.5%
census_tab1 |> filter(i == 0) |> summarise(sum(p_hat)) #36.2%

resu_tab1 <-
  purrr::pmap(
    params |> select(D,V=V,js) |> unique(),
    utility_assessment,
    freq = census_tab1,
    freq_name = "tab1",
    precision = 3,
    .progress = TRUE
  )

resu_tab1 |>
  # purrr::list_rbind() |>
  filter(!is.na(U1)) |>
  full_join(
    res_all |> # results from risk_experiment.R
      filter(type == "tab1")) |>
  ggplot() +
  geom_line(
    aes(x=U3, y=risk, color = as.factor(js), linetype = as.factor(D)) 
  ) +
  geom_point(
    aes(x=U3, y=risk, shape = as.factor(V), color = as.factor(js))
  ) +
  geom_hline(yintercept = 0.5, color = "grey25") +
  geom_vline(xintercept = 0.7, color = "grey25") +
  labs(x = "Risk Interval", y = "V") +
  scale_y_continuous("Risk", breaks = seq(0,1,0.1), labels = seq(0,1,0.1), expand = c(0,0), limits = c(0.5,1)) +
  scale_x_continuous("Utility", breaks = seq(0,1,0.1), labels = seq(0,1,0.1), expand = c(0,0), limits = c(0.7,1)) +
  scale_color_brewer("js", type="qual", palette=1) +
  scale_shape_discrete("V") +
  scale_linetype_discrete("D") +
  theme(legend.position = "bottom", legend.title.position = "top", panel.grid.minor = element_blank()) 

ggsave("risk_utility_TO_tab1.pdf", device = "pdf", units="cm",width = 24,height=15)
  
  


















