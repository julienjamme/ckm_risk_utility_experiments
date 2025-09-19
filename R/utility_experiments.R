
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



















