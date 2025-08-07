library(ckm)
library(dplyr)

source("R/risk_assessment.R")
source("R/utility_assessment.R")

s = 11 # confidentiality threshold

params <- expand.grid(
  D = c(10, 15, 20),
  V = c(10, 20, 30, 40),
  js = c(5:10),
  s = s,
  KEEP.OUT.ATTRS = FALSE
) |> as.data.frame() |>
  filter(D > js)|>
  filter(s > js)


# Test des variances
params <- params |>
  full_join(
    bind_cols(
      params |> select(D,js) |> unique(),
      data.frame( 
        Vmin = purrr::pmap(
          params |> select(D,js) |> unique(),
          ckm::test_matrices,
          Vmax = 40,
          precision = 1,
          .progress = TRUE
        ) |>
          purrr::list_c()
      )
    ),
    by = c("D","js")
  ) |>
  mutate(
    is_transition = V >= Vmin,
    Vf = ifelse(is_transition, V, ceiling(Vmin))
  ) |>
  unique()

write.csv(params, file = file.path("data", paste0("params",".csv")), row.names = FALSE)


# Sur freq_census_tab2
rep_data <- "data"
tab <- "freq_census_tab2"
file <- file.path(rep_data, paste0(tab, ".csv"))

freqs <- read.csv(file)

utility <- purrr::pmap(
  params |> select(D,V=Vf,js),
  utility_assessment,
  freq = freqs,
  freq_name = tab,
  precision = 5,
  .progress = TRUE
) |>
  purrr::list_rbind()

risk <- purrr::pmap(
  params |> select(D,V=Vf,js,s),
  risk_assessment,
  prior_pi = "custom",
  freq = freqs,
  freq_name = tab,
  I = 1:(s-1), 
  J = 1:(s-1),
  .progress = TRUE
) |>
  purrr::list_rbind()


