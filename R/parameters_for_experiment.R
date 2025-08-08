# preparation of parameters for experiment

library(ckm)
library(dplyr)

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
