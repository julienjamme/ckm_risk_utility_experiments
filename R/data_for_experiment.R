library(dplyr)

# recup tableaux statistiques du recensement de la population

temp <- tempfile()
download.file("https://www.insee.fr/fr/statistiques/fichier/8582668/TD_FOR2_2022_csv.zip",temp)
census_tab <- readr::read_delim(
  unz(temp, "TD_FOR2_2022.csv"), 
  delim = ";", 
  col_types = readr::cols(NB = "n", .default="c")
) |>
  mutate(DEP = substr(CODGEO, 1, 2)) |>
  mutate(NB = as.integer(ifelse(is.na(NB),0,round(NB, digits = 0))))
str(census_tab)
unlink(temp)

nrow(census_tab) # plus de 3 millions de cases
census_tab |> filter(NB < 1) |> nrow() 
census_tab |> filter(NB == 1) |> nrow() 
summary(census_tab$NB)
# un tableau avec très peu de zéros 
# et plus d'un quart de 1

hist(census_tab |> filter(NB < 100) |> pull(NB), breaks = 0:100)

nb_zeros <- census_tab |> filter(NB < 1) |> nrow() +
  (length(unique(census_tab$CODGEO)) * length(unique(census_tab$DIPL_19)) * length(unique(census_tab$AGEQ65)) * length(unique(census_tab$SEXE)) - nrow(census_tab))

freq_cens_tab <- census_tab |>
  group_by(i=NB) |>
  summarise(N = n(), .groups="drop") |>
  mutate(N = ifelse(i==0,nb_zeros,N)) |>
  mutate(p_hat = N/sum(N))

freq_cens_tab |>
  filter(i < 100) |>
  ggplot() +
  geom_bar(aes(x=i,y=p_hat), stat="identity")

# part de cases sensibles selon le seuil de confidentialité
purrr::map(5:11, \(s) freq_cens_tab |> filter(i > 0 & i < s) |> summarise(p_sens = sum(p_hat)) |> mutate(s = s)) |> purrr::list_rbind()
# tableau avec 79% de cases sensibles pour s=11


# Second tableau: par département
census_tab2 <- census_tab |>
  select(-CODGEO, -LIBGEO,-NIVGEO) |>
  group_by(across(where(is.character))) |>
  summarise(NB = sum(NB), .groups="drop")

nb_zeros2 <- census_tab2 |> filter(NB < 1) |> nrow() +
  (length(unique(census_tab2$DEP)) * length(unique(census_tab2$DIPL_19)) * length(unique(census_tab2$AGEQ65)) * length(unique(census_tab2$SEXE)) - nrow(census_tab2))


nrow(census_tab2) # 15318 cases
census_tab2 |> filter(NB == 0) |> nrow() 
census_tab2 |> filter(NB == 1) |> nrow() 
summary(census_tab2$NB)
# 3 zéros et 73 uniques !


freq_cens_tab2 <- census_tab2 |>
  group_by(i=NB) |>
  summarise(N = n(), .groups="drop") |>
  mutate(N = ifelse(i==0,nb_zeros2,N)) |>
  mutate(p_hat = N/sum(N))

freq_cens_tab2 |>
  filter(i < 100) |>
  ggplot() +
  geom_bar(aes(x=i,y=p_hat), stat="identity")
# Les uniques sont toujours relativement la case la plus fréquente mais avec une part <1% (contre >25% auparavant)


purrr::map(5:11, \(s) freq_cens_tab2 |> filter(i > 0 & i < s) |> summarise(p_sens = sum(p_hat)) |> mutate(s = s)) |> purrr::list_rbind()
# tableau avec 2% de cases sensibles pour s=11


purrr::iwalk(
  list(freq_census_tab1 = freq_cens_tab, freq_census_tab2 = freq_cens_tab2),  
  \(d,n) write.csv(d, file = file.path("data", paste0(n,".csv")), row.names = FALSE)
)



