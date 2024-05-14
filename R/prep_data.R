library(here)
library(tidyverse)

df_ <- read_rds(here("data", "combined_data.rds"))

# add marmoset sex and age in years to the dataset.
df <- df_ |>
  dplyr::mutate(sex = case_when(
    participant == "Crumble" ~ "female",
    participant == "Harmonia" ~ "female",
    participant == "Icarus" ~ "male",
    participant == "Kore" ~ "female",
    participant == "Muffin" ~ "female",
    participant == "Nereus" ~ "male",
    participant == "Nyx" ~ "female",
    participant == "Quintas" ~ "male",
    TRUE ~ NA_character_
  ) |> factor(),
  age = case_when(
    participant == "Crumble" ~ 10,
    participant == "Harmonia" ~ 2,
    participant == "Icarus" ~ 2,
    participant == "Kore" ~ 2,
    participant == "Muffin" ~ 8,
    participant == "Nereus" ~ 2,
    participant == "Nyx" ~ 7,
    participant == "Quintas" ~ 6,
    TRUE ~ NA_integer_),
  date = lubridate::ymd(date)) |>
  dplyr::arrange(participant, date) |>
  # number of days since the beginning of each marmoset testing
  dplyr::mutate(day = as.integer(date - first(date)), .by = participant) |>

