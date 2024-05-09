library(here)
library(tidyverse)
library(readxl)  # for reading excel files


# regex patterns

ptn_dose <- "(?<=_)([^_]+)(?=_)"
ptn_last_number <- "([-+]?[0-9]*\\.[0-9]+)"

# Define the folder path
folder_path <- here("test_data")  # Replace with your actual folder path

# Get all excel files in the folder
excel_files <- list.files(path = folder_path, pattern = "^[^~]*\\.xlsx$",
                          full.names = TRUE)

named_excel_files <- purrr::set_names(excel_files, nm = basename(excel_files))

df_ <- named_excel_files |>
  map_dfr(read_excel, .id = "file") |>
  dplyr::filter(!is.na(status))

df <- df_ |>
  dplyr::mutate(dose = str_extract(file, ptn_dose) |> factor(), .after = file) |>
  dplyr::mutate(test_day = parse_number(file), .after = dose) |>
  dplyr::mutate(mouse_x_last = stringi::stri_match_last_regex(mouse.x, ptn_last_number)[, 1],
                .after = mouse.x) |>
  dplyr::mutate(mouse_y_last = stringi::stri_match_last_regex(mouse.y, ptn_last_number)[, 1],
                .after = mouse.y)


