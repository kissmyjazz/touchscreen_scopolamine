library(here)
library(tidyverse)
library(readxl)  # for reading excel files
library(writexl)


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
                .after = mouse.y) |>
  dplyr::mutate(mouse2_x_last = stringi::stri_match_last_regex(mouse_2.x, ptn_last_number)[, 1],
                .after = mouse_2.x) |>
  dplyr::mutate(mouse2_y_last = stringi::stri_match_last_regex(mouse_2.y, ptn_last_number)[, 1],
                .after = mouse_2.y) |>
  dplyr::mutate(mouse_time_last = stringi::stri_match_last_regex(mouse.time, ptn_last_number)[, 1],
                .after = mouse.time) |>
  dplyr::mutate(mouse2_time_last = stringi::stri_match_last_regex(mouse_2.time, ptn_last_number)[, 1],
                .after = mouse_2.time) |>
  dplyr::mutate(mouse_num_touch = str_count(mouse.x, ptn_last_number), .after = mouse_x_last) |>
  dplyr::mutate(mouse2_num_touch = str_count(mouse_2.x, ptn_last_number), .after = mouse2_x_last)

write_xlsx(df, here("data", "combined_data.xlsx"))
