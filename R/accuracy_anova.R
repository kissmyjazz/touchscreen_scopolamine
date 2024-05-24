library(here)
library(tidyverse)
library(ggstatsplot)
library(afex)
library(emmeans)
library(kableExtra)
library(flextable)

path <- here("data", "processed_data.rds")

df_accuracy <- read_rds(path) |>
  dplyr::filter(response_num_touch <= 20) |>
  dplyr::filter(participant != "Muffin") |>
  dplyr::group_by(participant, dose, test_day) |>
  dplyr::summarise(accuracy = mean(response_correct))

aov_accuracy <- aov_ez("participant", "accuracy", df_accuracy, within = c("dose", "test_day"))
aov_accuracy |> nice(aov = TRUE, p.adjust ="holm") |>
  as_flextable() |>
  flextable::set_table_properties(width = .5) |>
  save_as_docx(path = here("tables", "accuracy_anova_table.docx"))

summary(aov_accuracy)

df_accuracy_age <- read_rds(path) |>
  dplyr::filter(response_num_touch <= 20) |>
  dplyr::filter(participant != "Muffin") |>
  dplyr::group_by(participant, dose, age_c) |>
  dplyr::summarise(accuracy = mean(response_correct))

aov_accuracy_age <- aov_ez("participant", "accuracy", df_accuracy_age, within = c("dose"),
                           between = "age_c")
aov_accuracy |> nice(aov = TRUE, p.adjust ="holm") |>
  as_flextable() |>
  flextable::set_table_properties(width = .5) |>
  save_as_docx(path = here("tables", "accuracy_anova_table.docx"))

summary(aov_accuracy)

gg_afex_accuracy <- afex_plot(aov_accuracy, x = "test_day", trace = "dose", error = "within",
                     factor_levels = list(test_day = c("1", "2", "3", "4", "5")),
                     point_arg = list(size = 3), data_arg = list(cex = 2)) +
  theme_classic(base_size = 14) +
  labs(title = "Accuracy by test day and scopolamine dose",
       x = "Test Day",
       y = "Accuracy")

gg_afex_accuracy
