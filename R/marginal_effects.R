library(here)
library(tidyverse)
library(gamlss)
library(ggsci)
library(nlme)

theme_set(theme_classic(base_size = 14))
dodge <- position_dodge(width = 0.2)

# load data and models ----------------------------------------------------
path <- here("data", "processed_data.rds")

df <- read_rds(path) |>
  dplyr::filter(response_num_touch <= 20) |>
  dplyr::select(participant, dose, test_day, choice_latency,
                response_side, age_c, sex, age, initiation_time_last,
                initiation_num_touch, response_correct, response_num_touch,
                choice_latency_prev, response_correct_prev, day, distance_to_center,
                trial) |>
  dplyr::mutate(choice_latency_c = scale(choice_latency) |> as.numeric(),
                initiation_time_last_c = scale(initiation_time_last) |> as.numeric(),
                choice_latency_prev_c = scale(choice_latency_prev) |> as.numeric(),
                distance_to_center_c = scale(distance_to_center) |> as.numeric()) |>
  na.omit()

m_success <- read_rds(here("data", "gamlss_success.rds"))

df_success <- df |> bind_cols(pred = predict(m_success, output = "matrix", what = "mu", type = "link",
                        se.fit = FALSE)) |>
  bind_cols(pred_se = predict(m_success, output = "matrix", what = "mu", type = "link",
                              se.fit = TRUE)$se.fit) |>
  dplyr::mutate(predic = plogis(pred),
              upper_ci = plogis(pred + 1.96 * pred_se),
              lower_ci = plogis(pred - 1.96 * pred_se))


df_sum_success <- df_success |>
  dplyr::group_by(dose, sex) |>
  dplyr::summarise(mean_pred = mean(predic),
                   mean_resp = mean(response_correct),
                   mean_upper_ci = mean(upper_ci),
                   mean_lower_ci = mean(lower_ci))



gg_success <- ggplot(df_sum_success, aes(dose, mean_pred, colour = sex,
                                     ymin = mean_lower_ci,
                                     ymax = mean_upper_ci)) +
              geom_linerange(position = dodge, linewidth = 1) +
              geom_point(aes(y = mean_resp, group = sex), position = dodge,
                         size = 6, color = "grey60", show.legend = FALSE) +
              geom_point(position = dodge, size = 3) +
  scale_color_jco() +
  labs(y = "Accuracy", x = "Dose")

gg_success
