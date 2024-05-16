library(here)
library(tidyverse)
library(kableExtra)
library(lme4)
library(rms)
library(brms)
library(splines)
library(easystats)

# load data ---------------------------------------------------------------
df <- read_rds(here("data", "processed_data.rds")) |>
  dplyr::filter(response_num_touch <= 20) |>
  dplyr::select(participant, dose, test_day, choice_latency,
                response_side, age_c, sex, age, initiation_time_last,
                initiation_num_touch, response_correct, response_num_touch,
                choice_latency_prev, response_correct_prev, day, distance_to_center,
                trial) |>
  dplyr::mutate(choice_latency = scale(choice_latency) |> as.numeric(),
                initiation_time_last = scale(initiation_time_last) |> as.numeric(),
                choice_latency_prev = scale(choice_latency_prev) |> as.numeric(),
                distance_to_center = scale(distance_to_center) |> as.numeric()) |>
  na.omit() # to have the same numbe rof observations for trials with lookback to a previous trial


# glmer -------------------------------------------------------------------
# probability of correct decision
m_success <- glmer(response_correct ~ sex*dose + ns(test_day, 3) + age + ns(trial, 3) +
                      choice_latency + response_correct_prev + response_num_touch +
                      distance_to_center +
                      (day|participant), family = binomial,
                     data = df)


check_model(m_success)

mb_success <- brm(response_correct ~ sex*dose + ns(test_day, 3) + age + ns(trial, 3) +
                     choice_latency + response_correct_prev + response_num_touch +
                     distance_to_center +
                     (day|participant), family = bernoulli(link="logit"),
                   data = df)