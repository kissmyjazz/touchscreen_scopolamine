library(here)
library(tidyverse)
library(kableExtra)
library(lme4)
library(rms)
library(brms)
library(splines)
library(easystats)
library(lmerTest)
library(gamlss)
library(gamlss.dist)
library(gamlss.add)

path <- "C:/Users/jazzh/OneDrive/Documents/R/Code/scopolamine/processed_data.rds"
# load data ---------------------------------------------------------------
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
  na.omit() # to have the same numbe rof observations for trials with lookback to a previous trial


# glmer -------------------------------------------------------------------
# probability of correct decision

m_success <- gamlss(response_correct ~ sex*dose + pbz(test_day) + age + pbz(trial) +
                     choice_latency_c + response_correct_prev + response_num_touch +
                     distance_to_center_c, re(random = ~pb(day)|participant), family = BI(),
                   data = df)
AIC(m_success)
summary(m_success)
summary(m_success_glmer)
plot(m_success)
termplot(m_success)

write_rds(m_success, "C:/Users/jazzh/OneDrive/Documents/R/Code/scopolamine/gamlss_success.rds")

m_success_glmer <- glmer(response_correct ~ sex*dose + ns(test_day, 3) + age + ns(trial, 3) +
                      choice_latency_c + response_correct_prev + response_num_touch +
                      distance_to_center_c + (day|participant), family = binomial,
                    data = df)
AIC(m_success)
summary(m_success)

df_10 <- df |> dplyr::filter(choice_latency <= 10)

m_latency <- gamlss(choice_latency ~ sex*dose + pbz(test_day) + age + pbz(trial) +
                      response_correct + response_correct_prev + response_num_touch +
                      distance_to_center_c + choice_latency_prev + initiation_time_last_c +
                      response_side + re(random = ~pb(day)|participant) + (1|test_day), family = GB2(),
                    nu.formula = ~dose, i.control = glim.control(cyc = 500, bf.cyc = 100), method=mixed(1, 40),
                    data = df_10)

summary(m_latency)
plot(m_latency)

write_rds(m_latency, "C:/Users/jazzh/OneDrive/Documents/R/Code/scopolamine/gamlss_latency.rds")

AIC(m_latency)

AICcmodavg::aictab(list(m1 = m_latency, m3 = m_latency_s1))

dist <- fitDist(df$response_correct, k = 2, type = "binom", trace = FALSE, try.gamlss = TRUE)
dist
