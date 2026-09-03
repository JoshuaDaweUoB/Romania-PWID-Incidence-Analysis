# load packages
pacman::p_load(dplyr, lubridate, stringi, withr)

## set wd
setwd("C:/Users/vl22683/OneDrive - University of Bristol/Documents/Publications/Romania PWID/data")

# set seed
set.seed(666)

n <- 100000

# id function
random_id <- function(n) {
  stri_rand_strings(n, 15, pattern = "[A-Z0-9]")
}

# random number function
random_date <- function(n, start, end) {
  as.Date(runif(n, as.numeric(as.Date(start)), as.numeric(as.Date(end))), origin = "1970-01-01")
}

sim_data <- tibble(
  id = random_id(n),
  appointment_dte = random_date(n, "2013-01-01", "2022-12-31"),
  gender = sample(c("M", "F"), n, replace = TRUE, prob = c(0.75, 0.25)),
  dob = random_date(n, "1955-01-01", "2004-12-31"),
  idu = rbinom(n, 1, 0.85),
  drug_type = sample(c("heroin", "new_psychoactive_substances", "other"), n, replace = TRUE, prob = c(0.6, 0.3, 0.1)),
  syringe_secondary = rbinom(n, 1, 0.3),
  sex_work_current = rbinom(n, 1, 0.1),
  msm_current = rbinom(n, 1, 0.05),
  homeless_current = rbinom(n, 1, 0.2),
  ethnic_minor = rbinom(n, 1, 0.3),
  hiv = rbinom(n, 1, 0.08),
  hcv = rbinom(n, 1, 0.6),
  syringes_distributed_1ml = rpois(n, 15),
  syringes_distributed_2ml = rpois(n, 5),
  syringes_recovered = rpois(n, 12),
  alcohol_pad_distributed = rpois(n, 10),
  medical_assistance_session = rbinom(n, 1, 0.15),
  referring = rbinom(n, 1, 0.1),
  accompanying_transport = rbinom(n, 1, 0.05),
  hiv_test_result = rbinom(n, 1, 0.4),
  hcv_test_result = rbinom(n, 1, 0.4),
  screening_tb = rbinom(n, 1, 0.1),
  diagnosed_tb = rbinom(n, 1, 0.02),
  tb_treatment_adherence = rbinom(n, 1, 0.7),
  tb_treatment_incentive = rbinom(n, 1, 0.5),
  psych_counselling = rbinom(n, 1, 0.1),
  social_counselling = rbinom(n, 1, 0.1)
) %>%
  mutate(
    appointment_year = year(appointment_dte),
    appointment_month = month(appointment_dte),
    dob_year = year(dob),
    dob_month = month(dob),
    hiv_test_rslt = ifelse(hiv_test_result == 1, rbinom(n, 1, 0.05), NA),
    hcv_test_rslt = ifelse(hcv_test_result == 1, rbinom(n, 1, 0.15), NA),
    referring_dte = if_else(referring == 1, appointment_dte + days(sample(0:14, n, replace = TRUE)), as.Date(NA)),
    accompanying_transport_dte = if_else(accompanying_transport == 1, appointment_dte + days(sample(0:14, n, replace = TRUE)), as.Date(NA)),
    tb_treatment_dte = if_else(diagnosed_tb == 1, appointment_dte + days(sample(0:30, n, replace = TRUE)), as.Date(NA))
  ) %>%
  dplyr::select(
    id, appointment_dte, appointment_year, appointment_month,
    gender, dob_month, dob_year, dob,
    idu, drug_type, syringe_secondary, sex_work_current, msm_current,
    homeless_current, ethnic_minor, hiv, hcv,
    syringes_distributed_1ml, syringes_distributed_2ml, syringes_recovered,
    alcohol_pad_distributed, medical_assistance_session,
    referring, referring_dte, accompanying_transport, accompanying_transport_dte,
    hiv_test_result, hcv_test_result, hiv_test_rslt, hcv_test_rslt,
    screening_tb, diagnosed_tb, tb_treatment_dte, tb_treatment_adherence,
    tb_treatment_incentive, psych_counselling, social_counselling
  )

# save
write.csv(sim_data, "simulated_harm_reduction_data.csv", row.names = FALSE)
