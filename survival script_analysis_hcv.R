# load packages
pacman::p_load(dplyr, arsenal, survival)

## baseline analysis

# load analyses data
romania_pwid_hcv_analysis <- read_excel("hcv_data_analysis.xlsx") 

# baseline characteristics sex work
romania_pwid_hcv_analysis <- romania_pwid_hcv_analysis %>%
  group_by(id) %>%
  mutate(id_seq = row_number())

analysis_data_hcv_bl <- subset(romania_pwid_hcv_analysis, id_seq == 1)

## longitudinal analysis


























# overall incidence rate
total_days_hcv <- sum(romania_pwid_hcv_analysis$days_risk)
total_cases <- sum(romania_pwid_hcv_analysis$hcv_test_rslt)
incidence_rate <- (total_cases / total_days_hcv) * 365.25 *100

cat("Incidence rate of HCV per 100 person years:", incidence_rate)

# unadjusted hazard ratio - time
romania_pwid_year = coxph(
  Surv(time = appointment_dte, time2 = appointment_dte_lag, event = hcv_test_rslt) ~ midpoint_year, 
  data = romania_pwid_hcv_analysis
)

summary(romania_pwid_year)

# by year incidence rate
incidence_rate_year <- romania_pwid_hcv_analysis %>%
  group_by(midpoint_year) %>%
  summarize(
    events = sum(hcv_test_rslt == 1, na.rm = TRUE),     
    person_time_years = sum(days_risk/365.25),    
    incidence_rate = (events / person_time_years)*100,     
    lower_ci = ifelse(events > 0, (qpois(0.025, events) / person_time_years)*100, NA),
    upper_ci = ifelse(events > 0, (qpois(0.975, events + 1) / person_time_years)*100, NA)
  )

incidence_rate_year

# unadjusted hazard ratio - sex
romania_pwid_sex = coxph(
  Surv(time = appointment_dte, time2 = appointment_dte_lag, event = hcv_test_rslt) ~ gender, 
  data = romania_pwid_hcv_analysis
)

summary(romania_pwid_sex)

# unadjusted hazard ratio - ethnicity
romania_pwid_ethnicity = coxph(
  Surv(time = appointment_dte, time2 = appointment_dte_lag, event = hcv_test_rslt) ~ ethnic_roma, 
  data = romania_pwid_hcv_analysis
)

summary(romania_pwid_ethnicity)

# unadjusted hazard ratio - homelessness
romania_pwid_homeless = coxph(
  Surv(time = appointment_dte, time2 = appointment_dte_lag, event = hcv_test_rslt) ~ homeless_current, 
  data = romania_pwid_hcv_analysis
)

summary(romania_pwid_homeless)

# unadjusted hazard ratio - sex work
romania_pwid_sexwork = coxph(
  Surv(time = appointment_dte, time2 = appointment_dte_lag, event = hcv_test_rslt) ~ sex_work_current + gender, 
  data = romania_pwid_hcv_analysis
)

summary(romania_pwid_sexwork)

# unadjusted hazard ratio - msm
romania_pwid_msm = coxph(
  Surv(time = appointment_dte, time2 = appointment_dte_lag, event = hcv_test_rslt) ~ msm_current, 
  data = romania_pwid_hcv_analysis
)

summary(romania_pwid_msm)

# adjusted hazard ratio - all covariates
romania_pwid_adjusted = coxph(
  Surv(time = appointment_dte, time2 = appointment_dte_lag, event = hcv_test_rslt) ~ gender + ethnic_roma + homeless_current + sex_work_current + msm_current, 
  data = romania_pwid_hcv_analysis
)

summary(romania_pwid_adjusted)


