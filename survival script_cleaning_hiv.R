## load packages
pacman::p_load(dplyr, tidyr, writexl, readxl)

## set wd
setwd("C:/Users/vl22683/OneDrive - University of Bristol/Documents/Publications/Romania PWID/data")

## load data
romania_pwid_raw <- read_excel("ARAS DATA IDU 2013-2022.xlsx") 

## baseline HIV cohort

# remove rows where hiv test result is missing
romania_pwid_hiv <- romania_pwid_raw[!is.na(romania_pwid_raw$hiv_test_rslt), ]

# remove rows where hiv test result is indeterminate 
romania_pwid_hiv <- romania_pwid_hiv %>%
  filter(!hiv_test_rslt == 3)

# create sequence of visits by ID
romania_pwid_hiv <- romania_pwid_hiv %>%
  group_by(id) %>%
  arrange(id, appointment_dte) %>%
  mutate(appointment_seq = row_number())

romania_pwid_hiv <- ungroup(romania_pwid_hiv)

# HIV test results by visit
romania_pwid_hiv_summary <- table(romania_pwid_hiv$appointment_seq, romania_pwid_hiv$hiv_test_rslt)
print(romania_pwid_hiv_summary) 

# remove IDs where hiv positive at baseline
ids_to_remove <- romania_pwid_hiv %>%
  filter(appointment_seq == 1 & hiv_test_rslt == 2) %>%
  pull(id)

romania_pwid_hiv <- romania_pwid_hiv %>%
  filter(!(id %in% ids_to_remove))

# HIV test results by visit
romania_pwid_hiv_summary <- table(romania_pwid_hiv$appointment_seq, romania_pwid_hiv$hiv_test_rslt)
print(romania_pwid_hiv_summary) 

# restrict to participants with multiple tests
romania_pwid_hiv <- romania_pwid_hiv %>%
  group_by(id) %>%
  arrange(id) %>%
  mutate(hiv_test_seq = row_number())

# HIV test results by visit
romania_pwid_hiv_summary <- table(romania_pwid_hiv$hiv_test_seq, romania_pwid_hiv$hiv_test_rslt)
print(romania_pwid_hiv_summary) 

# remove participants with only one test
romania_pwid_hiv <- romania_pwid_hiv %>%
  group_by(id) %>%
  filter(!(max(hiv_test_seq, na.rm = TRUE) == 1)) %>%
  ungroup()

# HIV test results by visit
romania_pwid_hiv_summary <- table(romania_pwid_hiv$hiv_test_seq, romania_pwid_hiv$hiv_test_rslt)
print(romania_pwid_hiv_summary) 

# remove hiv tests after first positive
romania_pwid_hiv <- romania_pwid_hiv %>%
  group_by(id) %>%
  mutate(first_hiv_positive_dte = ifelse(hiv_test_rslt == 2, appointment_dte, NA)) %>%
  mutate(first_hiv_positive_dte = min(first_hiv_positive_dte, na.rm = TRUE)) %>%
  ungroup()

romania_pwid_hiv <- romania_pwid_hiv %>%
  filter(is.na(first_hiv_positive_dte) | appointment_dte <= first_hiv_positive_dte)

# HIV test results by visit
romania_pwid_hiv_summary <- table(romania_pwid_hiv$hiv_test_seq, romania_pwid_hiv$hiv_test_rslt)
print(romania_pwid_hiv_summary) 

# create hiv testing dataframe 
romania_pwid_hiv_test <- subset(romania_pwid_hiv, select = c(id, appointment_dte, hiv_test_seq, hiv_test_rslt)) 

# create lag of appointment dates and hiv tests
romania_pwid_hiv_test <- romania_pwid_hiv_test %>%
  arrange(id, hiv_test_seq) %>%  
  group_by(id) %>%
  mutate(appointment_dte_lag = lead(appointment_dte),
         hiv_test_rslt_lag = lead(hiv_test_rslt)) 

# remove empty rows
romania_pwid_hiv_test <- romania_pwid_hiv_test[!is.na(romania_pwid_hiv_test$hiv_test_rslt_lag), ]

# convert to dates
romania_pwid_hiv_test <- romania_pwid_hiv_test %>%
  mutate(appointment_dte = as.Date(appointment_dte, format = "%d-%m-%Y")) %>%
  mutate(appointment_dte_lag = as.Date(appointment_dte_lag, format = "%d-%m-%Y"))

# days at risk
romania_pwid_hiv_test <- romania_pwid_hiv_test %>%
  mutate(days_risk = appointment_dte_lag-appointment_dte)

romania_pwid_hiv_test <- romania_pwid_hiv_test %>%
  mutate(days_risk = ifelse(hiv_test_rslt_lag == 2, days_risk / 2, days_risk))


                            
write_xlsx(romania_pwid_hiv_test,"hiv_test_data.xlsx")













