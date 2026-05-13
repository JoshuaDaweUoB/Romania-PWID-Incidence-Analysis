## load packages
pacman::p_load(dplyr, tidyr, withr, lubridate, MASS, writexl, readxl, arsenal, survival, broom, ggplot2, purrr, tableone, stringr)

## set wd
setwd("C:/Users/vl22683/OneDrive - University of Bristol/Documents/Publications/Romania PWID/data")

## oat treatment data

# load
romania_pwid_treatment <- read_excel("FZC CDI 2013_2022_in tratament.xlsx")

# rename date column
romania_pwid_treatment <- romania_pwid_treatment %>%
  rename(appointment_dte = Data_A) %>%
  rename(id = "COD ALT")

# make date column date format
romania_pwid_treatment <- romania_pwid_treatment %>%
  mutate(appointment_dte = as.Date(appointment_dte))

print(names(romania_pwid_treatment))

# delete redunant column and add oat column
romania_pwid_treatment <- romania_pwid_treatment %>%
  dplyr::select(appointment_dte, id) %>%
  mutate(oat = 1)

# tab out number of oat rows
table(romania_pwid_treatment$oat, useNA="ifany") ## 1376 rows
n_distinct(romania_pwid_treatment$id) ## 1066 distinct

# first recorded oat 
romania_pwid_treatment <- romania_pwid_treatment %>%
  mutate(appointment_dte = as.Date(appointment_dte)) %>%
  group_by(id) %>%
  mutate(
    oat_seq = cumsum(ifelse(oat == 1, 1, 0)),
    oat_first_dte = min(appointment_dte[oat == 1], na.rm = TRUE)
  ) %>%
  ungroup()

## exposure data

## load data
romania_pwid_raw <- read_excel("ARAS DATA IDU 2013-2022.xlsx")

# append treatment df to raw dataframe
missing_cols <- setdiff(names(romania_pwid_raw), names(romania_pwid_treatment))
romania_pwid_treatment[missing_cols] <- NA
romania_pwid_raw <- bind_rows(romania_pwid_raw, romania_pwid_treatment)

# save combined data
romania_pwid_hiv_combined <- romania_pwid_raw[!is.na(romania_pwid_raw$hiv_test_rslt) | !is.na(romania_pwid_raw$oat), ]
write.csv(romania_pwid_hiv_combined, "romania_pwid_hiv_combined.csv")

# recode gender 
romania_pwid_hiv <- romania_pwid_raw %>%
  mutate(
    gender = case_when(
      gender == 2 ~ 0,
      TRUE ~ gender
    ),
    gender = factor(gender, levels = c(0, 1), labels = c("Female", "Male"))
  )
  
# age four categories
romania_pwid_hiv <- romania_pwid_hiv %>%
  mutate(
    dob = as.Date(dob, format = "%d/%m/%Y"),
    age = as.numeric(difftime(Sys.Date(), dob, units = "days")) / 365.25,
    age_4cat = cut(
      age,
      breaks = c(-Inf, 30, 40, 50, Inf),
      labels = c("<30", "30-39", "40-49", "50+"),
      right = FALSE
    )
  )

# one year prior to test
romania_pwid_hiv <- romania_pwid_hiv %>%
  mutate(
    hiv_test_dte = if_else(!is.na(hiv_test_rslt), as.Date(appointment_dte), as.Date(NA)),
    hiv_test_dte_12m_prev = hiv_test_dte - years(1)
  )

# main drug injected
table(trimws(as.character(romania_pwid_hiv$drug_type)), useNA = "ifany")

romania_pwid_hiv <- romania_pwid_hiv %>%
  mutate(
    drug_type_main = case_when(
      drug_type == "0" | is.na(drug_type) ~ "Undeclared",
      str_detect(drug_type, "\\+") | str_length(drug_type) > 1 & str_detect(drug_type, "[HLM]") ~ "Polyconsumer",
      drug_type == "1" | drug_type == "H" ~ "Heroin",
      drug_type == "2" | drug_type == "L" ~ "Legal",
      drug_type == "3" ~ "Polyconsumer",
      drug_type == "4" ~ "Other drugs",
      drug_type == "5" | drug_type == "M" ~ "Methadone",
      TRUE ~ "Other"
    )
  )

table(romania_pwid_hiv$drug_type_main, romania_pwid_hiv$drug_type, useNA = "ifany")

# sequence negative hiv tests

# create row ids
romania_pwid_hiv <- romania_pwid_hiv %>%
  arrange(id, appointment_dte) %>%
  mutate(row_id = row_number())

# subset negative tests and sequence
hiv_tests <- romania_pwid_hiv %>%
  filter(!is.na(hiv_test_rslt)) %>%
  arrange(id, appointment_dte) %>%
  group_by(id) %>%
  mutate(hiv_test_seq = row_number()) %>%
  ungroup() %>%
  dplyr::select(row_id, hiv_test_seq) 

# merge back using row_id
romania_pwid_hiv <- romania_pwid_hiv %>%
  left_join(hiv_tests, by = "row_id")

# ensure hiv_test_rslt is numeric
romania_pwid_hiv <- romania_pwid_hiv %>%
  mutate(hiv_test_rslt = as.numeric(hiv_test_rslt))

# date format appointment_dte
romania_pwid_hiv <- romania_pwid_hiv %>%
  mutate(appointment_dte = as.Date(substr(appointment_dte, 1, 10)))

# date of hiv test
romania_pwid_hiv <- romania_pwid_hiv %>%
  mutate(hiv_test_dte = dplyr::if_else(hiv_test_rslt %in% c(1, 2), appointment_dte, as.Date(NA)))

# first recorded hiv test
first_test <- baseline_analysis_hiv %>%
  summarise(hiv_test_dte = min(hiv_test_dte, na.rm = TRUE))
print(first_test)

# last recorded hiv test
last_test <- baseline_analysis_hiv %>%
  summarise(hiv_test_dte = max(hiv_test_dte, na.rm = TRUE))
print(last_test)

# sequence of negative tests
hiv_neg_test_seq <- romania_pwid_hiv %>%
  filter(hiv_test_rslt == 1) %>%
  arrange(id, appointment_dte) %>%
  group_by(id) %>%
  mutate(neg_hiv_seq = row_number()) %>%
  ungroup() %>%
  dplyr::select(row_id, neg_hiv_seq) 

# merge back using row_id
romania_pwid_hiv <- romania_pwid_hiv %>%
  left_join(hiv_neg_test_seq, by = "row_id")

# create last_hiv_test_dte
romania_pwid_hiv <- romania_pwid_hiv %>%
  group_by(id) %>%
  mutate(
    last_hiv_test_dte = max(appointment_dte, na.rm = TRUE),
    last_hiv_test_dte = replace(last_hiv_test_dte, is.infinite(last_hiv_test_dte), NA),
    last_hiv_test_dte = as.Date(last_hiv_test_dte, origin = "1970-01-01")
  ) %>%
  ungroup()

# lifetime exposure variables

# recode roma ethnicity
romania_pwid_hiv <- romania_pwid_hiv %>%
  group_by(id) %>%
  mutate(ethnic_roma_ever = ifelse(any(ethnic_roma == 1, na.rm = TRUE), 1, 0)) %>%
  ungroup() %>%
  mutate(ethnic_roma_ever = factor(ethnic_roma_ever, levels = c(0, 1)))

# recode hiv ever
romania_pwid_hiv <- romania_pwid_hiv %>%
  group_by(id) %>%
  mutate(hiv_ever = ifelse(any(hiv_test_rslt == 2, na.rm = TRUE), 1, 0)) %>%
  ungroup() %>%
  mutate(hiv_ever = factor(hiv_ever, levels = c(0, 1)))

# recode oat
romania_pwid_hiv <- romania_pwid_hiv %>%
  group_by(id) %>%
  mutate(oat_ever = ifelse(any(oat == 1, na.rm = TRUE), 1, 0)) %>%
  ungroup() %>%
  mutate(oat_ever = factor(oat_ever, levels = c(0, 1)))

# recode sex work
romania_pwid_hiv <- romania_pwid_hiv %>%
  group_by(id) %>%
  mutate(sex_work_ever = ifelse(any(sex_work_current), 1, 0)) %>%
  ungroup() %>%
  mutate(sex_work_ever = factor(sex_work_ever, levels = c(0, 1)))

# recode homelessness
romania_pwid_hiv <- romania_pwid_hiv %>%
  group_by(id) %>%
  mutate(homeless_ever = ifelse(any(homeless_current), 1, 0)) %>%
  ungroup() %>%
  mutate(homeless_ever = factor(homeless_ever, levels = c(0, 1)))

# make vars factors
romania_pwid_hiv <- romania_pwid_hiv %>%
  mutate(
    oat_current = factor(oat_ever, levels = c(0, 1)),
    sex_work_current = factor(sex_work_current, levels = c(0, 1)),
    homeless_current = factor(homeless_current, levels = c(0, 1))
  )

# tab out changing lifetime vars
current_vars <- c("oat", "oat_ever", "sex_work_current", "sex_work_ever", "homeless_current", "homeless_ever")
table_current <- CreateTableOne(vars = current_vars, data = romania_pwid_hiv)
print(table_current, showAllLevels = TRUE)

# tab out unchanging lifetime vars
vars <- c("ethnic_roma_ever", "hiv_ever")
table_roma_hiv <- CreateTableOne(vars = vars, data = romania_pwid_hiv)
print(table_roma_hiv, showAllLevels = TRUE)

# date exposures occured
romania_pwid_hiv <- romania_pwid_hiv %>%
  mutate(
    oat_dte = dplyr::if_else(oat == 1, appointment_dte, as.Date(NA)),
    sex_work_current_dte = dplyr::if_else(sex_work_current == 1, appointment_dte, as.Date(NA)),
    homeless_current_dte = dplyr::if_else(homeless_current == 1, appointment_dte, as.Date(NA))
  )

# recode other values of _ever to 1 for ids with any current exposure
romania_pwid_hiv <- romania_pwid_hiv %>%
  group_by(id) %>%
  mutate(
    oat_ever = as.integer(any(oat == 1, na.rm = TRUE)),
    sex_work_ever = as.integer(any(sex_work_current == 1, na.rm = TRUE)),
    homeless_ever = as.integer(any(homeless_current == 1, na.rm = TRUE))
  ) %>%
  ungroup()

# force onto all rows
romania_pwid_hiv <- romania_pwid_hiv %>%
  group_by(id) %>%
  mutate(
    oat_ever = max(oat_ever, na.rm = TRUE),
    homeless_ever = max(homeless_ever, na.rm = TRUE),
    sex_work_ever = max(sex_work_ever_4cat, na.rm = TRUE)
  ) %>%
  ungroup()

# QA check
romania_pwid_hiv %>%
  group_by(id) %>%
  summarise(
    oat_check = n_distinct(oat_ever),
    homeless_check = n_distinct(homeless_ever),
    hiv_check = n_distinct(hiv_ever)
  ) %>%
  summarise(across(everything(), max))

# make vars factors for table
romania_pwid_hiv <- romania_pwid_hiv %>%
  mutate(
    oat_ever = factor(oat_ever, levels = c(0, 1)),
    sex_work_ever = factor(sex_work_ever, levels = c(0, 1)),
    homeless_ever = factor(homeless_ever, levels = c(0, 1)),
    ethnic_roma_ever = factor(ethnic_roma_ever, levels = c(0, 1)),    
  )

current_vars <- c("oat", "oat_ever", "sex_work_current", "sex_work_ever", "homeless_current", "homeless_ever")
table_current <- CreateTableOne(vars = current_vars, data = romania_pwid_hiv)
print(table_current, showAllLevels = TRUE)

# distinct rows of oat
n_distinct(romania_pwid_hiv$id[romania_pwid_hiv$oat_ever == 1]) ## 1066 rows (correct)

# find first exposure date
romania_pwid_hiv <- romania_pwid_hiv %>%
  group_by(id) %>%
  mutate(
    oat_first_exposure_dte = min(oat_dte, na.rm = TRUE),
    sex_work_current_first_exposure_dte = min(sex_work_current_dte, na.rm = TRUE),
    homeless_current_first_exposure_dte = min(homeless_current_dte, na.rm = TRUE)
  ) %>%
  ungroup()

# set _ever variables to 0 if not exposed
ever_vars <- c("oat_ever", "sex_work_ever", "homeless_ever", "hiv_ever", "ethnic_roma_ever")
romania_pwid_hiv <- romania_pwid_hiv %>%
  mutate(
    across(all_of(ever_vars),
      ~ factor(ifelse(. == 1, 1, 0),
        levels = c(0, 1)
        )))

# select and order vars
romania_pwid_hiv <- romania_pwid_hiv %>%
  dplyr::select(
    id,
    oat, oat_ever,
    sex_work_current, sex_work_ever,
    homeless_current, homeless_ever,
    ethnic_roma, ethnic_roma_ever,
    everything()
  )

ever_vars <- c("oat", "oat_ever", "sex_work_current", "sex_work_ever", "homeless_current", "homeless_ever", "ethnic_roma", "ethnic_roma_ever")
table_ever <- CreateTableOne(vars = ever_vars, data = romania_pwid_hiv)
print(table_ever, showAllLevels = TRUE)

# distinct rows of oat
n_distinct(romania_pwid_hiv$id[romania_pwid_hiv$oat_ever == 1]) ## 1066 rows (correct)

# find first negative hiv test date for each id
first_neg_dates <- romania_pwid_hiv %>%
  filter(neg_hiv_seq == 1) %>%
  group_by(id) %>%
  summarise(first_hiv_neg_test_dte = min(appointment_dte, na.rm = TRUE), .groups = "drop")

# relevel sex work variable
romania_pwid_hiv <- romania_pwid_hiv %>%
  mutate(
    sex_work_ever_4cat = factor(
      case_when(
      sex_work_ever == 0 & gender == "Female" ~ 0,
      sex_work_ever == 1 & gender == "Female" ~ 1, 
      sex_work_ever == 0 & gender == "Male" ~ 2,
      sex_work_ever == 1 & gender == "Male" ~ 3
    ),
    levels = c(0, 1, 2, 3), 
    labels = c("No sex work - female", "Sex work - female", "No sex work - male", "Sex work - male")
  ))

# save overall cohort

# table of exposures
current_vars <- c("oat_ever", "homeless_ever", "sex_work_ever", "hiv_ever", "ethnic_roma_ever")
current_table <- CreateTableOne(
  vars = current_vars,
  data = romania_pwid_hiv
)
print(current_table, showAllLevels = TRUE)

# id sequence
overall_data <- romania_pwid_hiv %>%
  group_by(id) %>%
  arrange(id, appointment_dte) %>%
  mutate(id_seq = row_number()) %>%
  ungroup()

# table of exposures
current_vars <- c("oat_ever", "homeless_ever", "sex_work_ever", "hiv_ever", "ethnic_roma_ever")
current_table <- CreateTableOne(
  vars = current_vars,
  data = overall_data
)
print(current_table, showAllLevels = TRUE)

# restrict to one row per id
overall_data <- overall_data %>%
  filter(id_seq == 1)

# table of exposures
current_vars <- c("oat_ever", "homeless_ever", "sex_work_ever", "hiv_ever", "ethnic_roma_ever")
current_table <- CreateTableOne(
  vars = current_vars,
  data = overall_data
)
print(current_table, showAllLevels = TRUE)

# distinct rows of oat
n_distinct(romania_pwid_hiv$id[romania_pwid_hiv$oat_ever == 1]) ## 1066 rows (correct)

# restrict to columns of interest
overall_data <- overall_data %>%
  select(id, gender, age_4cat, ethnic_roma_ever, sex_work_ever, sex_work_ever_4cat, homeless_ever, oat_ever, drug_type_main)

# generate a table
table_vars <- c("gender", "age_4cat", "ethnic_roma_ever", "sex_work_ever_4cat", "homeless_ever", "oat_ever", "drug_type_main")

overall_table <- CreateTableOne(
  vars = table_vars,
  data = overall_data
)
print(overall_table, showAllLevels = TRUE)

# save overall data
saveRDS(overall_data, file = "overall_data.rds")

## baseline hiv cohort

# remove rows where hiv test result is missing
romania_pwid_hiv <- romania_pwid_hiv[!is.na(romania_pwid_hiv$hiv_test_rslt), ]

# distinct rows of oat
n_distinct(romania_pwid_hiv$id[romania_pwid_hiv$oat_ever == 1]) ## 1066 rows (correct)

# remove rows where hiv test result is indeterminate
romania_pwid_hiv <- romania_pwid_hiv %>%
  filter(!hiv_test_rslt == 3)

# sequence by id 
romania_pwid_hiv <- romania_pwid_hiv %>%
  group_by(id) %>%
  arrange(id, appointment_dte) %>%
  mutate(id_seq = row_number()) %>%
  ungroup(id)

# keep rows where id_seq equals 1
romania_pwid_hiv_bl <- romania_pwid_hiv %>%
  filter(id_seq == 1)

# generate a table
table_vars <- c("gender", "age_4cat", "ethnic_roma_ever", "sex_work_ever_4cat", "homeless_ever", "oat_ever", "drug_type_main")

overall_table <- CreateTableOne(
  vars = table_vars,
  data = romania_pwid_hiv_bl
)
print(overall_table, showAllLevels = TRUE)

# distinct rows of oat
n_distinct(romania_pwid_hiv_bl$id[romania_pwid_hiv_bl$oat_ever == 1]) ## 1065 rows (dropped one)

# hiv test results
table(romania_pwid_hiv_bl$hiv_test_rslt)

# save df of all participants tested for hiv
write.csv(romania_pwid_hiv_bl, "romania_pwid_hiv_bl.csv", row.names = FALSE)

# keep rows where hiv negative
romania_pwid_hiv_bl_neg <- romania_pwid_hiv_bl %>%
  filter(hiv_test_rslt == 1)

# save df of all hiv negative participants
write.csv(romania_pwid_hiv_bl_neg, "romania_pwid_hiv_bl_neg.csv", row.names = FALSE)

## longitudinal data

# identify ids positive at baseline
positive_at_baseline <- romania_pwid_hiv %>%
  filter(id_seq == 1 & hiv_test_rslt == 2) %>%
  pull(id)

# remove ids where positive at baseline
romania_pwid_hiv <- romania_pwid_hiv %>%
  filter(!(id %in% positive_at_baseline))

# sequence by id 
romania_pwid_hiv <- romania_pwid_hiv %>%
  arrange(id, appointment_dte) %>%
  group_by(id) %>%
  mutate(id_seq = row_number()) %>%
  ungroup(id)

# highest value in the id_seq column
highest_id_seq <- romania_pwid_hiv %>%
  summarise(max_id_seq = max(id_seq, na.rm = TRUE))
print(highest_id_seq)

# hiv test results by visit
romania_pwid_hiv_summary <- table(romania_pwid_hiv$appointment_seq, romania_pwid_hiv$hiv_test_rslt)
print(romania_pwid_hiv_summary) 

# sequence hiv tests by id
romania_pwid_hiv <- romania_pwid_hiv %>%
  group_by(id) %>%
  arrange(id) %>%
  mutate(hiv_test_seq = row_number())

# hiv test results by test number
romania_pwid_hiv_summary <- table(romania_pwid_hiv$hiv_test_seq, romania_pwid_hiv$hiv_test_rslt)
print(romania_pwid_hiv_summary)

# distinct rows of id
n_distinct(romania_pwid_hiv$id)

# remove participants with only one test
romania_pwid_hiv <- romania_pwid_hiv %>%
  group_by(id) %>%
  filter(!(max(hiv_test_seq, na.rm = TRUE) == 1)) %>%
  ungroup()

# participants included in longitudinal analysis
romania_pwid_hiv_long <- romania_pwid_hiv 

romania_pwid_hiv_long <- romania_pwid_hiv_long %>%
  arrange(id, appointment_dte) %>%
  group_by(id) %>%
  mutate(id_seq = row_number()) %>%
  ungroup(id)

romania_pwid_hiv_long <- romania_pwid_hiv_long %>%
  filter(id_seq == 1)

# distinct rows of id
n_distinct(romania_pwid_hiv_long$id)
count(romania_pwid_hiv_long)

# save data for supplements
write.csv(romania_pwid_hiv_long, "romania_pwid_hiv_included.csv", row.names = FALSE)

# new id sequence
romania_pwid_hiv <- romania_pwid_hiv %>%
  arrange(id) %>%
  mutate(id_seq = cumsum(!duplicated(id)))

# Find the highest value in the id_seq column
highest_id_seq <- romania_pwid_hiv %>%
  summarise(max_id_seq = max(id_seq, na.rm = TRUE))

# number of ids
cat("Highest value in id_seq:\n")
print(highest_id_seq)

# hiv test results by test number
romania_pwid_hiv_summary <- table(romania_pwid_hiv$hiv_test_seq, romania_pwid_hiv$hiv_test_rslt)
print(romania_pwid_hiv_summary) 

# create dataframe of individuals who tested after first positive
romania_pwid_hiv_after_pos <- romania_pwid_hiv %>%
  group_by(id) %>%
  mutate(
    first_hiv_positive_dte = ifelse(hiv_test_rslt == 2, appointment_dte, NA),
    first_hiv_positive_dte = if (all(is.na(first_hiv_positive_dte))) NA else min(first_hiv_positive_dte, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  filter(!is.na(first_hiv_positive_dte) & appointment_dte > first_hiv_positive_dte)

# remove hiv tests after the first positive
romania_pwid_hiv <- romania_pwid_hiv %>%
  group_by(id) %>%
  mutate(
    first_hiv_positive_dte = ifelse(hiv_test_rslt == 2, appointment_dte, NA),
    first_hiv_positive_dte = if (all(is.na(first_hiv_positive_dte))) NA else min(first_hiv_positive_dte, na.rm = TRUE)
  ) %>%
  ungroup()

# Filter rows to keep only those before or on the first positive test date
romania_pwid_hiv <- romania_pwid_hiv %>%
  filter(is.na(first_hiv_positive_dte) | appointment_dte <= first_hiv_positive_dte)

# hiv test results by visit
romania_pwid_hiv_summary <- table(romania_pwid_hiv$hiv_test_seq, romania_pwid_hiv$hiv_test_rslt)
print(romania_pwid_hiv_summary)

# appointment_dte is a date
romania_pwid_hiv <- romania_pwid_hiv %>%
  mutate(appointment_dte = as.Date(appointment_dte, format = "%Y-%m-%d"))

# sequence tests by id and date
romania_pwid_hiv <- romania_pwid_hiv %>%
  arrange(id, appointment_dte) %>%
  group_by(id) %>%
  mutate(hiv_test_seq = row_number()) %>%
  ungroup()

# create intervals
romania_pwid_hiv_test <- romania_pwid_hiv %>%
  mutate(
    appointment_dte = as.Date(appointment_dte, format = "%Y-%m-%d")
  ) %>%
  arrange(id, appointment_dte) %>%
  group_by(id) %>%
  mutate(
    appointment_dte_start = appointment_dte,
    appointment_dte_end = lead(appointment_dte),
    hiv_test_rslt_start = hiv_test_rslt,
    hiv_test_rslt_end = lead(hiv_test_rslt)
  ) %>%
  ungroup() %>%
  filter(!is.na(appointment_dte_end)) %>%
  mutate(
    days_risk = as.numeric(appointment_dte_end - appointment_dte_start),
    py = days_risk / 365.25
  ) %>%
    dplyr::select(
    id, appointment_dte_start, appointment_dte_end,
    hiv_test_rslt_start, hiv_test_rslt_end, days_risk, py,
    oat_ever, sex_work_ever_4cat, homeless_ever, ethnic_roma_ever, hiv_ever,
    gender, age_4cat,
    drug_type_main,
  ) %>%
  rename(
    appointment_dte = appointment_dte_start,
    appointment_dte_lag = appointment_dte_end,
    hiv_baseline = hiv_test_rslt_start,
    hiv_test_rslt = hiv_test_rslt_end
  )

# change test results to 0 and 1
romania_pwid_hiv_test <- romania_pwid_hiv_test %>%
  mutate(
    hiv_baseline = case_when(
      hiv_baseline == 1 ~ 0,
      hiv_baseline == 2 ~ 1,
      TRUE ~ hiv_baseline
    ),
    hiv_test_rslt = case_when(
      hiv_test_rslt == 1 ~ 0,
      hiv_test_rslt == 2 ~ 1,
      TRUE ~ hiv_test_rslt
    )
  )

# QA for rows where appointment_dte_lag is less than appointment_dte
invalid_rows <- romania_pwid_hiv_test %>%
  filter(appointment_dte_lag < appointment_dte)
cat("Number of rows where appointment_dte_lag is less than appointment_dte:", nrow(invalid_rows), "\n")

# date format 
romania_pwid_hiv_test <- romania_pwid_hiv_test %>%
  mutate(
    appointment_dte = as.Date(appointment_dte),
    appointment_dte_lag = as.Date(appointment_dte_lag)
  )

# exposure variables 
exposure_vars <- c("oat_ever", "sex_work_ever_4cat", "homeless_ever", "ethnic_roma_ever", "hiv_ever", "gender", "age_4cat", "drug_type_main")

# table of exposure variables
exposure_vars <- CreateTableOne(vars = exposure_vars, data = romania_pwid_hiv_test)

print(exposure_vars)

# check that dates are correctly lagged
View(romania_pwid_hiv_test)

# testing data
write.csv(romania_pwid_hiv_test, "romania_pwid_hiv_test.csv")

## overall incidence estimate

# incident cases
cases <- sum(romania_pwid_hiv_test$hiv_baseline == 0 & romania_pwid_hiv_test$hiv_test_rslt == 1, na.rm = TRUE)

# person-time
romania_pwid_hiv_test <- romania_pwid_hiv_test %>%
  mutate(py = as.numeric(appointment_dte_lag - appointment_dte) / 365.25)

person_time <- sum(romania_pwid_hiv_test$py, na.rm = TRUE)

# incidence per 100 PY
ir <- (cases / person_time) * 100

# 95% CI
lower <- (qchisq(0.025, 2 * cases) / 2) / person_time * 100
upper <- (qchisq(0.975, 2 * (cases + 1)) / 2) / person_time * 100

cat("hiv Incidence Rate:", round(ir, 2), "per 100 PY (95% CI:", round(lower, 2), "-", round(upper, 2), 
    "| Cases:", cases, "| Person-years:", round(person_time, 2), ")\n")

## random-point sampling with 10000 iterations approach

# seroconversion intervals
seroconversion_intervals <- romania_pwid_hiv_test %>%
  filter(hiv_baseline == 0 & hiv_test_rslt == 1)

# generate random infection dates
romania_pwid_hiv_test_iterations <- seroconversion_intervals %>%
  rowwise() %>%
  mutate(
    iteration = list(1:10000),
    random_infection_dtes = list(
      as.Date(
        runif(10000,
              min = as.numeric(appointment_dte),
              max = as.numeric(appointment_dte_lag)),
        origin = "1970-01-01"
      )
    )
  ) %>%
  unnest(c(iteration, random_infection_dtes)) %>%
  ungroup() %>%
  mutate(
    days_risk = as.numeric(random_infection_dtes - appointment_dte),
    person_years = days_risk / 365.25,
    midpoint_year = lubridate::year(random_infection_dtes),
    appointment_dte_lag = random_infection_dtes
  )

# always-negative intervals
romania_pwid_hiv_test_negatives <- romania_pwid_hiv_test %>%
  filter(hiv_test_rslt == 0) %>%
  tidyr::crossing(iteration = 1:10000) %>%
  mutate(
    imputed_infection_dte = NA,
    days_risk = as.numeric(days_risk),
    person_years = days_risk / 365.25,
    midpoint_year = NA
  )

# check for rows where appointment_dte_lag is less than appointment_dte
invalid_rows <- romania_pwid_hiv_test_iterations %>%
  filter(appointment_dte_lag < appointment_dte)
cat("rows where appointment_dte_lag is less than appointment_dte:", nrow(invalid_rows), "\n")

# split each iteration into a separate dataframe
romania_pwid_hiv_test_iterations <- romania_pwid_hiv_test_iterations %>%
  mutate(
    days_risk = as.numeric(days_risk),
    person_years = days_risk / 365.25
  )

# only one negative interval per id
romania_pwid_hiv_test_negatives <- romania_pwid_hiv_test %>%
  filter(hiv_test_rslt == 0) %>%
  tidyr::crossing(iteration = 1:10000) %>%
  mutate(
    imputed_infection_dte = NA,
    days_risk = as.numeric(days_risk),
    person_years = days_risk / 365.25,
    midpoint_year = NA
  )

split_dataframes <- split(
  bind_rows(romania_pwid_hiv_test_iterations, romania_pwid_hiv_test_negatives),
  bind_rows(romania_pwid_hiv_test_iterations, romania_pwid_hiv_test_negatives)$iteration
)

# name each dataframe in the list
names(split_dataframes) <- paste0("iteration_", seq_along(split_dataframes))

# Find duplicate IDs in the first iteration
dup_ids <- names(which(table(split_dataframes[[1]]$id) > 1))

# List of columns you want
wanted_cols <- c("id", "appointment_dte", "appointment_dte_lag", "days_risk", "person_years", "hiv_test_rslt")

# Only select columns that exist in the dataframe
available_cols <- intersect(wanted_cols, colnames(split_dataframes[[1]]))

duplicates_df <- split_dataframes[[1]] %>%
  filter(id %in% dup_ids) %>%
  dplyr::select(all_of(available_cols)) %>%
  arrange(id, appointment_dte)

print(duplicates_df)
View(duplicates_df)

## wide format dataframes for hiv incidence analysis

# list to store the results
processed_dataframes_hiv <- list()

# years to create columns
required_years <- 2013:2022

# loop 10000 iterations
for (i in 1:10000) {
  cat("Processing iteration", i, "of", 10000, "\n")
  
  # dataframe for the current iteration
  df <- split_dataframes[[i]]
  
  # person-years for each year of observation
  person_years_df <- df %>%
    rowwise() %>%
    mutate(
      start_year = year(appointment_dte),
      end_year = year(appointment_dte_lag),
      start_date = appointment_dte,
      end_date = appointment_dte_lag
    ) %>%
    do({
      data <- .
      years <- seq(data$start_year, data$end_year)
      person_years <- sapply(years, function(year) {
        start <- max(as.Date(paste0(year, "-01-01")), data$start_date)
        end <- min(as.Date(paste0(year, "-12-31")), data$end_date)
        as.numeric(difftime(end, start, units = "days")) / 365.25
      })
      names(person_years) <- years
      data.frame(t(person_years))
    }) %>%
    ungroup()
  
  # check person_years_df is empty
  if (nrow(person_years_df) == 0) {
    cat("person_years_df is empty for iteration", i, "\n")
    processed_dataframes_hiv[[i]] <- NULL
    next
  }
  
  # merge the person_years_df with the original dataframe
  df <- bind_cols(df, person_years_df)
  
  # Ensure all required columns are present
  for (year in required_years) {
    column_name <- paste0("hiv_test_", year)
    if (!(column_name %in% names(df))) {
      df[[column_name]] <- 0
    }
  }
  
  # Populate the hiv_test_20xx columns based on midpoint_year and hiv_test_rslt
  for (year in required_years) {
    column_name <- paste0("hiv_test_", year)
    df[[column_name]] <- ifelse(df$midpoint_year == year & df$hiv_test_rslt == 1, 1, df[[column_name]])
  }
  
  # person-year columns are present
  for (year in required_years) {
    if (!(as.character(year) %in% names(df))) {
      df[[as.character(year)]] <- 0
    }
  }
  
  # Store the processed dataframe in the list
  processed_dataframes_hiv[[i]] <- df
}

# create year variable
for (i in 1:length(processed_dataframes_hiv)) {
  # Get the processed dataframe for the current iteration
  df <- processed_dataframes_hiv[[i]]
  
  # year column
  df <- df %>%
    mutate(year = as.numeric(format(appointment_dte_lag, "%Y")))
  
  # factor year
  df$year <- as.factor(df$year)
  
  # store
  processed_dataframes_hiv[[i]] <- df
}

# first processed dataframe
df <- processed_dataframes_hiv[[1]]

# histogram of person_years
ggplot(df, aes(x = person_years)) +
  geom_histogram(binwidth = 0.1, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Person Years",
       x = "Person Years",
       y = "Frequency") +
  theme_minimal()

# save wide dataframes
saveRDS(processed_dataframes_hiv, file = "processed_dataframes_hiv.rds")

# function to reshape dataframes
process_dataframe <- function(df) {
  # rename year column
  if ("year" %in% colnames(df)) {
    df <- df %>%
      rename(existing_year = year)
  }
  
  # reshape the columns X2013 to X2022 to long format
  df_long <- df %>%
    pivot_longer(cols = starts_with("X"), 
                 names_to = "year", 
                 names_prefix = "X", 
                 values_to = "time_at_risk") %>%
    filter(!is.na(time_at_risk))  # Remove rows where time_at_risk is NA
  
  # recode hiv_test_rslt to 0 when it is invalid, NA, or year does not equal midpoint_year
  df_long <- df_long %>%
    mutate(hiv_test_rslt = ifelse(is.na(hiv_test_rslt) | !is.numeric(hiv_test_rslt), 0,
                                ifelse(year == midpoint_year, hiv_test_rslt, NA)))
  # keep only the specified columns
  df_long <- df_long %>%
    dplyr::select(id, hiv_test_rslt, appointment_dte, appointment_dte_lag, year, midpoint_year, time_at_risk)
  
  # sort by id and then by year
  df_long <- df_long %>%
    arrange(id, year)
  
  return(df_long)
}

# load wide dataframes
processed_dataframes_hiv <- readRDS("processed_dataframes_hiv.rds")

# list to store long dataframes
processed_dataframes_long_hiv <- list()

# loop over dataframes in processed_dataframes_hiv
for (i in 1:length(processed_dataframes_hiv)) {
  cat("Processing dataframe", i, "of", length(processed_dataframes_hiv), "\n")
  processed_dataframes_long_hiv[[i]] <- process_dataframe(processed_dataframes_hiv[[i]])
}

# save long dataframes
saveRDS(processed_dataframes_long_hiv, file = "processed_dataframes_long_hiv.rds")

