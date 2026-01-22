## load packages
pacman::p_load(tidyr, withr, lubridate, MASS, writexl, readxl, arsenal, survival, broom, ggplot2, dplyr)

## set wd
setwd("C:/Users/vl22683/OneDrive - University of Bristol/Documents/Publications/Romania PWID/data")

## Baseline prevalence and predictors of HCV infection
 
# load data
baseline_analysis_hcv <- read.csv("romania_pwid_hcv_bl.csv")

# sequence by id 
baseline_analysis_hcv <- baseline_analysis_hcv %>%
  arrange(id, appointment_dte) %>%
  group_by(id) %>%
  mutate(id_seq = row_number()) %>%
  ungroup()

# highest value in the id_seq column
highest_id_seq <- baseline_analysis_hcv %>%
  summarise(max_id_seq = max(id_seq, na.rm = TRUE))
cat("Highest value in id_seq:\n")
print(highest_id_seq)

# keep rows where appointment_seq equals 1
baseline_analysis_hcv <- baseline_analysis_hcv %>%
  filter(appointment_seq == 1)

# recode variables
baseline_analysis_hcv <- baseline_analysis_hcv %>%
  mutate(
    ethnic_roma_ever = ifelse(is.na(ethnic_roma), 0, ethnic_roma),
    gender = ifelse(gender == 2, 0, gender) 
  )

# change test results to 0 and 1
baseline_analysis_hcv <- baseline_analysis_hcv %>%
  mutate(hcv_test_rslt = case_when(
    hcv_test_rslt == 1 ~ 0,
    hcv_test_rslt == 2 ~ 1,
    TRUE ~ hcv_test_rslt
  ))

# look at results
baseline_analysis_hcv$hcv_test_rslt <- factor(baseline_analysis_hcv$hcv_test_rslt, levels = c(0, 1), labels = c("Negative", "Positive"))
table1 <- CreateTableOne(vars = "hcv_test_rslt", data = baseline_analysis_hcv)
print(table1, showAllLevels = TRUE)

# create summary table
hcv_summary_table <- baseline_analysis_hcv %>%
  mutate(
    test_year = as.character(lubridate::year(as.Date(hcv_test_dte)))
  ) %>%
  dplyr::select(
    ethnic_roma_ever,
    oat_ever,
    gender, age_4cat, test_year, hcv_test_rslt
  ) %>%
  mutate(across(
    c(
      ethnic_roma_ever,
      oat_ever,
      gender, age_4cat
    ),
    as.character
  )) %>%
  pivot_longer(
    cols = c(
      ethnic_roma_ever,
      oat_ever,
      gender, age_4cat, test_year
    ),
    names_to = "Variable",
    values_to = "Level"
  ) %>%
  group_by(Variable, Level, hcv_test_rslt) %>%
  summarise(
    Count = n(),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = hcv_test_rslt,
    values_from = Count,
    values_fill = 0
  ) %>%
  rename(
    HCV_Negative = Negative,
    HCV_Positive = Positive
  ) %>%
  mutate(
    Total = HCV_Negative + HCV_Positive,
    Proportion_Positive = (HCV_Positive / Total) * 100
  ) %>%
  group_by(Variable) %>%
  mutate(
    ref_level = case_when(
      Variable == "age_4cat" ~ "<30",
      Variable == "gender" ~ "Female",
      Variable == "test_year" ~ "2013",
      TRUE ~ "0"
    ),
    ref_pos = HCV_Positive[Level == ref_level][1],
    ref_neg = HCV_Negative[Level == ref_level][1],
    OR = ifelse(Level == ref_level, 1, (HCV_Positive / HCV_Negative) / (ref_pos / ref_neg)),
    logOR = ifelse(Level == ref_level, NA, log(OR)),
    SE_logOR = ifelse(Level == ref_level, NA, sqrt(1/HCV_Positive + 1/HCV_Negative + 1/ref_pos + 1/ref_neg)),
    CI_lower = ifelse(Level == ref_level, NA, exp(logOR - 1.96 * SE_logOR)),
    CI_upper = ifelse(Level == ref_level, NA, exp(logOR + 1.96 * SE_logOR))
  ) %>%
  ungroup() %>%
  select(-ref_level, -ref_pos, -ref_neg, -logOR, -SE_logOR)

# format frequencies and ORs
hcv_summary_table <- hcv_summary_table %>%
  mutate(
    num_perc = sprintf("%d (%.1f)", HCV_Positive, Proportion_Positive),
    or_formatted = ifelse(
      is.na(OR),
      "",
      sprintf("%.2f (%.2f-%.2f)", OR, CI_lower, CI_upper)
    )
  )

# save the summary table
write.csv(hcv_summary_table, "hcv_summary_table.csv", row.names = FALSE)

# HCV tests per year (all tests up to and including first positive per person)
hcv_all_tests <- read.csv("romania_pwid_hcv_bl.csv")

# Recode test results to 0/1
hcv_all_tests <- hcv_all_tests %>%
  mutate(hcv_test_rslt = case_when(
    hcv_test_rslt == 1 ~ 0,
    hcv_test_rslt == 2 ~ 1,
    TRUE ~ hcv_test_rslt
  ))

# Sort by id and date, then keep only tests up to and including first positive
hcv_all_tests <- hcv_all_tests %>%
  arrange(id, hcv_test_dte) %>%
  group_by(id) %>%
  mutate(
    cumulative_positive = cumsum(hcv_test_rslt),
    # Keep if: never positive yet (cumulative = 0) OR this is the first positive (cumulative = 1 and result = 1)
    keep_test = cumulative_positive == 0 | (cumulative_positive == 1 & hcv_test_rslt == 1)
  ) %>%
  filter(keep_test) %>%
  ungroup() %>%
  select(-cumulative_positive, -keep_test)

# Create year from test date and summarise
hcv_tests_by_year <- hcv_all_tests %>%
  mutate(
    hcv_test_dte = as.Date(hcv_test_dte),
    test_year = year(hcv_test_dte)
  ) %>%
  filter(!is.na(test_year)) %>%
  group_by(test_year) %>%
  summarise(
    n_tests = n(),
    n_positive = sum(hcv_test_rslt == 1, na.rm = TRUE),
    n_negative = sum(hcv_test_rslt == 0, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    prop_positive = (n_positive / n_tests) * 100,
    n_perc_positive = sprintf("%d (%.1f%%)", n_positive, prop_positive)
  )

# Add total row
hcv_tests_total <- hcv_tests_by_year %>%
  summarise(
    test_year = "Total",
    n_tests = sum(n_tests),
    n_positive = sum(n_positive),
    n_negative = sum(n_negative)
  ) %>%
  mutate(
    prop_positive = (n_positive / n_tests) * 100,
    n_perc_positive = sprintf("%d (%.1f%%)", n_positive, prop_positive)
  )

hcv_tests_by_year <- bind_rows(
  hcv_tests_by_year %>% mutate(test_year = as.character(test_year)),
  hcv_tests_total
)

print(hcv_tests_by_year)

# Save the table
write.csv(hcv_tests_by_year, "hcv_tests_by_year.csv", row.names = FALSE)

# ## differences between excluded and included in longitudinal analysis

# load data
baseline_analysis_hcv <- read.csv("romania_pwid_hcv_bl.csv")
romania_pwid_hcv_test <- read.csv("romania_pwid_hcv_test.csv", stringsAsFactors = FALSE)

# create included columns
romania_pwid_hcv_test$included2 <- "Yes"
baseline_analysis_hcv$included <- "No"

# create included dataframe
romania_pwid_hcv_test_included <- romania_pwid_hcv_test[, c("id", "included2")]
romania_pwid_hcv_test_included <- romania_pwid_hcv_test_included[!duplicated(romania_pwid_hcv_test_included$id), ]
baseline_analysis_hcv <- baseline_analysis_hcv %>%
  left_join(romania_pwid_hcv_test_included, by = "id")

# replace No includes with Yes and delete included2
baseline_analysis_hcv$included[baseline_analysis_hcv$included2 == "Yes"] <- "Yes"
baseline_analysis_hcv$included2 <- NULL

# sequence by id 
baseline_analysis_hcv <- baseline_analysis_hcv %>%
  arrange(id, appointment_dte) %>%
  group_by(id) %>%
  mutate(id_seq = row_number()) %>%
  ungroup()

# highest value in the id_seq column
highest_id_seq <- baseline_analysis_hcv %>%
  summarise(max_id_seq = max(id_seq, na.rm = TRUE))
cat("Highest value in id_seq:\n")
print(highest_id_seq)

# keep rows where appointment_seq equals 1
baseline_analysis_hcv <- baseline_analysis_hcv %>%
  filter(appointment_seq == 1)

# change test results to 0 and 1
baseline_analysis_hcv <- baseline_analysis_hcv %>%
  mutate(hcv_test_rslt = case_when(
    hcv_test_rslt == 1 ~ 0,
    hcv_test_rslt == 2 ~ 1,
    TRUE ~ hcv_test_rslt
  ))

# drop individuals who are positive at baseline
baseline_analysis_hcv <- baseline_analysis_hcv %>%
  filter(hcv_test_rslt == 0)

# recode variables
baseline_analysis_hcv <- baseline_analysis_hcv %>%
  mutate(
    sex_work_12m = ifelse(is.na(sex_work_current), 0, sex_work_current),
    msm_12m = ifelse(is.na(msm_current), 0, msm_current),
    homeless_12m = ifelse(is.na(homeless_current), 0, homeless_current),
    ethnic_roma_ever = ifelse(is.na(ethnic_roma), 0, ethnic_roma),
    gender = ifelse(gender == 2, 0, gender) 
  )

# tables of included vs. excluded
included_summary_table <- baseline_analysis_hcv %>%
  dplyr::select(
    sex_work_12m, sex_work_ever,
    homeless_12m, homeless_ever,
    ethnic_roma_ever,
    oat_12m, oat_ever,
    gender, age_4cat, included
  ) %>%
  mutate(across(
    c(
      sex_work_12m, sex_work_ever,
      homeless_12m, homeless_ever,
      ethnic_roma_ever,
      oat_12m, oat_ever,
      gender, age_4cat
    ),
    as.character
  )) %>%
  pivot_longer(
    cols = c(
      sex_work_12m, sex_work_ever,
      homeless_12m, homeless_ever,
      ethnic_roma_ever,
      oat_12m, oat_ever,
      gender, age_4cat
    ),
    names_to = "Variable",
    values_to = "Level"
  ) %>%
  group_by(Variable, Level, included) %>%
  summarise(
    Count = n(),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = included,
    values_from = Count,
    values_fill = 0
  ) %>%
  rename(
    Included_Yes = Yes,
    Included_No = No
  ) %>%
  mutate(
    Total = Included_Yes + Included_No,
    Proportion_Included = (Included_Yes / Total) * 100
  ) %>%
  group_by(Variable) %>%
  mutate(
    ref_level = case_when(
      Variable == "age_4cat" ~ "<30",
      Variable == "gender" ~ "Female",
      TRUE ~ "0"
    ),
    ref_yes = Included_Yes[Level == ref_level][1],
    ref_no = Included_No[Level == ref_level][1],
    OR = ifelse(Level == ref_level, 1, (Included_Yes / Included_No) / (ref_yes / ref_no)),
    logOR = ifelse(Level == ref_level, NA, log(OR)),
    SE_logOR = ifelse(Level == ref_level, NA, sqrt(1/Included_Yes + 1/Included_No + 1/ref_yes + 1/ref_no)),
    CI_lower = ifelse(Level == ref_level, NA, exp(logOR - 1.96 * SE_logOR)),
    CI_upper = ifelse(Level == ref_level, NA, exp(logOR + 1.96 * SE_logOR))
  ) %>%
  ungroup() %>%
  select(-ref_level, -ref_yes, -ref_no, -logOR, -SE_logOR) %>%
  mutate(
    num_perc = sprintf("%d (%.1f)", Included_Yes, Proportion_Included),
    or_formatted = ifelse(
      is.na(OR),
      "",
      sprintf("%.2f (%.2f-%.2f)", OR, CI_lower, CI_upper)
    )
  )

write.csv(included_summary_table, "hcv_included_summary_table.csv", row.names = FALSE)

# tables stratified by gender
gender_levels <- unique(baseline_analysis_hcv$gender)

included_summary_tables_by_gender <- lapply(gender_levels, function(g) {
  tab <- baseline_analysis_hcv %>%
    filter(gender == g) %>%
    dplyr::select(all_of(vars_to_summarize), included) %>%
    mutate(across(all_of(vars_to_summarize), as.character)) %>%
    pivot_longer(
      cols = all_of(vars_to_summarize),
      names_to = "Variable",
      values_to = "Level"
    ) %>%
    group_by(Variable, Level, included) %>%
    summarise(
      Count = n(),
      .groups = "drop"
    ) %>%
    pivot_wider(
      names_from = included,
      values_from = Count,
      values_fill = 0
    ) %>%
    rename(
      Included_Yes = Yes,
      Included_No = No
    ) %>%
    mutate(
      Total = Included_Yes + Included_No,
      Proportion_Included = (Included_Yes / Total) * 100
    ) %>%
    group_by(Variable) %>%
    mutate(
      ref_level = case_when(
        Variable == "age_4cat" ~ "<30",
        TRUE ~ "0"
      ),
      ref_yes = Included_Yes[Level == ref_level][1],
      ref_no = Included_No[Level == ref_level][1],
      OR = ifelse(Level == ref_level, 1, (Included_Yes / Included_No) / (ref_yes / ref_no)),
      logOR = ifelse(Level == ref_level, NA, log(OR)),
      SE_logOR = ifelse(Level == ref_level, NA, sqrt(1/Included_Yes + 1/Included_No + 1/ref_yes + 1/ref_no)),
      CI_lower = ifelse(Level == ref_level, NA, exp(logOR - 1.96 * SE_logOR)),
      CI_upper = ifelse(Level == ref_level, NA, exp(logOR + 1.96 * SE_logOR)),
      num_perc = sprintf("%d (%.1f)", Included_Yes, Proportion_Included),
      or_formatted = ifelse(
        is.na(OR), "",
        sprintf("%.2f (%.2f-%.2f)", OR, CI_lower, CI_upper)
      ),
      Gender = g
    ) %>%
    ungroup() %>%
    select(-ref_level, -ref_yes, -ref_no, -logOR, -SE_logOR)
  tab
})

for (i in seq_along(gender_levels)) {
  write.csv(
    included_summary_tables_by_gender[[i]],
    paste0("hcv_included_summary_table_gender_", gender_levels[i], ".csv"),
    row.names = FALSE
  )
}

## cox risk factor analysis

# load data
romania_pwid_hcv_test <- read.csv("romania_pwid_hcv_test.csv", stringsAsFactors = FALSE)

## cox risk factor analysis

# load data
romania_pwid_hcv_test <- read.csv("romania_pwid_hcv_test.csv", stringsAsFactors = FALSE)

# exposures
exposure_vars <- c("oat_12m", "oat_ever", "sex_work_12m", "sex_work_ever", "msm_12m", "msm_ever", "homeless_12m", "homeless_ever", "ethnic_roma_ever", "hiv_ever", "gender", "age_2cat")

results_list <- list()

# Relevel binary variables to factors with "No"/"Yes"
binary_vars <- c("oat_12m", "oat_ever", "sex_work_12m", "sex_work_ever", "msm_12m", "msm_ever", "homeless_12m", "homeless_ever", "ethnic_roma_ever", "hiv_ever")
for (var in binary_vars) {
  romania_pwid_hcv_test[[var]] <- factor(ifelse(romania_pwid_hcv_test[[var]] == 1, "Yes", "No"), levels = c("No", "Yes"))
}

for (var in exposure_vars) {
  # If variable is a factor, get levels; otherwise, use unique values
  levels_var <- if (is.factor(romania_pwid_hcv_test[[var]])) {
    levels(romania_pwid_hcv_test[[var]])
  } else {
    unique(romania_pwid_hcv_test[[var]])
  }
  
  for (lev in levels_var) {
    subset_data <- romania_pwid_hcv_test[romania_pwid_hcv_test[[var]] == lev & !is.na(romania_pwid_hcv_test[[var]]), ]
    cases <- sum(subset_data$hcv_test_rslt == 1, na.rm = TRUE)
    person_years <- sum(subset_data$py, na.rm = TRUE)
    
    # Fit Cox model for the variable (overall, not per level)
    formula <- as.formula(paste("Surv(py, hcv_test_rslt) ~", var))
    model <- coxph(formula, data = romania_pwid_hcv_test)
    hr <- exp(coef(model))
    ci <- exp(confint(model))
    
    # Only add HR/CI for the current level if it matches the coefficient name
    if (paste0(var, lev) %in% names(hr)) {
      results_list[[length(results_list) + 1]] <- data.frame(
        Variable = var,
        Level = lev,
        HR = hr[paste0(var, lev)],
        CI_lower = ci[paste0(var, lev), 1],
        CI_upper = ci[paste0(var, lev), 2],
        Cases = cases,
        Person_Years = person_years
      )
    } else {
      # For reference level (usually not shown in HR output)
      results_list[[length(results_list) + 1]] <- data.frame(
        Variable = var,
        Level = lev,
        HR = NA,
        CI_lower = NA,
        CI_upper = NA,
        Cases = cases,
        Person_Years = person_years
      )
    }
  }
}

results_df <- do.call(rbind, results_list)
write_xlsx(results_df, "cox_model_results_hcv.xlsx")

# stratified by sex
romania_pwid_hcv_test_male <- romania_pwid_hcv_test[romania_pwid_hcv_test$gender == "Male", ]
romania_pwid_hcv_test_female <- romania_pwid_hcv_test[romania_pwid_hcv_test$gender == "Female", ]
exposure_vars_strat <- setdiff(exposure_vars, "gender")

# Function to run your analysis loop, using exposure_vars_strat
run_cox_analysis <- function(data, gender_label) {
  results_list <- list()
  for (var in exposure_vars_strat) {  # Use exposure_vars_strat here!
    levels_var <- if (is.factor(data[[var]])) {
      levels(data[[var]])
    } else {
      unique(data[[var]])
    }
    for (lev in levels_var) {
      subset_data <- data[data[[var]] == lev & !is.na(data[[var]]), ]
      cases <- sum(subset_data$hcv_test_rslt == 1, na.rm = TRUE)
      person_years <- sum(subset_data$py, na.rm = TRUE)
      formula <- as.formula(paste("Surv(py, hcv_test_rslt) ~", var))
      model <- coxph(formula, data = data)
      hr <- exp(coef(model))
      ci <- exp(confint(model))
      if (paste0(var, lev) %in% names(hr)) {
        results_list[[length(results_list) + 1]] <- data.frame(
          Gender = gender_label,
          Variable = var,
          Level = lev,
          HR = hr[paste0(var, lev)],
          CI_lower = ci[paste0(var, lev), 1],
          CI_upper = ci[paste0(var, lev), 2],
          Cases = cases,
          Person_Years = person_years
        )
      } else {
        results_list[[length(results_list) + 1]] <- data.frame(
          Gender = gender_label,
          Variable = var,
          Level = lev,
          HR = NA,
          CI_lower = NA,
          CI_upper = NA,
          Cases = cases,
          Person_Years = person_years
        )
      }
    }
  }
  do.call(rbind, results_list)
}

# Now run the analysis
results_male <- run_cox_analysis(romania_pwid_hcv_test_male, "Male")
results_female <- run_cox_analysis(romania_pwid_hcv_test_female, "Female")
results_df_gender <- rbind(results_male, results_female)
write_xlsx(results_df_gender, "cox_model_results_hcv_by_gender.xlsx")

## longitudinal analysis with Rubin's correction

# load dataframes
processed_dataframes_hcv <- readRDS("processed_dataframes_hcv.rds")
processed_dataframes_long_hcv <- readRDS("processed_dataframes_long_hcv.rds")

# sequence hcv_test_rslt by id and identify any IDs with multiple positive hcv_test_rslts
multiple_positive_ids <- processed_dataframes_long_hcv[[1]] %>%
  group_by(id) %>%
  summarise(total_positive = sum(hcv_test_rslt, na.rm = TRUE)) %>%
  filter(total_positive > 1)

# print IDs with multiple positive hcv_test_rslts
print(multiple_positive_ids)

# rows with multiple positive hcv_test_rslts for verification
multiple_positive_rows <- processed_dataframes_long_hcv[[1]] %>%
  filter(id %in% multiple_positive_ids$id)

# Count incident infections (hcv_test_rslt == 1) in the first long dataframe
incident_infections <- sum(processed_dataframes_hcv[[1]]$hcv_test_rslt == 1, na.rm = TRUE)
incident_infections_long <- sum(processed_dataframes_long_hcv[[1]]$hcv_test_rslt == 1, na.rm = TRUE)
person_years_long <- sum(processed_dataframes_long_hcv[[1]]$time_at_risk, na.rm = TRUE)

cat("Number of incident infections in the first wide dataframe:", incident_infections, "\n")
cat("Number of incident infections in the first long dataframe:", incident_infections_long, 
    "| Person-years at risk:", round(person_years_long, 2), "\n")

# create 10000 dataframes of summed yearly person-years and incident cases

# list to store the summed dataframes
summed_dataframes <- list()

# loop through all 10000 processed dataframes
for (i in 1:10000) {
  cat("Processing summed dataframe for iteration", i, "of", 10000, "\n")
  
  # processed dataframe for current iteration
  df <- processed_dataframes_hcv[[i]]
  
  # check dataframe is NULL
  if (is.null(df)) {
    next
  }
  
  # sum columns
  summed_df <- df %>%
    summarise(
      hcv_test_rslt = sum(hcv_test_rslt, na.rm = TRUE),
      days_risk = sum(days_risk, na.rm = TRUE),
      person_years = sum(person_years, na.rm = TRUE),
      X2013 = sum(X2013, na.rm = TRUE),
      X2014 = sum(X2014, na.rm = TRUE),
      X2015 = sum(X2015, na.rm = TRUE),
      X2016 = sum(X2016, na.rm = TRUE),
      X2017 = sum(X2017, na.rm = TRUE),
      X2018 = sum(X2018, na.rm = TRUE),
      X2019 = sum(X2019, na.rm = TRUE),
      X2020 = sum(X2020, na.rm = TRUE),
      X2021 = sum(X2021, na.rm = TRUE),
      X2022 = sum(X2022, na.rm = TRUE),
      hcv_test_2013 = sum(hcv_test_2013, na.rm = TRUE),
      hcv_test_2014 = sum(hcv_test_2014, na.rm = TRUE),
      hcv_test_2015 = sum(hcv_test_2015, na.rm = TRUE),
      hcv_test_2016 = sum(hcv_test_2016, na.rm = TRUE),
      hcv_test_2017 = sum(hcv_test_2017, na.rm = TRUE),
      hcv_test_2018 = sum(hcv_test_2018, na.rm = TRUE),
      hcv_test_2019 = sum(hcv_test_2019, na.rm = TRUE),
      hcv_test_2020 = sum(hcv_test_2020, na.rm = TRUE),
      hcv_test_2021 = sum(hcv_test_2021, na.rm = TRUE),
      hcv_test_2022 = sum(hcv_test_2022, na.rm = TRUE)
    )
  
  # store summed dataframe in the list
  summed_dataframes[[i]] <- summed_df
}

# combine summed dataframes
final_summed_df_hcv <- bind_rows(summed_dataframes[1:10000])

# new column hcv_test_qa which sums up all the hcv_test_20xx columns
final_summed_df_hcv <- final_summed_df_hcv %>%
  mutate(hcv_test_qa = rowSums(across(starts_with("hcv_test_20")), na.rm = TRUE))

# columns for overall incidence rate and 95% confidence interval
final_summed_df_hcv <- final_summed_df_hcv %>%
  mutate(
    overall_incidence_rate = (hcv_test_rslt / person_years) * 100,
    standard_error = sqrt(hcv_test_rslt) / person_years * 100,
    lower_bound_95CI = overall_incidence_rate - (1.96 * standard_error),
    upper_bound_95CI = overall_incidence_rate + (1.96 * standard_error)
  )

final_summed_df_hcv <- final_summed_df_hcv %>%
  mutate(
    incidence_rate_2013 = hcv_test_2013 / X2013 * 100,
    incidence_rate_2014 = hcv_test_2014 / X2014 * 100,
    incidence_rate_2015 = hcv_test_2015 / X2015 * 100,
    incidence_rate_2016 = hcv_test_2016 / X2016 * 100,
    incidence_rate_2017 = hcv_test_2017 / X2017 * 100,
    incidence_rate_2018 = hcv_test_2018 / X2018 * 100,
    incidence_rate_2019 = hcv_test_2019 / X2019 * 100,
    incidence_rate_2020 = hcv_test_2020 / X2020 * 100,
    incidence_rate_2021 = hcv_test_2021 / X2021 * 100,
    incidence_rate_2022 = hcv_test_2022 / X2022 * 100
  )

# calculate mean, 2.5th percentile, and 97.5th percentile for the overall incidence rate
mean_incidence_rate <- mean(final_summed_df_hcv$overall_incidence_rate, na.rm = TRUE)
lower_bound_overall <- quantile(final_summed_df_hcv$overall_incidence_rate, 0.025, na.rm = TRUE)
upper_bound_overall <- quantile(final_summed_df_hcv$overall_incidence_rate, 0.975, na.rm = TRUE)

# calculate the mean incidence rates for each year from 2013 to 2022
yearly_means_hcv <- sapply(2013:2022, function(year) {
  if (paste0("incidence_rate_", year) %in% colnames(final_summed_df_hcv)) {
    mean(final_summed_df_hcv[[paste0("incidence_rate_", year)]], na.rm = TRUE)
  } else {
    NA
  }
})
print(yearly_means_hcv)

yearly_lower_bounds_hcv <- sapply(2013:2022, function(year) {
  if (paste0("incidence_rate_", year) %in% colnames(final_summed_df_hcv)) {
    quantile(final_summed_df_hcv[[paste0("incidence_rate_", year)]], 0.025, na.rm = TRUE)
  } else {
    NA
  }
})
print(yearly_lower_bounds_hcv)

yearly_upper_bounds_hcv <- sapply(2013:2022, function(year) {
  if (paste0("incidence_rate_", year) %in% colnames(final_summed_df_hcv)) {
    quantile(final_summed_df_hcv[[paste0("incidence_rate_", year)]], 0.975, na.rm = TRUE)
  } else {
    NA
  }
})
print(yearly_upper_bounds_hcv)

mean_hcv_infections_hcv <- sapply(2013:2022, function(year) {
  if (paste0("hcv_test_", year) %in% colnames(final_summed_df_hcv)) {
    mean(final_summed_df_hcv[[paste0("hcv_test_", year)]], na.rm = TRUE)
  } else {
    NA
  }
})
print(mean_hcv_infections_hcv)

mean_person_years_hcv <- sapply(2013:2022, function(year) {
  if (paste0("X", year) %in% colnames(final_summed_df_hcv)) {
    mean(final_summed_df_hcv[[paste0("X", year)]], na.rm = TRUE)
  } else {
    NA
  }
})
print(mean_person_years_hcv)

# overall mean number of HCV infections and person-years
overall_mean_infections_hcv <- mean(rowSums(final_summed_df_hcv[paste0("hcv_test_", 2013:2022)], na.rm = TRUE), na.rm = TRUE)
overall_mean_person_years_hcv <- mean(rowSums(final_summed_df_hcv[paste0("X", 2013:2022)], na.rm = TRUE), na.rm = TRUE)

# dataframe with the overall and yearly incidence rates and lower bounds (means)
results_df_mean_hcv <- data.frame(
  Incidence_year = c("Overall incidence rate", as.character(2013:2022)),
  Incidence_rate = c(mean_incidence_rate_hcv, yearly_means_hcv),
  Lower_bound = c(lower_bound_overall_hcv, yearly_lower_bounds_hcv),
  Upper_bound = c(upper_bound_overall_hcv, yearly_upper_bounds_hcv),
  Mean_HCV_infections = c(overall_mean_infections_hcv, mean_hcv_infections_hcv),
  Mean_person_years = c(overall_mean_person_years_hcv, mean_person_years_hcv)
)

# save overall incidence results
write.csv(results_df_mean_hcv, "overall_incidence_results_df_mean_hcv.csv", row.names = TRUE)

# list to store the summed dataframes
summed_dataframes_two_yearly_hcv <- list()

# loop all 10000 processed dataframes
for (i in 1:10000) {
  cat("Iteration", i, "of", 10000, "\n")
  
  # Get the processed dataframe for the current iteration
  df <- processed_dataframes_hcv[[i]]
  
  # Check if the dataframe is NULL
  if (is.null(df)) {
    next
  }
  
  # Sum the specified columns for two-year intervals
  summed_df_two_yearly_hcv <- df %>%
    summarise(
      hcv_test_2013_2014 = sum(hcv_test_2013, hcv_test_2014, na.rm = TRUE),
      hcv_test_2015_2016 = sum(hcv_test_2015, hcv_test_2016, na.rm = TRUE),
      hcv_test_2017_2018 = sum(hcv_test_2017, hcv_test_2018, na.rm = TRUE),
      hcv_test_2019_2020 = sum(hcv_test_2019, hcv_test_2020, na.rm = TRUE),
      hcv_test_2021_2022 = sum(hcv_test_2021, hcv_test_2022, na.rm = TRUE),
      person_years_2013_2014 = sum(X2013, X2014, na.rm = TRUE),
      person_years_2015_2016 = sum(X2015, X2016, na.rm = TRUE),
      person_years_2017_2018 = sum(X2017, X2018, na.rm = TRUE),
      person_years_2019_2020 = sum(X2019, X2020, na.rm = TRUE),
      person_years_2021_2022 = sum(X2021, X2022, na.rm = TRUE)
    )
  
  # store summed dataframe in the list
  summed_dataframes_two_yearly_hcv[[i]] <- summed_df_two_yearly_hcv
}

# combine dataframes from all iterations
final_summed_df_two_yearly_hcv <- bind_rows(summed_dataframes_two_yearly_hcv)

# add incidence rate columns for each two-year interval
final_summed_df_two_yearly_hcv <- final_summed_df_two_yearly_hcv %>%
  mutate(
    incidence_rate_2013_2014 = hcv_test_2013_2014 / person_years_2013_2014 * 100,
    incidence_rate_2015_2016 = hcv_test_2015_2016 / person_years_2015_2016 * 100,
    incidence_rate_2017_2018 = hcv_test_2017_2018 / person_years_2017_2018 * 100,
    incidence_rate_2019_2020 = hcv_test_2019_2020 / person_years_2019_2020 * 100,
    incidence_rate_2021_2022 = hcv_test_2021_2022 / person_years_2021_2022 * 100
  )

# Rubin's rules for two-year intervals
intervals <- c("2013_2014", "2015_2016", "2017_2018", "2019_2020", "2021_2022")
rubin_results <- lapply(intervals, function(interval) {
  rates <- final_summed_df_two_yearly_hcv[[paste0("incidence_rate_", interval)]]
  rates <- rates[!is.na(rates)]
  M <- length(rates)
  mean_ir <- mean(rates)
  
  # standard error for each imputation
  infections <- final_summed_df_two_yearly_hcv[[paste0("hcv_test_", interval)]]
  person_years <- final_summed_df_two_yearly_hcv[[paste0("person_years_", interval)]]
  se_vector <- sqrt(infections) / person_years * 100
  
  within_var <- mean(se_vector^2, na.rm = TRUE)
  between_var <- var(rates, na.rm = TRUE)
  total_var <- within_var + (1 + 1/M) * between_var
  se_total <- sqrt(total_var)
  ci_lower <- mean_ir - 1.96 * se_total
  ci_upper <- mean_ir + 1.96 * se_total
  data.frame(
    Interval = gsub("_", "-", interval),
    Incidence_rate = mean_ir,
    Lower_bound = ci_lower,
    Upper_bound = ci_upper,
    Mean_HCV_infections = mean(infections, na.rm = TRUE),
    Mean_person_years = mean(person_years, na.rm = TRUE)
  )
})

results_df_two_yearly_rubin_hcv <- do.call(rbind, rubin_results)

# Rubin's rule for overall incidence
overall_rates <- final_summed_df_hcv$overall_incidence_rate
overall_rates <- overall_rates[!is.na(overall_rates)]
M <- length(overall_rates)
mean_incidence_rate <- mean(overall_rates)

# standard error for each imputation
infections <- final_summed_df_hcv$hcv_test_rslt
person_years <- final_summed_df_hcv$person_years
se_vector <- sqrt(infections) / person_years * 100

within_var <- mean(se_vector^2, na.rm = TRUE)
between_var <- var(overall_rates, na.rm = TRUE)
total_var <- within_var + (1 + 1/M) * between_var
se_total <- sqrt(total_var)
lower_bound_overall <- mean_incidence_rate - 1.96 * se_total
upper_bound_overall <- mean_incidence_rate + 1.96 * se_total

# means for infections and person-years
overall_mean_infections_hcv <- mean(infections, na.rm = TRUE)
overall_mean_person_years_hcv <- mean(person_years, na.rm = TRUE)

# row for overall incidence
overall_row <- data.frame(
  Interval = "Overall",
  Incidence_rate = mean_incidence_rate,
  Lower_bound = lower_bound_overall,
  Upper_bound = upper_bound_overall,
  Mean_HCV_infections = overall_mean_infections_hcv,
  Mean_person_years = overall_mean_person_years_hcv
)

# combine with existing dataframe
results_df_two_yearly_rubin_hcv <- rbind(overall_row, results_df_two_yearly_rubin_hcv)

# negative incidence rates or bounds to 0.001
results_df_two_yearly_rubin_hcv <- results_df_two_yearly_rubin_hcv %>%
  mutate(
    Incidence_rate = ifelse(Incidence_rate < 0, 0.001, Incidence_rate),
    Lower_bound = ifelse(Lower_bound < 0, 0.001, Lower_bound),
    Upper_bound = ifelse(Upper_bound < 0, 0.001, Upper_bound)
  )

print(results_df_two_yearly_rubin_hcv)

# save two-year interval results
write.csv(results_df_two_yearly_rubin_hcv, "results_df_two_yearly_rubin_hcv.csv", row.names = FALSE)

## rate ratios for trends in incidence

# load two-yearly interval data
results_df_two_yearly_rubin_hcv <- read.csv("results_df_two_yearly_rubin_hcv.csv", stringsAsFactors = FALSE)

# numeric columns
results_df_two_yearly_rubin_hcv$Mean_HCV_infections <- as.numeric(results_df_two_yearly_rubin_hcv$Mean_HCV_infections)
results_df_two_yearly_rubin_hcv$Mean_person_years <- as.numeric(results_df_two_yearly_rubin_hcv$Mean_person_years)

# assign 2017-2018 as reference category
ref_idx <- which(results_df_two_yearly_rubin_hcv$Interval == "2017-2018")
ref_cases <- cases[ref_idx]
ref_py <- py[ref_idx]

# rate ratio using Poisson approximation
calculate_rr <- function(c1, py1, c2, py2) {
  if (c1 == 0 || py1 == 0 || c2 == 0 || py2 == 0) {
    return(c(NA, NA, NA))
  }
  rr <- (c1 / py1) / (c2 / py2)
  se_log_rr <- sqrt(1 / c1 + 1 / c2)
  lower <- exp(log(rr) - 1.96 * se_log_rr)
  upper <- exp(log(rr) + 1.96 * se_log_rr)
  c(rr, lower, upper)
}

# dataframe
rr_exact <- data.frame(
  Interval = results_df_two_yearly_rubin_hcv$Interval,
  Rate_Ratio = NA,
  Lower_95CI = NA,
  Upper_95CI = NA
)

# loop through intervals
for (i in seq_along(cases)) {
  if (i == ref_idx) {
    rr_exact[i, 2:4] <- c(1, 1, 1)
  } else {
    rr_exact[i, 2:4] <- calculate_rr(cases[i], py[i], ref_cases, ref_py)
  }
}

# combine results
results_df_two_yearly_rubin_hcv <- cbind(results_df_two_yearly_rubin_hcv, rr_exact[, -1])

# save two-year interval results
write.csv(results_df_two_yearly_rubin_hcv, "results_df_two_yearly_rubin_hcv_rr.csv", row.names = FALSE)

# load two-year interval results
results_df_two_yearly_rubin_hcv <- read.csv("results_df_two_yearly_rubin_hcv.csv", stringsAsFactors = FALSE)

# hcv incidence over time
hcv_incidence_plot_rubin <- ggplot(results_df_two_yearly_rubin_hcv, aes(x = Interval, y = Incidence_rate)) +
  geom_line(group = 1, color = "gray") + 
  geom_point(shape = 18, size = 3, color = "gray") + 
  geom_errorbar(aes(ymin = Lower_bound, ymax = Upper_bound), width = 0.2, color = "black") +
  theme_minimal(base_size = 14) +
  labs(
    x = "Two-yearly Interval",
    y = "Mean Incidence Rate per 100 Person-Years"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

ggsave("plots/hcv_incidence_plot_rubin.png", plot = hcv_incidence_plot_rubin, width = 10, height = 6, dpi = 300)
