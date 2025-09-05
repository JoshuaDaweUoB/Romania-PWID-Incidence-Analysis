## load packages
pacman::p_load(tidyr, withr, lubridate, MASS, writexl, readxl, arsenal, epitools, survival, broom, ggplot2, dplyr)

## set wd
setwd("C:/Users/vl22683/OneDrive - University of Bristol/Documents/Publications/Romania PWID/data")

# ## Baseline prevalence and predictors of hiv infection

# load data
baseline_analysis_hiv <- read.csv("romania_pwid_hiv_bl.csv")

# sequence by id 
baseline_analysis_hiv <- baseline_analysis_hiv %>%
  arrange(id, appointment_dte) %>%
  group_by(id) %>%
  mutate(id_seq = row_number()) %>%
  ungroup()

# highest value in the id_seq column
highest_id_seq <- baseline_analysis_hiv %>%
  summarise(max_id_seq = max(id_seq, na.rm = TRUE))
cat("Highest value in id_seq:\n")
print(highest_id_seq)

# keep rows where appointment_seq equals 1
baseline_analysis_hiv <- baseline_analysis_hiv %>%
  filter(appointment_seq == 1)

# recode variables
baseline_analysis_hiv <- baseline_analysis_hiv %>%
  mutate(
    sex_work_12m = ifelse(is.na(sex_work_current), 0, sex_work_current),
    msm_12m = ifelse(is.na(msm_current), 0, msm_current),
    homeless_12m = ifelse(is.na(homeless_current), 0, homeless_current),
    ethnic_roma_ever = ifelse(is.na(ethnic_roma), 0, ethnic_roma),
    gender = ifelse(gender == 2, 0, gender) 
  )

# change test results to 0 and 1
baseline_analysis_hiv <- baseline_analysis_hiv %>%
  mutate(hiv_test_rslt = case_when(
    hiv_test_rslt == 1 ~ 0,
    hiv_test_rslt == 2 ~ 1,
    TRUE ~ hiv_test_rslt
  ))

# look at results
baseline_analysis_hiv$hiv_test_rslt <- factor(baseline_analysis_hiv$hiv_test_rslt, levels = c(0, 1), labels = c("Negative", "Positive"))
table1 <- CreateTableOne(vars = "hiv_test_rslt", data = baseline_analysis_hiv)
print(table1, showAllLevels = TRUE)

# create summary table
hiv_summary_table <- baseline_analysis_hiv %>%
  dplyr::select(
    sex_work_12m, sex_work_ever,
    homeless_12m, homeless_ever,
    ethnic_roma_ever,
    oat_12m, oat_ever,
    gender, age_4cat, hiv_test_rslt
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
  group_by(Variable, Level, hiv_test_rslt) %>%
  summarise(
    Count = n(),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = hiv_test_rslt,
    values_from = Count,
    values_fill = 0
  ) %>%
  rename(
    hiv_Negative = Negative,
    hiv_Positive = Positive
  ) %>%
  mutate(
    Total = hiv_Negative + hiv_Positive,
    Proportion_Positive = (hiv_Positive / Total) * 100
  ) %>%
  group_by(Variable) %>%
  mutate(
    ref_level = case_when(
      Variable == "age_4cat" ~ "<30",
      Variable == "gender" ~ "Female",
      TRUE ~ "0"
    ),
    ref_pos = hiv_Positive[Level == ref_level][1],
    ref_neg = hiv_Negative[Level == ref_level][1],
    OR = ifelse(Level == ref_level, 1, (hiv_Positive / hiv_Negative) / (ref_pos / ref_neg)),
    logOR = ifelse(Level == ref_level, NA, log(OR)),
    SE_logOR = ifelse(Level == ref_level, NA, sqrt(1/hiv_Positive + 1/hiv_Negative + 1/ref_pos + 1/ref_neg)),
    CI_lower = ifelse(Level == ref_level, NA, exp(logOR - 1.96 * SE_logOR)),
    CI_upper = ifelse(Level == ref_level, NA, exp(logOR + 1.96 * SE_logOR))
  ) %>%
  ungroup() %>%
  select(-ref_level, -ref_pos, -ref_neg, -logOR, -SE_logOR)

# format frequencies and ORs
hiv_summary_table <- hiv_summary_table %>%
  mutate(
    num_perc = sprintf("%d (%.1f)", hiv_Positive, Proportion_Positive),
    or_formatted = ifelse(
      is.na(OR),
      "",
      sprintf("%.2f (%.2f-%.2f)", OR, CI_lower, CI_upper)
    )
  )

# save the summary table
write.csv(hiv_summary_table, "hiv_summary_table.csv", row.names = FALSE)

# tables stratified by gender

# list of variables for tables
vars_to_summarize <- c(
  "sex_work_12m", "sex_work_ever",
  "homeless_12m", "homeless_ever",
  "ethnic_roma_ever",
  "oat_12m", "oat_ever",
  "age_4cat"
)

# loop over gender and create tables
gender_levels <- unique(baseline_analysis_hiv$gender)

hiv_summary_tables_by_gender <- lapply(gender_levels, function(g) {
  tab <- baseline_analysis_hiv %>%
    filter(gender == g) %>%
    dplyr::select(all_of(vars_to_summarize), hiv_test_rslt) %>%
    mutate(across(all_of(vars_to_summarize), as.character)) %>%
    pivot_longer(
      cols = all_of(vars_to_summarize),
      names_to = "Variable",
      values_to = "Level"
    ) %>%
    group_by(Variable, Level, hiv_test_rslt) %>%
    summarise(
      Count = n(),
      .groups = "drop"
    ) %>%
    pivot_wider(
      names_from = hiv_test_rslt,
      values_from = Count,
      values_fill = 0
    ) %>%
    rename(
      hiv_Negative = Negative,
      hiv_Positive = Positive
    ) %>%
    mutate(
      Total = hiv_Negative + hiv_Positive,
      Proportion_Positive = (hiv_Positive / Total) * 100
    ) %>%
    group_by(Variable) %>%
    mutate(
      ref_level = case_when(
        Variable == "age_4cat" ~ "<30",
        TRUE ~ "0"
      ),
      ref_pos = hiv_Positive[Level == ref_level][1],
      ref_neg = hiv_Negative[Level == ref_level][1],
      OR = ifelse(Level == ref_level, 1, (hiv_Positive / hiv_Negative) / (ref_pos / ref_neg)),
      logOR = ifelse(Level == ref_level, NA, log(OR)),
      SE_logOR = ifelse(Level == ref_level, NA, sqrt(1/hiv_Positive + 1/hiv_Negative + 1/ref_pos + 1/ref_neg)),
      CI_lower = ifelse(Level == ref_level, NA, exp(logOR - 1.96 * SE_logOR)),
      CI_upper = ifelse(Level == ref_level, NA, exp(logOR + 1.96 * SE_logOR)),
      num_perc = sprintf("%d (%.1f)", hiv_Positive, Proportion_Positive),
      or_formatted = ifelse(
        is.na(OR), "",
        sprintf("%.2f (%.2f-%.2f)", OR, CI_lower, CI_upper)
      ),
      Gender = g
    ) %>%
    ungroup() %>%
    select(-ref_level, -ref_pos, -ref_neg, -logOR, -SE_logOR)
  tab
})

for (i in seq_along(gender_levels)) {
  write.csv(
    hiv_summary_tables_by_gender[[i]],
    paste0("hiv_summary_table_gender_", gender_levels[i], ".csv"),
    row.names = FALSE
  )
}

# ## differences between excluded and included in longitudinal analysis

# load data
baseline_analysis_hiv <- read.csv("romania_pwid_hiv_bl.csv")
romania_pwid_hiv_test <- read.csv("romania_pwid_hiv_test.csv", stringsAsFactors = FALSE)

# create included columns
romania_pwid_hiv_test$included2 <- "Yes"
baseline_analysis_hiv$included <- "No"

# create included dataframe
romania_pwid_hiv_test_included <- romania_pwid_hiv_test[, c("id", "included2")]
romania_pwid_hiv_test_included <- romania_pwid_hiv_test_included[!duplicated(romania_pwid_hiv_test_included$id), ]
baseline_analysis_hiv <- baseline_analysis_hiv %>%
  left_join(romania_pwid_hiv_test_included, by = "id")

# replace No includes with Yes and delete included2
baseline_analysis_hiv$included[baseline_analysis_hiv$included2 == "Yes"] <- "Yes"
baseline_analysis_hiv$included2 <- NULL

# sequence by id 
baseline_analysis_hiv <- baseline_analysis_hiv %>%
  arrange(id, appointment_dte) %>%
  group_by(id) %>%
  mutate(id_seq = row_number()) %>%
  ungroup()

# highest value in the id_seq column
highest_id_seq <- baseline_analysis_hiv %>%
  summarise(max_id_seq = max(id_seq, na.rm = TRUE))
cat("Highest value in id_seq:\n")
print(highest_id_seq)

# keep rows where appointment_seq equals 1
baseline_analysis_hiv <- baseline_analysis_hiv %>%
  filter(appointment_seq == 1)
View(baseline_analysis_hiv)

# change test results to 0 and 1
baseline_analysis_hiv <- baseline_analysis_hiv %>%
  mutate(hiv_test_rslt = case_when(
    hiv_test_rslt == 1 ~ 0,
    hiv_test_rslt == 2 ~ 1,
    TRUE ~ hiv_test_rslt
  ))

# drop individuals who are positive at baseline
baseline_analysis_hiv <- baseline_analysis_hiv %>%
  filter(hiv_test_rslt == 0)

# recode variables
baseline_analysis_hiv <- baseline_analysis_hiv %>%
  mutate(
    sex_work_12m = ifelse(is.na(sex_work_current), 0, sex_work_current),
    msm_12m = ifelse(is.na(msm_current), 0, msm_current),
    homeless_12m = ifelse(is.na(homeless_current), 0, homeless_current),
    ethnic_roma_ever = ifelse(is.na(ethnic_roma), 0, ethnic_roma),
    gender = ifelse(gender == 2, 0, gender) 
  )

# tables of included vs. excluded
included_summary_table <- baseline_analysis_hiv %>%
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

write.csv(included_summary_table, "hiv_included_summary_table.csv", row.names = FALSE)

# tables stratified by gender
gender_levels <- unique(baseline_analysis_hiv$gender)

included_summary_tables_by_gender <- lapply(gender_levels, function(g) {
  tab <- baseline_analysis_hiv %>%
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
    paste0("hiv_included_summary_table_gender_", gender_levels[i], ".csv"),
    row.names = FALSE
  )
}

## cox risk factor analysis

# load data
romania_pwid_hiv_test <- read.csv("romania_pwid_hiv_test.csv", stringsAsFactors = FALSE)

# exposures
exposure_vars <- c("oat_12m", "oat_ever", "sex_work_12m", "sex_work_ever", "msm_12m", "msm_ever", "homeless_12m", "homeless_ever", "ethnic_roma_ever", "hiv_ever", "gender", "age_2cat")

results_list <- list()

# Relevel binary variables to factors with "No"/"Yes"
binary_vars <- c("oat_12m", "oat_ever", "sex_work_12m", "sex_work_ever", "msm_12m", "msm_ever", "homeless_12m", "homeless_ever", "ethnic_roma_ever", "hiv_ever")
for (var in binary_vars) {
  romania_pwid_hiv_test[[var]] <- factor(ifelse(romania_pwid_hiv_test[[var]] == 1, "Yes", "No"), levels = c("No", "Yes"))
}

for (var in exposure_vars) {
  # If variable is a factor, get levels; otherwise, use unique values
  levels_var <- if (is.factor(romania_pwid_hiv_test[[var]])) {
    levels(romania_pwid_hiv_test[[var]])
  } else {
    unique(romania_pwid_hiv_test[[var]])
  }
  
  for (lev in levels_var) {
    subset_data <- romania_pwid_hiv_test[romania_pwid_hiv_test[[var]] == lev & !is.na(romania_pwid_hiv_test[[var]]), ]
    cases <- sum(subset_data$hiv_test_rslt == 1, na.rm = TRUE)
    person_years <- sum(subset_data$py, na.rm = TRUE)
    
    # Fit Cox model for the variable (overall, not per level)
    formula <- as.formula(paste("Surv(py, hiv_test_rslt) ~", var))
    model <- coxph(formula, data = romania_pwid_hiv_test)
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
write_xlsx(results_df, "cox_model_results_hiv.xlsx")

# stratified by sex
romania_pwid_hiv_test_male <- romania_pwid_hiv_test[romania_pwid_hiv_test$gender == "Male", ]
romania_pwid_hiv_test_female <- romania_pwid_hiv_test[romania_pwid_hiv_test$gender == "Female", ]
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
      cases <- sum(subset_data$hiv_test_rslt == 1, na.rm = TRUE)
      person_years <- sum(subset_data$py, na.rm = TRUE)
      formula <- as.formula(paste("Surv(py, hiv_test_rslt) ~", var))
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
results_male <- run_cox_analysis(romania_pwid_hiv_test_male, "Male")
results_female <- run_cox_analysis(romania_pwid_hiv_test_female, "Female")
results_df_gender <- rbind(results_male, results_female)
write_xlsx(results_df_gender, "cox_model_results_hiv_by_gender.xlsx")


## longitudinal analysis with Rubin's correction

# sequence hiv_test_rslt by id and identify any IDs with multiple positive hiv_test_rslts
multiple_positive_ids <- processed_dataframes_long_hiv[[1]] %>%
  group_by(id) %>%
  summarise(total_positive = sum(hiv_test_rslt, na.rm = TRUE)) %>%
  filter(total_positive > 1)

# print IDs with multiple positive hiv_test_rslts
print(multiple_positive_ids)

# rows with multiple positive hiv_test_rslts for verification
multiple_positive_rows <- processed_dataframes_long_hiv[[1]] %>%
  filter(id %in% multiple_positive_ids$id)

# load dataframes
processed_dataframes_hiv <- readRDS("processed_dataframes_hiv.rds")
processed_dataframes_long_hiv <- readRDS("processed_dataframes_long_hiv.rds")

# Count incident infections (hiv_test_rslt == 1) in the first long dataframe
incident_infections <- sum(processed_dataframes_hiv[[1]]$hiv_test_rslt == 1, na.rm = TRUE)
incident_infections_long <- sum(processed_dataframes_long_hiv[[1]]$hiv_test_rslt == 1, na.rm = TRUE)
person_years_long <- sum(processed_dataframes_long_hiv[[1]]$time_at_risk, na.rm = TRUE)

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
  df <- processed_dataframes_hiv[[i]]
  
  # check dataframe is NULL
  if (is.null(df)) {
    next
  }
  
  # sum columns
  summed_df <- df %>%
    summarise(
      hiv_test_rslt = sum(hiv_test_rslt, na.rm = TRUE),
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
      hiv_test_2013 = sum(hiv_test_2013, na.rm = TRUE),
      hiv_test_2014 = sum(hiv_test_2014, na.rm = TRUE),
      hiv_test_2015 = sum(hiv_test_2015, na.rm = TRUE),
      hiv_test_2016 = sum(hiv_test_2016, na.rm = TRUE),
      hiv_test_2017 = sum(hiv_test_2017, na.rm = TRUE),
      hiv_test_2018 = sum(hiv_test_2018, na.rm = TRUE),
      hiv_test_2019 = sum(hiv_test_2019, na.rm = TRUE),
      hiv_test_2020 = sum(hiv_test_2020, na.rm = TRUE),
      hiv_test_2021 = sum(hiv_test_2021, na.rm = TRUE),
      hiv_test_2022 = sum(hiv_test_2022, na.rm = TRUE)
    )
  
  # store summed dataframe in the list
  summed_dataframes[[i]] <- summed_df
}

# combine summed dataframes
final_summed_df_hiv <- bind_rows(summed_dataframes[1:10000])

# new column hiv_test_qa which sums up all the hiv_test_20xx columns
final_summed_df_hiv <- final_summed_df_hiv %>%
  mutate(hiv_test_qa = rowSums(across(starts_with("hiv_test_20")), na.rm = TRUE))

# columns for overall incidence rate and 95% confidence interval
final_summed_df_hiv <- final_summed_df_hiv %>%
  mutate(
    overall_incidence_rate = (hiv_test_rslt / person_years) * 100,
    standard_error = sqrt(hiv_test_rslt) / person_years * 100,
    lower_bound_95CI = overall_incidence_rate - (1.96 * standard_error),
    upper_bound_95CI = overall_incidence_rate + (1.96 * standard_error)
  )

final_summed_df_hiv <- final_summed_df_hiv %>%
  mutate(
    incidence_rate_2013 = hiv_test_2013 / X2013 * 100,
    incidence_rate_2014 = hiv_test_2014 / X2014 * 100,
    incidence_rate_2015 = hiv_test_2015 / X2015 * 100,
    incidence_rate_2016 = hiv_test_2016 / X2016 * 100,
    incidence_rate_2017 = hiv_test_2017 / X2017 * 100,
    incidence_rate_2018 = hiv_test_2018 / X2018 * 100,
    incidence_rate_2019 = hiv_test_2019 / X2019 * 100,
    incidence_rate_2020 = hiv_test_2020 / X2020 * 100,
    incidence_rate_2021 = hiv_test_2021 / X2021 * 100,
    incidence_rate_2022 = hiv_test_2022 / X2022 * 100
  )

# calculate mean, 2.5th percentile, and 97.5th percentile for the overall incidence rate
mean_incidence_rate <- mean(final_summed_df_hiv$overall_incidence_rate, na.rm = TRUE)
lower_bound_overall <- quantile(final_summed_df_hiv$overall_incidence_rate, 0.025, na.rm = TRUE)
upper_bound_overall <- quantile(final_summed_df_hiv$overall_incidence_rate, 0.975, na.rm = TRUE)

# calculate the mean incidence rates for each year from 2013 to 2022
yearly_means_hiv <- sapply(2013:2022, function(year) {
  if (paste0("incidence_rate_", year) %in% colnames(final_summed_df_hiv)) {
    mean(final_summed_df_hiv[[paste0("incidence_rate_", year)]], na.rm = TRUE)
  } else {
    NA
  }
})
print(yearly_means_hiv)

yearly_lower_bounds_hiv <- sapply(2013:2022, function(year) {
  if (paste0("incidence_rate_", year) %in% colnames(final_summed_df_hiv)) {
    quantile(final_summed_df_hiv[[paste0("incidence_rate_", year)]], 0.025, na.rm = TRUE)
  } else {
    NA
  }
})
print(yearly_lower_bounds_hiv)

yearly_upper_bounds_hiv <- sapply(2013:2022, function(year) {
  if (paste0("incidence_rate_", year) %in% colnames(final_summed_df_hiv)) {
    quantile(final_summed_df_hiv[[paste0("incidence_rate_", year)]], 0.975, na.rm = TRUE)
  } else {
    NA
  }
})
print(yearly_upper_bounds_hiv)

mean_hiv_infections_hiv <- sapply(2013:2022, function(year) {
  if (paste0("hiv_test_", year) %in% colnames(final_summed_df_hiv)) {
    mean(final_summed_df_hiv[[paste0("hiv_test_", year)]], na.rm = TRUE)
  } else {
    NA
  }
})
print(mean_hiv_infections_hiv)

mean_person_years_hiv <- sapply(2013:2022, function(year) {
  if (paste0("X", year) %in% colnames(final_summed_df_hiv)) {
    mean(final_summed_df_hiv[[paste0("X", year)]], na.rm = TRUE)
  } else {
    NA
  }
})
print(mean_person_years_hiv)

# overall mean number of hiv infections and person-years
overall_mean_infections_hiv <- mean(rowSums(final_summed_df_hiv[paste0("hiv_test_", 2013:2022)], na.rm = TRUE), na.rm = TRUE)
overall_mean_person_years_hiv <- mean(rowSums(final_summed_df_hiv[paste0("X", 2013:2022)], na.rm = TRUE), na.rm = TRUE)

# dataframe with the overall and yearly incidence rates and lower bounds (means)
results_df_mean_hiv <- data.frame(
  Incidence_year = c("Overall incidence rate", as.character(2013:2022)),
  Incidence_rate = c(mean_incidence_rate_hiv, yearly_means_hiv),
  Lower_bound = c(lower_bound_overall_hiv, yearly_lower_bounds_hiv),
  Upper_bound = c(upper_bound_overall_hiv, yearly_upper_bounds_hiv),
  Mean_hiv_infections = c(overall_mean_infections_hiv, mean_hiv_infections_hiv),
  Mean_person_years = c(overall_mean_person_years_hiv, mean_person_years_hiv)
)

# save overall incidence results
write.csv(results_df_mean_hiv, "overall_incidence_results_df_mean_hiv.csv", row.names = TRUE)

# list to store the summed dataframes
summed_dataframes_two_yearly_hiv <- list()

# loop all 10000 processed dataframes
for (i in 1:10000) {
  cat("Iteration", i, "of", 10000, "\n")
  
  # Get the processed dataframe for the current iteration
  df <- processed_dataframes_hiv[[i]]
  
  # Check if the dataframe is NULL
  if (is.null(df)) {
    next
  }
  
  # Sum the specified columns for two-year intervals
  summed_df_two_yearly_hiv <- df %>%
    summarise(
      hiv_test_2013_2014 = sum(hiv_test_2013, hiv_test_2014, na.rm = TRUE),
      hiv_test_2015_2016 = sum(hiv_test_2015, hiv_test_2016, na.rm = TRUE),
      hiv_test_2017_2018 = sum(hiv_test_2017, hiv_test_2018, na.rm = TRUE),
      hiv_test_2019_2020 = sum(hiv_test_2019, hiv_test_2020, na.rm = TRUE),
      hiv_test_2021_2022 = sum(hiv_test_2021, hiv_test_2022, na.rm = TRUE),
      person_years_2013_2014 = sum(X2013, X2014, na.rm = TRUE),
      person_years_2015_2016 = sum(X2015, X2016, na.rm = TRUE),
      person_years_2017_2018 = sum(X2017, X2018, na.rm = TRUE),
      person_years_2019_2020 = sum(X2019, X2020, na.rm = TRUE),
      person_years_2021_2022 = sum(X2021, X2022, na.rm = TRUE)
    )
  
  # store summed dataframe in the list
  summed_dataframes_two_yearly_hiv[[i]] <- summed_df_two_yearly_hiv
}

# combine dataframes from all iterations
final_summed_df_two_yearly_hiv <- bind_rows(summed_dataframes_two_yearly_hiv)

# add incidence rate columns for each two-year interval
final_summed_df_two_yearly_hiv <- final_summed_df_two_yearly_hiv %>%
  mutate(
    incidence_rate_2013_2014 = hiv_test_2013_2014 / person_years_2013_2014 * 100,
    incidence_rate_2015_2016 = hiv_test_2015_2016 / person_years_2015_2016 * 100,
    incidence_rate_2017_2018 = hiv_test_2017_2018 / person_years_2017_2018 * 100,
    incidence_rate_2019_2020 = hiv_test_2019_2020 / person_years_2019_2020 * 100,
    incidence_rate_2021_2022 = hiv_test_2021_2022 / person_years_2021_2022 * 100
  )

# Rubin's rules for two-year intervals
intervals <- c("2013_2014", "2015_2016", "2017_2018", "2019_2020", "2021_2022")
rubin_results <- lapply(intervals, function(interval) {
  rates <- final_summed_df_two_yearly_hiv[[paste0("incidence_rate_", interval)]]
  rates <- rates[!is.na(rates)]
  M <- length(rates)
  mean_ir <- mean(rates)
  
  # standard error for each imputation
  infections <- final_summed_df_two_yearly_hiv[[paste0("hiv_test_", interval)]]
  person_years <- final_summed_df_two_yearly_hiv[[paste0("person_years_", interval)]]
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
    Mean_hiv_infections = mean(infections, na.rm = TRUE),
    Mean_person_years = mean(person_years, na.rm = TRUE)
  )
})

results_df_two_yearly_rubin_hiv <- do.call(rbind, rubin_results)
print(results_df_two_yearly_rubin_hiv)

# Rubin's rule for overall incidence
overall_rates <- final_summed_df_hiv$overall_incidence_rate
overall_rates <- overall_rates[!is.na(overall_rates)]
M <- length(overall_rates)
mean_incidence_rate <- mean(overall_rates)

# standard error for each imputation
infections <- final_summed_df_hiv$hiv_test_rslt
person_years <- final_summed_df_hiv$person_years
se_vector <- sqrt(infections) / person_years * 100

within_var <- mean(se_vector^2, na.rm = TRUE)
between_var <- var(overall_rates, na.rm = TRUE)
total_var <- within_var + (1 + 1/M) * between_var
se_total <- sqrt(total_var)
lower_bound_overall <- mean_incidence_rate - 1.96 * se_total
upper_bound_overall <- mean_incidence_rate + 1.96 * se_total

# means for infections and person-years
overall_mean_infections_hiv <- mean(infections, na.rm = TRUE)
overall_mean_person_years_hiv <- mean(person_years, na.rm = TRUE)

# row for overall incidence
overall_row <- data.frame(
  Interval = "Overall",
  Incidence_rate = mean_incidence_rate,
  Lower_bound = lower_bound_overall,
  Upper_bound = upper_bound_overall,
  Mean_hiv_infections = overall_mean_infections_hiv,
  Mean_person_years = overall_mean_person_years_hiv
)

# combine with dataframe
results_df_two_yearly_rubin_hiv <- rbind(overall_row, results_df_two_yearly_rubin_hiv)

# negative incidence rates or bounds to 0.001
results_df_two_yearly_rubin_hiv <- results_df_two_yearly_rubin_hiv %>%
  mutate(
    Incidence_rate = ifelse(Incidence_rate < 0, 0.001, Incidence_rate),
    Lower_bound = ifelse(Lower_bound < 0, 0.001, Lower_bound),
    Upper_bound = ifelse(Upper_bound < 0, 0.001, Upper_bound)
  )

# save two-year interval results
write.csv(results_df_two_yearly_rubin_hiv, "results_df_two_yearly_rubin_hiv.csv", row.names = FALSE)

## rate ratios for trends in HIV incidence

# load two-yearly interval data
results_df_two_yearly_rubin_hiv <- read.csv("results_df_two_yearly_rubin_hiv.csv", stringsAsFactors = FALSE)
View(results_df_two_yearly_rubin_hiv)

# convert to numeric
results_df_two_yearly_rubin_hiv$Mean_hiv_infections <- as.numeric(results_df_two_yearly_rubin_hiv$Mean_hiv_infections)
results_df_two_yearly_rubin_hiv$Mean_person_years <- as.numeric(results_df_two_yearly_rubin_hiv$Mean_person_years)

# Set 2013–2014 as reference
ref_idx <- which(results_df_two_yearly_rubin_hiv$Interval == "2013-2014")
ref_cases <- cases[ref_idx]
ref_py <- py[ref_idx]

# rate ratio function using Poisson approximation
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

# results df
rr_exact <- data.frame(
  Interval = results_df_two_yearly_rubin_hiv$Interval,
  Rate_Ratio = NA,
  Lower_95CI = NA,
  Upper_95CI = NA
)

# rate ratios
for (i in seq_along(cases)) {
  if (i == ref_idx) {
    rr_exact[i, 2:4] <- c(1, 1, 1)
  } else {
    rr_exact[i, 2:4] <- calculate_rr(cases[i], py[i], ref_cases, ref_py)
  }
}

# combine results
results_df_two_yearly_rubin_hiv <- cbind(results_df_two_yearly_rubin_hiv, rr_exact[, -1])

# save rate ratios
write.csv(results_df_two_yearly_rubin_hiv, "results_df_two_yearly_rubin_hiv_rr.csv", row.names = FALSE)

# load two-year interval results
results_df_two_yearly_rubin_hiv <- read.csv("results_df_two_yearly_rubin_hiv.csv", stringsAsFactors = FALSE)

# hiv incidence over time
hiv_incidence_plot_rubin <- ggplot(results_df_two_yearly_rubin_hiv, aes(x = Interval, y = Incidence_rate)) +
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

ggsave("plots/hiv_incidence_plot_rubin.png", plot = hiv_incidence_plot_rubin, width = 10, height = 6, dpi = 300)

