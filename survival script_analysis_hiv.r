## load packages
pacman::p_load(tidyr, withr, lubridate, MASS, writexl, readxl, arsenal, survival, broom, ggplot2, dplyr)

## set wd
setwd("C:/Users/vl22683/OneDrive - University of Bristol/Documents/Publications/Romania PWID/data")

# ## Baseline prevalence and predictors of hiv infection

# # load data
# baseline_analysis_hiv <- romania_pwid_raw

# # sequence by id 
# baseline_analysis_hiv <- baseline_analysis_hiv %>%
#   arrange(id) %>%  # Ensure rows are sorted by id
#   mutate(id_seq = cumsum(!duplicated(id)))  # Increment by 1 for each new id

# View(baseline_analysis_hiv)

# # highest value in the id_seq column
# highest_id_seq <- baseline_analysis_hiv %>%
#   summarise(max_id_seq = max(id_seq, na.rm = TRUE))
# cat("Highest value in id_seq:\n")
# print(highest_id_seq)

# # remove rows where hiv test result is missing
# baseline_analysis_hiv <- baseline_analysis_hiv[!is.na(baseline_analysis_hiv$hiv_test_rslt), ]

# # remove rows where hiv test result is indeterminate
# baseline_analysis_hiv <- baseline_analysis_hiv %>%
#   filter(!hiv_test_rslt == 3)

# # create sequence of visits by ID
# baseline_analysis_hiv <- baseline_analysis_hiv %>%
#   group_by(id) %>%
#   arrange(id, appointment_dte) %>%
#   mutate(appointment_seq = row_number())

# baseline_analysis_hiv <- ungroup(baseline_analysis_hiv)

# # keep rows where appointment_seq equals 1
# baseline_analysis_hiv <- baseline_analysis_hiv %>%
#   filter(appointment_seq == 1)

# # subset romania_pwid_hiv to include only rows that match id, appointment_dte, and hiv_test_seq in romania_pwid_hiv_test
# baseline_analysis_hiv <- baseline_analysis_hiv %>%
#   mutate(
#     sex_work_current = ifelse(is.na(sex_work_current), 0, sex_work_current),
#     msm_current = ifelse(is.na(msm_current), 0, msm_current),
#     homeless_current = ifelse(is.na(homeless_current), 0, homeless_current),
#     ethnic_roma = ifelse(is.na(ethnic_roma), 0, ethnic_roma),
#     gender = ifelse(gender == 2, 0, gender) 
#   )

# # ensure dob is in the correct Date format
# baseline_analysis_hiv <- baseline_analysis_hiv %>%
#   mutate(dob = as.Date(dob, format = "%d/%m/%Y")) %>%
#   mutate(age_years = as.numeric(difftime(appointment_dte, dob, units = "days")) / 365.25) %>%
#   mutate(age_bin = ifelse(age_years < 30, 0, 1))

# # change test results to 0 and 1
# baseline_analysis_hiv <- baseline_analysis_hiv %>%
#   mutate(hiv_test_rslt = case_when(
#     hiv_test_rslt == 1 ~ 0,
#     hiv_test_rslt == 2 ~ 1,
#     TRUE ~ hiv_test_rslt
#   ))

# # Filter the dataset to include only rows where hiv_test_rslt is 0 or 1
# baseline_analysis_hiv <- baseline_analysis_hiv %>%
#   filter(hiv_test_rslt %in% c(0, 1))

# # summary table with variables as rows and hiv_test_rslt levels as columns
# hiv_summary_table <- baseline_analysis_hiv %>%
#   dplyr::select(sex_work_current, homeless_current, ethnic_roma, gender, age_bin, hiv_test_rslt) %>%
#   pivot_longer(
#     cols = c(sex_work_current, homeless_current, ethnic_roma, gender, age_bin),
#     names_to = "Variable",
#     values_to = "Level"
#   ) %>%
#   group_by(Variable, Level, hiv_test_rslt) %>%
#   summarise(
#     Count = n(),
#     .groups = "drop"
#   ) %>%
#   pivot_wider(
#     names_from = hiv_test_rslt,
#     values_from = Count,
#     values_fill = 0  # Fill missing values with 0
#   ) %>%
#   rename(
#     hiv_Negative = `0`,
#     hiv_Positive = `1`
#   ) %>%
#   mutate(
#     Total = hiv_Negative + hiv_Positive,
#     Proportion_Positive = (hiv_Positive / Total) * 100
#   )

# # print and save the summary table
# print(hiv_summary_table)
# write.csv(hiv_summary_table, "hiv_summary_table.csv", row.names = FALSE)

# # filter dataset to include only rows where gender == 1
# baseline_analysis_hiv_gender_1 <- baseline_analysis_hiv %>%
#   filter(gender == 1)

# # summary table for msm_current with hiv_test_rslt levels as columns
# msm_summary_table <- baseline_analysis_hiv_gender_1 %>%
#   dplyr::select(msm_current, hiv_test_rslt) %>%
#   group_by(msm_current, hiv_test_rslt) %>%
#   summarise(
#     Count = n(),
#     .groups = "drop"
#   ) %>%
#   pivot_wider(
#     names_from = hiv_test_rslt,
#     values_from = Count,
#     values_fill = 0
#   ) %>%
#   rename(
#     hiv_Negative = `0`,
#     hiv_Positive = `1`
#   ) %>%
#   mutate(
#     Total = hiv_Negative + hiv_Positive,
#     Proportion_Positive = (hiv_Positive / Total) * 100
#   )

# # print and save msm summary table
# print(msm_summary_table)
# write.csv(msm_summary_table, "msm_summary_table_gender_1.csv", row.names = FALSE)

# ## differences between excluded and included in longitudinal analysis

# # remove rows where hiv test result is missing
# romania_pwid_hiv_tb2 <- romania_pwid_raw[!is.na(romania_pwid_raw$hiv_test_rslt), ]

# # remove rows where hiv test result is indeterminate
# romania_pwid_hiv_tb2 <- romania_pwid_hiv_tb2 %>%
#   filter(!hiv_test_rslt == 3)

# # create sequence of visits by ID
# romania_pwid_hiv_tb2 <- romania_pwid_hiv_tb2 %>%
#   group_by(id) %>%
#   arrange(id, appointment_dte) %>%
#   mutate(appointment_seq = row_number())

# romania_pwid_hiv_tb2 <- ungroup(romania_pwid_hiv_tb2)

# # remove IDs where hiv positive at baseline
# ids_to_remove <- romania_pwid_hiv_tb2 %>%
#   filter(appointment_seq == 1 & hiv_test_rslt == 2) %>%
#   pull(id)

# romania_pwid_hiv_tb2 <- romania_pwid_hiv_tb2 %>%
#   filter(!(id %in% ids_to_remove))

# # flag participants with multiple tests
# romania_pwid_hiv_tb2 <- romania_pwid_hiv_tb2 %>%
#   group_by(id) %>%
#   arrange(id) %>%
#   mutate(hiv_test_seq = row_number())

# # create indicator of multiple tests vs. single test
# romania_pwid_hiv_tb2 <- romania_pwid_hiv_tb2 %>%
#   group_by(id) %>%
#   mutate(
#     hiv_test_seq_max = max(hiv_test_seq, na.rm = TRUE),
#     hiv_test_seq_bin = ifelse(hiv_test_seq_max == 1, 0, 1)
#   ) %>%
#   ungroup()

# # keep participants first test
# romania_pwid_hiv_tb2 <- romania_pwid_hiv_tb2 %>%
#   filter(hiv_test_seq == 1)

# # summary table for hiv_test_seq_bin
# hiv_test_seq_bin_summary <- romania_pwid_hiv_tb2 %>%
#   group_by(hiv_test_seq_bin) %>%
#   summarise(
#     Count = n(),
#     Proportion = (n() / nrow(romania_pwid_hiv_tb2)) * 100
#   )

# # summary table
# print(hiv_test_seq_bin_summary)

# # ensure dob is in the correct Date format
# romania_pwid_hiv_tb2 <- romania_pwid_hiv_tb2 %>%
#   mutate(dob = as.Date(dob, format = "%d/%m/%Y")) %>%
#   mutate(age_years = as.numeric(difftime(appointment_dte, dob, units = "days")) / 365.25) %>%
#   mutate(age_bin = ifelse(age_years < 30, 0, 1))

# # create table of differences between included and excluded participants

# # create a summary table
# summary_table <- romania_pwid_hiv_tb2 %>%
#   dplyr::select(sex_work_current, homeless_current, ethnic_roma, age_bin, gender, hiv_test_seq_bin) %>%
#   pivot_longer(
#     cols = c(sex_work_current, homeless_current, ethnic_roma, age_bin, gender),
#     names_to = "Variable",
#     values_to = "Level"
#   ) %>%
#   group_by(Variable, Level, hiv_test_seq_bin) %>%
#   summarise(
#     Count = n(),
#     .groups = "drop"
#   ) %>%
#   pivot_wider(
#     names_from = hiv_test_seq_bin,
#     values_from = Count,
#     values_fill = 0
#   ) %>%
#   rename(
#     Single_Test = `0`,
#     Multiple_Tests = `1`
#   ) %>%
#   mutate(
#     Total = Single_Test + Multiple_Tests,
#     Proportion_Multiple_Tests = (Multiple_Tests / Total) * 100
#   )

# # print and save summary table
# print(summary_table)
# write.csv(summary_table, "hiv_test_seq_bin_summary_table.csv", row.names = FALSE)

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
  
  # Calculate standard error for each imputation
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

# save two-year interval results
write.csv(results_df_two_yearly_rubin_hiv, "results_df_two_yearly_rubin_hiv.csv", row.names = FALSE)

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

ggsave("plots/hiv_incidence_plot_rubin.png", plot = hiv_incidence_plot_rubin, width = 8, height = 6, dpi = 300)
