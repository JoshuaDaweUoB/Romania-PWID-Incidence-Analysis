## load packages
pacman::p_load(dplyr, tidyr, withr, lubridate, MASS, writexl, readxl, arsenal, survival, broom, ggplot2)

## set wd
setwd("C:/Users/vl22683/OneDrive - University of Bristol/Documents/Publications/Romania PWID/data")

## baseline analysis

# load analyses data
romania_pwid_hcv_analysis <- read_excel("hcv_data_analysis.xlsx") 

# baseline characteristics sex work
romania_pwid_hcv_analysis <- romania_pwid_hcv_analysis %>%
  group_by(id) %>%
  mutate(id_seq = row_number())

analysis_data_hcv_bl <- subset(romania_pwid_hcv_analysis, id_seq == 1)

## longitudinal analysis

# analysis with packages

# load the dataframes
processed_dataframes <- readRDS("processed_dataframes.rds")

# Verify that the dataframes are loaded correctly
View(processed_dataframes[[1]])
View(processed_dataframes[[1000]])

# Function to make years long for Poisson regression
expand_person_time <- function(df) {
  df %>%
    rowwise() %>%
    mutate(years_followed = list(seq(from = as.Date(appointment_dte), 
                                     to = as.Date(appointment_dte_lag), 
                                     by = "year"))) %>%
    unnest(years_followed) %>%
    mutate(year = as.numeric(format(years_followed, "%Y"))) %>%
    group_by(id, year) %>%
    summarise(
      hcv_test_rslt = ifelse(year == midpoint_year, 1, 0),  # Set hcv_test_rslt to 1 for midpoint_year, otherwise 0
      person_years = as.numeric(difftime(min(appointment_dte_lag, as.Date(paste0(year, "-12-31"))), 
                                         max(appointment_dte, as.Date(paste0(year, "-01-01"))), 
                                         units = "days")) / 365.25,
      .groups = "drop"
    ) %>%
    ungroup() %>%
    complete(year = seq(min(year), max(year), by = 1), fill = list(hcv_test_rslt = 0, person_years = 0)) %>%
    mutate(id = unique(df$id)[1])  # Ensure the id is retained correctly
}



# Initialize a list to store the expanded dataframes
processed_dataframes_long <- list()

# Apply the function to each dataframe in processed_dataframes and store the results
for (i in 1:length(processed_dataframes)) {
  cat("Expanding dataframe", i, "of", length(processed_dataframes), "\n")
  processed_dataframes_long[[i]] <- expand_person_time(processed_dataframes[[i]])
}

View(processed_dataframes_long[[1]])
View(processed_dataframes_long[[1000]])

# Save the list of expanded dataframes to a file
saveRDS(processed_dataframes_long, file = "processed_dataframes_long.rds")









# Initialize lists to store the models
poisson_models <- list()
poisson_models2 <- list()

# Loop through all 1000 processed dataframes
for (i in 1:1000) {
  cat("Fitting models for dataframe", i, "of", 1000, "\n")
  
  # Get the processed dataframe for the current iteration
  df <- processed_dataframes[[i]]
  
   # Add a small constant to person_years to avoid log(0)
  epsilon <- 1e-10
  df <- df %>%
  mutate(person_years = ifelse(person_years == 0, epsilon, person_years))

  # Fit the Poisson model
  poisson_model <- glm(hcv_test_rslt ~ offset(log(person_years)), family = poisson(link = "log"), data = df)
  poisson_model2 <- glm(hcv_test_rslt ~ year + offset(log(person_years)), family = poisson(link = "log"), data = df)

  # Store the models in the lists
  poisson_models[[i]] <- poisson_model
  poisson_models2[[i]] <- poisson_model2
}

# Initialize lists to store the results for Poisson models
poisson_results <- list()
poisson_results2 <- list()

# Initialize lists to store the results for Poisson models
poisson_results <- list()
poisson_results2 <- list()

# Loop through all 1000 Poisson models
for (i in 1:1000) {
  # Get the Poisson models for the current iteration
  poisson_model <- poisson_models[[i]]
  poisson_model2 <- poisson_models2[[i]]
  
  # Extract the intercept and its standard error for the first Poisson model
  intercept1 <- coef(poisson_model)[1]
  se_intercept1 <- sqrt(vcov(poisson_model)[1, 1])
  
  # Calculate the incidence rate and 95% confidence intervals for the first Poisson model
  incidence_rate1 <- exp(intercept1)
  lower_ci1 <- exp(intercept1 - 1.96 * se_intercept1)
  upper_ci1 <- exp(intercept1 + 1.96 * se_intercept1)
  
  # Store the results for the first Poisson model in a list
  poisson_results[[i]] <- data.frame(
    iteration = i,
    incidence_rate = incidence_rate1,
    lower_ci = lower_ci1,
    upper_ci = upper_ci1
  )
  
  # Extract the intercept and its standard error for the second Poisson model
  intercept2 <- coef(poisson_model2)[1]
  se_intercept2 <- sqrt(vcov(poisson_model2)[1, 1])
  
  # Calculate the incidence rate and 95% confidence intervals for the second Poisson model
  incidence_rate2 <- exp(intercept2)
  lower_ci2 <- exp(intercept2 - 1.96 * se_intercept2)
  upper_ci2 <- exp(intercept2 + 1.96 * se_intercept2)
  
  # Store the results for the second Poisson model in a list
  poisson_results2[[i]] <- data.frame(
    iteration = i,
    incidence_rate = incidence_rate2,
    lower_ci = lower_ci2,
    upper_ci = upper_ci2
  )
}

# Combine the results into single dataframes
results_df_poisson <- do.call(rbind, poisson_results)
results_df_poisson2 <- do.call(rbind, poisson_results2)

# Print the first few rows of the results dataframes
cat("Results for the first Poisson model:\n")
print(head(results_df_poisson))

cat("Results for the second Poisson model:\n")
print(head(results_df_poisson2))

# Save the results dataframes to CSV files
write.csv(results_df_poisson, "poisson_model_results.csv", row.names = FALSE)
write.csv(results_df_poisson2, "poisson_model2_results.csv", row.names = FALSE)

# create 1000 dataframes of summed yearly person-years and incident cases

# Initialize an empty list to store the summed dataframes
summed_dataframes <- list()

# Loop through all 1000 processed dataframes
for (i in 1:1000) {
  cat("Processing summed dataframe for iteration", i, "of", 1000, "\n")
  
  # Get the processed dataframe for the current iteration
  df <- processed_dataframes[[i]]
  
  # Check if the dataframe is NULL
  if (is.null(df)) {
    next
  }
  
  # Sum the specified columns
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
  
  # Store the summed dataframe in the list
  summed_dataframes[[i]] <- summed_df
}

View(summed_dataframes[[1]])
View(summed_dataframes[[1000]])

# Combine the dataframes from summed_dataframes[[1]] to summed_dataframes[[10]]
final_summed_df <- bind_rows(summed_dataframes[1:1000])

# Print the final combined dataframe
cat("Final combined dataframe:\n")
print(head(final_summed_df))

# Create a new column hcv_test_qa which sums up all the hcv_test_20xx columns
final_summed_df <- final_summed_df %>%
  mutate(hcv_test_qa = rowSums(across(starts_with("hcv_test_20")), na.rm = TRUE))

# Function to calculate incidence rate and 95% CI per 100 person-years using negative binomial distribution
calculate_incidence_rate_nb <- function(events, person_years) {
  rate <- (events / person_years) * 100
  se <- sqrt(events + (events^2 / person_years)) / person_years * 100
  lower <- max(0, rate - 1.96 * se)
  upper <- rate + 1.96 * se
  return(c(rate, lower, upper))
}

# Calculate the overall incidence rate and 95% CI per 100 person-years for each row using negative binomial distribution
final_summed_df <- final_summed_df %>%
  rowwise() %>%
  mutate(
    overall_incidence_rate = calculate_incidence_rate_nb(hcv_test_rslt, person_years)[1],
    overall_incidence_lower = calculate_incidence_rate_nb(hcv_test_rslt, person_years)[2],
    overall_incidence_upper = calculate_incidence_rate_nb(hcv_test_rslt, person_years)[3]
  ) %>%
  ungroup()

# Calculate yearly incidence rates and 95% CIs per 100 person-years for each row using negative binomial distribution
for (year in 2013:2022) {
  final_summed_df <- final_summed_df %>%
    rowwise() %>%
    mutate(
      !!paste0("incidence_rate_", year) := calculate_incidence_rate_nb(get(paste0("hcv_test_", year)), get(paste0("X", year)))[1],
      !!paste0("incidence_lower_", year) := calculate_incidence_rate_nb(get(paste0("hcv_test_", year)), get(paste0("X", year)))[2],
      !!paste0("incidence_upper_", year) := calculate_incidence_rate_nb(get(paste0("hcv_test_", year)), get(paste0("X", year)))[3]
    ) %>%
    ungroup()
}

# View the final combined dataframe for QA
View(final_summed_df)

# calculate the median, 2.5th percentile, and 97.5th percentile for the overall incidence rate
median_incidence_rate <- median(final_summed_df$overall_incidence_rate, na.rm = TRUE)
lower_bound_overall <- quantile(final_summed_df$overall_incidence_rate, 0.025, na.rm = TRUE)
upper_bound_overall <- quantile(final_summed_df$overall_incidence_rate, 0.975, na.rm = TRUE)

# Calculate the median incidence rates for each year from 2013 to 2022
yearly_medians <- sapply(2013:2022, function(year) {
  median(final_summed_df[[paste0("incidence_rate_", year)]], na.rm = TRUE)
})

yearly_lower_bounds <- sapply(2013:2022, function(year) {
  quantile(final_summed_df[[paste0("incidence_rate_", year)]], 0.025, na.rm = TRUE)
})

yearly_upper_bounds <- sapply(2013:2022, function(year) {
  quantile(final_summed_df[[paste0("incidence_rate_", year)]], 0.975, na.rm = TRUE)
})

# Create a new dataframe with the overall and yearly incidence rates and lower bounds
results_df <- data.frame(
  Incidence_year = c("Overall incidence rate", as.character(2013:2022)),
  Incidence_rate = c(median_incidence_rate, yearly_medians),
  Lower_bound = c(lower_bound_overall, yearly_lower_bounds),
  Upper_bound = c(upper_bound_overall, yearly_upper_bounds)
)

# Print the results dataframe
cat("Results dataframe:\n")
print(results_df)
View(results_df)

# Write the results dataframe to an Excel file
write_xlsx(results_df, "hcv_incidence_results.xlsx")

# calculate the median, 2.5th percentile, and 97.5th percentile for the overall incidence rate
median_incidence_rate <- median(results_df_poisson$incidence_rate, na.rm = TRUE)
lower_bound_overall <- quantile(results_df_poisson$lower_ci, 0.025, na.rm = TRUE)
upper_bound_overall <- quantile(results_df_poisson$upper_ci, 0.975, na.rm = TRUE)

# Create a new dataframe with the overall and yearly incidence rates and lower bounds
results_poisson <- data.frame(
  Incidence_year = c("Overall incidence rate"),
  Incidence_rate = c(median_incidence_rate),
  Lower_bound = c(lower_bound_overall),
  Upper_bound = c(upper_bound_overall)
)

# calculate the median, 2.5th percentile, and 97.5th percentile for the overall incidence rate
median_incidence_rate <- median(results_df_poisson$incidence_rate, na.rm = TRUE)
lower_bound_overall <- quantile(results_df_poisson$lower_ci, 0.025, na.rm = TRUE)
upper_bound_overall <- quantile(results_df_poisson$upper_ci, 0.975, na.rm = TRUE)
median_incidence_rate
lower_bound_overall
upper_bound_overall
























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


