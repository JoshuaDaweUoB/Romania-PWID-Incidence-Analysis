## load packages
pacman::p_load(tidyr, withr, lubridate, MASS, writexl, readxl, arsenal, survival, broom, ggplot2, dplyr)

## set wd
setwd("C:/Users/vl22683/OneDrive - University of Bristol/Documents/Publications/Romania PWID/data")

## longitudinal analysis

# analysis with packages

# load the dataframes
processed_dataframes <- readRDS("processed_dataframes.rds")

# Verify that the dataframes are loaded correctly
View(processed_dataframes[[1]])
View(processed_dataframes[[1000]])

# Define the function to process each dataframe
process_dataframe <- function(df) {
  # Rename the existing "year" column (if it exists) to avoid duplication
  if ("year" %in% colnames(df)) {
    df <- df %>%
      rename(existing_year = year)
  }
  
  # Reshape the columns X2013 to X2022 to long format
  df_long <- df %>%
    pivot_longer(cols = starts_with("X"), 
                 names_to = "year", 
                 names_prefix = "X", 
                 values_to = "time_at_risk") %>%
    filter(!is.na(time_at_risk))  # Remove rows where time_at_risk is NA
  
  # Recode hcv_test_rslt to 0 when it is invalid, NA, or year does not equal midpoint_year
  df_long <- df_long %>%
    mutate(hcv_test_rslt = ifelse(is.na(hcv_test_rslt) | !is.numeric(hcv_test_rslt) | year != midpoint_year, 0, hcv_test_rslt))
  
  # Keep only the specified columns
  df_long <- df_long %>%
    dplyr::select(id, hcv_test_rslt, appointment_dte, appointment_dte_lag, year, midpoint_year, time_at_risk)
  
  # Sort by id and then by year
  df_long <- df_long %>%
    arrange(id, year)
  
  return(df_long)
}

# Load the dataframes
processed_dataframes <- readRDS("processed_dataframes.rds")

# Initialize a list to store the processed dataframes
processed_dataframes_long <- list()

# Loop over all dataframes in processed_dataframes
for (i in 1:length(processed_dataframes)) {
  cat("Processing dataframe", i, "of", length(processed_dataframes), "\n")
  processed_dataframes_long[[i]] <- process_dataframe(processed_dataframes[[i]])
}

# View the first processed dataframe for verification
View(processed_dataframes_long[[1]])

# Calculate the total of the hcv_test_rslt and time_at_risk columns
totals <- processed_dataframes_long[[1]] %>%
  summarise(
    total_hcv_test_rslt = sum(hcv_test_rslt, na.rm = TRUE),
    total_time_at_risk = sum(time_at_risk, na.rm = TRUE)
  )

# Print the totals
cat("Totals for hcv_test_rslt and time_at_risk:\n")
print(totals)

# Verify the original dataframe before processing
original_totals <- processed_dataframes[[1]] %>%
  summarise(
    total_hcv_test_rslt = sum(hcv_test_rslt, na.rm = TRUE),
    total_person_years = sum(person_years, na.rm = TRUE)
  )

# Print the original totals
cat("Original totals for hcv_test_rslt and person_years:\n")
print(original_totals)

# View the first processed dataframe for verification
View(processed_dataframes_long[[1]])

# Sequence hcv_test_rslt by id and identify any IDs with multiple positive hcv_test_rslts
multiple_positive_ids <- processed_dataframes_long[[1]] %>%
  group_by(id) %>%
  summarise(total_positive = sum(hcv_test_rslt, na.rm = TRUE)) %>%
  filter(total_positive > 1)

# Print the IDs with multiple positive hcv_test_rslts
cat("IDs with multiple positive hcv_test_rslts:\n")
print(multiple_positive_ids)

# View the rows with multiple positive hcv_test_rslts for verification
multiple_positive_rows <- processed_dataframes_long[[1]] %>%
  filter(id %in% multiple_positive_ids$id)

View(multiple_positive_rows)

## manual calculations

# load the dataframes
processed_dataframes <- readRDS("processed_dataframes.rds")
processed_dataframes_long <- readRDS("processed_dataframes_long.rds")

# Load the dataframes
processed_dataframes_long <- readRDS("processed_dataframes_long.rds")

# View the first dataframe for verification
View(processed_dataframes_long[[1]])


# view first datafarme
View(processed_dataframes[[1]])
View(processed_dataframes_long[[1]])



















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

# yearly poisson regression

# Initialize lists to store the models and results
poisson_models_long <- list()
poisson_results_long <- list()

# Loop through all 1000 processed dataframes
for (i in 1:1000) {
  cat("Fitting models for dataframe", i, "of", 1000, "\n")
  
  # Get the processed dataframe for the current iteration
  df <- processed_dataframes_long[[i]]
  
  # Fit the Poisson model
  poisson_model_long <- glm(hcv_test_rslt ~ year + offset(log(time_at_risk)), family = poisson(link = "log"), data = df)
  
  # Store the model in the list
  poisson_models_long[[i]] <- poisson_model_long
  
  # Extract the coefficients and their standard errors
  coef_df <- as.data.frame(coef(summary(poisson_model_long)))
  coef_df$year <- rownames(coef_df)
  coef_df$iteration <- i
  
  # Calculate the incidence rate and 95% confidence intervals for each year
  coef_df <- coef_df %>%
    mutate(
      incidence_rate = exp(Estimate) * 100,
      lower_ci = exp(Estimate - 1.96 * `Std. Error`) * 100,
      upper_ci = exp(Estimate + 1.96 * `Std. Error`) * 100
    )
  
  # Store the results in the list
  poisson_results_long[[i]] <- coef_df
}

# Combine the results into a single dataframe
results_poisson_model_long <- do.call(rbind, poisson_results_long)

# Print the first few rows of the results dataframe
cat("Results for the Poisson model:\n")
print(head(results_poisson_model_long))

# Save the results dataframes to CSV files
write.csv(results_poisson_model_long, "results_poisson_model_long_results.csv", row.names = TRUE)

## poisson modelling

# Initialize a list to store the yearly incidence rates and confidence intervals
yearly_incidence_results <- list()

# Loop through all 1000 Poisson models
for (i in 1:1000) {
  # Get the Poisson model for the current iteration
  poisson_model <- poisson_models_long[[i]]
  
  # Extract the coefficients and their standard errors for each year
  coef_df <- as.data.frame(coef(summary(poisson_model)))
  coef_df$year <- rownames(coef_df)
  coef_df$iteration <- i
  
  # Calculate the incidence rate and 95% confidence intervals for each year
  coef_df <- coef_df %>%
    mutate(
      incidence_rate = exp(Estimate) * 100,
      lower_ci = exp(Estimate - 1.96 * `Std. Error`) * 100,
      upper_ci = exp(Estimate + 1.96 * `Std. Error`) * 100
    )
  
  # Store the results in the list
  yearly_incidence_results[[i]] <- coef_df
}

# Save the yearly incidence results as 1000 dataframes
for (i in 1:1000) {
  write.csv(yearly_incidence_results[[i]], paste0("yearly_incidence_results_", i, ".csv"), row.names = TRUE)
}

# Combine the yearly incidence results into a single dataframe
results_yearly_incidence <- do.call(rbind, yearly_incidence_results)

# Filter the results to include only the specified years
filtered_results_yearly_incidence <- results_yearly_incidence %>%
  filter(year %in% c("year2014", "year2015", "year2016", "year2017", "year2018", "year2019", "year2020", "year2021", "year2022"))

# Print the first few rows of the filtered yearly incidence results dataframe
cat("Filtered yearly incidence results:\n")
print(head(filtered_results_yearly_incidence))

# Save the filtered yearly incidence results dataframe to a CSV file
write.csv(filtered_results_yearly_incidence, "filtered_yearly_incidence_results.csv", row.names = TRUE)

# Calculate the 2.5th, 97.5th, and median percentiles for each year
uncertainty_intervals <- filtered_results_yearly_incidence %>%
  group_by(year) %>%
  summarize(
    median_incidence_rate = median(incidence_rate),
    lower_ci = quantile(incidence_rate, 0.025),
    upper_ci = quantile(incidence_rate, 0.975)
  )

# Print the first few rows of the uncertainty intervals dataframe
cat("Uncertainty intervals for yearly incidence rates:\n")
print(head(uncertainty_intervals))

# Save the uncertainty intervals dataframe to a CSV file
write.csv(uncertainty_intervals, "uncertainty_intervals_yearly_incidence.csv", row.names = TRUE)

# Calculate the overall incidence rate and 95% confidence intervals for the specified years
overall_incidence_results <- filtered_results_yearly_incidence %>%
  summarize(
    median_incidence_rate = median(incidence_rate),
    lower_ci = quantile(incidence_rate, 0.025),
    upper_ci = quantile(incidence_rate, 0.975)
  )

# Print the overall incidence results
cat("Overall incidence results:\n")
print(overall_incidence_results)

# Save the overall incidence results to a CSV file
write.csv(overall_incidence_results, "overall_incidence_results.csv", row.names = TRUE)