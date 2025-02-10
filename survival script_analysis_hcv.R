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
process_dataframe <- function(df, midpoint_year) {
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
  
  # Recode hcv_test_rslt and midpoint_year to 0 when they are invalid or NA
  df_long <- df_long %>%
    mutate(hcv_test_rslt = ifelse(is.na(hcv_test_rslt) | !is.numeric(hcv_test_rslt), 0, hcv_test_rslt))
  
  # Set hcv_test_rslt to 1 when year equals midpoint_year, otherwise 0
  df_long <- df_long %>%
    mutate(hcv_test_rslt = ifelse(year == midpoint_year, 1, hcv_test_rslt))
  
  # Keep only the specified columns
  df_long <- df_long %>%
    dplyr::select(id, hcv_test_rslt, appointment_dte, appointment_dte_lag, year, time_at_risk)
  
  # Sort by id and then by year
  df_long <- df_long %>%
    arrange(id, year)
  
  return(df_long)
}

# Initialize a list to store the processed dataframes
processed_dataframes_long <- list()

# Loop over all 1000 dataframes in processed_dataframes
for (i in 1:length(processed_dataframes)) {
  cat("Processing dataframe", i, "of", length(processed_dataframes), "\n")
  processed_dataframes_long[[i]] <- process_dataframe(processed_dataframes[[i]], midpoint_year)
}

# View the first and last processed dataframes for verification
View(processed_dataframes_long[[1]])
View(processed_dataframes_long[[length(processed_dataframes)]])

# Save the list of expanded dataframes to a file
saveRDS(processed_dataframes_long, file = "processed_dataframes_long.rds")

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