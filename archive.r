

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


# Load first dataframe
midpoint_dataframe <- processed_dataframes[[1]]

# Ensure `year` is numeric or character before using it in `case_when()`
if (is.factor(midpoint_dataframe$year)) {
  midpoint_dataframe <- midpoint_dataframe %>%
    mutate(year = as.numeric(as.character(year)))  # Convert factor to numeric
} else if (is.character(midpoint_dataframe$year)) {
  midpoint_dataframe <- midpoint_dataframe %>%
    mutate(year = as.numeric(year))  # Convert character to numeric
}

# Create new columns
midpoint_dataframe <- midpoint_dataframe %>%
  mutate(
    midpoint_date = ifelse(
      hcv_test_rslt == 1 | hcv_test_rslt == 0,  # Calculate midpoint for both 1 and 0
      as.Date((as.numeric(appointment_dte) + as.numeric(appointment_dte_lag)) / 2, origin = "1970-01-01"),
      NA  # Set to NA for rows where hcv_test_rslt is not 1 or 0
    ),
    midpoint_date = as.Date(midpoint_date),  # Ensure midpoint_date is formatted as a Date
    midpoint_year = year(midpoint_date),  # Extract the year from midpoint_date
    year = case_when(
      !is.na(midpoint_year) ~ midpoint_year,  # Replace with midpoint_year if not NA
      TRUE ~ year  # Otherwise, keep the original year
    ),
    person_years = ifelse(
      hcv_test_rslt == 1, 
      as.numeric(difftime(midpoint_date, appointment_dte, units = "days")) / 365.25,  # Use midpoint_date - appointment_dte when hcv_test_rslt == 1
      as.numeric(difftime(appointment_dte_lag, appointment_dte, units = "days")) / 365.25  # Use appointment_dte_lag - appointment_dte otherwise
    )
  )

# Ensure midpoint_date is explicitly formatted as a Date
midpoint_dataframe <- midpoint_dataframe %>%
  mutate(midpoint_date = as.Date(midpoint_date))

# Keep only the specified columns in midpoint_dataframe
midpoint_dataframe <- midpoint_dataframe %>%
  select(
    id, 
    appointment_dte, 
    appointment_dte_lag, 
    hcv_test_rslt, 
    random_infection_dtes, 
    person_years, 
    midpoint_year, 
    midpoint_date, 
    year
  )

# View the updated dataframe
View(midpoint_dataframe)

# Calculate total HCV infections and total person-years
total_hcv_infections <- sum(midpoint_dataframe$hcv_test_rslt, na.rm = TRUE)
total_person_years <- sum(midpoint_dataframe$person_years, na.rm = TRUE)

# Calculate the overall incidence rate (per 100 person-years)
overall_incidence_rate <- (total_hcv_infections / total_person_years) * 100

# Calculate the 95% confidence intervals
lower_bound <- overall_incidence_rate - 1.96 * sqrt(total_hcv_infections / (total_person_years^2)) * 100
upper_bound <- overall_incidence_rate + 1.96 * sqrt(total_hcv_infections / (total_person_years^2)) * 100

# Print the results
cat("Total New Infections:", total_hcv_infections, "\n")
cat("Total Person-Years:", total_person_years, "\n")
cat("Overall HCV Incidence Rate (per 100 person-years):", overall_incidence_rate, "\n")
cat("95% Confidence Interval: [", lower_bound, ", ", upper_bound, "]\n")

# Define two-yearly intervals
midpoint_dataframe <- midpoint_dataframe %>%
  mutate(
    two_year_interval = case_when(
      year %in% c(2013, 2014) ~ "2013-2014",
      year %in% c(2015, 2016) ~ "2015-2016",
      year %in% c(2017, 2018) ~ "2017-2018",
      year %in% c(2019, 2020) ~ "2019-2020",
      year %in% c(2021, 2022) ~ "2021-2022",
      TRUE ~ NA_character_  # Exclude years outside the range
    )
  )

# Group by two-year intervals and calculate totals
two_yearly_results <- midpoint_dataframe %>%
  filter(!is.na(two_year_interval)) %>%  # Exclude rows without a valid interval
  group_by(two_year_interval) %>%
  summarise(
    total_hcv_infections = sum(hcv_test_rslt, na.rm = TRUE),
    total_person_years = sum(person_years, na.rm = TRUE),
    incidence_rate = (total_hcv_infections / total_person_years) * 100,
    lower_bound = (total_hcv_infections / total_person_years) * 100 - 
                  1.96 * sqrt(total_hcv_infections / (total_person_years^2)) * 100,
    upper_bound = (total_hcv_infections / total_person_years) * 100 + 
                  1.96 * sqrt(total_hcv_infections / (total_person_years^2)) * 100
  )

# Print the results
cat("Two-Yearly Interval Results:\n")
print(two_yearly_results)
View(two_yearly_results)
