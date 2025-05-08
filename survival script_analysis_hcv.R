## load packages
pacman::p_load(tidyr, withr, lubridate, MASS, writexl, readxl, arsenal, survival, broom, ggplot2, dplyr)

## set wd
setwd("C:/Users/vl22683/OneDrive - University of Bristol/Documents/Publications/Romania PWID/data")

## longitudinal analysis- approach 1

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
View(final_summed_df)

# Create a new column hcv_test_qa which sums up all the hcv_test_20xx columns
final_summed_df <- final_summed_df %>%
  mutate(hcv_test_qa = rowSums(across(starts_with("hcv_test_20")), na.rm = TRUE))

# Add columns for overall incidence rate and 95% confidence interval
final_summed_df <- final_summed_df %>%
  mutate(
    overall_incidence_rate = (hcv_test_rslt / person_years) * 100,  # Incidence rate per 100 person-years
    standard_error = sqrt(hcv_test_rslt) / person_years * 100,      # Standard error of the incidence rate
    lower_bound_95CI = overall_incidence_rate - (1.96 * standard_error),  # Lower bound of 95% CI
    upper_bound_95CI = overall_incidence_rate + (1.96 * standard_error)   # Upper bound of 95% CI
  )

# View the final combined dataframe for QA
View(final_summed_df)

# Calculate the median, 2.5th percentile, and 97.5th percentile for the overall incidence rate
median_incidence_rate <- median(final_summed_df$overall_incidence_rate, na.rm = TRUE)
lower_bound_overall <- quantile(final_summed_df$overall_incidence_rate, 0.025, na.rm = TRUE)
upper_bound_overall <- quantile(final_summed_df$overall_incidence_rate, 0.975, na.rm = TRUE)

# Calculate the median incidence rates for each year from 2013 to 2022
yearly_medians <- sapply(2013:2022, function(year) {
  if (paste0("incidence_rate_", year) %in% colnames(final_summed_df)) {
    median(final_summed_df[[paste0("incidence_rate_", year)]], na.rm = TRUE)
  } else {
    NA
  }
})
cat("Yearly medians:\n")
print(yearly_medians)

yearly_lower_bounds <- sapply(2013:2022, function(year) {
  if (paste0("incidence_rate_", year) %in% colnames(final_summed_df)) {
    quantile(final_summed_df[[paste0("incidence_rate_", year)]], 0.025, na.rm = TRUE)
  } else {
    NA
  }
})
cat("Yearly lower bounds:\n")
print(yearly_lower_bounds)

yearly_upper_bounds <- sapply(2013:2022, function(year) {
  if (paste0("incidence_rate_", year) %in% colnames(final_summed_df)) {
    quantile(final_summed_df[[paste0("incidence_rate_", year)]], 0.975, na.rm = TRUE)
  } else {
    NA
  }
})
cat("Yearly upper bounds:\n")
print(yearly_upper_bounds)

median_hcv_infections <- sapply(2013:2022, function(year) {
  if (paste0("hcv_test_", year) %in% colnames(final_summed_df)) {
    median(final_summed_df[[paste0("hcv_test_", year)]], na.rm = TRUE)
  } else {
    NA
  }
})
cat("Median HCV infections:\n")
print(median_hcv_infections)

median_person_years <- sapply(2013:2022, function(year) {
  if (paste0("X", year) %in% colnames(final_summed_df)) {
    median(final_summed_df[[paste0("X", year)]], na.rm = TRUE)
  } else {
    NA
  }
})
cat("Median person-years:\n")
print(median_person_years)

# Calculate the overall median number of HCV infections and person-years
overall_median_hcv_infections <- median(rowSums(final_summed_df[paste0("hcv_test_", 2013:2022)], na.rm = TRUE), na.rm = TRUE)
overall_median_person_years <- median(rowSums(final_summed_df[paste0("X", 2013:2022)], na.rm = TRUE), na.rm = TRUE)

# Create a new dataframe with the overall and yearly incidence rates and lower bounds
results_df <- data.frame(
  Incidence_year = c("Overall incidence rate", as.character(2013:2022)),
  Incidence_rate = c(median_incidence_rate, yearly_medians),
  Lower_bound = c(lower_bound_overall, yearly_lower_bounds),
  Upper_bound = c(upper_bound_overall, yearly_upper_bounds),
  Median_HCV_infections = c(overall_median_hcv_infections, median_hcv_infections),
  Median_person_years = c(overall_median_person_years, median_person_years)
)

# Print the results dataframe
cat("Results dataframe:\n")
print(results_df)
View(results_df)

# Save the overall incidence results to a CSV file
write.csv(results_df, "overall_incidence_results_df.csv", row.names = TRUE)

# Create 1000 dataframes of summed two-year intervals for person-years and incident cases

# Initialize an empty list to store the summed dataframes
summed_dataframes_two_yearly <- list()

# Loop through all 1000 processed dataframes
for (i in 1:1000) {
  cat("Processing summed dataframe for iteration", i, "of", 1000, "\n")
  
  # Get the processed dataframe for the current iteration
  df <- processed_dataframes[[i]]
  
  # Check if the dataframe is NULL
  if (is.null(df)) {
    next
  }
  
  # Sum the specified columns for two-year intervals
  summed_df_two_yearly <- df %>%
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
  
  # Store the summed dataframe in the list
  summed_dataframes_two_yearly[[i]] <- summed_df_two_yearly
}

# Combine the dataframes from all iterations
final_summed_df_two_yearly <- bind_rows(summed_dataframes_two_yearly)

# Add incidence rate columns for each two-year interval
final_summed_df_two_yearly <- final_summed_df_two_yearly %>%
  mutate(
    incidence_rate_2013_2014 = hcv_test_2013_2014 / person_years_2013_2014 * 100,
    incidence_rate_2015_2016 = hcv_test_2015_2016 / person_years_2015_2016 * 100,
    incidence_rate_2017_2018 = hcv_test_2017_2018 / person_years_2017_2018 * 100,
    incidence_rate_2019_2020 = hcv_test_2019_2020 / person_years_2019_2020 * 100,
    incidence_rate_2021_2022 = hcv_test_2021_2022 / person_years_2021_2022 * 100
  )

# Debugging: Check the updated column names
cat("Updated columns in final_summed_df_two_yearly:\n")
print(colnames(final_summed_df_two_yearly))

# Print the final combined dataframe
cat("Final combined dataframe:\n")
print(head(final_summed_df_two_yearly))
View(final_summed_df_two_yearly)

# Debugging: Check the structure of final_summed_df_two_yearly
cat("Columns in final_summed_df_two_yearly:\n")
print(colnames(final_summed_df_two_yearly))

# Calculate the median incidence rates for each two-year interval
two_yearly_medians <- sapply(c("2013_2014", "2015_2016", "2017_2018", "2019_2020", "2021_2022"), function(interval) {
  if (paste0("incidence_rate_", interval) %in% colnames(final_summed_df_two_yearly)) {
    median(final_summed_df_two_yearly[[paste0("incidence_rate_", interval)]], na.rm = TRUE)
  } else {
    NA  # Return NA if the column does not exist
  }
})
cat("Two-yearly medians:\n")
print(two_yearly_medians)

two_yearly_lower_bounds <- sapply(c("2013_2014", "2015_2016", "2017_2018", "2019_2020", "2021_2022"), function(interval) {
  if (paste0("incidence_rate_", interval) %in% colnames(final_summed_df_two_yearly)) {
    quantile(final_summed_df_two_yearly[[paste0("incidence_rate_", interval)]], 0.025, na.rm = TRUE)
  } else {
    NA
  }
})
cat("Two-yearly lower bounds:\n")
print(two_yearly_lower_bounds)

two_yearly_upper_bounds <- sapply(c("2013_2014", "2015_2016", "2017_2018", "2019_2020", "2021_2022"), function(interval) {
  if (paste0("incidence_rate_", interval) %in% colnames(final_summed_df_two_yearly)) {
    quantile(final_summed_df_two_yearly[[paste0("incidence_rate_", interval)]], 0.975, na.rm = TRUE)
  } else {
    NA
  }
})
cat("Two-yearly upper bounds:\n")
print(two_yearly_upper_bounds)

# Create a new dataframe with the two-year interval results
results_df_two_yearly <- data.frame(
  Interval = c("2013-2014", "2015-2016", "2017-2018", "2019-2020", "2021-2022"),
  Incidence_rate = two_yearly_medians,
  Lower_bound = two_yearly_lower_bounds,
  Upper_bound = two_yearly_upper_bounds,
  Median_HCV_infections = median_hcv_infections_two_yearly,
  Median_person_years = median_person_years_two_yearly
)

# Print the results dataframe
cat("Two-Year Interval Results dataframe:\n")
print(results_df_two_yearly)
View(results_df_two_yearly)

# Save the two-year interval results to a CSV file
write.csv(results_df_two_yearly, "results_df_two_yearly.csv", row.names = FALSE)

# figure of incidence over time
HCV_incidence_plot <- ggplot(results_df_two_yearly, aes(x = Interval, y = Incidence_rate)) +
  geom_line(group = 1, color = "gray") + 
  geom_point(shape = 18, size = 3, color = "gray") + 
  geom_errorbar(aes(ymin = Lower_bound, ymax = Upper_bound), width = 0.2, color = "black") +  # Error bars
  theme_minimal(base_size = 14) +  # Use a minimal theme
  labs(
    x = "Two-yearly Interval",
    y = "Incidence Rate per 100 Person-Years"  
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    panel.background = element_rect(fill = "white", color = NA),  # Set panel background to white
    plot.background = element_rect(fill = "white", color = NA)  # Set plot background to white
  )

# Save the plot as a PNG file in the "plots" folder
ggsave("plots/HCV_incidence_plot.png", plot = HCV_incidence_plot, width = 8, height = 6, dpi = 300)


## loop over 1000 iterations

# Initialize a list to store the results for all 1000 dataframes
all_two_yearly_results <- list()

# Loop over all 1000 dataframes in the list
for (i in 1:length(processed_dataframes_long)) {
  cat("Processing dataframe", i, "of", length(processed_dataframes_long), "\n")
  
  # Load the current dataframe
  midpoint_dataframe <- processed_dataframes_long[[i]]
  
  # Replace midpoint_year with NA if hcv_test_rslt is negative
  midpoint_dataframe <- midpoint_dataframe %>%
    mutate(
      midpoint_year = ifelse(hcv_test_rslt == 0, NA, midpoint_year)  # Replace midpoint_year with NA if hcv_test_rslt == 0
    )
  
  # Create a dataframe with rows for years 2013 to 2022 and calculate cases and years_at_risk
  yearly_data <- midpoint_dataframe %>%
    group_by(year) %>%
    summarise(
      cases = sum(hcv_test_rslt, na.rm = TRUE),        # Sum of hcv_test_rslt for each year
      years_at_risk = sum(time_at_risk, na.rm = TRUE)  # Sum of time_at_risk for each year
    ) %>%
    filter(year %in% 2013:2022)  # Ensure only rows for years 2013 to 2022 are included
  
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
      total_hcv_infections = sum(hcv_test_rslt, na.rm = TRUE),  # Total cases
      total_person_years = sum(time_at_risk, na.rm = TRUE),     # Total person-years
      incidence_rate = (total_hcv_infections / total_person_years) * 100,  # Incidence rate per 100 person-years
      lower_bound = (total_hcv_infections / total_person_years) * 100 - 
                    1.96 * sqrt(total_hcv_infections / (total_person_years^2)) * 100,  # Lower 95% CI
      upper_bound = (total_hcv_infections / total_person_years) * 100 + 
                    1.96 * sqrt(total_hcv_infections / (total_person_years^2)) * 100   # Upper 95% CI
    )
  
  # Store the results in the list
  all_two_yearly_results[[i]] <- two_yearly_results
}

# Combine all results into a single dataframe
combined_two_yearly_results <- bind_rows(all_two_yearly_results, .id = "iteration")

# Save the combined results to a CSV file
write.csv(combined_two_yearly_results, "combined_two_yearly_results.csv", row.names = FALSE)

# View the combined results
View(combined_two_yearly_results)

# Initialize a list to store the results for all 1000 dataframes
all_two_yearly_results <- list()

## cox regression analysis
View(processed_dataframes[[1]])

# Initialize a list to store the results for all 1000 dataframes
all_cox_results <- list()

# Loop over all 1000 dataframes in processed_dataframes
for (i in 1:length(processed_dataframes)) {
  cat("Processing Cox regression for dataframe", i, "of", length(processed_dataframes), "\n")
  
  # Load the current dataframe
  df <- processed_dataframes[[i]]
  
  # Ensure the dataframe has no missing values for the variables of interest
  df <- df %>%
    filter(!is.na(hcv_test_rslt) & !is.na(gender) & !is.na(dob) & 
             !is.na(sex_work_current) & !is.na(msm_current) & 
             !is.na(homeless_current) & !is.na(ethnic_roma))
  
  # Create a survival object
  surv_obj <- Surv(time = df$days_risk, event = df$hcv_test_rslt)
  
  # List of exposures
  exposures <- c("gender", "age_years", "sex_work_current", "homeless_current", "ethnic_roma")
  
  # Run univariate Cox regressions
  univariate_results <- lapply(exposures, function(var) {
    formula <- as.formula(paste("surv_obj ~", var))
    tryCatch(
      coxph(formula, data = df),
      error = function(e) {
        cat("Error in Cox regression for", var, ":", e$message, "\n")
        NULL
      }
    )
  })
  
  # Run a multivariable Cox regression adjusting for all exposures
  multivariable_formula <- as.formula(paste("surv_obj ~", paste(exposures, collapse = " + ")))
  multivariable_cox <- tryCatch(
    coxph(multivariable_formula, data = df),
    error = function(e) {
      cat("Error in multivariable Cox regression:", e$message, "\n")
      NULL
    }
  )
  
  # Store the results for this dataframe
  all_cox_results[[i]] <- list(
    univariate = univariate_results,
    multivariable = multivariable_cox
  )
}

# Save the results to a file
saveRDS(all_cox_results, file = "all_cox_results.rds")

# Example: Print the summary of the multivariable Cox regression for the first dataframe
cat("\nMultivariable Cox regression for dataframe 1:\n")
print(summary(all_cox_results[[1]]$multivariable))

# Initialize an empty list to store the results for each iteration
cox_point_estimates <- list()
lower_95CI_list <- list()
upper_95CI_list <- list()

# Loop through all 1000 iterations in all_cox_results
for (i in 1:length(all_cox_results)) {
  # Extract the univariate Cox regression results for this iteration
  univariate_results <- all_cox_results[[i]]$univariate
  
  # Extract the hazard ratios (HR) and confidence intervals for each exposure
  hr_values <- sapply(univariate_results, function(model) {
    if (!is.null(model)) {
      exp(coef(model))  # Extract and exponentiate the coefficient to get the HR
    } else {
      NA  # If the model is NULL, return NA
    }
  })
  
  lower_95CI <- sapply(univariate_results, function(model) {
    if (!is.null(model)) {
      exp(confint(model)[, 1])  # Extract and exponentiate the lower bound of the CI
    } else {
      NA
    }
  })
  
  upper_95CI <- sapply(univariate_results, function(model) {
    if (!is.null(model)) {
      exp(confint(model)[, 2])  # Extract and exponentiate the upper bound of the CI
    } else {
      NA
    }
  })
  
  # Extract the multivariable Cox regression results for this iteration
  multivariable_model <- all_cox_results[[i]]$multivariable
  
  # Extract the hazard ratios (HR) and confidence intervals for the multivariable model
  multivariable_hr <- if (!is.null(multivariable_model)) {
    exp(coef(multivariable_model))  # Extract and exponentiate the coefficients
  } else {
    rep(NA, length(hr_values))  # If the model is NULL, return NA for all exposures
  }
  
  multivariable_lower_95CI <- if (!is.null(multivariable_model)) {
    exp(confint(multivariable_model)[, 1])  # Extract and exponentiate the lower bound of the CI
  } else {
    rep(NA, length(hr_values))
  }
  
  multivariable_upper_95CI <- if (!is.null(multivariable_model)) {
    exp(confint(multivariable_model)[, 2])  # Extract and exponentiate the upper bound of the CI
  } else {
    rep(NA, length(hr_values))
  }
  
  # Combine the univariate and multivariable HRs and CIs into a single row
  combined_row <- c(univariate_hr = hr_values, multivariable_hr = multivariable_hr)
  lower_95CI_row <- c(univariate_lower_95CI = lower_95CI, multivariable_lower_95CI = multivariable_lower_95CI)
  upper_95CI_row <- c(univariate_upper_95CI = upper_95CI, multivariable_upper_95CI = multivariable_upper_95CI)
  
  # Store the rows in the respective lists
  cox_point_estimates[[i]] <- combined_row
  lower_95CI_list[[i]] <- lower_95CI_row
  upper_95CI_list[[i]] <- upper_95CI_row
}

# Combine all rows into dataframes
cox_point_estimates_df <- do.call(rbind, cox_point_estimates)
lower_95CI_df <- do.call(rbind, lower_95CI_list)
upper_95CI_df <- do.call(rbind, upper_95CI_list)

# Assign column names to the dataframes
first_non_null <- all_cox_results[[which(!sapply(all_cox_results, is.null))[1]]]
colnames(cox_point_estimates_df) <- c(
  paste0("univariate_", names(first_non_null$univariate)),
  paste0("multivariable_", names(first_non_null$multivariable$coefficients))
)
colnames(lower_95CI_df) <- colnames(cox_point_estimates_df)
colnames(upper_95CI_df) <- colnames(cox_point_estimates_df)

# Calculate the median, lower, and upper bounds for the hazard ratios and confidence intervals
summary_table <- data.frame(
  Variable = colnames(cox_point_estimates_df),
  Median_HR = apply(cox_point_estimates_df, 2, median, na.rm = TRUE),
  Lower_95CI = apply(lower_95CI_df, 2, median, na.rm = TRUE),
  Upper_95CI = apply(upper_95CI_df, 2, median, na.rm = TRUE)
)

# View the resulting summary table
View(summary_table)

# Save the summary table to a CSV file
write.csv(summary_table, "cox_regression_summary_table.csv", row.names = FALSE)

## analysis among males

# Initialize a list to store the results for all 1000 dataframes
all_cox_results_male <- list()

# Loop over all 1000 dataframes in processed_dataframes
for (i in 1:length(processed_dataframes)) {
  cat("Processing Cox regression for dataframe", i, "of", length(processed_dataframes), "\n")
  
  # Load the current dataframe
  df <- processed_dataframes[[i]]
  
  # Filter the dataframe to include only rows where gender == 0 (male)
  df <- df %>%
    filter(gender == 0 & 
           !is.na(hcv_test_rslt) & !is.na(dob) & 
           !is.na(sex_work_current) & !is.na(msm_current) & 
           !is.na(homeless_current) & !is.na(ethnic_roma))
  
  # Check if the filtered dataframe is empty
  if (nrow(df) == 0) {
    cat("Filtered dataframe is empty for iteration", i, ". Skipping.\n")
    all_cox_results_male[[i]] <- NULL
    next
  }
  
  # Create a survival object
  surv_obj_male <- Surv(time = df$days_risk, event = df$hcv_test_rslt)
  
  # List of exposures
  exposures_male <- c("age_years", "sex_work_current", "msm_current", "homeless_current", "ethnic_roma")
  
  # Run univariate Cox regressions
  univariate_results_male <- lapply(exposures_male, function(var) {
    formula <- as.formula(paste("surv_obj_male ~", var))
    tryCatch(
      coxph(formula, data = df),
      error = function(e) {
        cat("Error in Cox regression for", var, ":", e$message, "\n")
        NULL
      }
    )
  })
  
  # Run a multivariable Cox regression adjusting for all exposures
  multivariable_formula_male <- as.formula(paste("surv_obj_male ~", paste(exposures_male, collapse = " + ")))
  multivariable_cox_male <- tryCatch(
    coxph(multivariable_formula_male, data = df),
    error = function(e) {
      cat("Error in multivariable Cox regression:", e$message, "\n")
      NULL
    }
  )
  
  # Store the results for this dataframe
  all_cox_results_male[[i]] <- list(
    univariate_male = univariate_results_male,
    multivariable_male = multivariable_cox_male
  )
}

# Save the results to a file
saveRDS(all_cox_results_male, file = "all_cox_results_male.rds")

# Initialize an empty list to store the results for each iteration
cox_point_estimates_male <- list()
lower_95CI_list_male <- list()
upper_95CI_list_male <- list()

# Loop through all 1000 iterations in all_cox_results_male
for (i in 1:length(all_cox_results_male)) {
  # Extract the univariate Cox regression results for this iteration
  univariate_results_male <- all_cox_results_male[[i]]$univariate_male
  
  # Extract the hazard ratios (HR) and confidence intervals for each exposure
  hr_values_male <- sapply(univariate_results_male, function(model) {
    if (!is.null(model)) {
      exp(coef(model))  # Extract and exponentiate the coefficient to get the HR
    } else {
      NA  # If the model is NULL, return NA
    }
  })
  
  lower_95CI_male <- sapply(univariate_results_male, function(model) {
    if (!is.null(model)) {
      exp(confint(model)[, 1])  # Extract and exponentiate the lower bound of the CI
    } else {
      NA
    }
  })
  
  upper_95CI_male <- sapply(univariate_results_male, function(model) {
    if (!is.null(model)) {
      exp(confint(model)[, 2])  # Extract and exponentiate the upper bound of the CI
    } else {
      NA
    }
  })
  
  # Extract the multivariable Cox regression results for this iteration
  multivariable_model_male <- all_cox_results_male[[i]]$multivariable_male
  
  # Extract the hazard ratios (HR) and confidence intervals for the multivariable model
  multivariable_hr_male <- if (!is.null(multivariable_model_male)) {
    exp(coef(multivariable_model_male))  # Extract and exponentiate the coefficients
  } else {
    rep(NA, length(hr_values_male))  # If the model is NULL, return NA for all exposures
  }
  
  multivariable_lower_95CI_male <- if (!is.null(multivariable_model_male)) {
    exp(confint(multivariable_model_male)[, 1])  # Extract and exponentiate the lower bound of the CI
  } else {
    rep(NA, length(hr_values_male))
  }
  
  multivariable_upper_95CI_male <- if (!is.null(multivariable_model_male)) {
    exp(confint(multivariable_model_male)[, 2])  # Extract and exponentiate the upper bound of the CI
  } else {
    rep(NA, length(hr_values_male))
  }
  
  # Combine the univariate and multivariable HRs and CIs into a single row
  combined_row_male <- c(univariate_hr_male = hr_values_male, multivariable_hr_male = multivariable_hr_male)
  lower_95CI_row_male <- c(univariate_lower_95CI_male = lower_95CI_male, multivariable_lower_95CI_male = multivariable_lower_95CI_male)
  upper_95CI_row_male <- c(univariate_upper_95CI_male = upper_95CI_male, multivariable_upper_95CI_male = multivariable_upper_95CI_male)
  
  # Store the rows in the respective lists
  cox_point_estimates_male[[i]] <- combined_row_male
  lower_95CI_list_male[[i]] <- lower_95CI_row_male
  upper_95CI_list_male[[i]] <- upper_95CI_row_male
}

# Combine all rows into dataframes
cox_point_estimates_df_male <- do.call(rbind, cox_point_estimates_male)
lower_95CI_df_male <- do.call(rbind, lower_95CI_list_male)
upper_95CI_df_male <- do.call(rbind, upper_95CI_list_male)

# Assign column names to the dataframes
first_non_null_male <- all_cox_results_male[[which(!sapply(all_cox_results_male, is.null))[1]]]
colnames(cox_point_estimates_df_male) <- c(
  paste0("univariate_", names(first_non_null_male$univariate_male)),
  paste0("multivariable_", names(first_non_null_male$multivariable_male$coefficients))
)
colnames(lower_95CI_df_male) <- colnames(cox_point_estimates_df_male)
colnames(upper_95CI_df_male) <- colnames(cox_point_estimates_df_male)

# Calculate the median, lower, and upper bounds for the hazard ratios and confidence intervals
summary_table_male <- data.frame(
  Variable = colnames(cox_point_estimates_df_male),
  Median_HR = apply(cox_point_estimates_df_male, 2, median, na.rm = TRUE),
  Lower_95CI = apply(lower_95CI_df_male, 2, median, na.rm = TRUE),
  Upper_95CI = apply(upper_95CI_df_male, 2, median, na.rm = TRUE)
)

# View the resulting summary table
View(summary_table_male)

# Save the summary table to a CSV file
write.csv(summary_table_male, "cox_regression_summary_table_male.csv", row.names = FALSE)

## Analysis among females

# Initialize a list to store the results for all 1000 dataframes
all_cox_results_female <- list()

# Loop over all 1000 dataframes in processed_dataframes
for (i in 1:length(processed_dataframes)) {
  cat("Processing Cox regression for dataframe", i, "of", length(processed_dataframes), "\n")
  
  # Load the current dataframe
  df <- processed_dataframes[[i]]
  
  # Filter the dataframe to include only rows where gender == 1 (female)
  df <- df %>%
    filter(gender == 1 & 
           !is.na(hcv_test_rslt) & !is.na(dob) & 
           !is.na(sex_work_current) & 
           !is.na(homeless_current) & !is.na(ethnic_roma))
  
  # Check if the filtered dataframe is empty
  if (nrow(df) == 0) {
    cat("Filtered dataframe is empty for iteration", i, ". Skipping.\n")
    all_cox_results_female[[i]] <- NULL
    next
  }
  
  # Create a survival object
  surv_obj_female <- Surv(time = df$days_risk, event = df$hcv_test_rslt)
  
  # List of exposures (excluding msm_current)
  exposures_female <- c("age_years", "sex_work_current", "homeless_current", "ethnic_roma")
  
  # Run univariate Cox regressions
  univariate_results_female <- lapply(exposures_female, function(var) {
    formula <- as.formula(paste("surv_obj_female ~", var))
    tryCatch(
      coxph(formula, data = df),
      error = function(e) {
        cat("Error in Cox regression for", var, ":", e$message, "\n")
        NULL
      }
    )
  })
  
  # Run a multivariable Cox regression adjusting for all exposures
  multivariable_formula_female <- as.formula(paste("surv_obj_female ~", paste(exposures_female, collapse = " + ")))
  multivariable_cox_female <- tryCatch(
    coxph(multivariable_formula_female, data = df),
    error = function(e) {
      cat("Error in multivariable Cox regression:", e$message, "\n")
      NULL
    }
  )
  
  # Store the results for this dataframe
  all_cox_results_female[[i]] <- list(
    univariate_female = univariate_results_female,
    multivariable_female = multivariable_cox_female
  )
}

# Save the results to a file
saveRDS(all_cox_results_female, file = "all_cox_results_female.rds")

# Initialize an empty list to store the results for each iteration
cox_point_estimates_female <- list()
lower_95CI_list_female <- list()
upper_95CI_list_female <- list()

# Loop through all 1000 iterations in all_cox_results_female
for (i in 1:length(all_cox_results_female)) {
  # Extract the univariate Cox regression results for this iteration
  univariate_results_female <- all_cox_results_female[[i]]$univariate_female
  
  # Extract the hazard ratios (HR) and confidence intervals for each exposure
  hr_values_female <- sapply(univariate_results_female, function(model) {
    if (!is.null(model)) {
      exp(coef(model))  # Extract and exponentiate the coefficient to get the HR
    } else {
      NA  # If the model is NULL, return NA
    }
  })
  
  lower_95CI_female <- sapply(univariate_results_female, function(model) {
    if (!is.null(model)) {
      exp(confint(model)[, 1])  # Extract and exponentiate the lower bound of the CI
    } else {
      NA
    }
  })
  
  upper_95CI_female <- sapply(univariate_results_female, function(model) {
    if (!is.null(model)) {
      exp(confint(model)[, 2])  # Extract and exponentiate the upper bound of the CI
    } else {
      NA
    }
  })
  
  # Extract the multivariable Cox regression results for this iteration
  multivariable_model_female <- all_cox_results_female[[i]]$multivariable_female
  
  # Extract the hazard ratios (HR) and confidence intervals for the multivariable model
  multivariable_hr_female <- if (!is.null(multivariable_model_female)) {
    exp(coef(multivariable_model_female))  # Extract and exponentiate the coefficients
  } else {
    rep(NA, length(hr_values_female))  # If the model is NULL, return NA for all exposures
  }
  
  multivariable_lower_95CI_female <- if (!is.null(multivariable_model_female)) {
    exp(confint(multivariable_model_female)[, 1])  # Extract and exponentiate the lower bound of the CI
  } else {
    rep(NA, length(hr_values_female))
  }
  
  multivariable_upper_95CI_female <- if (!is.null(multivariable_model_female)) {
    exp(confint(multivariable_model_female)[, 2])  # Extract and exponentiate the upper bound of the CI
  } else {
    rep(NA, length(hr_values_female))
  }
  
  # Combine the univariate and multivariable HRs and CIs into a single row
  combined_row_female <- c(univariate_hr_female = hr_values_female, multivariable_hr_female = multivariable_hr_female)
  lower_95CI_row_female <- c(univariate_lower_95CI_female = lower_95CI_female, multivariable_lower_95CI_female = multivariable_lower_95CI_female)
  upper_95CI_row_female <- c(univariate_upper_95CI_female = upper_95CI_female, multivariable_upper_95CI_female = multivariable_upper_95CI_female)
  
  # Store the rows in the respective lists
  cox_point_estimates_female[[i]] <- combined_row_female
  lower_95CI_list_female[[i]] <- lower_95CI_row_female
  upper_95CI_list_female[[i]] <- upper_95CI_row_female
}

# Combine all rows into dataframes
cox_point_estimates_df_female <- do.call(rbind, cox_point_estimates_female)
lower_95CI_df_female <- do.call(rbind, lower_95CI_list_female)
upper_95CI_df_female <- do.call(rbind, upper_95CI_list_female)

# Assign column names to the dataframes
first_non_null_female <- all_cox_results_female[[which(!sapply(all_cox_results_female, is.null))[1]]]
colnames(cox_point_estimates_df_female) <- c(
  paste0("univariate_", names(first_non_null_female$univariate_female)),
  paste0("multivariable_", names(first_non_null_female$multivariable_female$coefficients))
)
colnames(lower_95CI_df_female) <- colnames(cox_point_estimates_df_female)
colnames(upper_95CI_df_female) <- colnames(cox_point_estimates_df_female)

# Calculate the median, lower, and upper bounds for the hazard ratios and confidence intervals
summary_table_female <- data.frame(
  Variable = colnames(cox_point_estimates_df_female),
  Median_HR = apply(cox_point_estimates_df_female, 2, median, na.rm = TRUE),
  Lower_95CI = apply(lower_95CI_df_female, 2, median, na.rm = TRUE),
  Upper_95CI = apply(upper_95CI_df_female, 2, median, na.rm = TRUE)
)

# View the resulting summary table
View(summary_table_female)

# Save the summary table to a CSV file
write.csv(summary_table_female, "cox_regression_summary_table_female.csv", row.names = FALSE)

# same analysis on long dataframes

# Loop over all 1000 dataframes in the list
for (i in 1:length(processed_dataframes_long)) {
  cat("Processing dataframe", i, "of", length(processed_dataframes_long), "\n")
  
  # Load the current dataframe
  midpoint_dataframe <- processed_dataframes_long[[i]]
  
  # Replace midpoint_year with NA if hcv_test_rslt is negative
  midpoint_dataframe <- midpoint_dataframe %>%
    mutate(
      midpoint_year = ifelse(hcv_test_rslt == 0, NA, midpoint_year)  # Replace midpoint_year with NA if hcv_test_rslt == 0
    )
  
  # Create a dataframe with rows for years 2013 to 2022 and calculate cases and years_at_risk
  yearly_data <- midpoint_dataframe %>%
    group_by(year) %>%
    summarise(
      cases = sum(hcv_test_rslt, na.rm = TRUE),        # Sum of hcv_test_rslt for each year
      years_at_risk = sum(time_at_risk, na.rm = TRUE)  # Sum of time_at_risk for each year
    ) %>%
    filter(year %in% 2013:2022)  # Ensure only rows for years 2013 to 2022 are included
  
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
      total_hcv_infections = sum(hcv_test_rslt, na.rm = TRUE),  # Total cases
      total_person_years = sum(time_at_risk, na.rm = TRUE),     # Total person-years
      incidence_rate = (total_hcv_infections / total_person_years) * 100,  # Incidence rate per 100 person-years
      lower_bound = (total_hcv_infections / total_person_years) * 100 - 
                    1.96 * sqrt(total_hcv_infections / (total_person_years^2)) * 100,  # Lower 95% CI
      upper_bound = (total_hcv_infections / total_person_years) * 100 + 
                    1.96 * sqrt(total_hcv_infections / (total_person_years^2)) * 100   # Upper 95% CI
    )
  
  # Store the results in the list
  all_two_yearly_results[[i]] <- two_yearly_results
}

# Combine all results into a single dataframe
combined_two_yearly_results <- bind_rows(all_two_yearly_results, .id = "iteration")

# Save the combined results to a CSV file
write.csv(combined_two_yearly_results, "combined_two_yearly_results.csv", row.names = FALSE)

# View the combined results
View(combined_two_yearly_results)

# Calculate incidence trends over time
# Group by two-yearly intervals and calculate the median and percentiles
incidence_trends <- combined_two_yearly_results %>%
  group_by(two_year_interval) %>%
  summarise(
    median_incidence_rate = median(incidence_rate, na.rm = TRUE),  # Median incidence rate
    lower_bound = quantile(incidence_rate, 0.025, na.rm = TRUE),  # 2.5th percentile
    upper_bound = quantile(incidence_rate, 0.975, na.rm = TRUE),  # 97.5th percentile
    median_total_person_years = median(total_person_years, na.rm = TRUE),  # Median total person-years
    median_total_hcv_infections = median(total_hcv_infections, na.rm = TRUE)  # Median total HCV infections
  )

# View the incidence trends
print(incidence_trends)
View(incidence_trends)

# Save the incidence trends to a CSV file
write.csv(incidence_trends, "incidence_trends.csv", row.names = FALSE)

# Create a plot for the incidence trends
HCV_incidence_trends_plot <- ggplot(incidence_trends, aes(x = two_year_interval, y = median_incidence_rate)) +
  geom_line(group = 1, color = "gray", linewidth = 0.8, linetype = "solid") +  # Solid gray line for trends
  geom_point(shape = 18, size = 4, color = "gray") +  # Gray diamonds for points
  geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound), width = 0.1, color = "black", size = 0.8) +  # Black error bars
  theme_minimal(base_size = 14) +  # Minimal theme
  labs(
    x = "Two-Yearly Interval",
    y = "Median Incidence Rate (per 100 Person-Years)"
  ) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, max(incidence_trends$upper_bound, na.rm = TRUE) * 1.1)) +  # Adjust y-axis limits
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for better readability
    axis.title.x = element_text(margin = margin(t = 10)),  # Add margin to x-axis title
    axis.title.y = element_text(margin = margin(r = 10)),  # Add margin to y-axis title
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    panel.background = element_rect(fill = "white", color = NA),  # Set panel background to white
    plot.background = element_rect(fill = "white", color = NA)  # Set plot background to white
  )

# Save the plot as a PNG file
ggsave("plots/HCV_incidence_trends_plot.png", plot = HCV_incidence_trends_plot, width = 10, height = 6, dpi = 300)

## Cox regression analysis

View(processed_dataframes_long[[1]])
View(processed_dataframes[[1]])
