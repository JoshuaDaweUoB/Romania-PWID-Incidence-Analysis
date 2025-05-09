## load packages
pacman::p_load(dplyr, tidyr, withr, lubridate, MASS, writexl, readxl, arsenal, survival, broom, ggplot2)

## set wd
setwd("C:/Users/vl22683/OneDrive - University of Bristol/Documents/Publications/Romania PWID/data")

## load data
midpoint_dataframe <- read.csv("romania_pwid_hcv_test.csv")

# Ensure appointment_dte and appointment_dte_lag are in Date format
midpoint_dataframe <- midpoint_dataframe %>%
  mutate(
    appointment_dte = as.Date(appointment_dte, format = "%Y-%m-%d"),
    appointment_dte_lag = as.Date(appointment_dte_lag, format = "%Y-%m-%d")
  )

# Calculate person-years for each year of observation
person_years_df <- midpoint_dataframe %>%
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

# Merge the person_years_df with the original dataframe
midpoint_dataframe <- bind_cols(midpoint_dataframe, person_years_df)

# Ensure appointment_dte and appointment_dte_lag are in Date format
midpoint_dataframe <- midpoint_dataframe %>%
  mutate(
    appointment_dte = as.Date(appointment_dte, format = "%Y-%m-%d"),
    appointment_dte_lag = as.Date(appointment_dte_lag, format = "%Y-%m-%d")
  )

# Create the midpoint_year column
midpoint_dataframe <- midpoint_dataframe %>%
  mutate(
    midpoint_date = as.Date((as.numeric(appointment_dte) + as.numeric(appointment_dte_lag)) / 2, origin = "1970-01-01"),
    midpoint_year = year(midpoint_date)  # Extract the year from the midpoint_date
  )

# Define the years for which we need to create columns
required_years <- 2013:2022

# Ensure all required columns are present
for (year in required_years) {
  column_name <- paste0("hcv_test_", year)
  if (!(column_name %in% names(midpoint_dataframe))) {
    midpoint_dataframe[[column_name]] <- 0  # Initialize the column with 0
  }
}

# Populate the hcv_test_20xx columns based on midpoint_year and hcv_test_rslt
for (year in required_years) {
  column_name <- paste0("hcv_test_", year)
  midpoint_dataframe[[column_name]] <- ifelse(
    !is.na(midpoint_dataframe$midpoint_year) &  # Ensure midpoint_year is not NA
    midpoint_dataframe$midpoint_year == year & 
    midpoint_dataframe$hcv_test_rslt == 1,
    1,
    midpoint_dataframe[[column_name]]  # Retain the existing value if the condition is not met
  )
}

# View the updated dataframe
View(midpoint_dataframe)

# Create a new column 'year' by extracting the year from 'appointment_dte_lag'
midpoint_dataframe <- midpoint_dataframe %>%
  mutate(year = as.numeric(format(appointment_dte_lag, "%Y")))

# Convert 'year' to a factor to treat it as a categorical variable
midpoint_dataframe$year <- as.factor(midpoint_dataframe$year)

# View the updated dataframe
View(midpoint_dataframe)

# Rename the existing "year" column (if it exists) to avoid duplication
if ("year" %in% colnames(midpoint_dataframe)) {
  midpoint_dataframe <- midpoint_dataframe %>%
    rename(existing_year = year)
}

# Reshape the columns X2013 to X2022 to long format
midpoint_dataframe_long <- midpoint_dataframe %>%
  pivot_longer(
    cols = starts_with("X"), 
    names_to = "year", 
    names_prefix = "X", 
    values_to = "time_at_risk"
  ) %>%
  filter(!is.na(time_at_risk))  # Remove rows where time_at_risk is NA

# Drop rows where the 'year' column is empty or contains only spaces
midpoint_dataframe_long <- midpoint_dataframe_long %>%
  filter(year != "" & !is.na(year))  # Remove rows where 'year' is empty or NA

# Recode hcv_test_rslt to 0 if the 'year' column does not equal the 'midpoint_year' column
midpoint_dataframe_long <- midpoint_dataframe_long %>%
  mutate(
    hcv_test_rslt = ifelse(year == midpoint_year, hcv_test_rslt, 0)
  )

# Recode the hcv_test_rslt_20xx columns to 0 if the column 'year' does not equal 'midpoint_year'
for (year in required_years) {
  column_name <- paste0("hcv_test_", year)
  if (column_name %in% colnames(midpoint_dataframe_long)) {
    midpoint_dataframe_long[[column_name]] <- ifelse(
      midpoint_dataframe_long$year == midpoint_dataframe_long$midpoint_year,
      midpoint_dataframe_long[[column_name]],  # Retain the existing value if the condition is met
      0  # Set to 0 if the condition is not met
    )
  }
}

# View the updated dataframe
View(midpoint_dataframe_long)

# Define two-yearly intervals
midpoint_dataframe_long <- midpoint_dataframe_long %>%
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
two_yearly_results_midpoint <- midpoint_dataframe_long %>%
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

# View the results
print(two_yearly_results_midpoint)
View(two_yearly_results_midpoint)

# Save the two-yearly results to a CSV file
write.csv(two_yearly_results_midpoint, "two_yearly_results_long_midpoint.csv", row.names = FALSE)

# Create a plot for the two-yearly interval results
HCV_incidence_plot_midpoint <- ggplot(two_yearly_results_midpoint, aes(x = two_year_interval, y = incidence_rate)) +
  geom_line(group = 1, color = "gray", linewidth = 0.8, linetype = "solid") +  # Make the line gray and adjust thickness
  geom_point(shape = 18, size = 4, color = "gray") +  # Make the diamonds (points) gray
  geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound), width = 0.1, color = "black", size = 0.8) +  # Adjust error bar width and size
  theme_minimal(base_size = 14) +  # Use a minimal theme
  labs(
    x = "Two-Yearly Interval",
    y = "Incidence Rate (per 100 Person-Years)"
  ) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, max(two_yearly_results$upper_bound, na.rm = TRUE) * 1.1)) +  # Adjust y-axis limits
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for better readability
    axis.title.x = element_text(margin = margin(t = 10)),  # Add margin to x-axis title
    axis.title.y = element_text(margin = margin(r = 10)),  # Add margin to y-axis title
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    panel.background = element_rect(fill = "white", color = NA),  # Set panel background to white
    plot.background = element_rect(fill = "white", color = NA)  # Set plot background to white
  )

# Save the plot as a PNG file in the "plots" folder with the suffix "_midpoint"
ggsave("plots/HCV_incidence_plot_midpoint_long.png", plot = HCV_incidence_plot_midpoint, width = 10, height = 6, dpi = 300)

# Calculate the incidence rate
cases <- 94
person_years <- 766.4
incidence_rate <- (cases / person_years) * 100

# Calculate the standard error
standard_error <- sqrt(cases) / person_years * 100

# Calculate the 95% confidence interval
lower_bound <- incidence_rate - (1.96 * standard_error)
upper_bound <- incidence_rate + (1.96 * standard_error)

# Print the results
cat("Incidence Rate:", incidence_rate, "per 100 person-years\n")
cat("95% CI: [", lower_bound, ", ", upper_bound, "]\n")

## sensitivity analysis for one dataframe with random imputation

# Process only the first dataframe
cat("Processing the first dataframe\n")

# Load the first dataframe
midpoint_dataframe <- processed_dataframes_long[[1]]

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

# Add incidence rates and 95% confidence intervals to the yearly data
yearly_data <- yearly_data %>%
  mutate(
    incidence_rate = (cases / years_at_risk) * 100,  # Incidence rate per 100 person-years
    standard_error = sqrt(cases) / years_at_risk * 100,  # Standard error
    lower_bound = incidence_rate - (1.96 * standard_error),  # Lower 95% CI
    upper_bound = incidence_rate + (1.96 * standard_error)   # Upper 95% CI
  )

# Save the yearly data to a CSV file
write.csv(yearly_data, "yearly_results_first_dataframe.csv", row.names = FALSE)

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

# Save the two-yearly results to a CSV file
write.csv(two_yearly_results, "two_yearly_results_first_dataframe.csv", row.names = FALSE)

# Create the plot for the incidence trends
HCV_incidence_trends_plot <- ggplot(two_yearly_results, aes(x = two_year_interval, y = incidence_rate)) +
  geom_line(group = 1, color = "gray", linewidth = 0.8, linetype = "solid") +  # Solid gray line for trends
  geom_point(shape = 18, size = 4, color = "gray") +  # Gray diamonds for points
  geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound), width = 0.1, color = "black", size = 0.8) +  # Black error bars
  theme_minimal(base_size = 14) +  # Minimal theme
  labs(
    x = "Two-Yearly Interval",
    y = "Incidence Rate (per 100 Person-Years)"
  ) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, max(two_yearly_results$upper_bound, na.rm = TRUE) * 1.1)) +  # Adjust y-axis limits
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
ggsave("plots/HCV_incidence_trends_plot_first_dataframe.png", plot = HCV_incidence_trends_plot, width = 10, height = 6, dpi = 300)

# Print a message indicating the plot and tables have been saved
cat("Incidence trends plot and tables for the first dataframe have been saved.\n")

# Given data
cases <- 94
person_years <- 651.6350955

# Calculate incidence rate
incidence_rate <- (cases / person_years) * 100

# Calculate standard error
standard_error <- sqrt(cases) / person_years * 100

# Calculate 95% confidence interval
lower_bound <- incidence_rate - (1.96 * standard_error)
upper_bound <- incidence_rate + (1.96 * standard_error)

# Print results
cat("Incidence Rate:", round(incidence_rate, 2), "per 100 person-years\n")
cat("95% CI: [", round(lower_bound, 2), ", ", round(upper_bound, 2), "]\n")

## sensitivity analysis for >31 days and <2 years

View(processed_dataframes[[1]])

# Loop over the 1000 dataframes in processed_dataframes
processed_dataframes_s3 <- lapply(seq_along(processed_dataframes), function(i) {
  # Get the dataframe
  df <- processed_dataframes[[i]]
  
  # Filter rows where days_risk is between 31 and 730.5 (inclusive)
  df <- df %>%
    filter(days_risk >= 31 & days_risk <= 730.5)
  
  # Assign a new name with the suffix "_s3"
  assign(paste0("processed_dataframe_", i, "_s3"), df, envir = .GlobalEnv)
  
  return(df)
})

# Save the filtered dataframes to a file
saveRDS(processed_dataframes_s3, file = "processed_dataframes_s3.rds")

# Print a message indicating the filtering and saving are complete
cat("Rows with days_risk < 31 or > 730.5 have been removed, and dataframes have been saved as 'processed_dataframes_s3.rds'.\n")

# Verify that the dataframes are loaded correctly
View(processed_dataframes_s3[[1]])
View(processed_dataframes_s3[[1000]])

# Initialize a list to store the processed dataframes
processed_dataframes_long_s3 <- list()

# Loop over all dataframes in processed_dataframes_s3
for (i in 1:length(processed_dataframes_s3)) {
  cat("Processing dataframe", i, "of", length(processed_dataframes_s3), "\n")
  
  # Process each dataframe using the process_dataframe function
  processed_dataframes_long_s3[[i]] <- process_dataframe(processed_dataframes_s3[[i]])
}

# Save the list of long format processed dataframes to a file with the suffix _s3
saveRDS(processed_dataframes_long_s3, file = "processed_dataframes_long_s3.rds")

# Print a message indicating the processing is complete
cat("Processing complete. Processed dataframes saved with the suffix '_s3'.\n")

View(processed_dataframes_long_s3[[1]])

# Initialize a list to store the results for all 1000 dataframes
all_two_yearly_results <- list()

# Loop over all 1000 dataframes in processed_dataframes_long_s3
for (i in 1:length(processed_dataframes_long_s3)) {
  cat("Processing dataframe", i, "of", length(processed_dataframes_long_s3), "\n")
  
  # Load the current dataframe
  midpoint_dataframe <- processed_dataframes_long_s3[[i]]
  
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
write.csv(combined_two_yearly_results, "combined_two_yearly_results_s3.csv", row.names = FALSE)

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
write.csv(incidence_trends, "incidence_trends_s3.csv", row.names = FALSE)

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
ggsave("plots/HCV_incidence_trends_plot_s3.png", plot = HCV_incidence_trends_plot, width = 10, height = 6, dpi = 300)

# Create 1000 dataframes of summed yearly person-years and incident cases

# Initialize an empty list to store the summed dataframes
summed_dataframes_s3 <- list()

# Loop through all 1000 processed dataframes in processed_dataframes_s3
for (i in 1:1000) {
  cat("Processing summed dataframe for iteration", i, "of", 1000, "\n")
  
  # Get the processed dataframe for the current iteration
  df <- processed_dataframes_s3[[i]]
  
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
  summed_dataframes_s3[[i]] <- summed_df
}

# View the first and last summed dataframes for verification
View(summed_dataframes_s3[[1]])
View(summed_dataframes_s3[[1000]])

# Combine the dataframes from summed_dataframes_s3 into a single dataframe
final_summed_df_s3 <- bind_rows(summed_dataframes_s3)

# Print the final combined dataframe
cat("Final combined dataframe:\n")
print(head(final_summed_df_s3))
View(final_summed_df_s3)

# Create a new column hcv_test_qa which sums up all the hcv_test_20xx columns
final_summed_df_s3 <- final_summed_df_s3 %>%
  mutate(hcv_test_qa = rowSums(across(starts_with("hcv_test_20")), na.rm = TRUE))

# Add columns for overall incidence rate and 95% confidence interval
final_summed_df_s3 <- final_summed_df_s3 %>%
  mutate(
    overall_incidence_rate = (hcv_test_rslt / person_years) * 100,  # Incidence rate per 100 person-years
    standard_error = sqrt(hcv_test_rslt) / person_years * 100,      # Standard error of the incidence rate
    lower_bound_95CI = overall_incidence_rate - (1.96 * standard_error),  # Lower bound of 95% CI
    upper_bound_95CI = overall_incidence_rate + (1.96 * standard_error)   # Upper bound of 95% CI
  )

# View the final combined dataframe for QA
View(final_summed_df_s3)

# Calculate the median, 2.5th percentile, and 97.5th percentile for the overall incidence rate
median_incidence_rate <- median(final_summed_df_s3$overall_incidence_rate, na.rm = TRUE)
lower_bound_overall <- quantile(final_summed_df_s3$overall_incidence_rate, 0.025, na.rm = TRUE)
upper_bound_overall <- quantile(final_summed_df_s3$overall_incidence_rate, 0.975, na.rm = TRUE)

# Calculate the median incidence rates for each year from 2013 to 2022
yearly_medians <- sapply(2013:2022, function(year) {
  if (paste0("incidence_rate_", year) %in% colnames(final_summed_df_s3)) {
    median(final_summed_df_s3[[paste0("incidence_rate_", year)]], na.rm = TRUE)
  } else {
    NA
  }
})
cat("Yearly medians:\n")
print(yearly_medians)

yearly_lower_bounds <- sapply(2013:2022, function(year) {
  if (paste0("incidence_rate_", year) %in% colnames(final_summed_df_s3)) {
    quantile(final_summed_df_s3[[paste0("incidence_rate_", year)]], 0.025, na.rm = TRUE)
  } else {
    NA
  }
})
cat("Yearly lower bounds:\n")
print(yearly_lower_bounds)

yearly_upper_bounds <- sapply(2013:2022, function(year) {
  if (paste0("incidence_rate_", year) %in% colnames(final_summed_df_s3)) {
    quantile(final_summed_df_s3[[paste0("incidence_rate_", year)]], 0.975, na.rm = TRUE)
  } else {
    NA
  }
})
cat("Yearly upper bounds:\n")
print(yearly_upper_bounds)

median_hcv_infections <- sapply(2013:2022, function(year) {
  if (paste0("hcv_test_", year) %in% colnames(final_summed_df_s3)) {
    median(final_summed_df_s3[[paste0("hcv_test_", year)]], na.rm = TRUE)
  } else {
    NA
  }
})
cat("Median HCV infections:\n")
print(median_hcv_infections)

median_person_years <- sapply(2013:2022, function(year) {
  if (paste0("X", year) %in% colnames(final_summed_df_s3)) {
    median(final_summed_df_s3[[paste0("X", year)]], na.rm = TRUE)
  } else {
    NA
  }
})
cat("Median person-years:\n")
print(median_person_years)

# Calculate the overall median number of HCV infections and person-years
overall_median_hcv_infections <- median(rowSums(final_summed_df_s3[paste0("hcv_test_", 2013:2022)], na.rm = TRUE), na.rm = TRUE)
overall_median_person_years <- median(rowSums(final_summed_df_s3[paste0("X", 2013:2022)], na.rm = TRUE), na.rm = TRUE)

# Create a new dataframe with the overall and yearly incidence rates and lower bounds
results_df_s3 <- data.frame(
  Incidence_year = c("Overall incidence rate", as.character(2013:2022)),
  Incidence_rate = c(median_incidence_rate, yearly_medians),
  Lower_bound = c(lower_bound_overall, yearly_lower_bounds),
  Upper_bound = c(upper_bound_overall, yearly_upper_bounds),
  Median_HCV_infections = c(overall_median_hcv_infections, median_hcv_infections),
  Median_person_years = c(overall_median_person_years, median_person_years)
)

# Print the results dataframe
cat("Results dataframe:\n")
print(results_df_s3)
View(results_df_s3)

# Save the overall incidence results to a CSV file
write.csv(results_df_s3, "overall_incidence_results_df_s3.csv", row.names = TRUE)