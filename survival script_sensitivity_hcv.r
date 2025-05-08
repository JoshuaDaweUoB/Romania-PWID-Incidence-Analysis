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