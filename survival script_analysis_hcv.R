## load packages
pacman::p_load(tidyr, withr, lubridate, MASS, writexl, readxl, arsenal, survival, broom, ggplot2, dplyr)

## set wd
setwd("C:/Users/vl22683/OneDrive - University of Bristol/Documents/Publications/Romania PWID/data")

## longitudinal analysis

# function to process each dataframe
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

# Verify that the dataframes are loaded correctly
View(processed_dataframes[[1]])
View(processed_dataframes[[1000]])

# Initialize a list to store the processed dataframes
processed_dataframes_long <- list()

# Loop over all dataframes in processed_dataframes
for (i in 1:length(processed_dataframes)) {
  cat("Processing dataframe", i, "of", length(processed_dataframes), "\n")
  processed_dataframes_long[[i]] <- process_dataframe(processed_dataframes[[i]])
}

# Save the list of long format processed dataframes to a file
saveRDS(processed_dataframes_long, file = "processed_dataframes_long.rds")

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
median_hcv_infections <- sapply(2013:2022, function(year) {
  median(final_summed_df[[paste0("hcv_test_", year)]], na.rm = TRUE)
})
median_person_years <- sapply(2013:2022, function(year) {
  median(final_summed_df[[paste0("X", year)]], na.rm = TRUE)
})

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
    x = "Time Interval",
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





