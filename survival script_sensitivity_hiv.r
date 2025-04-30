
## sensitivity analysis
midpoint_dataframe <- processed_dataframes[[1]]

# Keep only the specified columns in midpoint_dataframe
midpoint_dataframe <- midpoint_dataframe %>%
  select(
    id, 
    appointment_dte, 
    appointment_dte_lag, 
    hcv_test_rslt, 
    time_at_risk
  )

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

  # Ensure all required columns are present
  for (year in required_years) {
    column_name <- paste0("hcv_test_", year)
    if (!(column_name %in% names(df))) {
      midpoint_dataframe[[column_name]] <- 0
    }
  }
  
  # Populate the hcv_test_20xx columns based on midpoint_year and hcv_test_rslt
  for (year in required_years) {
    column_name <- paste0("hcv_test_", year)
    midpoint_dataframe[[column_name]] <- ifelse(midpoint_dataframe$midpoint_year == year & df$hcv_test_rslt == 1, 1, midpoint_dataframe[[column_name]])
  }
  
  # Ensure all required person-year columns are present
  for (year in required_years) {
    if (!(as.character(year) %in% names(df))) {
      df[[as.character(year)]] <- 0
    }
  }




# View the reshaped dataframe
View(midpoint_dataframe_long)

# Replace midpoint_year with NA if hcv_test_rslt is negative
midpoint_dataframe_long <- midpoint_dataframe_long %>%
  mutate(
    midpoint_year = ifelse(hcv_test_rslt == 0, NA, midpoint_year)  # Replace midpoint_year with NA if hcv_test_rslt == 0
  )

# Create a dataframe with rows for years 2013 to 2022 and calculate cases and years_at_risk
yearly_data <- midpoint_dataframe_long %>%
  group_by(year) %>%
  summarise(
    cases = sum(hcv_test_rslt, na.rm = TRUE),        # Sum of hcv_test_rslt for each year
    years_at_risk = sum(time_at_risk, na.rm = TRUE)  # Sum of time_at_risk for each year
  ) %>%
  filter(year %in% 2013:2022)  # Ensure only rows for years 2013 to 2022 are included

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
two_yearly_results <- midpoint_dataframe_long %>%
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
print(two_yearly_results)
View(two_yearly_results)

# Save the two-yearly results to a CSV file
write.csv(two_yearly_results, "two_yearly_results_long.csv", row.names = FALSE)

# Create a plot for the two-yearly interval results
HCV_incidence_plot_midpoint <- ggplot(two_yearly_results, aes(x = two_year_interval, y = incidence_rate)) +
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





