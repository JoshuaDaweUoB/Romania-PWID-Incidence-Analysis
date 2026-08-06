## load packages
pacman::p_load(dplyr, tidyr, withr, lubridate, MASS, writexl, readxl, arsenal, survival, broom, ggplot2)

## set wd
setwd("C:/Users/vl22683/OneDrive - University of Bristol/Documents/Publications/Romania PWID/data")

## load data
midpoint_dataframe <- read.csv("romania_pwid_hcv_test.csv")

# appointment_dte and appointment_dte_lag in date format
midpoint_dataframe <- midpoint_dataframe %>%
  mutate(
    appointment_dte = as.Date(appointment_dte, format = "%Y-%m-%d"),
    appointment_dte_lag = as.Date(appointment_dte_lag, format = "%Y-%m-%d")
  )

# person-years for each year of observation
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

# merge person_years_df with the original dataframe
midpoint_dataframe <- bind_cols(midpoint_dataframe, person_years_df)

# appointment_dte and appointment_dte_lag in date format
midpoint_dataframe <- midpoint_dataframe %>%
  mutate(
    appointment_dte = as.Date(appointment_dte, format = "%Y-%m-%d"),
    appointment_dte_lag = as.Date(appointment_dte_lag, format = "%Y-%m-%d")
  )

# midpoint_year column
midpoint_dataframe <- midpoint_dataframe %>%
  mutate(
    midpoint_date = as.Date((as.numeric(appointment_dte) + as.numeric(appointment_dte_lag)) / 2, origin = "1970-01-01"),
    midpoint_year = year(midpoint_date)
  )

# years for columns
required_years <- 2013:2022

# required columns are present
for (year in required_years) {
  column_name <- paste0("hcv_test_", year)
  if (!(column_name %in% names(midpoint_dataframe))) {
    midpoint_dataframe[[column_name]] <- 0
  }
}

# hcv_test_20xx columns based on midpoint_year and hcv_test_rslt
for (year in required_years) {
  column_name <- paste0("hcv_test_", year)
  midpoint_dataframe[[column_name]] <- ifelse(
    !is.na(midpoint_dataframe$midpoint_year) &  # Ensure midpoint_year is not NA
    midpoint_dataframe$midpoint_year == year & 
    midpoint_dataframe$hcv_test_rslt == 1,
    1,
    midpoint_dataframe[[column_name]]
  )
}

# create year column
midpoint_dataframe <- midpoint_dataframe %>%
  mutate(year = as.numeric(format(appointment_dte_lag, "%Y")))

# year to factor
midpoint_dataframe$year <- as.factor(midpoint_dataframe$year)

# rename year
if ("year" %in% colnames(midpoint_dataframe)) {
  midpoint_dataframe <- midpoint_dataframe %>%
    rename(existing_year = year)
}

# reshape columns X2013 to X2022 to long format
midpoint_dataframe_long <- midpoint_dataframe %>%
  pivot_longer(
    cols = starts_with("X"), 
    names_to = "year", 
    names_prefix = "X", 
    values_to = "time_at_risk"
  ) %>%
  filter(!is.na(time_at_risk))

# drop if year is empty
midpoint_dataframe_long <- midpoint_dataframe_long %>%
  filter(year != "" & !is.na(year))

# recode hcv_test_rslt
midpoint_dataframe_long <- midpoint_dataframe_long %>%
  mutate(
    hcv_test_rslt = ifelse(year == midpoint_year, hcv_test_rslt, 0)
  )

# recode hcv_test_rslt_20xx
for (year in required_years) {
  column_name <- paste0("hcv_test_", year)
  if (column_name %in% colnames(midpoint_dataframe_long)) {
    midpoint_dataframe_long[[column_name]] <- ifelse(
      midpoint_dataframe_long$year == midpoint_dataframe_long$midpoint_year,
      midpoint_dataframe_long[[column_name]],
      0
    )
  }
}

# two-yearly intervals
midpoint_dataframe_long <- midpoint_dataframe_long %>%
  mutate(
    two_year_interval = case_when(
      year %in% c(2013, 2014) ~ "2013-2014",
      year %in% c(2015, 2016) ~ "2015-2016",
      year %in% c(2017, 2018) ~ "2017-2018",
      year %in% c(2019, 2020) ~ "2019-2020",
      year %in% c(2021, 2022) ~ "2021-2022",
      TRUE ~ NA_character_
    )
  )

# group two-year intervals and calculate totals
two_yearly_results_midpoint <- midpoint_dataframe_long %>%
  filter(!is.na(two_year_interval)) %>%
  group_by(two_year_interval) %>%
  summarise(
    total_hcv_infections = sum(hcv_test_rslt, na.rm = TRUE),
    total_person_years = sum(time_at_risk, na.rm = TRUE),
    incidence_rate = (total_hcv_infections / total_person_years) * 100,
    lower_bound = (total_hcv_infections / total_person_years) * 100 - 
                  1.96 * sqrt(total_hcv_infections / (total_person_years^2)) * 100,
    upper_bound = (total_hcv_infections / total_person_years) * 100 + 
                  1.96 * sqrt(total_hcv_infections / (total_person_years^2)) * 100
  )

# results
print(two_yearly_results_midpoint)

# save two-yearly results
write.csv(two_yearly_results_midpoint, "two_yearly_results_long_midpoint_hcv.csv", row.names = FALSE)

# plot for the two-yearly interval results
HCV_incidence_plot_midpoint <- ggplot(two_yearly_results_midpoint, aes(x = two_year_interval, y = incidence_rate)) +
  geom_line(group = 1, color = "gray", linewidth = 0.8, linetype = "solid") + 
  geom_point(shape = 18, size = 4, color = "gray") + 
  geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound), width = 0.1, color = "black", size = 0.8) +
  theme_minimal(base_size = 14) +
  labs(
    x = "Two-Yearly Interval",
    y = "Incidence Rate (per 100 Person-Years)"
  ) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, max(two_yearly_results_midpoint$upper_bound, na.rm = TRUE) * 1.1)) +  # Adjust y-axis limits
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), 
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

# save the plot
ggsave("plots/HCV_incidence_plot_midpoint_long.png", plot = HCV_incidence_plot_midpoint, width = 10, height = 6, dpi = 300)

## sensitivity analysis for one dataframe with random imputation

# load the first dataframe
processed_dataframes_long <- readRDS("processed_dataframes_long_hcv.rds")
midpoint_dataframe <- processed_dataframes_long[[1]]

# replace midpoint_year with NA if hcv_test_rslt is negative
midpoint_dataframe <- midpoint_dataframe %>%
  mutate(
    midpoint_year = ifelse(hcv_test_rslt == 0, NA, midpoint_year)
  )

# create a dataframe with rows for years 2013 to 2022 and calculate cases and years_at_risk
yearly_data <- midpoint_dataframe %>%
  group_by(year) %>%
  summarise(
    cases = sum(hcv_test_rslt, na.rm = TRUE),
    years_at_risk = sum(time_at_risk, na.rm = TRUE)
  ) %>%
  filter(year %in% 2013:2022)

# incidence rates and 95% confidence intervals
yearly_data <- yearly_data %>%
  mutate(
    incidence_rate = (cases / years_at_risk) * 100,
    standard_error = sqrt(cases) / years_at_risk * 100,
    lower_bound = incidence_rate - (1.96 * standard_error),
    upper_bound = incidence_rate + (1.96 * standard_error)
  )

# save yearly data
write.csv(yearly_data, "yearly_results_first_dataframe_hcv.csv", row.names = FALSE)

# two-yearly intervals
midpoint_dataframe <- midpoint_dataframe %>%
  mutate(
    two_year_interval = case_when(
      year %in% c(2013, 2014) ~ "2013-2014",
      year %in% c(2015, 2016) ~ "2015-2016",
      year %in% c(2017, 2018) ~ "2017-2018",
      year %in% c(2019, 2020) ~ "2019-2020",
      year %in% c(2021, 2022) ~ "2021-2022",
      TRUE ~ NA_character_
    )
  )

# calculate two-year intervals totals
two_yearly_results <- midpoint_dataframe %>%
  filter(!is.na(two_year_interval)) %>%
  group_by(two_year_interval) %>%
  summarise(
    total_hcv_infections = sum(hcv_test_rslt, na.rm = TRUE),
    total_person_years = sum(time_at_risk, na.rm = TRUE),
    incidence_rate = (total_hcv_infections / total_person_years) * 100,
    lower_bound = (qchisq(0.025, 2 * total_hcv_infections) / 2) / total_person_years * 100,
    upper_bound = (qchisq(0.975, 2 * (total_hcv_infections + 1)) / 2) / total_person_years * 100
  )

# save two-yearly results
write.csv(two_yearly_results, "two_yearly_results_first_dataframe_hcv.csv", row.names = FALSE)

# plot the incidence trends
HCV_incidence_trends_plot <- ggplot(two_yearly_results, aes(x = two_year_interval, y = incidence_rate)) +
  geom_line(group = 1, color = "gray", linewidth = 0.8, linetype = "solid") + 
  geom_point(shape = 18, size = 4, color = "gray") +
  geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound), width = 0.1, color = "black", size = 0.8) +
  theme_minimal(base_size = 14) +
  labs(
    x = "Two-Yearly Interval",
    y = "Incidence Rate (per 100 Person-Years)"
  ) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, max(two_yearly_results$upper_bound, na.rm = TRUE) * 1.1)) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

# save plot
ggsave("plots/HCV_incidence_trends_plot_first_dataframe.png", plot = HCV_incidence_trends_plot, width = 10, height = 6, dpi = 300)

## sensitivity analysis for >31 days and <2 years

# load data
processed_dataframes_hcv <- readRDS("processed_dataframes_hcv.rds")

# create filtered list
processed_dataframes_hcv_s3 <- lapply(processed_dataframes_hcv, function(df) {

  df %>%
    filter(days_risk >= 31 & days_risk <= 730.5)

})

# save _s3 dataframes
saveRDS(processed_dataframes_hcv_s3, file = "processed_dataframes_hcv_s3.rds")

# load _s3 data
processed_dataframes_hcv_s3 <- readRDS("processed_dataframes_hcv_s3.rds")

# list for processed dataframes
processed_dataframes_hcv_long_s3 <- list()

# function to reshape dataframes
process_dataframe <- function(df) {
  # rename year column
  if ("year" %in% colnames(df)) {
    df <- df %>%
      rename(existing_year = year)
  }
  
  # reshape the columns X2013 to X2022 to long format
  df_long <- df %>%
    pivot_longer(cols = starts_with("X"), 
                 names_to = "year", 
                 names_prefix = "X", 
                 values_to = "time_at_risk") %>%
    filter(!is.na(time_at_risk))
  
  # recode hcv_test_rslt to 0 when it is invalid, NA, or year does not equal midpoint_year
  df_long <- df_long %>%
    mutate(hcv_test_rslt = ifelse(is.na(hcv_test_rslt) | !is.numeric(hcv_test_rslt), 0,
                                ifelse(year == midpoint_year, hcv_test_rslt, NA)))
  # keep only the specified columns
  df_long <- df_long %>%
    dplyr::select(id, hcv_test_rslt, appointment_dte, appointment_dte_lag, year, midpoint_year, time_at_risk)
  
  # sort by id and then by year
  df_long <- df_long %>%
    arrange(id, year)
  
  return(df_long)
}

# loop over processed_dataframes_hcv_s3
for (i in 1:length(processed_dataframes_hcv_s3)) {
  cat("Processing dataframe", i, "of", length(processed_dataframes_hcv_s3), "\n")
  
  # process each dataframe
  processed_dataframes_hcv_long_s3[[i]] <- process_dataframe(processed_dataframes_hcv_s3[[i]])
}

# save long format processed dataframes to a file with the suffix _s3
saveRDS(processed_dataframes_hcv_long_s3, file = "processed_dataframes_long_hcv_s3.rds")

# list for all 10000 dataframes
all_two_yearly_results_hcv <- list()

# loop over 10000 dataframes
for (i in 1:length(processed_dataframes_hcv_long_s3)) {
  cat("Processing dataframe", i, "of", length(processed_dataframes_hcv_long_s3), "\n")
  
  # load dataframe
  midpoint_dataframe <- processed_dataframes_hcv_long_s3[[i]]
  
  # midpoint_year with NA if hcv_test_rslt is negative
  midpoint_dataframe <- midpoint_dataframe %>%
    mutate(
      midpoint_year = ifelse(hcv_test_rslt == 0, NA, midpoint_year)
    )
  
  # dataframe with rows for years 2013 to 2022 and calculate cases and years_at_risk
  yearly_data <- midpoint_dataframe %>%
    group_by(year) %>%
    summarise(
      cases = sum(hcv_test_rslt, na.rm = TRUE),
      years_at_risk = sum(time_at_risk, na.rm = TRUE)
    ) %>%
    filter(year %in% 2013:2022)
  
  # two-yearly intervals
  midpoint_dataframe <- midpoint_dataframe %>%
    mutate(
      two_year_interval = case_when(
        year %in% c(2013, 2014) ~ "2013-2014",
        year %in% c(2015, 2016) ~ "2015-2016",
        year %in% c(2017, 2018) ~ "2017-2018",
        year %in% c(2019, 2020) ~ "2019-2020",
        year %in% c(2021, 2022) ~ "2021-2022",
        TRUE ~ NA_character_ 
      )
    )

  # two-year intervals and totals
    two_yearly_results <- midpoint_dataframe %>%
      filter(!is.na(two_year_interval)) %>%
      group_by(two_year_interval) %>%
      summarise(
        total_hcv_infections = sum(hcv_test_rslt, na.rm = TRUE), 
        total_person_years = sum(time_at_risk, na.rm = TRUE),
        incidence_rate = (total_hcv_infections / total_person_years) * 100,
        lower_bound = (qchisq(0.025, 2 * total_hcv_infections) / 2) / total_person_years * 100,
        upper_bound = (qchisq(0.975, 2 * (total_hcv_infections + 1)) / 2) / total_person_years * 100
      )
  
  # save results
  all_two_yearly_results_hcv[[i]] <- two_yearly_results
}

# combine into one df
combined_two_yearly_results_hcv_s3 <- bind_rows(all_two_yearly_results_hcv, .id = "iteration")

# save to a csv file
write.csv(combined_two_yearly_results_hcv_s3, "combined_two_yearly_results_hcv_s3.csv", row.names = FALSE)

# view results
View(combined_two_yearly_results_hcv_s3)

# incidence trends over time

# two-yearly intervals and calculate the median and percentiles
incidence_trends_hcv_s3 <- combined_two_yearly_results_hcv_s3 %>%
  group_by(two_year_interval) %>%
  summarise(
    median_incidence_rate = median(incidence_rate, na.rm = TRUE),
    lower_bound = quantile(incidence_rate, 0.025, na.rm = TRUE),
    upper_bound = quantile(incidence_rate, 0.975, na.rm = TRUE),
    median_total_person_years = median(total_person_years, na.rm = TRUE),
    median_total_hcv_infections = median(total_hcv_infections, na.rm = TRUE)
  )

# incidence trends
print(incidence_trends_hcv_s3)
View(incidence_trends_hcv_s3)

# save incidence trends to a csv file
write.csv(incidence_trends_hcv_s3, "incidence_trends_hcv_s3.csv", row.names = FALSE)

# plot for the incidence trends
hcv_incidence_trends_plot <- ggplot(incidence_trends_hcv_s3, aes(x = two_year_interval, y = median_incidence_rate)) +
  geom_line(group = 1, color = "gray", linewidth = 0.8, linetype = "solid") +
  geom_point(shape = 18, size = 4, color = "gray") +
  geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound), width = 0.1, color = "black", size = 0.8) +
  theme_minimal(base_size = 14) +
  labs(
    x = "Two-Yearly Interval",
    y = "Median Incidence Rate (per 100 Person-Years)"
  ) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, max(incidence_trends_s3$upper_bound, na.rm = TRUE) * 1.1)) +

  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

# save the plot
ggsave("plots/hcv_incidence_trends_plot_s3.png", plot = hcv_incidence_trends_plot, width = 10, height = 6, dpi = 300)
