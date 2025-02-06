## load packages
pacman::p_load(dplyr, tidyr, withr, lubridate, MASS, writexl, readxl, arsenal, survival, broom, ggplot2)

## set wd
setwd("C:/Users/vl22683/OneDrive - University of Bristol/Documents/Publications/Romania PWID/data")

## load data
romania_pwid_raw <- read_excel("ARAS DATA IDU 2013-2022.xlsx") 

## baseline HCV cohort

# remove rows where hiv test result is missing
romania_pwid_hcv <- romania_pwid_raw[!is.na(romania_pwid_raw$hcv_test_rslt), ]

# remove rows where hiv test result is indeterminate 
romania_pwid_hcv <- romania_pwid_hcv %>%
  filter(!hcv_test_rslt == 3)

# create sequence of visits by ID
romania_pwid_hcv <- romania_pwid_hcv %>%
  group_by(id) %>%
  arrange(id, appointment_dte) %>%
  mutate(appointment_seq = row_number())

romania_pwid_hcv <- ungroup(romania_pwid_hcv)

# HCV test results by visit
romania_pwid_hcv_summary <- table(romania_pwid_hcv$hcv_test_rslt)
print(romania_pwid_hcv_summary) 

# remove IDs where hcv positive at baseline
ids_to_remove <- romania_pwid_hcv %>%
  filter(appointment_seq == 1 & hcv_test_rslt == 2) %>%
  pull(id)

romania_pwid_hcv <- romania_pwid_hcv %>%
  filter(!(id %in% ids_to_remove))

# HCV test results by visit
romania_pwid_hcv_summary <- table(romania_pwid_hcv$appointment_seq, romania_pwid_hcv$hcv_test_rslt)
print(romania_pwid_hcv_summary) 

# restrict to participants with multiple tests
romania_pwid_hcv <- romania_pwid_hcv %>%
  group_by(id) %>%
  arrange(id) %>%
  mutate(hcv_test_seq = row_number())

# HCV test results by visit
romania_pwid_hcv_summary <- table(romania_pwid_hcv$hcv_test_seq, romania_pwid_hcv$hcv_test_rslt)
print(romania_pwid_hcv_summary) 

# remove participants with only one test
romania_pwid_hcv <- romania_pwid_hcv %>%
  group_by(id) %>%
  filter(!(max(hcv_test_seq, na.rm = TRUE) == 1)) %>%
  ungroup()

# HCV test results by visit
romania_pwid_hcv_summary <- table(romania_pwid_hcv$hcv_test_seq, romania_pwid_hcv$hcv_test_rslt)
print(romania_pwid_hcv_summary) 

# remove hcv tests after first positive
romania_pwid_hcv <- romania_pwid_hcv %>%
  group_by(id) %>%
  mutate(first_hcv_positive_dte = ifelse(hcv_test_rslt == 2, appointment_dte, NA)) %>%
  mutate(first_hcv_positive_dte = min(first_hcv_positive_dte, na.rm = TRUE)) %>%
  ungroup()

romania_pwid_hcv <- romania_pwid_hcv %>%
  filter(is.na(first_hcv_positive_dte) | appointment_dte <= first_hcv_positive_dte)

# HCV test results by visit
romania_pwid_hcv_summary <- table(romania_pwid_hcv$hcv_test_seq, romania_pwid_hcv$hcv_test_rslt)
print(romania_pwid_hcv_summary) 

# create hcv testing dataframe 
romania_pwid_hcv_test <- subset(romania_pwid_hcv, select = c(id, appointment_dte, hcv_test_seq, hcv_test_rslt)) 

# create lag of appointment dates and hcv tests
romania_pwid_hcv_test <- romania_pwid_hcv_test %>%
  arrange(id, hcv_test_seq) %>%  
  group_by(id) %>%
  mutate(appointment_dte_lag = lead(appointment_dte),
         hcv_test_rslt_lag = lead(hcv_test_rslt)) 

# remove empty rows
romania_pwid_hcv_test <- romania_pwid_hcv_test[!is.na(romania_pwid_hcv_test$hcv_test_rslt_lag), ]

# convert to dates
romania_pwid_hcv_test <- romania_pwid_hcv_test %>%
  mutate(appointment_dte = as.Date(appointment_dte, format = "%d-%m-%Y")) %>%
  mutate(appointment_dte_lag = as.Date(appointment_dte_lag, format = "%d-%m-%Y"))

# days at risk
romania_pwid_hcv_test <- romania_pwid_hcv_test %>%
  mutate(days_risk = appointment_dte_lag-appointment_dte)

# change test results to 0 and 1
romania_pwid_hcv_test <- romania_pwid_hcv_test %>%
  mutate(hcv_test_rslt_lag = case_when(
    hcv_test_rslt_lag == 1 ~ 0,
    hcv_test_rslt_lag == 2 ~ 1,
    TRUE ~ hcv_test_rslt_lag
  ))
romania_pwid_hcv_test <- romania_pwid_hcv_test %>%
  mutate(hcv_test_rslt = case_when(
    hcv_test_rslt == 1 ~ 0,
    hcv_test_rslt == 2 ~ 1,
    TRUE ~ hcv_test_rslt
  ))

# rename hcv variables
romania_pwid_hcv_test <- romania_pwid_hcv_test %>%
  rename(
    hcv_baseline = hcv_test_rslt,
    hcv_test_rslt = hcv_test_rslt_lag
  )

# QA for rows where appointment_dte_lag is less than appointment_dte
invalid_rows <- romania_pwid_hcv_test %>%
  filter(appointment_dte_lag < appointment_dte)
cat("Number of rows where appointment_dte_lag is less than appointment_dte:", nrow(invalid_rows), "\n")

#### random-point sampling with 1000 iterations approach ####

# generate random infection dates
romania_pwid_hcv_test_iterations <- romania_pwid_hcv_test %>%
  filter(hcv_test_rslt == 1) %>%
  rowwise() %>%
  mutate(
    # generate 1000 random infection dates
    random_infection_dtes = list(
      as.Date(
        runif(1000,
              min = as.numeric(appointment_dte),
              max = as.numeric(appointment_dte_lag)),
        origin = "1970-01-01"
      )
    )
  ) %>%
  unnest_longer(random_infection_dtes) %>%
  group_by(id) %>%
  mutate(
    iteration = rep(1:1000, each = n() / 1000),  # create iteration groups
    days_risk = as.numeric(random_infection_dtes - appointment_dte),  # days at risk
    person_years = days_risk / 365.25,  # convert days at risk to person-years
    midpoint_year = year(random_infection_dtes),  # extract the year from random_infection_dtes
    appointment_dte_lag = random_infection_dtes  # set appointment_dte_lag to random_infection_dtes
  ) %>%
  ungroup()

# check for rows where appointment_dte_lag is less than appointment_dte
invalid_rows <- romania_pwid_hcv_test_iterations %>%
  filter(appointment_dte_lag < appointment_dte)
cat("Number of rows where appointment_dte_lag is less than appointment_dte:", nrow(invalid_rows), "\n")

# split each iteration into a separate dataframe
split_dataframes <- split(romania_pwid_hcv_test_iterations, romania_pwid_hcv_test_iterations$iteration)

# name each dataframe in the list
names(split_dataframes) <- paste0("iteration_", seq_along(split_dataframes))

# create dataframe of negatives
romania_pwid_hcv_test_negatives <- romania_pwid_hcv_test %>%
  filter(hcv_test_rslt == 0) %>%
  mutate(
    iteration = NA,
    random_infection_dtes = NA,
    person_years = days_risk / 365.25,
    midpoint_year = NA
  )

# QA for rows where appointment_dte_lag is less than appointment_dte
invalid_rows <- romania_pwid_hcv_test_negatives %>%
  filter(appointment_dte_lag < appointment_dte)
cat("Number of rows where appointment_dte_lag is less than appointment_dte:", nrow(invalid_rows), "\n")

# append the negatives dataframe to each of the 1000 dataframes
split_dataframes <- lapply(split_dataframes, function(df) {
  combined_df <- rbind(df, romania_pwid_hcv_test_negatives)
  return(combined_df)
})

# view the first and last dataframe for QA 
View(split_dataframes[["iteration_1"]])
View(split_dataframes[["iteration_1000"]])

# Initialize an empty list to store the results
processed_dataframes <- list()

# Define the years for which we need to create columns
required_years <- 2013:2022

# Loop through the first 10 iterations
for (i in 1:1000) {
  cat("Processing iteration", i, "of", 1000, "\n")
  
  # Get the dataframe for the current iteration
  df <- split_dataframes[[i]]
  
  # Print the initial dataframe for debugging
  cat("Initial dataframe:\n")
  print(head(df))
  
  # Calculate person-years for each year of observation
  person_years_df <- df %>%
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
  
  # Print the person_years_df for debugging
  cat("Person years dataframe:\n")
  print(head(person_years_df))
  
  # Check if person_years_df is empty
  if (nrow(person_years_df) == 0) {
    cat("person_years_df is empty for iteration", i, "\n")
    processed_dataframes[[i]] <- NULL
    next
  }
  
  # Merge the person_years_df with the original dataframe
  df <- bind_cols(df, person_years_df)
  
  # Ensure all required columns are present
  for (year in required_years) {
    column_name <- paste0("hcv_test_", year)
    if (!(column_name %in% names(df))) {
      df[[column_name]] <- 0
    }
  }
  
  # Populate the hcv_test_20xx columns based on midpoint_year and hcv_test_rslt
  for (year in required_years) {
    column_name <- paste0("hcv_test_", year)
    df[[column_name]] <- ifelse(df$midpoint_year == year & df$hcv_test_rslt == 1, 1, df[[column_name]])
  }
  
  # Ensure all required person-year columns are present
  for (year in required_years) {
    if (!(as.character(year) %in% names(df))) {
      df[[as.character(year)]] <- 0
    }
  }
  
  # Print the final dataframe for debugging
  cat("Final dataframe:\n")
  print(head(df))
  
  # Store the processed dataframe in the list
  processed_dataframes[[i]] <- df
}

# create year variable
for (i in 1:length(processed_dataframes)) {
  # Get the processed dataframe for the current iteration
  df <- processed_dataframes[[i]]
  
  # Create a new column 'year' by extracting the year from 'appointment_dte_lag'
  df <- df %>%
    mutate(year = as.numeric(format(appointment_dte_lag, "%Y")))
  
  # Convert 'year' to a factor to treat it as a categorical variable
  df$year <- as.factor(df$year)
  
  # Store the updated dataframe back in the list
  processed_dataframes[[i]] <- df
}

# View the first dataframe for QA
View(processed_dataframes[[1]])

# View the last dataframe for QA
View(processed_dataframes[[1000]])

# Get the first processed dataframe
df <- processed_dataframes[[1]]

# Create a histogram of the person_years column
ggplot(df, aes(x = person_years)) +
  geom_histogram(binwidth = 0.1, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Person Years",
       x = "Person Years",
       y = "Frequency") +
  theme_minimal()

# analysis with packages

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