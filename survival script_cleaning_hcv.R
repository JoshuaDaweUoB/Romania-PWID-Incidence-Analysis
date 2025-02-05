## load packages
pacman::p_load(dplyr, tidyr, withr, lubridate, MASS, writexl, readxl, arsenal, survival)

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
    person_years = days_risk / 365.25  # convert days at risk to person-years
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

# view the first and last dataframe for QA 
View(split_dataframes[["iteration_1"]])
View(split_dataframes[["iteration_1000"]])

# create dataframe of negatives
romania_pwid_hcv_test_negatives <- romania_pwid_hcv_test %>%
  filter(hcv_test_rslt == 0) %>%
  mutate(
    iteration = NA,
    random_infection_dtes = NA,
    person_years = days_risk / 365.25
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

# name each dataframe in the list
names(split_dataframes) <- paste0("iteration_", seq_along(split_dataframes))

# view the first and last dataframe for QA
View(split_dataframes[["iteration_1"]])
View(split_dataframes[["iteration_1000"]])


# Function to calculate person-years for each year of observation
calculate_person_years <- function(df) {
  df %>%
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
}

# Apply the function to each iteration
person_years_list <- lapply(split_dataframes, calculate_person_years)

















# convert time variables to numeric for all iterations
split_dataframes <- lapply(split_dataframes, function(df) {
  df <- df %>%
    mutate(
      appointment_dte = as.numeric(appointment_dte),
      appointment_dte_lag = as.numeric(appointment_dte_lag),
      random_infection_dtes = as.numeric(random_infection_dtes)
    )
  return(df)
})



# view the first and last dataframe for QA
View(split_dataframes[["iteration_1"]])
View(split_dataframes[["iteration_1000"]])

# initialize a list to store the results
results_list <- vector("list", length(split_dataframes))

# calculate total person-years and total incident cases for each dataframe
for (i in seq_along(split_dataframes)) {
  df <- split_dataframes[[i]]
  total_person_years <- sum(df$person_years, na.rm = TRUE)
  total_cases <- sum(df$hcv_test_rslt == 1, na.rm = TRUE)
  incidence_rate <- (total_cases / total_person_years) * 100
  results_list[[i]] <- data.frame(
    iteration = i,
    total_cases = total_cases,
    total_person_years = total_person_years,
    incidence_rate = incidence_rate
  )
}

# combine the results into a single dataframe
results_df <- do.call(rbind, results_list)

# calculate the median, 2.5th percentile, and 97.5th percentile for the incidence rate
median_incidence_rate <- median(results_df$incidence_rate, na.rm = TRUE)
lower_bound <- quantile(results_df$incidence_rate, 0.025, na.rm = TRUE)
upper_bound <- quantile(results_df$incidence_rate, 0.975, na.rm = TRUE)

# Summarize the model results
summary(romania_pwid_year)
















# add the uncertainty interval to the results dataframe
results_df <- results_df %>%
  mutate(
    median_incidence_rate = median_incidence_rate,
    lower_bound = lower_bound,
    upper_bound = upper_bound
  )

# view the results dataframe for QA
print(head(results_df))
View(results_df)

# print the uncertainty interval
cat("Median Incidence Rate (per 100 person-years):", median_incidence_rate, "\n")
cat("95% Uncertainty Interval:", lower_bound, "-", upper_bound, "\n")



# Combine all iterations into a single dataframe
combined_df <- bind_rows(split_dataframes)

# Ensure the necessary columns are present
combined_df <- combined_df %>%
  filter(!is.na(midpoint_year)) %>%
  mutate(
    hcv_test_rslt = as.numeric(hcv_test_rslt == 1)  # Convert to binary outcome
  )

# Fit the generalized linear model with a negative binomial distribution
model <- glm.nb(hcv_test_rslt ~ midpoint_year + offset(log(person_years)), data = combined_df)

# Summarize the model results
summary(model)

# Print the model coefficients
cat("Model Coefficients:\n")
print(coef(model))

# Calculate the incidence rate ratio (IRR) and 95% confidence intervals
exp_coef <- exp(coef(model))
conf_int <- exp(confint(model))

cat("Incidence Rate Ratio (IRR) and 95% Confidence Intervals:\n")
print(data.frame(IRR = exp_coef, Lower_CI = conf_int[, 1], Upper_CI = conf_int[, 2]))