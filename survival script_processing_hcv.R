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

# Save testing data
write.csv(romania_pwid_hcv_test, "romania_pwid_hcv_test.csv")

#### random-point sampling with 1000 iterations approach ####

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

View(person_years_df)

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

# Save the list of processed dataframes to a file
saveRDS(processed_dataframes, file = "processed_dataframes.rds")

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
