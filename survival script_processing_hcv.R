## load packages
pacman::p_load(dplyr, tidyr, withr, lubridate, MASS, writexl, readxl, arsenal, survival, broom, ggplot2, purrr)

## set wd
setwd("C:/Users/vl22683/OneDrive - University of Bristol/Documents/Publications/Romania PWID/data")

# ## load data
# romania_pwid_raw <- read_excel("ARAS DATA IDU 2013-2022.xlsx")

# ## baseline HCV cohort

# # remove rows where hiv test result is missing
# romania_pwid_hcv <- romania_pwid_raw[!is.na(romania_pwid_raw$hcv_test_rslt), ]

# # remove rows where hiv test result is indeterminate
# romania_pwid_hcv <- romania_pwid_hcv %>%
#   filter(!hcv_test_rslt == 3)

# # sequence by id 
# romania_pwid_hcv <- romania_pwid_hcv %>%
#   arrange(id) %>%  # Ensure rows are sorted by id
#   mutate(id_seq = cumsum(!duplicated(id)))  # Increment by 1 for each new id

# View(romania_pwid_hcv)

# # Find the highest value in the id_seq column
# highest_id_seq <- romania_pwid_hcv %>%
#   summarise(max_id_seq = max(id_seq, na.rm = TRUE))

# # Print the result
# cat("Highest value in id_seq:\n")
# print(highest_id_seq)

# # create sequence of visits by ID
# romania_pwid_hcv <- romania_pwid_hcv %>%
#   group_by(id) %>%
#   arrange(id, appointment_dte) %>%
#   mutate(appointment_seq = row_number())

# romania_pwid_hcv <- ungroup(romania_pwid_hcv)

# # HCV test results by visit
# romania_pwid_hcv_summary <- table(romania_pwid_hcv$hcv_test_rslt)
# print(romania_pwid_hcv_summary)

# # remove IDs where hcv positive at baseline
# ids_to_remove <- romania_pwid_hcv %>%
#   filter(appointment_seq == 1 & hcv_test_rslt == 2) %>%
#   pull(id)

# romania_pwid_hcv <- romania_pwid_hcv %>%
#   filter(!(id %in% ids_to_remove))

# # sequence by id 
# romania_pwid_hcv <- romania_pwid_hcv %>%
#   arrange(id) %>%  # Ensure rows are sorted by id
#   mutate(id_seq = cumsum(!duplicated(id)))  # Increment by 1 for each new id

# # Find the highest value in the id_seq column
# highest_id_seq <- romania_pwid_hcv %>%
#   summarise(max_id_seq = max(id_seq, na.rm = TRUE))

# # Print the result
# cat("Highest value in id_seq:\n")
# print(highest_id_seq)

# # HCV test results by visit
# romania_pwid_hcv_summary <- table(romania_pwid_hcv$appointment_seq, romania_pwid_hcv$hcv_test_rslt)
# print(romania_pwid_hcv_summary) 

# # restrict to participants with multiple tests
# romania_pwid_hcv <- romania_pwid_hcv %>%
#   group_by(id) %>%
#   arrange(id) %>%
#   mutate(hcv_test_seq = row_number())

# # HCV test results by visit
# romania_pwid_hcv_summary <- table(romania_pwid_hcv$hcv_test_seq, romania_pwid_hcv$hcv_test_rslt)
# print(romania_pwid_hcv_summary)

# # remove participants with only one test
# romania_pwid_hcv <- romania_pwid_hcv %>%
#   group_by(id) %>%
#   filter(!(max(hcv_test_seq, na.rm = TRUE) == 1)) %>%
#   ungroup()

# # sequence by id 
# romania_pwid_hcv <- romania_pwid_hcv %>%
#   arrange(id) %>%  # Ensure rows are sorted by id
#   mutate(id_seq = cumsum(!duplicated(id)))  # Increment by 1 for each new id

# # Find the highest value in the id_seq column
# highest_id_seq <- romania_pwid_hcv %>%
#   summarise(max_id_seq = max(id_seq, na.rm = TRUE))

# # Print the result
# cat("Highest value in id_seq:\n")
# print(highest_id_seq)

# # HCV test results by visit
# romania_pwid_hcv_summary <- table(romania_pwid_hcv$hcv_test_seq, romania_pwid_hcv$hcv_test_rslt)
# print(romania_pwid_hcv_summary) 

# # Remove HCV tests after the first positive
# romania_pwid_hcv <- romania_pwid_hcv %>%
#   group_by(id) %>%
#   mutate(
#     first_hcv_positive_dte = ifelse(hcv_test_rslt == 2, appointment_dte, NA),
#     first_hcv_positive_dte = if (all(is.na(first_hcv_positive_dte))) NA else min(first_hcv_positive_dte, na.rm = TRUE)
#   ) %>%
#   ungroup()

# # Filter rows to keep only those before or on the first positive test date
# romania_pwid_hcv <- romania_pwid_hcv %>%
#   filter(is.na(first_hcv_positive_dte) | appointment_dte <= first_hcv_positive_dte)

# # HCV test results by visit
# romania_pwid_hcv_summary <- table(romania_pwid_hcv$hcv_test_seq, romania_pwid_hcv$hcv_test_rslt)
# print(romania_pwid_hcv_summary)

# # create hcv testing dataframe
# romania_pwid_hcv_test <- subset(romania_pwid_hcv, select = c(id, appointment_dte, hcv_test_seq, hcv_test_rslt)) 

# # create lag of appointment dates and hcv tests
# romania_pwid_hcv_test <- romania_pwid_hcv_test %>%
#   arrange(id, hcv_test_seq) %>%
#   group_by(id) %>%
#   mutate(appointment_dte_lag = lead(appointment_dte),
#          hcv_test_rslt_lag = lead(hcv_test_rslt))

# # remove empty rows
# romania_pwid_hcv_test <- romania_pwid_hcv_test[!is.na(romania_pwid_hcv_test$hcv_test_rslt_lag), ]

# # convert to dates
# romania_pwid_hcv_test <- romania_pwid_hcv_test %>%
#   mutate(appointment_dte = as.Date(appointment_dte, format = "%d-%m-%Y")) %>%
#   mutate(appointment_dte_lag = as.Date(appointment_dte_lag, format = "%d-%m-%Y"))

# # days at risk
# romania_pwid_hcv_test <- romania_pwid_hcv_test %>%
#   mutate(days_risk = appointment_dte_lag-appointment_dte)

# # change test results to 0 and 1
# romania_pwid_hcv_test <- romania_pwid_hcv_test %>%
#   mutate(hcv_test_rslt_lag = case_when(
#     hcv_test_rslt_lag == 1 ~ 0,
#     hcv_test_rslt_lag == 2 ~ 1,
#     TRUE ~ hcv_test_rslt_lag
#   ))
# romania_pwid_hcv_test <- romania_pwid_hcv_test %>%
#   mutate(hcv_test_rslt = case_when(
#     hcv_test_rslt == 1 ~ 0,
#     hcv_test_rslt == 2 ~ 1,
#     TRUE ~ hcv_test_rslt
#   ))

# # rename hcv variables
# romania_pwid_hcv_test <- romania_pwid_hcv_test %>%
#   rename(
#     hcv_baseline = hcv_test_rslt,
#     hcv_test_rslt = hcv_test_rslt_lag
#   )

# # QA for rows where appointment_dte_lag is less than appointment_dte
# invalid_rows <- romania_pwid_hcv_test %>%
#   filter(appointment_dte_lag < appointment_dte)
# cat("Number of rows where appointment_dte_lag is less than appointment_dte:", nrow(invalid_rows), "\n")

# romania_pwid_hcv_test <- romania_pwid_hcv_test %>%
#   mutate(
#     appointment_dte = as.Date(appointment_dte),
#     appointment_dte_lag = as.Date(appointment_dte_lag)
#   )

# # Save testing data
# write.csv(romania_pwid_hcv_test, "romania_pwid_hcv_test.csv")

# load testing data
romania_pwid_hcv_test <- read.csv("romania_pwid_hcv_test.csv", stringsAsFactors = FALSE)

#### random-point sampling with 1000 iterations approach ####

View(romania_pwid_hcv_test)
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

View(romania_pwid_hcv_test)

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

# Loop through the first 1000 iterations
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

# Subset romania_pwid_hcv to include only rows that match id, appointment_dte, and hcv_test_seq in romania_pwid_hcv_test
romania_pwid_hcv_exposure <- romania_pwid_hcv %>%
  semi_join(romania_pwid_hcv_test, by = c("id", "appointment_dte", "hcv_test_seq")) %>%
  dplyr::select(id, appointment_dte, hcv_test_seq, gender, dob, sex_work_current,
                msm_current, homeless_current, ethnic_roma) %>%
  mutate(
    sex_work_current = ifelse(is.na(sex_work_current), 0, sex_work_current),
    msm_current = ifelse(is.na(msm_current), 0, msm_current),
    homeless_current = ifelse(is.na(homeless_current), 0, homeless_current),
    ethnic_roma = ifelse(is.na(ethnic_roma), 0, ethnic_roma),
    msm_current = ifelse(is.na(msm_current), 0, msm_current),
    gender = ifelse(gender == 2, 0, gender)  # Recode gender from 2 to 0
  )

# Ensure dob is in the correct Date format
romania_pwid_hcv_exposure <- romania_pwid_hcv_exposure %>%
  mutate(dob = as.Date(dob, format = "%d/%m/%Y"))  # Convert dob to Date format

# Generate age in years using dob and appointment_dte
romania_pwid_hcv_exposure <- romania_pwid_hcv_exposure %>%
  mutate(age_years = as.numeric(difftime(appointment_dte, dob, units = "days")) / 365.25)

# merge with processed dataframes 

# Define a function to merge exposure data with processed dataframes
merge_with_exposure <- function(processed_dataframes, romania_pwid_hcv_exposure) {
  # Ensure romania_pwid_hcv_exposure has no duplicate rows for id and appointment_dte
  romania_pwid_hcv_exposure <- romania_pwid_hcv_exposure %>%
    distinct(id, appointment_dte, .keep_all = TRUE)
  
  # Merge romania_pwid_hcv_exposure with each dataframe in processed_dataframes
  processed_dataframes <- lapply(processed_dataframes, function(df) {
    # Ensure df has no duplicate rows for id and appointment_dte
    df <- df %>%
      distinct(id, appointment_dte, .keep_all = TRUE)
    
    # Perform the left join with romania_pwid_hcv_exposure
    merged_df <- left_join(df, romania_pwid_hcv_exposure, by = c("id", "appointment_dte"))
    
    # Remove duplicate columns (keep the columns from romania_pwid_hcv_exposure)
    merged_df <- merged_df %>%
      dplyr::select(-ends_with(".x")) %>%  # Remove columns ending with ".x" (from the original dataframe)
      rename_with(~ gsub("\\.y$", "", .), ends_with(".y"))  # Remove ".y" suffix from merged columns
    
    return(merged_df)
  })
  
  return(processed_dataframes)
}

# Example usage of the function
processed_dataframes <- merge_with_exposure(processed_dataframes, romania_pwid_hcv_exposure)

# Save the updated processed_dataframes to a file
saveRDS(processed_dataframes, file = "processed_dataframes.rds")

# View the first merged dataframe for QA
View(processed_dataframes[[1]])

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

# Calculate incidence rates for each imputed dataset
incidence_rates <- sapply(processed_dataframes, function(df) {
  total_cases <- sum(df$hcv_test_rslt == 1, na.rm = TRUE)
  total_person_years <- sum(df$person_years, na.rm = TRUE)
  if (total_person_years == 0) return(NA)
  (total_cases / total_person_years) * 100
})

# Remove NA values if any
incidence_rates <- incidence_rates[!is.na(incidence_rates)]

# Rubin's rules
M <- length(incidence_rates)
mean_ir <- mean(incidence_rates)
between_var <- var(incidence_rates)
# If you have within-imputation variance, use it here. Otherwise, set to 0 for point estimate only.
within_var <- 0

# Total variance
total_var <- within_var + (1 + 1/M) * between_var
se_total <- sqrt(total_var)

# 95% CI
ci_lower <- mean_ir - 1.96 * se_total
ci_upper <- mean_ir + 1.96 * se_total

cat(sprintf("Incidence rate: %.2f per 100 person-years (Rubin's rules 95%% CI: %.2f – %.2f)\n",
            mean_ir, ci_lower, ci_upper))