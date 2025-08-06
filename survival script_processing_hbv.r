## load packages
pacman::p_load(dplyr, tidyr, withr, lubridate, MASS, writexl, readxl, arsenal, survival, broom, ggplot2, purrr)

## set wd
setwd("C:/Users/vl22683/OneDrive - University of Bristol/Documents/Publications/Romania PWID/data")

## load data
romania_pwid_raw <- read_excel("ARAS DATA IDU 2013-2022.xlsx")

## baseline hbv cohort

# remove rows where hbv test result is missing
romania_pwid_hbv <- romania_pwid_raw[!is.na(romania_pwid_raw$hbv_test_rslt), ]

# remove rows where hbv test result is indeterminate
romania_pwid_hbv <- romania_pwid_hbv %>%
  filter(!hbv_test_rslt == 3)

# sequence by id 
romania_pwid_hbv <- romania_pwid_hbv %>%
  arrange(id) %>%
  mutate(id_seq = cumsum(!duplicated(id))) 

# create sequence of visits by id
romania_pwid_hbv <- romania_pwid_hbv %>%
  group_by(id) %>%
  arrange(id, appointment_dte) %>%
  mutate(appointment_seq = row_number())
romania_pwid_hbv <- ungroup(romania_pwid_hbv)

# hbv test results
romania_pwid_hbv_rslts <- table(romania_pwid_hbv$hbv_test_rslt)
print(romania_pwid_hbv_rslts)

# identify ids positive at baseline
positive_at_baseline <- romania_pwid_hbv %>%
  filter(appointment_seq == 1 & hbv_test_rslt == 2) %>%
  pull(id)

# include ids where positive at baseline
positive_at_baseline_df <- romania_pwid_hbv %>%
  filter((id %in% positive_at_baseline))

# hbv test results by visit
positive_at_baseline_df_summary <- table(positive_at_baseline_df$hbv_test_rslt)
print(positive_at_baseline_df_summary)

# remove ids where positive at baseline
romania_pwid_hbv <- romania_pwid_hbv %>%
  filter(!(id %in% positive_at_baseline))

# sequence by id 
romania_pwid_hbv <- romania_pwid_hbv %>%
  arrange(id) %>%
  mutate(id_seq = cumsum(!duplicated(id)))

# highest value in the id_seq column
highest_id_seq <- romania_pwid_hbv %>%
  summarise(max_id_seq = max(id_seq, na.rm = TRUE))
print(highest_id_seq)

# hbv test results by visit
romania_pwid_hbv_summary <- table(romania_pwid_hbv$appointment_seq, romania_pwid_hbv$hbv_test_rslt)
print(romania_pwid_hbv_summary) 

# sequence hbv tests by id
romania_pwid_hbv <- romania_pwid_hbv %>%
  group_by(id) %>%
  arrange(id) %>%
  mutate(hbv_test_seq = row_number())

# hbv test results by test number
romania_pwid_hbv_summary <- table(romania_pwid_hbv$hbv_test_seq, romania_pwid_hbv$hbv_test_rslt)
print(romania_pwid_hbv_summary)

# remove participants with only one test
romania_pwid_hbv <- romania_pwid_hbv %>%
  group_by(id) %>%
  filter(!(max(hbv_test_seq, na.rm = TRUE) == 1)) %>%
  ungroup()

# new id sequence
romania_pwid_hbv <- romania_pwid_hbv %>%
  arrange(id) %>%
  mutate(id_seq = cumsum(!duplicated(id)))

# Find the highest value in the id_seq column
highest_id_seq <- romania_pwid_hbv %>%
  summarise(max_id_seq = max(id_seq, na.rm = TRUE))

# number of ids
cat("Highest value in id_seq:\n")
print(highest_id_seq)

# hbv test results by test number
romania_pwid_hbv_summary <- table(romania_pwid_hbv$hbv_test_seq, romania_pwid_hbv$hbv_test_rslt)
print(romania_pwid_hbv_summary) 

# create dataframe of individuals who tested after first positive
romania_pwid_hbv_after_pos <- romania_pwid_hbv %>%
  group_by(id) %>%
  mutate(
    first_hbv_positive_dte = ifelse(hbv_test_rslt == 2, appointment_dte, NA),
    first_hbv_positive_dte = if (all(is.na(first_hbv_positive_dte))) NA else min(first_hbv_positive_dte, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  filter(!is.na(first_hbv_positive_dte) & appointment_dte > first_hbv_positive_dte)

# remove hbv tests after the first positive
romania_pwid_hbv <- romania_pwid_hbv %>%
  group_by(id) %>%
  mutate(
    first_hbv_positive_dte = ifelse(hbv_test_rslt == 2, appointment_dte, NA),
    first_hbv_positive_dte = if (all(is.na(first_hbv_positive_dte))) NA else min(first_hbv_positive_dte, na.rm = TRUE)
  ) %>%
  ungroup()

# Filter rows to keep only those before or on the first positive test date
romania_pwid_hbv <- romania_pwid_hbv %>%
  filter(is.na(first_hbv_positive_dte) | appointment_dte <= first_hbv_positive_dte)

# hbv test results by visit
romania_pwid_hbv_summary <- table(romania_pwid_hbv$hbv_test_seq, romania_pwid_hbv$hbv_test_rslt)
print(romania_pwid_hbv_summary)

# create hbv testing dataframe
romania_pwid_hbv_test <- subset(romania_pwid_hbv, select = c(id, appointment_dte, hbv_test_seq, hbv_test_rslt)) 

# create lag of appointment dates and hbv tests
romania_pwid_hbv_test <- romania_pwid_hbv_test %>%
  arrange(id, hbv_test_seq) %>%
  group_by(id) %>%
  mutate(appointment_dte_lag = lead(appointment_dte),
         hbv_test_rslt_lag = lead(hbv_test_rslt))

# remove empty rows
romania_pwid_hbv_test <- romania_pwid_hbv_test[!is.na(romania_pwid_hbv_test$hbv_test_rslt_lag), ]

# convert to dates
romania_pwid_hbv_test <- romania_pwid_hbv_test %>%
  mutate(appointment_dte = as.Date(appointment_dte, format = "%d-%m-%Y")) %>%
  mutate(appointment_dte_lag = as.Date(appointment_dte_lag, format = "%d-%m-%Y"))

# days at risk
romania_pwid_hbv_test <- romania_pwid_hbv_test %>%
  mutate(days_risk = appointment_dte_lag-appointment_dte)

# change test results to 0 and 1
romania_pwid_hbv_test <- romania_pwid_hbv_test %>%
  mutate(hbv_test_rslt_lag = case_when(
    hbv_test_rslt_lag == 1 ~ 0,
    hbv_test_rslt_lag == 2 ~ 1,
    TRUE ~ hbv_test_rslt_lag
  ))
romania_pwid_hbv_test <- romania_pwid_hbv_test %>%
  mutate(hbv_test_rslt = case_when(
    hbv_test_rslt == 1 ~ 0,
    hbv_test_rslt == 2 ~ 1,
    TRUE ~ hbv_test_rslt
  ))

# rename hbv variables
romania_pwid_hbv_test <- romania_pwid_hbv_test %>%
  rename(
    hbv_baseline = hbv_test_rslt,
    hbv_test_rslt = hbv_test_rslt_lag
  )

# QA for rows where appointment_dte_lag is less than appointment_dte
invalid_rows <- romania_pwid_hbv_test %>%
  filter(appointment_dte_lag < appointment_dte)
cat("Number of rows where appointment_dte_lag is less than appointment_dte:", nrow(invalid_rows), "\n")

romania_pwid_hbv_test <- romania_pwid_hbv_test %>%
  mutate(
    appointment_dte = as.Date(appointment_dte),
    appointment_dte_lag = as.Date(appointment_dte_lag)
  )

# date format 
romania_pwid_hbv_test <- romania_pwid_hbv_test %>%
  mutate(
    appointment_dte = as.Date(appointment_dte),
    appointment_dte_lag = as.Date(appointment_dte_lag)
  )

# Save testing data
write.csv(romania_pwid_hbv_test, "romania_pwid_hbv_test.csv")

## overall incidence estimate
# incident cases
cases <- sum(romania_pwid_hbv_test$hbv_baseline == 0 & romania_pwid_hbv_test$hbv_test_rslt == 1, na.rm = TRUE)

# person-time
romania_pwid_hbv_test <- romania_pwid_hbv_test %>%
  mutate(py = as.numeric(appointment_dte_lag - appointment_dte) / 365.25)

person_time <- sum(romania_pwid_hbv_test$py, na.rm = TRUE)

# incidence per 100 PY
ir <- (cases / person_time) * 100

# 95% CI
lower <- (qchisq(0.025, 2 * cases) / 2) / person_time * 100
upper <- (qchisq(0.975, 2 * (cases + 1)) / 2) / person_time * 100

cat("hbv Incidence Rate:", round(ir, 2), "per 100 PY (95% CI:", round(lower, 2), "-", round(upper, 2), ")\n")

## random-point sampling with 1000 iterations approach

# generate random infection dates
romania_pwid_hbv_test_iterations <- romania_pwid_hbv_test %>%
  rowwise() %>%
  mutate(
    iteration = list(1:1000),
    random_infection_dtes = list(
      as.Date(
        runif(1000,
              min = as.numeric(appointment_dte),
              max = as.numeric(appointment_dte_lag)),
        origin = "1970-01-01"
      )
    )
  ) %>%
  unnest(c(iteration, random_infection_dtes)) %>%
  ungroup() %>%
  mutate(
    days_risk = as.numeric(random_infection_dtes - appointment_dte),
    person_years = days_risk / 365.25,
    midpoint_year = lubridate::year(random_infection_dtes),
    appointment_dte_lag = random_infection_dtes
  )

# check for rows where appointment_dte_lag is less than appointment_dte
invalid_rows <- romania_pwid_hbv_test_iterations %>%
  filter(appointment_dte_lag < appointment_dte)
cat("rows where appointment_dte_lag is less than appointment_dte:", nrow(invalid_rows), "\n")

# split each iteration into a separate dataframe
split_dataframes <- split(bind_rows(romania_pwid_hbv_test_iterations, romania_pwid_hbv_test_negatives),
                          .$iteration)

# name each dataframe in the list
names(split_dataframes) <- paste0("iteration_", seq_along(split_dataframes))

# create dataframe of negatives
romania_pwid_hbv_test_negatives <- romania_pwid_hbv_test %>%
  filter(hbv_test_rslt == 0) %>%
  tidyr::crossing(iteration = 1:1000) %>%
  mutate(
    imputed_infection_dte = NA,
    person_years = days_risk / 365.25,
    midpoint_year = NA
  )

# append the negatives dataframe to each of the 1000 dataframes
split_dataframes <- lapply(split_dataframes, function(df) {
  common_cols <- intersect(names(df), names(romania_pwid_hbv_test_negatives))
  df <- df[, common_cols]
  negatives <- romania_pwid_hbv_test_negatives[, common_cols]
  combined_df <- rbind(df, negatives)
  return(combined_df)
})

## wide format dataframes for hbv incidence analysis

# list to store the results
processed_dataframes_hbv <- list()

# years to create columns
required_years <- 2013:2022

# loop 1000 iterations
for (i in 1:1000) {
  cat("Processing iteration", i, "of", 1000, "\n")
  
  # dataframe for the current iteration
  df <- split_dataframes[[i]]
  
  # person-years for each year of observation
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
  
  # check person_years_df is empty
  if (nrow(person_years_df) == 0) {
    cat("person_years_df is empty for iteration", i, "\n")
    processed_dataframes_hbv[[i]] <- NULL
    next
  }
  
  # merge the person_years_df with the original dataframe
  df <- bind_cols(df, person_years_df)
  
  # Ensure all required columns are present
  for (year in required_years) {
    column_name <- paste0("hbv_test_", year)
    if (!(column_name %in% names(df))) {
      df[[column_name]] <- 0
    }
  }
  
  # Populate the hbv_test_20xx columns based on midpoint_year and hbv_test_rslt
  for (year in required_years) {
    column_name <- paste0("hbv_test_", year)
    df[[column_name]] <- ifelse(df$midpoint_year == year & df$hbv_test_rslt == 1, 1, df[[column_name]])
  }
  
  # person-year columns are present
  for (year in required_years) {
    if (!(as.character(year) %in% names(df))) {
      df[[as.character(year)]] <- 0
    }
  }
  
  # Store the processed dataframe in the list
  processed_dataframes_hbv[[i]] <- df
}

# create year variable
for (i in 1:length(processed_dataframes_hbv)) {
  # Get the processed dataframe for the current iteration
  df <- processed_dataframes_hbv[[i]]
  
  # year column
  df <- df %>%
    mutate(year = as.numeric(format(appointment_dte_lag, "%Y")))
  
  # factor year
  df$year <- as.factor(df$year)
  
  # store
  processed_dataframes_hbv[[i]] <- df
}

# first processed dataframe
df <- processed_dataframes_hbv[[1]]

# histogram of person_years
ggplot(df, aes(x = person_years)) +
  geom_histogram(binwidth = 0.1, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Person Years",
       x = "Person Years",
       y = "Frequency") +
  theme_minimal()

# save wide dataframes
saveRDS(processed_dataframes_hbv, file = "processed_dataframes_hbv.rds")

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
    filter(!is.na(time_at_risk))  # Remove rows where time_at_risk is NA
  
  # recode hbv_test_rslt to 0 when it is invalid, NA, or year does not equal midpoint_year
  df_long <- df_long %>%
    mutate(hbv_test_rslt = ifelse(is.na(hbv_test_rslt) | !is.numeric(hbv_test_rslt), 0,
                                ifelse(year == midpoint_year, hbv_test_rslt, NA)))
  # keep only the specified columns
  df_long <- df_long %>%
    dplyr::select(id, hbv_test_rslt, appointment_dte, appointment_dte_lag, year, midpoint_year, time_at_risk)
  
  # sort by id and then by year
  df_long <- df_long %>%
    arrange(id, year)
  
  return(df_long)
}

# load wide dataframes
processed_dataframes_hbv <- readRDS("processed_dataframes_hbv.rds")

# list to store long dataframes
processed_dataframes_long_hbv <- list()

# loop over dataframes in processed_dataframes_hbv
for (i in 1:length(processed_dataframes_hbv)) {
  cat("Processing dataframe", i, "of", length(processed_dataframes_hbv), "\n")
  processed_dataframes_long_hbv[[i]] <- process_dataframe(processed_dataframes_hbv[[i]])
}

# save long dataframes
saveRDS(processed_dataframes_long_hbv, file = "processed_dataframes_long_hbv.rds")