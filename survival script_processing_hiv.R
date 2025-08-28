## load packages
pacman::p_load(dplyr, tidyr, withr, lubridate, MASS, writexl, readxl, arsenal, survival, broom, ggplot2, purrr, tableone)

## set wd
setwd("C:/Users/vl22683/OneDrive - University of Bristol/Documents/Publications/Romania PWID/data")

## oat treatment data

# load
romania_pwid_treatment <- read_excel("FZC CDI 2013_2022_in tratament.xlsx")

# rename date column
romania_pwid_treatment <- romania_pwid_treatment %>%
  rename(appointment_dte = Data_A) %>%
  rename(id = "COD ALT")

# make date column date format
romania_pwid_treatment <- romania_pwid_treatment %>%
  mutate(appointment_dte = as.Date(appointment_dte))

# delete redunant column and add oat column
romania_pwid_treatment <- romania_pwid_treatment %>%
  select(appointment_dte, id) %>%
  mutate(oat = 1)

# first recorded oat 
romania_pwid_treatment <- romania_pwid_treatment %>%
  mutate(appointment_dte = as.Date(appointment_dte)) %>%
  group_by(id) %>%
  mutate(
    oat_seq = cumsum(ifelse(oat == 1, 1, 0)),
    oat_first_dte = min(appointment_dte[oat == 1], na.rm = TRUE)
  ) %>%
  ungroup()

## exposure data

## load data
romania_pwid_raw <- read_excel("ARAS DATA IDU 2013-2022.xlsx")

# append treatment df to raw dataframe
missing_cols <- setdiff(names(romania_pwid_raw), names(romania_pwid_treatment))
romania_pwid_treatment[missing_cols] <- NA
romania_pwid_raw <- bind_rows(romania_pwid_raw, romania_pwid_treatment)

# save combined data
romania_pwid_hiv_combined <- romania_pwid_raw[!is.na(romania_pwid_raw$hiv_test_rslt) | !is.na(romania_pwid_raw$oat), ]
write.csv(romania_pwid_hiv_combined, "romania_pwid_hiv_combined.csv")

# recode gender 
romania_pwid_hiv <- romania_pwid_raw %>%
  mutate(
    gender = case_when(
      gender == 2 ~ 0,
      TRUE ~ gender
    ),
    gender = factor(gender, levels = c(0, 1), labels = c("Female", "Male"))
  )
  
# age four categories
romania_pwid_hiv <- romania_pwid_hiv %>%
  mutate(
    dob = as.Date(dob, format = "%d/%m/%Y"),
    age = as.numeric(difftime(Sys.Date(), dob, units = "days")) / 365.25,
    age_4cat = cut(
      age,
      breaks = c(-Inf, 30, 40, 50, Inf),
      labels = c("<30", "30-39", "40-49", "50+"),
      right = FALSE
    )
  )
# age two categories
  romania_pwid_hiv <- romania_pwid_hiv %>%
  mutate(age_2cat = case_when(
    age < 40 ~ "<40",
    age >= 40 ~ "40+"
  ))

# replace exposures that happened before 

# find max value of exposure vars
romania_pwid_hiv <- romania_pwid_hiv %>%
  group_by(id) %>%
  mutate(
    oat_ever = ifelse(any(oat == 1, na.rm = TRUE), 1, oat),
    sex_work_ever = ifelse(any(sex_work_current == 1, na.rm = TRUE), 1, sex_work_current),
    msm_ever = ifelse(any(msm_current == 1, na.rm = TRUE), 1, msm_current),
    homeless_ever = ifelse(any(homeless_current == 1, na.rm = TRUE), 1, homeless_current),
    ethnic_roma_ever = ifelse(any(ethnic_roma == 1, na.rm = TRUE), 1, ethnic_roma),
    hiv_ever = ifelse(any(hiv == 1, na.rm = TRUE), 1, hiv)
  ) %>%
  ungroup()

# select and order vars
romania_pwid_hiv <- romania_pwid_hiv %>%
  dplyr::select(
    id,
    oat, oat_ever,
    sex_work_current, sex_work_ever,
    msm_current, msm_ever,
    homeless_current, homeless_ever,
    ethnic_roma, ethnic_roma_ever,
    hiv, hiv_ever,
    everything()
  )

# replace NAs with 0s
romania_pwid_hiv <- romania_pwid_hiv %>%
  mutate(across(ends_with("_ever"), ~replace_na(., 0)))

# convert to factors
romania_pwid_hiv <- romania_pwid_hiv %>%
  mutate(across(ends_with("_ever"), as.factor))

str(romania_pwid_hiv$hiv_test_rslt)

# sequence negative tests
# Step 0: create a temporary row ID
romania_pwid_hiv <- romania_pwid_hiv %>%
  mutate(row_id = row_number())

# Step 1: subset negative tests and sequence them
neg_tests <- romania_pwid_hiv %>%
  filter(hiv_test_rslt == 1) %>%
  arrange(id, appointment_dte) %>%
  group_by(id) %>%
  mutate(neg_hiv_seq = row_number()) %>%
  ungroup() %>%
  select(row_id, neg_hiv_seq)  # keep only row_id and sequence

# Step 2: merge back using row_id
romania_pwid_hiv <- romania_pwid_hiv %>%
  left_join(neg_tests, by = "row_id")

# ensure hiv_test_rslt is numeric
romania_pwid_hiv <- romania_pwid_hiv %>%
  mutate(hiv_test_rslt = as.numeric(hiv_test_rslt))

# create variable called first_hiv_neg_test_dte
# find first negative date for each id
first_neg_dates <- romania_pwid_hiv %>%
  filter(neg_hiv_seq == 1) %>%
  group_by(id) %>%
  summarise(first_hiv_neg_test_dte = min(appointment_dte, na.rm = TRUE), .groups = "drop")


100402644RMAIH1078M

View(first_neg_dates)
View(romania_pwid_hiv)
# join back to all rows
romania_pwid_hiv <- romania_pwid_hiv %>%
  left_join(first_neg_dates, by = "id") %>%
  mutate(
    before_first_neg = if_else(
      !is.na(first_hiv_neg_test_dte) & appointment_dte < first_hiv_neg_test_dte,
      1L, 0L
    )
  )


romania_pwid_hiv <- romania_pwid_hiv %>%
  mutate(
    before_first_neg = if_else(
      !is.na(first_hiv_neg_test_dte) & appointment_dte < first_hiv_neg_test_dte,
      1L, 0L
    )
  )

romania_pwid_hiv %>%
  count(before_first_neg)

romania_pwid_hiv %>%
  count(before_first_neg) %>%
  mutate(pct = n / sum(n) * 100)




# replace oat_ever with NA if oat equals 1 and appointment_dte is less than first_hiv_neg_test_dte
romania_pwid_hiv <- romania_pwid_hiv %>%
  mutate(oat_flag = ifelse(as.numeric(as.character(oat)) == 1 & appointment_dte < first_hiv_neg_test_dte, 1, NA))



table_oat_flag <- CreateTableOne(vars = "oat_flag", data = romania_pwid_hiv)
print(table_oat_flag, showAllLevels = TRUE)

romania_pwid_hiv$oat_ever <- as.character(romania_pwid_hiv$oat_ever)
romania_pwid_hiv$oat_ever <- ifelse(romania_pwid_hiv$oat_ever == "1" & romania_pwid_hiv$appointment_dte < romania_pwid_hiv$first_hiv_test_dte, NA, romania_pwid_hiv$oat_ever)
romania_pwid_hiv$oat_ever <- as.factor(romania_pwid_hiv$oat_ever)

table_oat_ever <- CreateTableOne(vars = "oat_ever", data = romania_pwid_hiv)
print(table_oat_ever, showAllLevels = TRUE)
View(romania_pwid_hiv)

# 12 months vars
romania_pwid_hiv <- romania_pwid_hiv %>%
  mutate(
    appointment_dte = as.Date(appointment_dte),
    homeless_current = ifelse(is.na(homeless_current), 0, as.numeric(homeless_current)),
    sex_work_current = ifelse(is.na(sex_work_current), 0, as.numeric(sex_work_current)),
    msm_current = ifelse(is.na(msm_current), 0, as.numeric(msm_current)),
    oat = ifelse(is.na(oat), 0, as.numeric(oat))
  ) %>%
  group_by(id) %>%
  arrange(appointment_dte) %>%
  mutate(
    homeless_12m = ifelse(
      !is.na(hcv_test_rslt),
      sapply(appointment_dte, function(test_date) {
        any(
          appointment_dte >= (test_date - days(365)) &
          appointment_dte <= test_date &
          homeless_current == 1
        )
      }) %>% as.integer(),
      0L
    ),
    sex_work_12m = ifelse(
      !is.na(hcv_test_rslt),
      sapply(appointment_dte, function(test_date) {
        any(
          appointment_dte >= (test_date - days(365)) &
          appointment_dte <= test_date &
          sex_work_current == 1
        )
      }) %>% as.integer(),
      0L
    ),
    msm_12m = ifelse(
      !is.na(hcv_test_rslt),
      sapply(appointment_dte, function(test_date) {
        any(
          appointment_dte >= (test_date - days(365)) &
          appointment_dte <= test_date &
          msm_current == 1
        )
      }) %>% as.integer(),
      0L
    ),
    oat_12m = ifelse(
      !is.na(hcv_test_rslt),
      sapply(appointment_dte, function(test_date) {
        any(
          appointment_dte >= (test_date - days(365)) &
          appointment_dte <= test_date &
          oat == 1
        )
      }) %>% as.integer(),
      0L
    )
  ) %>%
  ungroup()


# tidy up exposures
romania_pwid_hiv <- romania_pwid_hiv %>%
  mutate(homeless_12m = if_else(homeless_current == 1, 1L, homeless_12m))
romania_pwid_hiv <- romania_pwid_hiv %>%
  mutate(sex_work_12m = if_else(sex_work_current == 1, 1L, sex_work_12m))
romania_pwid_hiv <- romania_pwid_hiv %>%
  mutate(msm_12m = if_else(msm_current == 1, 1L, msm_12m))
romania_pwid_hiv <- romania_pwid_hiv %>%
  mutate(oat_12m = if_else(oat == 1, 1L, oat_12m))

romania_pwid_hiv$homeless_12m <- as.factor(romania_pwid_hiv$homeless_12m)
romania_pwid_hiv$homeless_current <- as.factor(romania_pwid_hiv$homeless_current)
romania_pwid_hiv$sex_work_12m <- as.factor(romania_pwid_hiv$sex_work_12m)
romania_pwid_hiv$sex_work_current <- as.factor(romania_pwid_hiv$sex_work_current)
romania_pwid_hiv$msm_12m <- as.factor(romania_pwid_hiv$msm_12m)
romania_pwid_hiv$msm_current <- as.factor(romania_pwid_hiv$msm_current)
romania_pwid_hiv$oat_12m <- as.factor(romania_pwid_hiv$oat_12m)

# table of exposures
current_vars <- c("oat_12m", "oat_ever", "homeless_12m", "homeless_current", "homeless_ever", "sex_work_12m", "sex_work_current", "sex_work_ever", "msm_12m", "msm_current", "msm_ever")
current_table <- CreateTableOne(
  vars = current_vars,
  data = romania_pwid_hiv
)
print(current_table, showAllLevels = TRUE)

## baseline hiv cohort

# remove rows where hiv test result is missing
romania_pwid_hiv <- romania_pwid_hiv[!is.na(romania_pwid_hiv$hiv_test_rslt), ]

# remove rows where hiv test result is indeterminate
romania_pwid_hiv <- romania_pwid_hiv %>%
  filter(!hiv_test_rslt == 3)

# sequence hiv tests by id
romania_pwid_hiv <- romania_pwid_hiv %>%
  group_by(id) %>%
  arrange(id, appointment_dte) %>%
  mutate(hiv_test_seq = row_number())

# sequence by id 
romania_pwid_hiv <- romania_pwid_hiv %>%
  arrange(id) %>%
  mutate(id_seq = cumsum(!duplicated(id))) 

# create sequence of visits by id
romania_pwid_hiv <- romania_pwid_hiv %>%
  group_by(id) %>%
  arrange(id, appointment_dte) %>%
  mutate(appointment_seq = row_number())
romania_pwid_hiv <- ungroup(romania_pwid_hiv)

# hiv test results
romania_pwid_hiv_rslts <- table(romania_pwid_hiv$hiv_test_rslt)
print(romania_pwid_hiv_rslts)

# identify ids positive at baseline
positive_at_baseline <- romania_pwid_hiv %>%
  filter(appointment_seq == 1 & hiv_test_rslt == 2) %>%
  pull(id)

# include ids where positive at baseline
positive_at_baseline_df <- romania_pwid_hiv %>%
  filter((id %in% positive_at_baseline))

# hiv test results by visit
positive_at_baseline_df_summary <- table(positive_at_baseline_df$hiv_test_rslt)
print(positive_at_baseline_df_summary)

# remove ids where positive at baseline
romania_pwid_hiv <- romania_pwid_hiv %>%
  filter(!(id %in% positive_at_baseline))

# sequence by id 
romania_pwid_hiv <- romania_pwid_hiv %>%
  arrange(id) %>%
  mutate(id_seq = cumsum(!duplicated(id)))

# highest value in the id_seq column
highest_id_seq <- romania_pwid_hiv %>%
  summarise(max_id_seq = max(id_seq, na.rm = TRUE))
print(highest_id_seq)

# hiv test results by visit
romania_pwid_hiv_summary <- table(romania_pwid_hiv$appointment_seq, romania_pwid_hiv$hiv_test_rslt)
print(romania_pwid_hiv_summary) 

# remove participants with only one test
romania_pwid_hiv <- romania_pwid_hiv %>%
  group_by(id) %>%
  filter(!(max(hiv_test_seq, na.rm = TRUE) == 1)) %>%
  ungroup()

# new id sequence
romania_pwid_hiv <- romania_pwid_hiv %>%
  arrange(id) %>%
  mutate(id_seq = cumsum(!duplicated(id)))

# Find the highest value in the id_seq column
highest_id_seq <- romania_pwid_hiv %>%
  summarise(max_id_seq = max(id_seq, na.rm = TRUE))

# number of ids
cat("Highest value in id_seq:\n")
print(highest_id_seq)

# hiv test results by test number
romania_pwid_hiv_summary <- table(romania_pwid_hiv$hiv_test_seq, romania_pwid_hiv$hiv_test_rslt)
print(romania_pwid_hiv_summary) 

# create dataframe of individuals who tested after first positive
romania_pwid_hiv_after_pos <- romania_pwid_hiv %>%
  group_by(id) %>%
  mutate(
    first_hiv_positive_dte = ifelse(hiv_test_rslt == 2, appointment_dte, NA),
    first_hiv_positive_dte = if (all(is.na(first_hiv_positive_dte))) NA else min(first_hiv_positive_dte, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  filter(!is.na(first_hiv_positive_dte) & appointment_dte > first_hiv_positive_dte)

# remove hiv tests after the first positive
romania_pwid_hiv <- romania_pwid_hiv %>%
  group_by(id) %>%
  mutate(
    first_hiv_positive_dte = ifelse(hiv_test_rslt == 2, appointment_dte, NA),
    first_hiv_positive_dte = if (all(is.na(first_hiv_positive_dte))) NA else min(first_hiv_positive_dte, na.rm = TRUE)
  ) %>%
  ungroup()

# Filter rows to keep only those before or on the first positive test date
romania_pwid_hiv <- romania_pwid_hiv %>%
  filter(is.na(first_hiv_positive_dte) | appointment_dte <= first_hiv_positive_dte)

# hiv test results by visit
romania_pwid_hiv_summary <- table(romania_pwid_hiv$hiv_test_seq, romania_pwid_hiv$hiv_test_rslt)
print(romania_pwid_hiv_summary)

# appointment_dte is a date
romania_pwid_hiv <- romania_pwid_hiv %>%
  mutate(appointment_dte = as.Date(appointment_dte, format = "%Y-%m-%d"))

# sequence tests by id and date
romania_pwid_hiv <- romania_pwid_hiv %>%
  arrange(id, appointment_dte) %>%
  group_by(id) %>%
  mutate(hiv_test_seq = row_number()) %>%
  ungroup()

# create intervals
romania_pwid_hiv_test <- romania_pwid_hiv %>%
  mutate(
    appointment_dte = as.Date(appointment_dte, format = "%Y-%m-%d")
  ) %>%
  arrange(id, appointment_dte) %>%
  group_by(id) %>%
  mutate(
    appointment_dte_start = appointment_dte,
    appointment_dte_end = lead(appointment_dte),
    hiv_test_rslt_start = hiv_test_rslt,
    hiv_test_rslt_end = lead(hiv_test_rslt)
  ) %>%
  ungroup() %>%
  filter(!is.na(appointment_dte_end)) %>%
  mutate(
    days_risk = as.numeric(appointment_dte_end - appointment_dte_start),
    py = days_risk / 365.25
  ) %>%
  dplyr::select(
    id, appointment_dte_start, appointment_dte_end, hiv_test_rslt_start, hiv_test_rslt_end,
    days_risk, py, oat_12m, oat_ever, sex_work_12m, sex_work_ever, msm_12m, msm_ever, homeless_12m, homeless_ever, ethnic_roma_ever, hiv_ever, gender, age_4cat, age_2cat
  ) %>%
  rename(
    appointment_dte = appointment_dte_start,
    appointment_dte_lag = appointment_dte_end,
    hiv_baseline = hiv_test_rslt_start,
    hiv_test_rslt = hiv_test_rslt_end
  )

# change test results to 0 and 1
romania_pwid_hiv_test <- romania_pwid_hiv_test %>%
  mutate(
    hiv_baseline = case_when(
      hiv_baseline == 1 ~ 0,
      hiv_baseline == 2 ~ 1,
      TRUE ~ hiv_baseline
    ),
    hiv_test_rslt = case_when(
      hiv_test_rslt == 1 ~ 0,
      hiv_test_rslt == 2 ~ 1,
      TRUE ~ hiv_test_rslt
    )
  )

# QA for rows where appointment_dte_lag is less than appointment_dte
invalid_rows <- romania_pwid_hiv_test %>%
  filter(appointment_dte_lag < appointment_dte)
cat("Number of rows where appointment_dte_lag is less than appointment_dte:", nrow(invalid_rows), "\n")

# date format 
romania_pwid_hiv_test <- romania_pwid_hiv_test %>%
  mutate(
    appointment_dte = as.Date(appointment_dte),
    appointment_dte_lag = as.Date(appointment_dte_lag)
  )

# List of exposure variables
exposure_vars <- c("oat_12m", "oat_ever", "sex_work_12", "sex_work_ever", "msm_12m", "msm_ever", "homeless_12m", "homeless_ever", "ethnic_roma_ever", "hiv_ever", "gender")

# Create TableOne summary stratified by hiv_ever
exposure_vars <- CreateTableOne(vars = exposure_vars, data = romania_pwid_hiv_test)

print(exposure_vars)

# check that dates are correctly lagged
View(romania_pwid_hiv_test)

# Save testing data
write.csv(romania_pwid_hiv_test, "romania_pwid_hiv_test.csv")

## overall incidence estimate

# incident cases
cases <- sum(romania_pwid_hiv_test$hiv_baseline == 0 & romania_pwid_hiv_test$hiv_test_rslt == 1, na.rm = TRUE)

# person-time
romania_pwid_hiv_test <- romania_pwid_hiv_test %>%
  mutate(py = as.numeric(appointment_dte_lag - appointment_dte) / 365.25)

person_time <- sum(romania_pwid_hiv_test$py, na.rm = TRUE)

# incidence per 100 PY
ir <- (cases / person_time) * 100

# 95% CI
lower <- (qchisq(0.025, 2 * cases) / 2) / person_time * 100
upper <- (qchisq(0.975, 2 * (cases + 1)) / 2) / person_time * 100

cat("hiv Incidence Rate:", round(ir, 2), "per 100 PY (95% CI:", round(lower, 2), "-", round(upper, 2), 
    "| Cases:", cases, "| Person-years:", round(person_time, 2), ")\n")

## random-point sampling with 10000 iterations approach

# seroconversion intervals
seroconversion_intervals <- romania_pwid_hiv_test %>%
  filter(hiv_baseline == 0 & hiv_test_rslt == 1)

# generate random infection dates
romania_pwid_hiv_test_iterations <- seroconversion_intervals %>%
  rowwise() %>%
  mutate(
    iteration = list(1:10000),
    random_infection_dtes = list(
      as.Date(
        runif(10000,
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

# always-negative intervals
romania_pwid_hiv_test_negatives <- romania_pwid_hiv_test %>%
  filter(hiv_test_rslt == 0) %>%
  tidyr::crossing(iteration = 1:10000) %>%
  mutate(
    imputed_infection_dte = NA,
    days_risk = as.numeric(days_risk),
    person_years = days_risk / 365.25,
    midpoint_year = NA
  )

# check for rows where appointment_dte_lag is less than appointment_dte
invalid_rows <- romania_pwid_hiv_test_iterations %>%
  filter(appointment_dte_lag < appointment_dte)
cat("rows where appointment_dte_lag is less than appointment_dte:", nrow(invalid_rows), "\n")

# split each iteration into a separate dataframe
romania_pwid_hiv_test_iterations <- romania_pwid_hiv_test_iterations %>%
  mutate(
    days_risk = as.numeric(days_risk),
    person_years = days_risk / 365.25
  )

# only one negative interval per id
romania_pwid_hiv_test_negatives <- romania_pwid_hiv_test %>%
  filter(hiv_test_rslt == 0) %>%
  tidyr::crossing(iteration = 1:10000) %>%
  mutate(
    imputed_infection_dte = NA,
    days_risk = as.numeric(days_risk),
    person_years = days_risk / 365.25,
    midpoint_year = NA
  )

split_dataframes <- split(
  bind_rows(romania_pwid_hiv_test_iterations, romania_pwid_hiv_test_negatives),
  bind_rows(romania_pwid_hiv_test_iterations, romania_pwid_hiv_test_negatives)$iteration
)

# name each dataframe in the list
names(split_dataframes) <- paste0("iteration_", seq_along(split_dataframes))

# Find duplicate IDs in the first iteration
dup_ids <- names(which(table(split_dataframes[[1]]$id) > 1))

# List of columns you want
wanted_cols <- c("id", "appointment_dte", "appointment_dte_lag", "days_risk", "person_years", "hiv_test_rslt")

# Only select columns that exist in the dataframe
available_cols <- intersect(wanted_cols, colnames(split_dataframes[[1]]))

duplicates_df <- split_dataframes[[1]] %>%
  filter(id %in% dup_ids) %>%
  dplyr::select(all_of(available_cols)) %>%
  arrange(id, appointment_dte)

print(duplicates_df)
View(duplicates_df)

## wide format dataframes for hiv incidence analysis

# list to store the results
processed_dataframes_hiv <- list()

# years to create columns
required_years <- 2013:2022

# loop 10000 iterations
for (i in 1:10000) {
  cat("Processing iteration", i, "of", 10000, "\n")
  
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
    processed_dataframes_hiv[[i]] <- NULL
    next
  }
  
  # merge the person_years_df with the original dataframe
  df <- bind_cols(df, person_years_df)
  
  # Ensure all required columns are present
  for (year in required_years) {
    column_name <- paste0("hiv_test_", year)
    if (!(column_name %in% names(df))) {
      df[[column_name]] <- 0
    }
  }
  
  # Populate the hiv_test_20xx columns based on midpoint_year and hiv_test_rslt
  for (year in required_years) {
    column_name <- paste0("hiv_test_", year)
    df[[column_name]] <- ifelse(df$midpoint_year == year & df$hiv_test_rslt == 1, 1, df[[column_name]])
  }
  
  # person-year columns are present
  for (year in required_years) {
    if (!(as.character(year) %in% names(df))) {
      df[[as.character(year)]] <- 0
    }
  }
  
  # Store the processed dataframe in the list
  processed_dataframes_hiv[[i]] <- df
}

# create year variable
for (i in 1:length(processed_dataframes_hiv)) {
  # Get the processed dataframe for the current iteration
  df <- processed_dataframes_hiv[[i]]
  
  # year column
  df <- df %>%
    mutate(year = as.numeric(format(appointment_dte_lag, "%Y")))
  
  # factor year
  df$year <- as.factor(df$year)
  
  # store
  processed_dataframes_hiv[[i]] <- df
}

# first processed dataframe
df <- processed_dataframes_hiv[[1]]

# histogram of person_years
ggplot(df, aes(x = person_years)) +
  geom_histogram(binwidth = 0.1, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Person Years",
       x = "Person Years",
       y = "Frequency") +
  theme_minimal()

# save wide dataframes
saveRDS(processed_dataframes_hiv, file = "processed_dataframes_hiv.rds")

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
  
  # recode hiv_test_rslt to 0 when it is invalid, NA, or year does not equal midpoint_year
  df_long <- df_long %>%
    mutate(hiv_test_rslt = ifelse(is.na(hiv_test_rslt) | !is.numeric(hiv_test_rslt), 0,
                                ifelse(year == midpoint_year, hiv_test_rslt, NA)))
  # keep only the specified columns
  df_long <- df_long %>%
    dplyr::select(id, hiv_test_rslt, appointment_dte, appointment_dte_lag, year, midpoint_year, time_at_risk)
  
  # sort by id and then by year
  df_long <- df_long %>%
    arrange(id, year)
  
  return(df_long)
}

# load wide dataframes
processed_dataframes_hiv <- readRDS("processed_dataframes_hiv.rds")

# list to store long dataframes
processed_dataframes_long_hiv <- list()

# loop over dataframes in processed_dataframes_hiv
for (i in 1:length(processed_dataframes_hiv)) {
  cat("Processing dataframe", i, "of", length(processed_dataframes_hiv), "\n")
  processed_dataframes_long_hiv[[i]] <- process_dataframe(processed_dataframes_hiv[[i]])
}

# save long dataframes
saveRDS(processed_dataframes_long_hiv, file = "processed_dataframes_long_hiv.rds")

