## load packages
pacman::p_load(dplyr, tidyr, withr, lubridate, writexl, readxl)

## set wd
setwd("C:/Users/vl22683/OneDrive - University of Bristol/Documents/Publications/Romania PWID/data")

## load data
romania_pwid_raw <- read_excel("ARAS DATA IDU 2013-2022.xlsx") 

test 
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


#### bootstrap approach ###

# number of bootstrap iterations
n_iterations <- 10

# store incidence rates for each iteration
incidence_rates <- numeric(n_iterations)

# Bootstrap loop
for (i in 1:n_iterations) {
  # Resample the dataset with replacement
  bootstrap_sample <- romania_pwid_hcv_test %>%
    sample_n(nrow(romania_pwid_hcv_test), replace = TRUE) %>%
    rowwise() %>%
    mutate(
      condition_met = hcv_test_rslt == 1,
      random_infection_date = if (condition_met) {
        as.Date(runif(1, as.numeric(appointment_dte), as.numeric(appointment_dte_lag)), origin = "1970-01-01")
      } else {
        NA
      },
      
      # calculate days at risk based on random infection date
      days_risk = if (condition_met) {
        as.numeric(random_infection_date - appointment_dte)
      } else {
        as.numeric(appointment_dte_lag - appointment_dte)
      }
    ) %>%
    ungroup()
  
  # calculate total days and cases for the bootstrap sample
  total_days_hcv <- sum(bootstrap_sample$days_risk)
  total_cases <- sum(bootstrap_sample$hcv_test_rslt)
  
  # calculate incidence rate for this iteration
  incidence_rate <- (total_cases / total_days_hcv) * 365.25 * 100
  incidence_rates[i] <- incidence_rate
}

# calculate the average incidence rate
mean_incidence_rate <- mean(incidence_rates)

# calculate the 95% uncertainty interval (2.5th and 97.5th percentiles)
incidence_rate_95ci <- quantile(incidence_rates, c(0.025, 0.975))

# results
cat("Average incidence rate of HCV per 100 person years:", mean_incidence_rate, "\n")
cat("95% Uncertainty Interval (2.5th and 97.5th percentiles):", incidence_rate_95ci, "\n")

#### random-point sampling with 1000 iterations approach ####

# generate random infection dates
romania_pwid_hcv_test_iterations <- romania_pwid_hcv_test %>%
  filter(hcv_test_rslt == 1) %>%  
  rowwise() %>%
  mutate(
    # generate 1000 random infection dates
    random_infection_dtes = list(
      as.Date(
        pmax(
          pmin(
            rnorm(1000, 
                  mean = as.numeric(appointment_dte) + (as.numeric(appointment_dte_lag) - as.numeric(appointment_dte)) / 2,
                  sd = (as.numeric(appointment_dte_lag) - as.numeric(appointment_dte)) / 4),
            as.numeric(appointment_dte_lag) 
          ),
          as.numeric(appointment_dte)  
        ),
        origin = "1970-01-01"
      )
    )
  ) %>%
  unnest_longer(random_infection_dtes) %>%  
  mutate(
    iteration = row_number(),  
    days_risk = as.numeric(random_infection_dtes - appointment_dte),  # days at risk
    person_years = days_risk / 365.25  # convert days at risk to person-years
  ) %>%
  ungroup()

# Calculate total person-years and total cases
total_person_years <- sum(romania_pwid_hcv_test_iterations$person_years, na.rm = TRUE)
total_cases <- n_distinct(romania_pwid_hcv_test_iterations$iteration)

# Calculate incidence rate (per 100 person-years)
incidence_rate <- (total_cases / total_person_years) * 100

# Poisson 95% confidence intervals
lower_bound <- (qpois(0.025, total_cases) / total_person_years) * 100
upper_bound <- (qpois(0.975, total_cases) / total_person_years) * 100

# Print results
cat("Incidence Rate (per 100 person-years):", incidence_rate, "\n")
cat("95% Confidence Interval:", lower_bound, "-", upper_bound, "\n")





















romania_pwid_hcv_test_iterations <- romania_pwid_hcv_test %>%
  rowwise() %>%
  mutate(
    condition_met = hcv_test_rslt == 1  # Only generate random infection dates for those who tested positive
  ) %>%
  mutate(
    # If the condition is met (hcv_test_rslt == 1), generate 1000 random infection dates from a normal distribution
    random_infection_dtes = if_else(
      condition_met,
      list(
        # Ensure the infection date is between appointment_dte and appointment_dte_lag
        as.Date(
          pmax(
            pmin(
              rnorm(1000, mean = as.numeric(appointment_dte) + (as.numeric(appointment_dte_lag) - as.numeric(appointment_dte)) / 2,
                    sd = (as.numeric(appointment_dte_lag) - as.numeric(appointment_dte)) / 4), # Use a fraction of the range as SD
              as.numeric(appointment_dte_lag)  # Ensure it's not after appointment_dte_lag
            ),
            as.numeric(appointment_dte)  # Ensure it's not before appointment_dte
          ),
          origin = "1970-01-01"
        )
      ),
      list(NA_Date_)  # Return NA_Date_ for those who didn't test positive
    )
  ) %>%
  unnest_longer(random_infection_dtes) %>%  # Expand random infection dates into rows
  mutate(
    iteration = row_number(), # Label each iteration
    days_risk = if_else(
      condition_met, 
      as.numeric(random_infection_dtes - appointment_dte),  # Calculate days at risk for positive tests
      as.numeric(appointment_dte_lag - appointment_dte)  # For others, use the full period (negative test to positive test date)
    ),
    person_years = days_risk / 365.25  # Convert days at risk to person-years
  ) %>%
  ungroup()


































# Calculate incidence rates for each iteration
incidence_rates <- romania_pwid_hcv_test_iterations %>%
  group_by(iteration) %>%
  summarize(
    total_infections = n(),
    total_person_years = sum(person_years, na.rm = TRUE),
    incidence_rate = total_infections / total_person_years * 100 # per 100 person-years
  ) %>%
  ungroup()

# Compute 95% uncertainty intervals
incidence_summary <- incidence_rates %>%
  summarize(
    mean_incidence_rate = mean(incidence_rate),
    ui_lower = quantile(incidence_rate, 0.025),
    ui_upper = quantile(incidence_rate, 0.975)
  )

# Print results
print(incidence_summary)




































# Generate random infection dates for each individual across 1000 iterations
infected_iterations <- romania_pwid_hcv_test %>%
  filter(hcv_test_rslt == 1) %>%  # Only include incident infections
  mutate(
    iteration = list(1:1000)  # Create iterations
  ) %>%
  unnest(iteration) %>%
  rowwise() %>%
  mutate(
    # Generate a unique random infection date for each iteration
    random_infection_dte = as.Date(
      runif(1, as.numeric(appointment_dte), as.numeric(appointment_dte_lag)), 
      origin = "1970-01-01"
    ),
    # Calculate days at risk for each iteration
    days_risk = as.numeric(random_infection_dte - appointment_dte),
    # Convert days at risk to person-years
    person_years = days_risk / 365.25
  ) %>%
  ungroup()

# Calculate total person-time using the infected_iterations dataframe
total_person_time <- infected_iterations %>%
  summarize(
    total_person_years = sum(person_years) / 1000  # Divide by 1000 iterations
  ) %>%
  pull(total_person_years)

# Calculate total infections and incidence rates for each iteration
incidence_rates <- infected_iterations %>%
  group_by(iteration) %>%
  summarize(
    total_infections = n(),  # Total infections in this iteration
    incidence_rate = (total_infections / total_person_time) * 100  # Incidence per 100 person-years
  )

# Construct 95% uncertainty intervals (UI) across all iterations
ui <- incidence_rates %>%
  summarize(
    lower_95 = quantile(incidence_rate, 0.025),
    upper_95 = quantile(incidence_rate, 0.975),
    mean_rate = mean(incidence_rate)
  )

# Output the results
print(ui)





























# generate random infection dates for each individual across 1000 iterations
infected_iterations <- romania_pwid_hcv_test %>%
  rowwise() %>%
  mutate(
    iteration = list(1:1000)
  ) %>%
  unnest(iteration) %>%
  mutate(
    # generate random infection dates
    random_infection_dte = as.Date(
      runif(n(), as.numeric(appointment_dte), as.numeric(appointment_dte_lag)), 
      origin = "1970-01-01"
    ),
    # calculate days at risk
    days_risk = as.numeric(pmin(random_infection_dte, appointment_dte_lag) - appointment_dte),
    # calculate person-years
    person_years = days_risk / 365.25
  ) %>%
  ungroup()

# check total person-time for all individuals across 1000 iterations
total_person_time <- sum(infected_iterations$person_years)
total_person_time <- total_person_time/1000

# Step 3: Calculate total infections for each iteration
incidence_rates <- infected_iterations %>%
  group_by(iteration) %>%
  reframe(
    total_infections = sum(hcv_test_rslt == 1),  # Total infections in this iteration
    incidence_rate = (total_infections / total_person_time) * 100  # Incidence rate per 100 person-years
)

# Debug: Print total infections in each iteration
print(incidence_rates)

# Step 4: Construct 95% uncertainty interval (UI)
ui <- incidence_rates %>%
  filter(is.finite(incidence_rate)) %>% # Exclude Inf or NaN values
  summarize(
    lower_95 = quantile(incidence_rate, 0.025),
    upper_95 = quantile(incidence_rate, 0.975),
    mean_rate = mean(incidence_rate)
  )

# Output the results
print(ui)


























# calculate incidence rate for each iteration
incidence_rates <- romania_pwid_hcv_test_iterations %>%
  group_by(iteration) %>%
  summarize(
    total_infections = sum(!is.na(random_infection_dtes)), # Count infections in this iteration
    total_person_time = sum(days_risk) / 365.25,          # Total person-years at risk
    incidence_rate = total_infections / total_person_time # Incidence rate per person-year
  ) %>%
  ungroup()

# calculate 95% uncertainty interval (UI)
ui <- incidence_rates %>%
  summarize(
    lower_95 = quantile(incidence_rate, 0.025),
    upper_95 = quantile(incidence_rate, 0.975),
    mean_rate = mean(incidence_rate)
  )

# Display results
print(ui)






# update days_risk with random infection date
romania_pwid_hcv_test <- romania_pwid_hcv_test %>%
  rowwise() %>%
  mutate(
  condition_met = hcv_test_rslt == 1,  # check incident infection
  # generate random infection date
    random_infection_dte = if (condition_met) {
      as.Date(runif(1, as.numeric(appointment_dte), as.numeric(appointment_dte_lag)), origin = "1970-01-01")
    } else {
      NA
    },
    
    # calculate days at risk
    days_risk = if (condition_met) {
      as.numeric(random_infection_dte - appointment_dte)
    } else {
      as.numeric(appointment_dte_lag - appointment_dte)
    }
  ) %>%
  ungroup()



# calculate midpoint year
romania_pwid_hcv_test <- romania_pwid_hcv_test %>%
  mutate(midpoint_year = if_else(
    hcv_test_rslt_lag == 0,
    year(appointment_dte_lag + (appointment_dte - appointment_dte_lag) / 2),
    year(random_infection_dte)
  ))

# rename hcv_test_rslt_lag
romania_pwid_hcv_test <- romania_pwid_hcv_test %>%
  select(-hcv_test_rslt) %>%
  rename(hcv_test_rslt = hcv_test_rslt_lag)

# save testing data
write_xlsx(romania_pwid_hcv_test,"hcv_test_data.xlsx")

## exposure data

# keep columns of interest
romania_pwid_hcv_exposure <- subset(romania_pwid_hcv, select = c(id, gender, dob, drug_type, sex_work_current, msm_current, homeless_current, ethnic_roma, hiv, pres_distributed, syringes_distributed_1ml, syringes_distributed_2ml, syringes_recovered)) 

# replace NA with 0
romania_pwid_hcv_exposure <- romania_pwid_hcv_exposure %>%
  mutate(across(c(sex_work_current, msm_current, homeless_current, ethnic_roma, hiv), ~ replace_na(., 0)))

# remove last row per id 
romania_pwid_hcv_exposure <- romania_pwid_hcv_exposure %>%
  arrange(id) %>%
  group_by(id) %>%
  slice(-n())  

# save exposure data
write_xlsx(romania_pwid_hcv_exposure,"hcv_exposure_data.xlsx")

# create analysis df

# sequence by id for merge
romania_pwid_hcv_exposure <- romania_pwid_hcv_exposure %>%
  arrange(id) %>%
  mutate(id_seq = row_number())

romania_pwid_hcv_test <- romania_pwid_hcv_test %>%
  rename(id_seq = hcv_test_seq)

# merge
romania_pwid_hcv_analysis <- left_join(romania_pwid_hcv_exposure, romania_pwid_hcv_test, by = c("id", "id_seq"))

# remove rows where days at risk is less than 30
romania_pwid_hcv_analysis <- romania_pwid_hcv_analysis %>%
  filter(!days_risk < 30)

# remove rows where days at risk is greater than 730
romania_pwid_hcv_analysis <- romania_pwid_hcv_analysis %>%
  filter(!days_risk > 730)

# make time variables numeric
romania_pwid_hcv_analysis$days_risk <- as.numeric(romania_pwid_hcv_analysis$days_risk)
romania_pwid_hcv_analysis$appointment_dte <- as.numeric(romania_pwid_hcv_analysis$appointment_dte)
romania_pwid_hcv_analysis$appointment_dte_lag <- as.numeric(romania_pwid_hcv_analysis$appointment_dte_lag)

# make exposure variables categorical
romania_pwid_hcv_analysis$midpoint_year <- factor(romania_pwid_hcv_analysis$midpoint_year)

# save data
write_xlsx(romania_pwid_hcv_analysis,"hcv_data_analysis.xlsx")






