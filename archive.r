

# # yearly poisson regression

# # Initialize lists to store the models and results
# poisson_models_long <- list()
# poisson_results_long <- list()

# # Loop through all 1000 processed dataframes
# for (i in 1:1000) {
#   cat("Fitting models for dataframe", i, "of", 1000, "\n")
  
#   # Get the processed dataframe for the current iteration
#   df <- processed_dataframes_long[[i]]
  
#   # Fit the Poisson model
#   poisson_model_long <- glm(hcv_test_rslt ~ year + offset(log(time_at_risk)), family = poisson(link = "log"), data = df)
  
#   # Store the model in the list
#   poisson_models_long[[i]] <- poisson_model_long
  
#   # Extract the coefficients and their standard errors
#   coef_df <- as.data.frame(coef(summary(poisson_model_long)))
#   coef_df$year <- rownames(coef_df)
#   coef_df$iteration <- i
  
#   # Calculate the incidence rate and 95% confidence intervals for each year
#   coef_df <- coef_df %>%
#     mutate(
#       incidence_rate = exp(Estimate) * 100,
#       lower_ci = exp(Estimate - 1.96 * `Std. Error`) * 100,
#       upper_ci = exp(Estimate + 1.96 * `Std. Error`) * 100
#     )
  
#   # Store the results in the list
#   poisson_results_long[[i]] <- coef_df
# }

# # Combine the results into a single dataframe
# results_poisson_model_long <- do.call(rbind, poisson_results_long)

# # Print the first few rows of the results dataframe
# cat("Results for the Poisson model:\n")
# print(head(results_poisson_model_long))

# # Save the results dataframes to CSV files
# write.csv(results_poisson_model_long, "results_poisson_model_long_results.csv", row.names = TRUE)

# ## poisson modelling

# # Initialize a list to store the yearly incidence rates and confidence intervals
# yearly_incidence_results <- list()

# # Loop through all 1000 Poisson models
# for (i in 1:1000) {
#   # Get the Poisson model for the current iteration
#   poisson_model <- poisson_models_long[[i]]
  
#   # Extract the coefficients and their standard errors for each year
#   coef_df <- as.data.frame(coef(summary(poisson_model)))
#   coef_df$year <- rownames(coef_df)
#   coef_df$iteration <- i
  
#   # Calculate the incidence rate and 95% confidence intervals for each year
#   coef_df <- coef_df %>%
#     mutate(
#       incidence_rate = exp(Estimate) * 100,
#       lower_ci = exp(Estimate - 1.96 * `Std. Error`) * 100,
#       upper_ci = exp(Estimate + 1.96 * `Std. Error`) * 100
#     )
  
#   # Store the results in the list
#   yearly_incidence_results[[i]] <- coef_df
# }

# # Save the yearly incidence results as 1000 dataframes
# for (i in 1:1000) {
#   write.csv(yearly_incidence_results[[i]], paste0("yearly_incidence_results_", i, ".csv"), row.names = TRUE)
# }

# # Combine the yearly incidence results into a single dataframe
# results_yearly_incidence <- do.call(rbind, yearly_incidence_results)

# # Filter the results to include only the specified years
# filtered_results_yearly_incidence <- results_yearly_incidence %>%
#   filter(year %in% c("year2014", "year2015", "year2016", "year2017", "year2018", "year2019", "year2020", "year2021", "year2022"))

# # Print the first few rows of the filtered yearly incidence results dataframe
# cat("Filtered yearly incidence results:\n")
# print(head(filtered_results_yearly_incidence))

# # Save the filtered yearly incidence results dataframe to a CSV file
# write.csv(filtered_results_yearly_incidence, "filtered_yearly_incidence_results.csv", row.names = TRUE)

# # Calculate the 2.5th, 97.5th, and median percentiles for each year
# uncertainty_intervals <- filtered_results_yearly_incidence %>%
#   group_by(year) %>%
#   summarize(
#     median_incidence_rate = median(incidence_rate),
#     lower_ci = quantile(incidence_rate, 0.025),
#     upper_ci = quantile(incidence_rate, 0.975)
#   )

# # Print the first few rows of the uncertainty intervals dataframe
# cat("Uncertainty intervals for yearly incidence rates:\n")
# print(head(uncertainty_intervals))

# # Save the uncertainty intervals dataframe to a CSV file
# write.csv(uncertainty_intervals, "uncertainty_intervals_yearly_incidence.csv", row.names = TRUE)

# # Calculate the overall incidence rate and 95% confidence intervals for the specified years
# overall_incidence_results <- filtered_results_yearly_incidence %>%
#   summarize(
#     median_incidence_rate = median(incidence_rate),
#     lower_ci = quantile(incidence_rate, 0.025),
#     upper_ci = quantile(incidence_rate, 0.975)
#   )

# # Print the overall incidence results
# cat("Overall incidence results:\n")
# print(overall_incidence_results)

# # Save the overall incidence results to a CSV file
# write.csv(overall_incidence_results, "overall_incidence_results.csv", row.names = TRUE)

# # Function to calculate incidence rate and 95% CI per 100 person-years using negative binomial distribution
# calculate_incidence_rate_nb <- function(events, person_years) {
#   rate <- (events / person_years) * 100
#   se <- sqrt(events + (events^2 / person_years)) / person_years * 100
#   lower <- max(0, rate - 1.96 * se)
#   upper <- rate + 1.96 * se
#   return(c(rate, lower, upper))
# }

# # Calculate the overall incidence rate and 95% CI per 100 person-years for each row using negative binomial distribution
# final_summed_df <- final_summed_df %>%
#   rowwise() %>%
#   mutate(
#     overall_incidence_rate = calculate_incidence_rate_nb(hcv_test_rslt, person_years)[1],
#     overall_incidence_lower = calculate_incidence_rate_nb(hcv_test_rslt, person_years)[2],
#     overall_incidence_upper = calculate_incidence_rate_nb(hcv_test_rslt, person_years)[3]
#   ) %>%
#   ungroup()

# # Calculate yearly incidence rates and 95% CIs per 100 person-years for each row using negative binomial distribution
# for (year in 2013:2022) {
#   final_summed_df <- final_summed_df %>%
#     rowwise() %>%
#     mutate(
#       !!paste0("incidence_rate_", year) := calculate_incidence_rate_nb(get(paste0("hcv_test_", year)), get(paste0("X", year)))[1],
#       !!paste0("incidence_lower_", year) := calculate_incidence_rate_nb(get(paste0("hcv_test_", year)), get(paste0("X", year)))[2],
#       !!paste0("incidence_upper_", year) := calculate_incidence_rate_nb(get(paste0("hcv_test_", year)), get(paste0("X", year)))[3]
#     ) %>%
#     ungroup()
# }


# # Load first dataframe
# midpoint_dataframe <- processed_dataframes[[1]]

# # Ensure `year` is numeric or character before using it in `case_when()`
# if (is.factor(midpoint_dataframe$year)) {
#   midpoint_dataframe <- midpoint_dataframe %>%
#     mutate(year = as.numeric(as.character(year)))  # Convert factor to numeric
# } else if (is.character(midpoint_dataframe$year)) {
#   midpoint_dataframe <- midpoint_dataframe %>%
#     mutate(year = as.numeric(year))  # Convert character to numeric
# }

# # Create new columns
# midpoint_dataframe <- midpoint_dataframe %>%
#   mutate(
#     midpoint_date = ifelse(
#       hcv_test_rslt == 1 | hcv_test_rslt == 0,  # Calculate midpoint for both 1 and 0
#       as.Date((as.numeric(appointment_dte) + as.numeric(appointment_dte_lag)) / 2, origin = "1970-01-01"),
#       NA  # Set to NA for rows where hcv_test_rslt is not 1 or 0
#     ),
#     midpoint_date = as.Date(midpoint_date),  # Ensure midpoint_date is formatted as a Date
#     midpoint_year = year(midpoint_date),  # Extract the year from midpoint_date
#     year = case_when(
#       !is.na(midpoint_year) ~ midpoint_year,  # Replace with midpoint_year if not NA
#       TRUE ~ year  # Otherwise, keep the original year
#     ),
#     person_years = ifelse(
#       hcv_test_rslt == 1, 
#       as.numeric(difftime(midpoint_date, appointment_dte, units = "days")) / 365.25,  # Use midpoint_date - appointment_dte when hcv_test_rslt == 1
#       as.numeric(difftime(appointment_dte_lag, appointment_dte, units = "days")) / 365.25  # Use appointment_dte_lag - appointment_dte otherwise
#     )
#   )

# # Ensure midpoint_date is explicitly formatted as a Date
# midpoint_dataframe <- midpoint_dataframe %>%
#   mutate(midpoint_date = as.Date(midpoint_date))

# # Keep only the specified columns in midpoint_dataframe
# midpoint_dataframe <- midpoint_dataframe %>%
#   select(
#     id, 
#     appointment_dte, 
#     appointment_dte_lag, 
#     hcv_test_rslt, 
#     random_infection_dtes, 
#     person_years, 
#     midpoint_year, 
#     midpoint_date, 
#     year
#   )

# # View the updated dataframe
# View(midpoint_dataframe)

# # Calculate total HCV infections and total person-years
# total_hcv_infections <- sum(midpoint_dataframe$hcv_test_rslt, na.rm = TRUE)
# total_person_years <- sum(midpoint_dataframe$person_years, na.rm = TRUE)

# # Calculate the overall incidence rate (per 100 person-years)
# overall_incidence_rate <- (total_hcv_infections / total_person_years) * 100

# # Calculate the 95% confidence intervals
# lower_bound <- overall_incidence_rate - 1.96 * sqrt(total_hcv_infections / (total_person_years^2)) * 100
# upper_bound <- overall_incidence_rate + 1.96 * sqrt(total_hcv_infections / (total_person_years^2)) * 100

# # Print the results
# cat("Total New Infections:", total_hcv_infections, "\n")
# cat("Total Person-Years:", total_person_years, "\n")
# cat("Overall HCV Incidence Rate (per 100 person-years):", overall_incidence_rate, "\n")
# cat("95% Confidence Interval: [", lower_bound, ", ", upper_bound, "]\n")

# # Define two-yearly intervals
# midpoint_dataframe <- midpoint_dataframe %>%
#   mutate(
#     two_year_interval = case_when(
#       year %in% c(2013, 2014) ~ "2013-2014",
#       year %in% c(2015, 2016) ~ "2015-2016",
#       year %in% c(2017, 2018) ~ "2017-2018",
#       year %in% c(2019, 2020) ~ "2019-2020",
#       year %in% c(2021, 2022) ~ "2021-2022",
#       TRUE ~ NA_character_  # Exclude years outside the range
#     )
#   )

# # Group by two-year intervals and calculate totals
# two_yearly_results <- midpoint_dataframe %>%
#   filter(!is.na(two_year_interval)) %>%  # Exclude rows without a valid interval
#   group_by(two_year_interval) %>%
#   summarise(
#     total_hcv_infections = sum(hcv_test_rslt, na.rm = TRUE),
#     total_person_years = sum(person_years, na.rm = TRUE),
#     incidence_rate = (total_hcv_infections / total_person_years) * 100,
#     lower_bound = (total_hcv_infections / total_person_years) * 100 - 
#                   1.96 * sqrt(total_hcv_infections / (total_person_years^2)) * 100,
#     upper_bound = (total_hcv_infections / total_person_years) * 100 + 
#                   1.96 * sqrt(total_hcv_infections / (total_person_years^2)) * 100
#   )

# # Print the results
# cat("Two-Yearly Interval Results:\n")
# print(two_yearly_results)
# View(two_yearly_results)





# # Load first dataframe
# midpoint_dataframe <- processed_dataframes_long[[1]]

# # replace midpoint year with NA if hcv test result is negative
# midpoint_dataframe <- midpoint_dataframe %>%
#   mutate(
#     midpoint_year = ifelse(hcv_test_rslt == 0, NA, midpoint_year)  # Replace midpoint_year with NA if hcv_test_rslt == 0
#   )

# # Create a dataframe with rows for years 2013 to 2022 and calculate cases and years_at_risk
# yearly_data <- midpoint_dataframe %>%
#   group_by(year) %>%
#   summarise(
#     cases = sum(hcv_test_rslt, na.rm = TRUE),        # Sum of hcv_test_rslt for each year
#     years_at_risk = sum(time_at_risk, na.rm = TRUE)  # Sum of time_at_risk for each year
#   ) %>%
#   filter(year %in% 2013:2022)  # Ensure only rows for years 2013 to 2022 are included

# # View the updated dataframe
# View(midpoint_dataframe)

# # Define two-yearly intervals
# midpoint_dataframe <- midpoint_dataframe %>%
#   mutate(
#     two_year_interval = case_when(
#       year %in% c(2013, 2014) ~ "2013-2014",
#       year %in% c(2015, 2016) ~ "2015-2016",
#       year %in% c(2017, 2018) ~ "2017-2018",
#       year %in% c(2019, 2020) ~ "2019-2020",
#       year %in% c(2021, 2022) ~ "2021-2022",
#       TRUE ~ NA_character_  # Exclude years outside the range
#     )
#   )

# # Group by two-year intervals and calculate totals
# two_yearly_results <- midpoint_dataframe %>%
#   filter(!is.na(two_year_interval)) %>%  # Exclude rows without a valid interval
#   group_by(two_year_interval) %>%
#   summarise(
#     total_hcv_infections = sum(hcv_test_rslt, na.rm = TRUE),  # Total cases
#     total_person_years = sum(time_at_risk, na.rm = TRUE),     # Total person-years
#     incidence_rate = (total_hcv_infections / total_person_years) * 100,  # Incidence rate per 100 person-years
#     lower_bound = (total_hcv_infections / total_person_years) * 100 - 
#                   1.96 * sqrt(total_hcv_infections / (total_person_years^2)) * 100,  # Lower 95% CI
#     upper_bound = (total_hcv_infections / total_person_years) * 100 + 
#                   1.96 * sqrt(total_hcv_infections / (total_person_years^2)) * 100   # Upper 95% CI
#   )

# # View the results
# print(two_yearly_results)
# View(two_yearly_results)

# # Save the two-yearly results to a CSV file
# write.csv(two_yearly_results, "two_yearly_results.csv", row.names = FALSE)

# # Create a plot for the two-yearly interval results
# HCV_incidence_plot_midpoint <- ggplot(two_yearly_results, aes(x = two_year_interval, y = incidence_rate)) +
#   geom_line(group = 1, color = "gray", linewidth = 0.8, linetype = "solid") +  # Make the line gray and adjust thickness
#   geom_point(shape = 18, size = 4, color = "gray") +  # Make the diamonds (points) gray
#   geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound), width = 0.1, color = "black", size = 0.8) +  # Adjust error bar width and size
#   theme_minimal(base_size = 14) +  # Use a minimal theme
#   labs(
#     x = "Two-Yearly Interval",
#     y = "Incidence Rate (per 100 Person-Years)"
#   ) +
#   scale_y_continuous(expand = c(0, 0), limits = c(0, max(two_yearly_results$upper_bound, na.rm = TRUE) * 1.1)) +  # Adjust y-axis limits
#   theme(
#     axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for better readability
#     axis.title.x = element_text(margin = margin(t = 10)),  # Add margin to x-axis title
#     axis.title.y = element_text(margin = margin(r = 10)),  # Add margin to y-axis title
#     panel.grid.major = element_blank(),  # Remove major gridlines
#     panel.grid.minor = element_blank(),  # Remove minor gridlines
#     panel.background = element_rect(fill = "white", color = NA),  # Set panel background to white
#     plot.background = element_rect(fill = "white", color = NA)  # Set plot background to white
#   )

# # Save the plot as a PNG file in the "plots" folder with the suffix "_midpoint"
# ggsave("plots/HCV_incidence_plot_midpoint.png", plot = HCV_incidence_plot_midpoint, width = 10, height = 6, dpi = 300)















# # Ensure appointment_dte and appointment_dte_lag are in Date format
# midpoint_dataframe <- midpoint_dataframe %>%
#   mutate(
#     appointment_dte = as.Date(appointment_dte, format = "%Y-%m-%d"),
#     appointment_dte_lag = as.Date(appointment_dte_lag, format = "%Y-%m-%d")
#   )

# # Calculate person-years for each year of observation
# person_years_df <- midpoint_dataframe %>%
#   rowwise() %>%
#   mutate(
#     start_year = year(appointment_dte),
#     end_year = year(appointment_dte_lag),
#     start_date = appointment_dte,
#     end_date = appointment_dte_lag
#   ) %>%
#   do({
#     data <- .
#     years <- seq(data$start_year, data$end_year)
#     person_years <- sapply(years, function(year) {
#       start <- max(as.Date(paste0(year, "-01-01")), data$start_date)
#       end <- min(as.Date(paste0(year, "-12-31")), data$end_date)
#       as.numeric(difftime(end, start, units = "days")) / 365.25
#     })
#     names(person_years) <- years
#     data.frame(t(person_years))
#   }) %>%
#   ungroup()

# View(person_years_df)

# # check for rows where appointment_dte_lag is less than appointment_dte
# invalid_rows <- midpoint_dataframe %>%
#   filter(appointment_dte_lag < appointment_dte)
# cat("Number of rows where appointment_dte_lag is less than appointment_dte:", nrow(invalid_rows), "\n")














# ### code being run now ###

# bootstrap_incidence_random <- function(data, n_infection_samples = 100) {
#   resample <- data %>% sample_n(size = nrow(data), replace = TRUE)
#   resample <- resample %>%
#     rowwise() %>%
#     mutate(
#       pt = if (hcv_test_rslt == 1) {
#         mean(
#           sapply(1:n_infection_samples, function(x) {
#             infection_date <- as.Date(runif(1, min = as.numeric(appointment_dte), max = as.numeric(appointment_dte_lag)), origin = "1970-01-01")
#             max(as.numeric(infection_date - appointment_dte), 1)
#           })
#         )
#       } else {
#         max(as.numeric(appointment_dte_lag - appointment_dte), 1)
#       }
#     ) %>%
#     ungroup()
#   total_pt <- sum(resample$pt)
#   cases <- sum(resample$hcv_test_rslt == 1)
#   (cases / total_pt) * 365.25 * 100
# }

# set.seed(42)
# bootstrap_results <- replicate(100, bootstrap_incidence_random(df_all_participants, n_infection_samples = 100))

# # Summarize results
# ir_mean <- mean(bootstrap_results, na.rm = TRUE)
# ir_lower <- quantile(bootstrap_results, 0.025, na.rm = TRUE)
# ir_upper <- quantile(bootstrap_results, 0.975, na.rm = TRUE)

# cat(sprintf("Incidence rate: %.2f per 100 person-years (95%% CI: %.2f – %.2f)\n",
#             ir_mean, ir_lower, ir_upper))





# ### new code to run later ###

# bootstrap_incidence_random <- function(data, n_infection_samples = 10) {
#   # Stratify: separate cases and non-cases
#   cases_data <- data %>% filter(hcv_test_rslt == 1)
#   noncases_data <- data %>% filter(hcv_test_rslt == 0)
  
#   # Bootstrap with replacement within each stratum
#   resample_cases <- cases_data %>% sample_n(nrow(cases_data), replace = TRUE)
#   resample_noncases <- noncases_data %>% sample_n(nrow(noncases_data), replace = TRUE)
  
#   # Combine resampled data
#   resample <- bind_rows(resample_cases, resample_noncases)
  
#   # Estimate person-time for each participant
#   resample <- resample %>%
#     rowwise() %>%
#     mutate(
#       pt = if (hcv_test_rslt == 1) {
#         mean(
#           sapply(1:n_infection_samples, function(x) {
#             infection_date <- as.Date(
#               runif(1, min = as.numeric(appointment_dte), max = as.numeric(appointment_dte_lag)),
#               origin = "1970-01-01"
#             )
#             max(as.numeric(infection_date - appointment_dte), 1)
#           })
#         )
#       } else {
#         max(as.numeric(appointment_dte_lag - appointment_dte), 1)
#       }
#     ) %>%
#     ungroup()
  
#   total_pt <- sum(resample$pt, na.rm = TRUE)
#   cases <- sum(resample$hcv_test_rslt == 1)
  
#   # Incidence rate per 100 person-years
#   (cases / total_pt) * 365.25 * 100
# }

# set.seed(42)
# bootstrap_results <- replicate(100, bootstrap_incidence_random(df_all_participants, n_infection_samples = 10))

# # Summarize bootstrap results
# ir_mean <- mean(bootstrap_results, na.rm = TRUE)
# ir_lower <- quantile(bootstrap_results, 0.025, na.rm = TRUE)
# ir_upper <- quantile(bootstrap_results, 0.975, na.rm = TRUE)

# cat(sprintf("Incidence rate: %.2f per 100 person-years (95%% CI: %.2f – %.2f)\n",
#             ir_mean, ir_lower, ir_upper))


# ### new code to run later ###

# bootstrap_incidence_random <- function(data) {
#   # Stratify: separate cases and non-cases
#   cases_data <- data %>% filter(hcv_test_rslt == 1)
#   noncases_data <- data %>% filter(hcv_test_rslt == 0)
  
#   # Bootstrap with replacement within each stratum
#   resample_cases <- cases_data %>% sample_n(nrow(cases_data), replace = TRUE)
#   resample_noncases <- noncases_data %>% sample_n(nrow(noncases_data), replace = TRUE)
  
#   # Combine resampled data
#   resample <- bind_rows(resample_cases, resample_noncases)
  
#   # Estimate person-time
#   resample <- resample %>%
#     rowwise() %>%
#     mutate(
#       pt = if (hcv_test_rslt == 1) {
#         # Sample ONE infection date between appointment_dte and appointment_dte_lag
#         infection_date <- as.Date(
#           runif(1, min = as.numeric(appointment_dte), max = as.numeric(appointment_dte_lag)),
#           origin = "1970-01-01"
#         )
#         max(as.numeric(infection_date - appointment_dte), 1)
#       } else {
#         # Full follow-up time for non-seroconverters
#         max(as.numeric(appointment_dte_lag - appointment_dte), 1)
#       }
#     ) %>%
#     ungroup()
  
#   total_pt <- sum(resample$pt, na.rm = TRUE)
#   cases <- sum(resample$hcv_test_rslt == 1)
  
#   # Incidence rate per 100 person-years
#   (cases / total_pt) * 365.25 * 100
# }

# set.seed(42)
# bootstrap_results <- replicate(100, bootstrap_incidence_random(df_all_participants))

# # Summarize bootstrap results
# ir_mean <- mean(bootstrap_results, na.rm = TRUE)
# ir_lower <- quantile(bootstrap_results, 0.025, na.rm = TRUE)
# ir_upper <- quantile(bootstrap_results, 0.975, na.rm = TRUE)

# cat(sprintf("Incidence rate: %.2f per 100 person-years (95%% CI: %.2f – %.2f)\n",
#             ir_mean, ir_lower, ir_upper))







# 1. Uncertainty in Infection Timing
# For people who seroconvert, you only know they were infected sometime between their last negative and first positive test.
# Assigning a random infection date within this interval (the "random-point" method) reflects the true uncertainty about when infection occurred.
# 2. Bootstrapping Adds Sampling Uncertainty
# By resampling participants with replacement, you account for sampling variability (i.e., how your results might change if you had a different sample from the same population).
# 3. Combining Both Uncertainties
# Your method combines infection timing uncertainty (random-point assignment) and sampling uncertainty (bootstrap resampling).
# This produces more realistic confidence intervals for incidence rates than methods that ignore one or both sources of uncertainty.
# 4. Standard Practice
# The random-point method is a standard approach for interval-censored data in infectious disease epidemiology.
# Bootstrapping is a standard approach for estimating confidence intervals when analytic solutions are not available or assumptions (e.g., normality) are not met.
# 5. Comparison to Other Methods
# The midpoint method (assigning infection to the midpoint of the interval) is simpler but underestimates uncertainty.
# Your approach is more robust and widely recommended for studies with interval-censored seroconversion.


















# # Function to calculate incidence rate from a bootstrap resample
# bootstrap_incidence <- function(data) {
#   resample <- data %>% sample_n(size = nrow(data), replace = TRUE)
  
#   resample <- resample %>%
#     mutate(
#       start_date = appointment_dte + 1,  # Fixed: should be appointment_dte, not appointment_dte_lag
#       infection_date = map2_dbl(start_date, appointment_dte_lag, ~ {
#         start_num <- as.numeric(.x)
#         end_num <- as.numeric(.y)
#         if (start_num > end_num) {
#           end_num
#         } else {
#           sample(start_num:end_num, 1)
#         }
#       }),
#       infection_date = as.Date(infection_date, origin = "1970-01-01"),
#       pt = as.numeric(infection_date - appointment_dte),
#       pt = ifelse(pt <= 0, 1, pt)  # min 1 day person-time
#     )
  
#   total_pt <- sum(resample$pt)
#   cases <- nrow(resample)
  
#   (cases / total_pt) * 365.25 * 100
# }

# # Filter valid seroconverters only
# df_seroconv <- romania_pwid_hcv_test %>%
#   filter(hcv_test_rslt == 1) 

# # Run bootstrap 1000 times
# set.seed(42)
# bootstrap_results <- replicate(100, bootstrap_incidence(df_seroconv))

# cat("Number of valid seroconverters:", nrow(df_seroconv), "\n")
# cat("Number of bootstrap results with NaN:", sum(is.nan(bootstrap_results)), "\n")
# print(bootstrap_results)

# # Summarize results
# ir_mean <- mean(bootstrap_results, na.rm = TRUE)
# ir_lower <- quantile(bootstrap_results, 0.025, na.rm = TRUE)
# ir_upper <- quantile(bootstrap_results, 0.975, na.rm = TRUE)

# cat(sprintf("Incidence rate: %.2f per 100 person-years (95%% CI: %.2f – %.2f)\n",
#             ir_mean, ir_lower, ir_upper))


























