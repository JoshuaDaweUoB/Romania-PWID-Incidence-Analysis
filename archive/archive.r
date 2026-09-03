

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







## loop over 1000 iterations

# Initialize a list to store the results for all 1000 dataframes
all_two_yearly_results <- list()

# Loop over all 1000 dataframes in the list
for (i in 1:length(processed_dataframes_long)) {
  cat("Processing dataframe", i, "of", length(processed_dataframes_long), "\n")
  
  # Load the current dataframe
  midpoint_dataframe <- processed_dataframes_long[[i]]
  
  # Replace midpoint_year with NA if hcv_test_rslt is negative
  midpoint_dataframe <- midpoint_dataframe %>%
    mutate(
      midpoint_year = ifelse(hcv_test_rslt == 0, NA, midpoint_year)  # Replace midpoint_year with NA if hcv_test_rslt == 0
    )
  
  # Create a dataframe with rows for years 2013 to 2022 and calculate cases and years_at_risk
  yearly_data <- midpoint_dataframe %>%
    group_by(year) %>%
    summarise(
      cases = sum(hcv_test_rslt, na.rm = TRUE),        # Sum of hcv_test_rslt for each year
      years_at_risk = sum(time_at_risk, na.rm = TRUE)  # Sum of time_at_risk for each year
    ) %>%
    filter(year %in% 2013:2022)  # Ensure only rows for years 2013 to 2022 are included
  
  # Define two-yearly intervals
  midpoint_dataframe <- midpoint_dataframe %>%
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
  two_yearly_results <- midpoint_dataframe %>%
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
  
  # Store the results in the list
  all_two_yearly_results[[i]] <- two_yearly_results
}

# Combine all results into a single dataframe
combined_two_yearly_results <- bind_rows(all_two_yearly_results, .id = "iteration")

# Save the combined results to a CSV file
write.csv(combined_two_yearly_results, "combined_two_yearly_results.csv", row.names = FALSE)

# View the combined results
View(combined_two_yearly_results)

# Initialize a list to store the results for all 1000 dataframes
all_two_yearly_results <- list()

# longitudinal analysis - approach 2

# Loop over all 1000 dataframes in the list
for (i in 1:length(processed_dataframes_long)) {
  cat("Processing dataframe", i, "of", length(processed_dataframes_long), "\n")
  
  # Load the current dataframe
  midpoint_dataframe <- processed_dataframes_long[[i]]
  
  # Replace midpoint_year with NA if hcv_test_rslt is negative
  midpoint_dataframe <- midpoint_dataframe %>%
    mutate(
      midpoint_year = ifelse(hcv_test_rslt == 0, NA, midpoint_year)  # Replace midpoint_year with NA if hcv_test_rslt == 0
    )
  
  # Create a dataframe with rows for years 2013 to 2022 and calculate cases and years_at_risk
  yearly_data <- midpoint_dataframe %>%
    group_by(year) %>%
    summarise(
      cases = sum(hcv_test_rslt, na.rm = TRUE),        # Sum of hcv_test_rslt for each year
      years_at_risk = sum(time_at_risk, na.rm = TRUE)  # Sum of time_at_risk for each year
    ) %>%
    filter(year %in% 2013:2022)  # Ensure only rows for years 2013 to 2022 are included
  
  # Define two-yearly intervals
  midpoint_dataframe <- midpoint_dataframe %>%
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
  two_yearly_results <- midpoint_dataframe %>%
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
  
  # Store the results in the list
  all_two_yearly_results[[i]] <- two_yearly_results
}

# Combine all results into a single dataframe
combined_two_yearly_results <- bind_rows(all_two_yearly_results, .id = "iteration")

# Save the combined results to a CSV file
write.csv(combined_two_yearly_results, "combined_two_yearly_results.csv", row.names = FALSE)

# View the combined results
View(combined_two_yearly_results)

# Calculate incidence trends over time
# Group by two-yearly intervals and calculate the median and percentiles
incidence_trends <- combined_two_yearly_results %>%
  group_by(two_year_interval) %>%
  summarise(
    median_incidence_rate = median(incidence_rate, na.rm = TRUE),  # Median incidence rate
    lower_bound = quantile(incidence_rate, 0.025, na.rm = TRUE),  # 2.5th percentile
    upper_bound = quantile(incidence_rate, 0.975, na.rm = TRUE),  # 97.5th percentile
    median_total_person_years = median(total_person_years, na.rm = TRUE),  # Median total person-years
    median_total_hcv_infections = median(total_hcv_infections, na.rm = TRUE)  # Median total HCV infections
  )

# View the incidence trends
print(incidence_trends)
View(incidence_trends)

# Save the incidence trends to a CSV file
write.csv(incidence_trends, "incidence_trends.csv", row.names = FALSE)

# Create a plot for the incidence trends
HCV_incidence_trends_plot <- ggplot(incidence_trends, aes(x = two_year_interval, y = median_incidence_rate)) +
  geom_line(group = 1, color = "gray", linewidth = 0.8, linetype = "solid") +  # Solid gray line for trends
  geom_point(shape = 18, size = 4, color = "gray") +  # Gray diamonds for points
  geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound), width = 0.1, color = "black", size = 0.8) +  # Black error bars
  theme_minimal(base_size = 14) +  # Minimal theme
  labs(
    x = "Two-Yearly Interval",
    y = "Median Incidence Rate (per 100 Person-Years)"
  ) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, max(incidence_trends$upper_bound, na.rm = TRUE) * 1.1)) +  # Adjust y-axis limits
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for better readability
    axis.title.x = element_text(margin = margin(t = 10)),  # Add margin to x-axis title
    axis.title.y = element_text(margin = margin(r = 10)),  # Add margin to y-axis title
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    panel.background = element_rect(fill = "white", color = NA),  # Set panel background to white
    plot.background = element_rect(fill = "white", color = NA)  # Set plot background to white
  )

# Save the plot as a PNG file
ggsave("plots/HCV_incidence_trends_plot.png", plot = HCV_incidence_trends_plot, width = 10, height = 6, dpi = 300)

# include only rows that match id, appointment_dte, and hcv_test_seq in romania_pwid_hcv_test
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
merge_with_exposure <- function(processed_dataframes_hcv, romania_pwid_hcv_exposure) {
  # Ensure romania_pwid_hcv_exposure has no duplicate rows for id and appointment_dte
  romania_pwid_hcv_exposure <- romania_pwid_hcv_exposure %>%
    distinct(id, appointment_dte, .keep_all = TRUE)
  
  # Merge romania_pwid_hcv_exposure with each dataframe in processed_dataframes_hcv
  processed_dataframes_hcv <- lapply(processed_dataframes_hcv, function(df) {
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
  
  return(processed_dataframes_hcv)
}

# Example usage of the function
processed_dataframes_hcv <- merge_with_exposure(processed_dataframes_hcv, romania_pwid_hcv_exposure)

# Save the updated processed_dataframes_hcv to a file
saveRDS(processed_dataframes_hcv, file = "processed_dataframes_hcv.rds")

# View the first merged dataframe for QA
View(processed_dataframes_hcv[[1]])


# ## cox regression analysis

# # Initialize a list to store the results for all 1000 dataframes
# all_cox_results <- list()

# # Loop over all 1000 dataframes in processed_dataframes
# for (i in 1:length(processed_dataframes)) {
#   cat("Processing Cox regression for dataframe", i, "of", length(processed_dataframes), "\n")
  
#   # Load the current dataframe
#   df <- processed_dataframes[[i]]
  
#   # Ensure the dataframe has no missing values for the variables of interest
#   df <- df %>%
#     filter(!is.na(hcv_test_rslt) & !is.na(gender) & !is.na(dob) & 
#              !is.na(sex_work_current) & !is.na(msm_current) & 
#              !is.na(homeless_current) & !is.na(ethnic_roma))
  
#   # Create a survival object
#   surv_obj <- Surv(time = df$days_risk, event = df$hcv_test_rslt)
  
#   # List of exposures
#   exposures <- c("gender", "age_years", "sex_work_current", "homeless_current", "ethnic_roma")
  
#   # Run univariate Cox regressions
#   univariate_results <- lapply(exposures, function(var) {
#     formula <- as.formula(paste("surv_obj ~", var))
#     tryCatch(
#       coxph(formula, data = df),
#       error = function(e) {
#         cat("Error in Cox regression for", var, ":", e$message, "\n")
#         NULL
#       }
#     )
#   })
  
#   # Run a multivariable Cox regression adjusting for all exposures
#   multivariable_formula <- as.formula(paste("surv_obj ~", paste(exposures, collapse = " + ")))
#   multivariable_cox <- tryCatch(
#     coxph(multivariable_formula, data = df),
#     error = function(e) {
#       cat("Error in multivariable Cox regression:", e$message, "\n")
#       NULL
#     }
#   )
  
#   # Store the results for this dataframe
#   all_cox_results[[i]] <- list(
#     univariate = univariate_results,
#     multivariable = multivariable_cox
#   )
# }

# # Save the results to a file
# saveRDS(all_cox_results, file = "all_cox_results.rds")

# # Example: Print the summary of the multivariable Cox regression for the first dataframe
# cat("\nMultivariable Cox regression for dataframe 1:\n")
# print(summary(all_cox_results[[1]]$multivariable))

# # Initialize an empty list to store the results for each iteration
# cox_point_estimates <- list()
# lower_95CI_list <- list()
# upper_95CI_list <- list()

# # Loop through all 1000 iterations in all_cox_results
# for (i in 1:length(all_cox_results)) {
#   # Extract the univariate Cox regression results for this iteration
#   univariate_results <- all_cox_results[[i]]$univariate
  
#   # Extract the hazard ratios (HR) and confidence intervals for each exposure
#   hr_values <- sapply(univariate_results, function(model) {
#     if (!is.null(model)) {
#       exp(coef(model))  # Extract and exponentiate the coefficient to get the HR
#     } else {
#       NA  # If the model is NULL, return NA
#     }
#   })
  
#   lower_95CI <- sapply(univariate_results, function(model) {
#     if (!is.null(model)) {
#       exp(confint(model)[, 1])  # Extract and exponentiate the lower bound of the CI
#     } else {
#       NA
#     }
#   })
  
#   upper_95CI <- sapply(univariate_results, function(model) {
#     if (!is.null(model)) {
#       exp(confint(model)[, 2])  # Extract and exponentiate the upper bound of the CI
#     } else {
#       NA
#     }
#   })
  
#   # Extract the multivariable Cox regression results for this iteration
#   multivariable_model <- all_cox_results[[i]]$multivariable
  
#   # Extract the hazard ratios (HR) and confidence intervals for the multivariable model
#   multivariable_hr <- if (!is.null(multivariable_model)) {
#     exp(coef(multivariable_model))  # Extract and exponentiate the coefficients
#   } else {
#     rep(NA, length(hr_values))  # If the model is NULL, return NA for all exposures
#   }
  
#   multivariable_lower_95CI <- if (!is.null(multivariable_model)) {
#     exp(confint(multivariable_model)[, 1])  # Extract and exponentiate the lower bound of the CI
#   } else {
#     rep(NA, length(hr_values))
#   }
  
#   multivariable_upper_95CI <- if (!is.null(multivariable_model)) {
#     exp(confint(multivariable_model)[, 2])  # Extract and exponentiate the upper bound of the CI
#   } else {
#     rep(NA, length(hr_values))
#   }
  
#   # Combine the univariate and multivariable HRs and CIs into a single row
#   combined_row <- c(univariate_hr = hr_values, multivariable_hr = multivariable_hr)
#   lower_95CI_row <- c(univariate_lower_95CI = lower_95CI, multivariable_lower_95CI = multivariable_lower_95CI)
#   upper_95CI_row <- c(univariate_upper_95CI = upper_95CI, multivariable_upper_95CI = multivariable_upper_95CI)
  
#   # Store the rows in the respective lists
#   cox_point_estimates[[i]] <- combined_row
#   lower_95CI_list[[i]] <- lower_95CI_row
#   upper_95CI_list[[i]] <- upper_95CI_row
# }

# # Combine all rows into dataframes
# cox_point_estimates_df <- do.call(rbind, cox_point_estimates)
# lower_95CI_df <- do.call(rbind, lower_95CI_list)
# upper_95CI_df <- do.call(rbind, upper_95CI_list)

# # Assign column names to the dataframes
# first_non_null <- all_cox_results[[which(!sapply(all_cox_results, is.null))[1]]]
# colnames(cox_point_estimates_df) <- c(
#   paste0("univariate_", names(first_non_null$univariate)),
#   paste0("multivariable_", names(first_non_null$multivariable$coefficients))
# )
# colnames(lower_95CI_df) <- colnames(cox_point_estimates_df)
# colnames(upper_95CI_df) <- colnames(cox_point_estimates_df)

# # Calculate the median, lower, and upper bounds for the hazard ratios and confidence intervals
# summary_table <- data.frame(
#   Variable = colnames(cox_point_estimates_df),
#   Median_HR = apply(cox_point_estimates_df, 2, median, na.rm = TRUE),
#   Lower_95CI = apply(lower_95CI_df, 2, median, na.rm = TRUE),
#   Upper_95CI = apply(upper_95CI_df, 2, median, na.rm = TRUE)
# )

# # View the resulting summary table
# View(summary_table)

# # Save the summary table to a CSV file
# write.csv(summary_table, "cox_regression_summary_table.csv", row.names = FALSE)

# ## analysis among males

# # Initialize a list to store the results for all 1000 dataframes
# all_cox_results_male <- list()

# # Loop over all 1000 dataframes in processed_dataframes
# for (i in 1:length(processed_dataframes)) {
#   cat("Processing Cox regression for dataframe", i, "of", length(processed_dataframes), "\n")
  
#   # Load the current dataframe
#   df <- processed_dataframes[[i]]
  
#   # Filter the dataframe to include only rows where gender == 0 (male)
#   df <- df %>%
#     filter(gender == 0 & 
#            !is.na(hcv_test_rslt) & !is.na(dob) & 
#            !is.na(sex_work_current) & !is.na(msm_current) & 
#            !is.na(homeless_current) & !is.na(ethnic_roma))
  
#   # Check if the filtered dataframe is empty
#   if (nrow(df) == 0) {
#     cat("Filtered dataframe is empty for iteration", i, ". Skipping.\n")
#     all_cox_results_male[[i]] <- NULL
#     next
#   }
  
#   # Create a survival object
#   surv_obj_male <- Surv(time = df$days_risk, event = df$hcv_test_rslt)
  
#   # List of exposures
#   exposures_male <- c("age_years", "sex_work_current", "msm_current", "homeless_current", "ethnic_roma")
  
#   # Run univariate Cox regressions
#   univariate_results_male <- lapply(exposures_male, function(var) {
#     formula <- as.formula(paste("surv_obj_male ~", var))
#     tryCatch(
#       coxph(formula, data = df),
#       error = function(e) {
#         cat("Error in Cox regression for", var, ":", e$message, "\n")
#         NULL
#       }
#     )
#   })
  
#   # Run a multivariable Cox regression adjusting for all exposures
#   multivariable_formula_male <- as.formula(paste("surv_obj_male ~", paste(exposures_male, collapse = " + ")))
#   multivariable_cox_male <- tryCatch(
#     coxph(multivariable_formula_male, data = df),
#     error = function(e) {
#       cat("Error in multivariable Cox regression:", e$message, "\n")
#       NULL
#     }
#   )
  
#   # Store the results for this dataframe
#   all_cox_results_male[[i]] <- list(
#     univariate_male = univariate_results_male,
#     multivariable_male = multivariable_cox_male
#   )
# }

# # Save the results to a file
# saveRDS(all_cox_results_male, file = "all_cox_results_male.rds")

# # Initialize an empty list to store the results for each iteration
# cox_point_estimates_male <- list()
# lower_95CI_list_male <- list()
# upper_95CI_list_male <- list()

# # Loop through all 1000 iterations in all_cox_results_male
# for (i in 1:length(all_cox_results_male)) {
#   # Extract the univariate Cox regression results for this iteration
#   univariate_results_male <- all_cox_results_male[[i]]$univariate_male
  
#   # Extract the hazard ratios (HR) and confidence intervals for each exposure
#   hr_values_male <- sapply(univariate_results_male, function(model) {
#     if (!is.null(model)) {
#       exp(coef(model))  # Extract and exponentiate the coefficient to get the HR
#     } else {
#       NA  # If the model is NULL, return NA
#     }
#   })
  
#   lower_95CI_male <- sapply(univariate_results_male, function(model) {
#     if (!is.null(model)) {
#       exp(confint(model)[, 1])  # Extract and exponentiate the lower bound of the CI
#     } else {
#       NA
#     }
#   })
  
#   upper_95CI_male <- sapply(univariate_results_male, function(model) {
#     if (!is.null(model)) {
#       exp(confint(model)[, 2])  # Extract and exponentiate the upper bound of the CI
#     } else {
#       NA
#     }
#   })
  
#   # Extract the multivariable Cox regression results for this iteration
#   multivariable_model_male <- all_cox_results_male[[i]]$multivariable_male
  
#   # Extract the hazard ratios (HR) and confidence intervals for the multivariable model
#   multivariable_hr_male <- if (!is.null(multivariable_model_male)) {
#     exp(coef(multivariable_model_male))  # Extract and exponentiate the coefficients
#   } else {
#     rep(NA, length(hr_values_male))  # If the model is NULL, return NA for all exposures
#   }
  
#   multivariable_lower_95CI_male <- if (!is.null(multivariable_model_male)) {
#     exp(confint(multivariable_model_male)[, 1])  # Extract and exponentiate the lower bound of the CI
#   } else {
#     rep(NA, length(hr_values_male))
#   }
  
#   multivariable_upper_95CI_male <- if (!is.null(multivariable_model_male)) {
#     exp(confint(multivariable_model_male)[, 2])  # Extract and exponentiate the upper bound of the CI
#   } else {
#     rep(NA, length(hr_values_male))
#   }
  
#   # Combine the univariate and multivariable HRs and CIs into a single row
#   combined_row_male <- c(univariate_hr_male = hr_values_male, multivariable_hr_male = multivariable_hr_male)
#   lower_95CI_row_male <- c(univariate_lower_95CI_male = lower_95CI_male, multivariable_lower_95CI_male = multivariable_lower_95CI_male)
#   upper_95CI_row_male <- c(univariate_upper_95CI_male = upper_95CI_male, multivariable_upper_95CI_male = multivariable_upper_95CI_male)
  
#   # Store the rows in the respective lists
#   cox_point_estimates_male[[i]] <- combined_row_male
#   lower_95CI_list_male[[i]] <- lower_95CI_row_male
#   upper_95CI_list_male[[i]] <- upper_95CI_row_male
# }

# # Combine all rows into dataframes
# cox_point_estimates_df_male <- do.call(rbind, cox_point_estimates_male)
# lower_95CI_df_male <- do.call(rbind, lower_95CI_list_male)
# upper_95CI_df_male <- do.call(rbind, upper_95CI_list_male)

# # Assign column names to the dataframes
# first_non_null_male <- all_cox_results_male[[which(!sapply(all_cox_results_male, is.null))[1]]]
# colnames(cox_point_estimates_df_male) <- c(
#   paste0("univariate_", names(first_non_null_male$univariate_male)),
#   paste0("multivariable_", names(first_non_null_male$multivariable_male$coefficients))
# )
# colnames(lower_95CI_df_male) <- colnames(cox_point_estimates_df_male)
# colnames(upper_95CI_df_male) <- colnames(cox_point_estimates_df_male)

# # Calculate the median, lower, and upper bounds for the hazard ratios and confidence intervals
# summary_table_male <- data.frame(
#   Variable = colnames(cox_point_estimates_df_male),
#   Median_HR = apply(cox_point_estimates_df_male, 2, median, na.rm = TRUE),
#   Lower_95CI = apply(lower_95CI_df_male, 2, median, na.rm = TRUE),
#   Upper_95CI = apply(upper_95CI_df_male, 2, median, na.rm = TRUE)
# )

# # View the resulting summary table
# View(summary_table_male)

# # Save the summary table to a CSV file
# write.csv(summary_table_male, "cox_regression_summary_table_male.csv", row.names = FALSE)

# ## Analysis among females

# # Initialize a list to store the results for all 1000 dataframes
# all_cox_results_female <- list()

# # Loop over all 1000 dataframes in processed_dataframes
# for (i in 1:length(processed_dataframes)) {
#   cat("Processing Cox regression for dataframe", i, "of", length(processed_dataframes), "\n")
  
#   # Load the current dataframe
#   df <- processed_dataframes[[i]]
  
#   # Filter the dataframe to include only rows where gender == 1 (female)
#   df <- df %>%
#     filter(gender == 1 & 
#            !is.na(hcv_test_rslt) & !is.na(dob) & 
#            !is.na(sex_work_current) & 
#            !is.na(homeless_current) & !is.na(ethnic_roma))
  
#   # Check if the filtered dataframe is empty
#   if (nrow(df) == 0) {
#     cat("Filtered dataframe is empty for iteration", i, ". Skipping.\n")
#     all_cox_results_female[[i]] <- NULL
#     next
#   }
  
#   # Create a survival object
#   surv_obj_female <- Surv(time = df$days_risk, event = df$hcv_test_rslt)
  
#   # List of exposures (excluding msm_current)
#   exposures_female <- c("age_years", "sex_work_current", "homeless_current", "ethnic_roma")
  
#   # Run univariate Cox regressions
#   univariate_results_female <- lapply(exposures_female, function(var) {
#     formula <- as.formula(paste("surv_obj_female ~", var))
#     tryCatch(
#       coxph(formula, data = df),
#       error = function(e) {
#         cat("Error in Cox regression for", var, ":", e$message, "\n")
#         NULL
#       }
#     )
#   })
  
#   # Run a multivariable Cox regression adjusting for all exposures
#   multivariable_formula_female <- as.formula(paste("surv_obj_female ~", paste(exposures_female, collapse = " + ")))
#   multivariable_cox_female <- tryCatch(
#     coxph(multivariable_formula_female, data = df),
#     error = function(e) {
#       cat("Error in multivariable Cox regression:", e$message, "\n")
#       NULL
#     }
#   )
  
#   # Store the results for this dataframe
#   all_cox_results_female[[i]] <- list(
#     univariate_female = univariate_results_female,
#     multivariable_female = multivariable_cox_female
#   )
# }

# # Save the results to a file
# saveRDS(all_cox_results_female, file = "all_cox_results_female.rds")

# # Initialize an empty list to store the results for each iteration
# cox_point_estimates_female <- list()
# lower_95CI_list_female <- list()
# upper_95CI_list_female <- list()

# # Loop through all 1000 iterations in all_cox_results_female
# for (i in 1:length(all_cox_results_female)) {
#   # Extract the univariate Cox regression results for this iteration
#   univariate_results_female <- all_cox_results_female[[i]]$univariate_female
  
#   # Extract the hazard ratios (HR) and confidence intervals for each exposure
#   hr_values_female <- sapply(univariate_results_female, function(model) {
#     if (!is.null(model)) {
#       exp(coef(model))  # Extract and exponentiate the coefficient to get the HR
#     } else {
#       NA  # If the model is NULL, return NA
#     }
#   })
  
#   lower_95CI_female <- sapply(univariate_results_female, function(model) {
#     if (!is.null(model)) {
#       exp(confint(model)[, 1])  # Extract and exponentiate the lower bound of the CI
#     } else {
#       NA
#     }
#   })
  
#   upper_95CI_female <- sapply(univariate_results_female, function(model) {
#     if (!is.null(model)) {
#       exp(confint(model)[, 2])  # Extract and exponentiate the upper bound of the CI
#     } else {
#       NA
#     }
#   })
  
#   # Extract the multivariable Cox regression results for this iteration
#   multivariable_model_female <- all_cox_results_female[[i]]$multivariable_female
  
#   # Extract the hazard ratios (HR) and confidence intervals for the multivariable model
#   multivariable_hr_female <- if (!is.null(multivariable_model_female)) {
#     exp(coef(multivariable_model_female))  # Extract and exponentiate the coefficients
#   } else {
#     rep(NA, length(hr_values_female))  # If the model is NULL, return NA for all exposures
#   }
  
#   multivariable_lower_95CI_female <- if (!is.null(multivariable_model_female)) {
#     exp(confint(multivariable_model_female)[, 1])  # Extract and exponentiate the lower bound of the CI
#   } else {
#     rep(NA, length(hr_values_female))
#   }
  
#   multivariable_upper_95CI_female <- if (!is.null(multivariable_model_female)) {
#     exp(confint(multivariable_model_female)[, 2])  # Extract and exponentiate the upper bound of the CI
#   } else {
#     rep(NA, length(hr_values_female))
#   }
  
#   # Combine the univariate and multivariable HRs and CIs into a single row
#   combined_row_female <- c(univariate_hr_female = hr_values_female, multivariable_hr_female = multivariable_hr_female)
#   lower_95CI_row_female <- c(univariate_lower_95CI_female = lower_95CI_female, multivariable_lower_95CI_female = multivariable_lower_95CI_female)
#   upper_95CI_row_female <- c(univariate_upper_95CI_female = upper_95CI_female, multivariable_upper_95CI_female = multivariable_upper_95CI_female)
  
#   # Store the rows in the respective lists
#   cox_point_estimates_female[[i]] <- combined_row_female
#   lower_95CI_list_female[[i]] <- lower_95CI_row_female
#   upper_95CI_list_female[[i]] <- upper_95CI_row_female
# }

# # Combine all rows into dataframes
# cox_point_estimates_df_female <- do.call(rbind, cox_point_estimates_female)
# lower_95CI_df_female <- do.call(rbind, lower_95CI_list_female)
# upper_95CI_df_female <- do.call(rbind, upper_95CI_list_female)

# # Assign column names to the dataframes
# first_non_null_female <- all_cox_results_female[[which(!sapply(all_cox_results_female, is.null))[1]]]
# colnames(cox_point_estimates_df_female) <- c(
#   paste0("univariate_", names(first_non_null_female$univariate_female)),
#   paste0("multivariable_", names(first_non_null_female$multivariable_female$coefficients))
# )
# colnames(lower_95CI_df_female) <- colnames(cox_point_estimates_df_female)
# colnames(upper_95CI_df_female) <- colnames(cox_point_estimates_df_female)

# # Calculate the median, lower, and upper bounds for the hazard ratios and confidence intervals
# summary_table_female <- data.frame(
#   Variable = colnames(cox_point_estimates_df_female),
#   Median_HR = apply(cox_point_estimates_df_female, 2, median, na.rm = TRUE),
#   Lower_95CI = apply(lower_95CI_df_female, 2, median, na.rm = TRUE),
#   Upper_95CI = apply(upper_95CI_df_female, 2, median, na.rm = TRUE)
# )

# # View the resulting summary table
# View(summary_table_female)

# # Save the summary table to a CSV file
# write.csv(summary_table_female, "cox_regression_summary_table_female.csv", row.names = FALSE)


# calculate the mean incidence rates for each two-year interval
two_yearly_means_hcv <- sapply(c("2013_2014", "2015_2016", "2017_2018", "2019_2020", "2021_2022"), function(interval) {
  if (paste0("incidence_rate_", interval) %in% colnames(final_summed_df_two_yearly_hcv)) {
    mean(final_summed_df_two_yearly_hcv[[paste0("incidence_rate_", interval)]], na.rm = TRUE)
  } else {
    NA
  }
})
print(two_yearly_means_hcv)

# calculate means for HCV infections and person-years for each two-year interval
mean_hcv_infections_two_yearly_hcv <- sapply(
  c("2013_2014", "2015_2016", "2017_2018", "2019_2020", "2021_2022"),
  function(interval) {
    colname <- paste0("hcv_test_", interval)
    if (colname %in% colnames(final_summed_df_two_yearly_hcv)) {
      mean(final_summed_df_two_yearly_hcv[[colname]], na.rm = TRUE)
    } else {
      NA
    }
  }
)

mean_person_years_two_yearly_hcv <- sapply(
  c("2013_2014", "2015_2016", "2017_2018", "2019_2020", "2021_2022"),
  function(interval) {
    colname <- paste0("person_years_", interval)
    if (colname %in% colnames(final_summed_df_two_yearly_hcv)) {
      mean(final_summed_df_two_yearly_hcv[[colname]], na.rm = TRUE)
    } else {
      NA
    }
  }
)

# calculate lower and upper bounds for each two-year interval
two_yearly_lower_bounds_hcv <- sapply(
  c("2013_2014", "2015_2016", "2017_2018", "2019_2020", "2021_2022"),
  function(interval) {
    colname <- paste0("incidence_rate_", interval)
    if (colname %in% colnames(final_summed_df_two_yearly_hcv)) {
      quantile(final_summed_df_two_yearly_hcv[[colname]], 0.025, na.rm = TRUE)
    } else {
      NA
    }
  }
)

two_yearly_upper_bounds_hcv <- sapply(
  c("2013_2014", "2015_2016", "2017_2018", "2019_2020", "2021_2022"),
  function(interval) {
    colname <- paste0("incidence_rate_", interval)
    if (colname %in% colnames(final_summed_df_two_yearly_hcv)) {
      quantile(final_summed_df_two_yearly_hcv[[colname]], 0.975, na.rm = TRUE)
    } else {
      NA
    }
  }
)




















