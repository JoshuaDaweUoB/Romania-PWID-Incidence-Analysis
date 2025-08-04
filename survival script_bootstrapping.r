## load packages
pacman::p_load(tidyr, withr, lubridate, MASS, writexl, readxl, arsenal, survival, broom, ggplot2, dplyr)

## set wd
setwd("C:/Users/vl22683/OneDrive - University of Bristol/Documents/Publications/Romania PWID/data")


# # ### bootstrapping

# Modified bootstrap function to calculate incidence by interval (no n_infection_samples argument)
bootstrap_incidence_by_interval <- function(data, intervals) {
  resample <- data %>% sample_n(size = nrow(data), replace = TRUE)
  resample <- resample %>%
    rowwise() %>%
    mutate(
      pt = if (hcv_test_rslt == 1) {
        # Single random infection date per seroconverter
        infection_date <- as.Date(runif(1, min = as.numeric(appointment_dte), max = as.numeric(appointment_dte_lag)), origin = "1970-01-01")
        max(as.numeric(infection_date - appointment_dte), 1)
      } else {
        max(as.numeric(appointment_dte_lag - appointment_dte), 1)
      }
    ) %>%
    ungroup()
  
  # For each interval, calculate incidence rate
  interval_results <- lapply(names(intervals), function(interval_name) {
    years <- intervals[[interval_name]]
    interval_data <- resample %>% filter(year(appointment_dte) %in% years)
    total_pt <- sum(interval_data$pt, na.rm = TRUE)
    cases <- sum(interval_data$hcv_test_rslt == 1, na.rm = TRUE)
    incidence_rate <- if (total_pt > 0) (cases / total_pt) * 365.25 * 100 else NA
    data.frame(
      interval = interval_name,
      incidence_rate = incidence_rate,
      total_pt = total_pt,
      cases = cases
    )
  })
  do.call(rbind, interval_results)
}

# Run bootstrap
set.seed(42)
n_boot <- 1000
bootstrap_interval_results <- vector("list", n_boot)
for (i in 1:n_boot) {
  bootstrap_interval_results[[i]] <- bootstrap_incidence_by_interval(romania_pwid_hcv_test, intervals)
  cat("Completed bootstrap:", i, "of", n_boot, "\n")
}

# Combine all bootstrap results into one dataframe
all_bootstrap_df <- bind_rows(bootstrap_interval_results, .id = "bootstrap")

# Summarise by interval: median and percentiles
incidence_trends_boot <- all_bootstrap_df %>%
  group_by(interval) %>%
  summarise(
    median_incidence_rate = median(incidence_rate, na.rm = TRUE),
    lower_bound = quantile(incidence_rate, 0.025, na.rm = TRUE),
    upper_bound = quantile(incidence_rate, 0.975, na.rm = TRUE),
    median_total_person_years = median(total_pt, na.rm = TRUE),
    median_total_hcv_infections = median(cases, na.rm = TRUE)
  )

# Save to CSV
write.csv(incidence_trends_boot, "incidence_trends_bootstrap.csv", row.names = FALSE)

# Plot
HCV_incidence_trends_plot_boot <- ggplot(incidence_trends_boot, aes(x = interval, y = median_incidence_rate)) +
  geom_line(group = 1, color = "gray", linewidth = 0.8, linetype = "solid") +
  geom_point(shape = 18, size = 4, color = "gray") +
  geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound), width = 0.1, color = "black", size = 0.8) +
  theme_minimal(base_size = 14) +
  labs(
    x = "Two-Yearly Interval",
    y = "Median Incidence Rate (per 100 Person-Years)"
  ) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, max(incidence_trends_boot$upper_bound, na.rm = TRUE) * 1.1)) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

ggsave("plots/HCV_incidence_trends_plot_bootstrap.png", plot = HCV_incidence_trends_plot_boot, width = 10, height = 6, dpi = 300)




# Define two-year intervals
intervals <- list(
  "2013-2014" = 2013:2014,
  "2015-2016" = 2015:2016,
  "2017-2018" = 2017:2018,
  "2019-2020" = 2019:2020,
  "2021-2022" = 2021:2022
)

# Modified bootstrap function to calculate incidence by interval
bootstrap_incidence_by_interval <- function(data, n_infection_samples, intervals) {
  resample <- data %>% sample_n(size = nrow(data), replace = TRUE)
  resample <- resample %>%
    rowwise() %>%
    mutate(
      pt = if (hcv_test_rslt == 1) {
        mean(
          sapply(1:n_infection_samples, function(x) {
            infection_date <- as.Date(runif(1, min = as.numeric(appointment_dte), max = as.numeric(appointment_dte_lag)), origin = "1970-01-01")
            max(as.numeric(infection_date - appointment_dte), 1)
          })
        )
      } else {
        max(as.numeric(appointment_dte_lag - appointment_dte), 1)
      }
    ) %>%
    ungroup()
  
  # For each interval, calculate incidence rate
  interval_results <- lapply(names(intervals), function(interval_name) {
    years <- intervals[[interval_name]]
    interval_data <- resample %>% filter(year(appointment_dte) %in% years)
    total_pt <- sum(interval_data$pt, na.rm = TRUE)
    cases <- sum(interval_data$hcv_test_rslt == 1, na.rm = TRUE)
    incidence_rate <- if (total_pt > 0) (cases / total_pt) * 365.25 * 100 else NA
    data.frame(
      interval = interval_name,
      incidence_rate = incidence_rate,
      total_pt = total_pt,
      cases = cases
    )
  })
  do.call(rbind, interval_results)
}

# Run bootstrap
set.seed(69)
n_boot <- 1000
n_infection_samples <- 100
bootstrap_interval_results <- vector("list", n_boot)
for (i in 1:n_boot) {
  bootstrap_interval_results[[i]] <- bootstrap_incidence_by_interval(romania_pwid_hcv_test, n_infection_samples, intervals)
  cat("Completed bootstrap:", i, "of", n_boot, "\n")
}

# Combine all bootstrap results into one dataframe
all_bootstrap_df <- bind_rows(bootstrap_interval_results, .id = "bootstrap")

# Summarize by interval: median and percentiles
incidence_trends_boot <- all_bootstrap_df %>%
  group_by(interval) %>%
  summarise(
    median_incidence_rate = median(incidence_rate, na.rm = TRUE),
    lower_bound = quantile(incidence_rate, 0.025, na.rm = TRUE),
    upper_bound = quantile(incidence_rate, 0.975, na.rm = TRUE),
    median_total_person_years = median(total_pt, na.rm = TRUE),
    median_total_hcv_infections = median(cases, na.rm = TRUE)
  )

# Save to CSV
write.csv(incidence_trends_boot, "incidence_trends_bootstrap.csv", row.names = FALSE)

# Plot
HCV_incidence_trends_plot_boot <- ggplot(incidence_trends_boot, aes(x = interval, y = median_incidence_rate)) +
  geom_line(group = 1, color = "gray", linewidth = 0.8, linetype = "solid") +
  geom_point(shape = 18, size = 4, color = "gray") +
  geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound), width = 0.1, color = "black", size = 0.8) +
  theme_minimal(base_size = 14) +
  labs(
    x = "Two-Yearly Interval",
    y = "Median Incidence Rate (per 100 Person-Years)"
  ) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, max(incidence_trends_boot$upper_bound, na.rm = TRUE) * 1.1)) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

ggsave("plots/HCV_incidence_trends_plot_bootstrap.png", plot = HCV_incidence_trends_plot_boot, width = 10, height = 6, dpi = 300)

# # Bootstrap function with random infection dates
# bootstrap_incidence_random <- function(data, n_infection_samples) {
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

# # Run bootstrap with progress messages
# set.seed(42)
# n_boot <- 1000
# n_infection_samples <- 1000
# bootstrap_results <- numeric(n_boot)
# for (i in 1:n_boot) {
#   bootstrap_results[i] <- bootstrap_incidence_random(romania_pwid_hcv_test, n_infection_samples)
#   cat("Completed bootstrap:", i, "of", n_boot, "\n")
# }

# # Summarize results
# ir_mean <- mean(bootstrap_results, na.rm = TRUE)
# ir_lower <- quantile(bootstrap_results, 0.025, na.rm = TRUE)
# ir_upper <- quantile(bootstrap_results, 0.975, na.rm = TRUE)

# cat(sprintf("Incidence rate: %.2f per 100 person-years (95%% CI: %.2f – %.2f)\n",
#             ir_mean, ir_lower, ir_upper))

# # Save as CSV
# write.csv(bootstrap_results, "bootstrap_results.csv", row.names = FALSE)

# # Save as RDS (recommended for R objects)
# saveRDS(bootstrap_results, "bootstrap_results.rds")
