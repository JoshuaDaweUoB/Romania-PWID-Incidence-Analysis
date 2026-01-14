## load packages
pacman::p_load(dplyr, tidyr, writexl, readxl, car, ggplot2, sandwich, lmtest, scales)

## set wd
setwd("C:/Users/vl22683/OneDrive - University of Bristol/Documents/Publications/Romania PWID/data")

## load quarterly data
quarter_df <- read_excel("quarterly_data cleaned.xlsx")

# Convert Quarter_start to Date if not already
quarter_df$Quarter_start <- as.Date(quarter_df$Quarter_start)

# Filter data to start from 2016 Q1
quarter_df <- quarter_df %>% filter(Quarter_start >= as.Date("2016-01-01"))

# Create sequential quarter number
quarter_df <- quarter_df %>% mutate(Quarter_seq = row_number())

# Create interruption indicator (1 = during 2020 Q2-Q4, 0 otherwise)
quarter_df <- quarter_df %>%
  mutate(interruption = ifelse(Quarter %in% c("2020 Q2", "2020 Q3", "2020 Q4"), 1, 0))

# Create time since start of interruption (0 before, 1,2,3 during interruption, continues counting after)
quarter_df <- quarter_df %>%
  mutate(Time_since_interrupt = case_when(
    interruption == 1 ~ Quarter_seq - min(Quarter_seq[interruption == 1]) + 1,
    Quarter_seq > max(Quarter_seq[interruption == 1]) ~ Quarter_seq - min(Quarter_seq[interruption == 1]) + 1,
    TRUE ~ 0
  ))

# Create post-interruption indicator (1 = after 2020 Q4, 0 otherwise)
quarter_df <- quarter_df %>%
  mutate(post = ifelse(Quarter_seq > max(Quarter_seq[interruption == 1]), 1, 0),
         Quarter_after = ifelse(post == 1, Quarter_seq - max(Quarter_seq[interruption == 1]), 0))

# ==============================
# MODEL AND PLOT FOR total_syringes
# ==============================
model_syringes <- glm(total_syringes ~ Quarter_seq + interruption + Time_since_interrupt + post:Quarter_after,
                      family = poisson(link = "log"), data = quarter_df)

# Robust standard errors
model_syringes_robust <- coeftest(model_syringes, vcov = vcovHC(model_syringes, type = "HC1"))
print(model_syringes_robust)

# Predicted values
quarter_df$Total_fit_syringes <- predict(model_syringes, type = "response")

# Plot
plot_syringes <- ggplot(quarter_df, aes(x = Quarter_seq)) +
  geom_point(aes(y = total_syringes), color = "black") +
  geom_line(aes(y = Total_fit_syringes), color = "black", linewidth = 1) +
  geom_vline(xintercept = min(quarter_df$Quarter_seq[quarter_df$interruption == 1]), 
             color = "black", linetype = "dotdash", linewidth = 1) +
  geom_vline(xintercept = max(quarter_df$Quarter_seq[quarter_df$interruption == 1]), 
             color = "black", linetype = "dotdash", linewidth = 1) +
  scale_x_continuous(breaks = seq(1, nrow(quarter_df), by = 4), 
                     labels = quarter_df$Quarter[seq(1, nrow(quarter_df), by = 4)]) +
  scale_y_continuous(labels = scales::comma) +  # formats numbers with commas
  labs(title = "Overall prescribing (Total Syringes)", 
       x = "Quarter", 
       y = "Number of syringes per quarter",
       caption = "Observed: points; Predicted: solid line") +
  theme_bw() +
  theme(panel.grid = element_blank())

ggsave("interrupted_time_series_total_syringes.png", plot_syringes)

# ==============================
# MODEL AND PLOT FOR total_recovered
# ==============================
model_recovered <- glm(total_recovered ~ Quarter_seq + interruption + Time_since_interrupt + post:Quarter_after,
                       family = poisson(link = "log"), data = quarter_df)

# Robust standard errors
model_recovered_robust <- coeftest(model_recovered, vcov = vcovHC(model_recovered, type = "HC1"))
print(model_recovered_robust)

# Predicted values
quarter_df$Total_fit_recovered <- predict(model_recovered, type = "response")

# Plot
plot_recovered <- ggplot(quarter_df, aes(x = Quarter_seq)) +
  geom_point(aes(y = total_recovered), color = "blue") +
  geom_line(aes(y = Total_fit_recovered), color = "blue", linewidth = 1) +
  geom_vline(xintercept = min(quarter_df$Quarter_seq[quarter_df$interruption == 1]), 
             color = "black", linetype = "dotdash", linewidth = 1) +
  geom_vline(xintercept = max(quarter_df$Quarter_seq[quarter_df$interruption == 1]), 
             color = "black", linetype = "dotdash", linewidth = 1) +
  scale_x_continuous(breaks = seq(1, nrow(quarter_df), by = 4), 
                     labels = quarter_df$Quarter[seq(1, nrow(quarter_df), by = 4)]) +
  scale_y_continuous(labels = scales::comma) +  # formats numbers with commas
  labs(title = "Overall prescribing (Total Recovered)", 
       x = "Quarter", 
       y = "Number of syringes recovered per quarter",
       caption = "Observed: points; Predicted: solid line") +
  theme_bw() +
  theme(panel.grid = element_blank())

ggsave("interrupted_time_series_total_recovered.png", plot_recovered)

# Combined plot for syringes and recovered
plot_combined <- ggplot(quarter_df, aes(x = Quarter_seq)) +
  # Syringes
  geom_point(aes(y = total_syringes, color = "Syringes distributed")) +
  geom_line(aes(y = Total_fit_syringes, color = "Syringes distributed"), linewidth = 1) +
  # Recovered
  geom_point(aes(y = total_recovered, color = "Syringes recovered")) +
  geom_line(aes(y = Total_fit_recovered, color = "Syringes recovered"), linewidth = 1) +
  # Intervention lines
  geom_vline(xintercept = min(quarter_df$Quarter_seq[quarter_df$interruption == 1]), 
             color = "black", linetype = "dotdash", linewidth = 1) +
  geom_vline(xintercept = max(quarter_df$Quarter_seq[quarter_df$interruption == 1]), 
             color = "black", linetype = "dotdash", linewidth = 1) +
  # Scales
  scale_x_continuous(breaks = seq(1, nrow(quarter_df), by = 4), 
                     labels = quarter_df$Quarter[seq(1, nrow(quarter_df), by = 4)]) +
  scale_y_continuous(labels = scales::comma, limits = c(0, 600000), breaks = seq(0, 600000, by = 50000)) +
  scale_color_manual(values = c("Syringes distributed" = "#F8766D", "Syringes recovered" = "#00BFC4")) +
  labs(x = "Quarter", 
       y = "Quarterly count",
       color = "Metric") +
  theme_bw() +
  theme(panel.grid = element_blank(),
      legend.position = "bottom",
      axis.text.x = element_text(color = "black"),
      axis.text.y = element_text(color = "black"))

ggsave("interrupted_time_series_combined.png", plot_combined, width = 12, height = 6)

# ==============================
# MODEL AND PLOT FOR unique_ids_syringes
# ==============================
model_unique_ids <- glm(unique_ids_syringes ~ Quarter_seq + interruption + Time_since_interrupt + post:Quarter_after,
                        family = poisson(link = "log"), data = quarter_df)

# Robust standard errors
model_unique_ids_robust <- coeftest(model_unique_ids, vcov = vcovHC(model_unique_ids, type = "HC1"))
print(model_unique_ids_robust)

# Predicted values
quarter_df$Total_fit_unique_ids <- predict(model_unique_ids, type = "response")

# Plot
plot_unique_ids <- ggplot(quarter_df, aes(x = Quarter_seq)) +
  geom_point(aes(y = unique_ids_syringes), color = "black") +
  geom_line(aes(y = Total_fit_unique_ids), color = "black", linewidth = 1) +
  geom_vline(xintercept = min(quarter_df$Quarter_seq[quarter_df$interruption == 1]), 
             color = "black", linetype = "dotdash", linewidth = 1) +
  geom_vline(xintercept = max(quarter_df$Quarter_seq[quarter_df$interruption == 1]), 
             color = "black", linetype = "dotdash", linewidth = 1) +
  scale_x_continuous(breaks = seq(1, nrow(quarter_df), by = 4), 
                     labels = quarter_df$Quarter[seq(1, nrow(quarter_df), by = 4)]) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Individuals accessing syringes", 
       x = "Quarter", 
       y = "Number of unique individuals per quarter") +
  theme_bw() +
  theme(panel.grid = element_blank())

ggsave("interrupted_time_series_unique_ids_syringes.png", plot_unique_ids)

# ==============================
# MODEL AND PLOT FOR total_visits_syringe
# ==============================
model_visits <- glm(total_visits_syringe ~ Quarter_seq + interruption + Time_since_interrupt + post:Quarter_after,
                    family = poisson(link = "log"), data = quarter_df)

# Robust standard errors
model_visits_robust <- coeftest(model_visits, vcov = vcovHC(model_visits, type = "HC1"))
print(model_visits_robust)

# Predicted values
quarter_df$Total_fit_visits <- predict(model_visits, type = "response")

# Plot
plot_visits <- ggplot(quarter_df, aes(x = Quarter_seq)) +
  geom_point(aes(y = total_visits_syringe), color = "black") +
  geom_line(aes(y = Total_fit_visits), color = "black", linewidth = 1) +
  geom_vline(xintercept = min(quarter_df$Quarter_seq[quarter_df$interruption == 1]), 
             color = "black", linetype = "dotdash", linewidth = 1) +
  geom_vline(xintercept = max(quarter_df$Quarter_seq[quarter_df$interruption == 1]), 
             color = "black", linetype = "dotdash", linewidth = 1) +
  scale_x_continuous(breaks = seq(1, nrow(quarter_df), by = 4), 
                     labels = quarter_df$Quarter[seq(1, nrow(quarter_df), by = 4)]) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Total visits to service for syringes", 
       x = "Quarter", 
       y = "Number of visits per quarter") +
  theme_bw() +
  theme(panel.grid = element_blank())

ggsave("interrupted_time_series_total_visits_syringe.png", plot_visits)

# ==============================
# COMBINED PLOT FOR unique_ids_syringes AND total_visits_syringe
# ==============================
plot_combined_visits <- ggplot(quarter_df, aes(x = Quarter_seq)) +
  # Unique IDs
  geom_point(aes(y = unique_ids_syringes, color = "Individuals accessing")) +
  geom_line(aes(y = Total_fit_unique_ids, color = "Individuals accessing"), linewidth = 1) +
  # Total visits
  geom_point(aes(y = total_visits_syringe, color = "Total visits")) +
  geom_line(aes(y = Total_fit_visits, color = "Total visits"), linewidth = 1) +
  # Intervention lines
  geom_vline(xintercept = min(quarter_df$Quarter_seq[quarter_df$interruption == 1]), 
             color = "black", linetype = "dotdash", linewidth = 1) +
  geom_vline(xintercept = max(quarter_df$Quarter_seq[quarter_df$interruption == 1]), 
             color = "black", linetype = "dotdash", linewidth = 1) +
  # Scales
  scale_x_continuous(breaks = seq(1, nrow(quarter_df), by = 4), 
                     labels = quarter_df$Quarter[seq(1, nrow(quarter_df), by = 4)]) +
  scale_y_continuous(labels = scales::comma) +
  scale_color_manual(values = c("Individuals accessing" = "#F8766D", "Total visits" = "#00BFC4")) +
  labs(x = "Quarter", 
       y = "Count per quarter") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.position = "bottom",
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"))

ggsave("interrupted_time_series_combined_visits.png", plot_combined_visits, width = 12, height = 6)

# ==============================
# MODEL AND PLOT FOR avg_syr_per_person
# ==============================
# Calculate average syringes per person
quarter_df <- quarter_df %>%
  mutate(avg_syr_per_person = ifelse(unique_ids_syringes > 0, total_syringes / unique_ids_syringes, NA_real_))

# Use the SAME model structure as other outcomes
model_avg <- lm(avg_syr_per_person ~ Quarter_seq + interruption + post + post:Quarter_after,
                data = quarter_df)

# Robust standard errors
model_avg_robust <- coeftest(model_avg, vcov = vcovHC(model_avg, type = "HC1"))
print(model_avg_robust)

# Predicted values for ALL rows - keep all values, don't set to NA
quarter_df$Avg_fit <- predict(model_avg, newdata = quarter_df)

# Plot - same style as other plots, line goes through interruption
plot_avg <- ggplot(quarter_df, aes(x = Quarter_seq)) +
  geom_point(aes(y = avg_syr_per_person), color = "darkgreen", na.rm = TRUE) +
  geom_line(aes(y = Avg_fit), color = "darkgreen", linewidth = 1) +
  geom_vline(xintercept = min(quarter_df$Quarter_seq[quarter_df$interruption == 1]), 
             color = "black", linetype = "dotdash", linewidth = 1) +
  geom_vline(xintercept = max(quarter_df$Quarter_seq[quarter_df$interruption == 1]), 
             color = "black", linetype = "dotdash", linewidth = 1) +
  scale_x_continuous(breaks = seq(1, nrow(quarter_df), by = 4), 
                     labels = quarter_df$Quarter[seq(1, nrow(quarter_df), by = 4)]) +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA)) +
  labs(title = "Average syringes per person per quarter", 
       x = "Quarter", 
       y = "Average syringes per person") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"))

# Save
ggsave("interrupted_time_series_avg_syr_per_person.png", plot_avg, width = 12, height = 6)

View(quarter_df)


# ==============================
# MODEL AND PLOT FOR avg_syr_per_person
# ==============================
# Calculate average syringes per person - set to 0 during lockdown
quarter_df <- quarter_df %>%
  mutate(avg_syr_per_person = case_when(
    unique_ids_syringes > 0 ~ total_syringes / unique_ids_syringes,
    Quarter %in% c("2020 Q2", "2020 Q3", "2020 Q4") ~ 0,
    TRUE ~ NA_real_
  ))

# Model with interruption, Time_since_interrupt, AND post for the recovery jump
model_avg <- lm(avg_syr_per_person ~ Quarter_seq + interruption + Time_since_interrupt + post + post:Quarter_after,
                data = quarter_df)

# Robust standard errors
model_avg_robust <- coeftest(model_avg, vcov = vcovHC(model_avg, type = "HC1"))
print(model_avg_robust)

# Predicted values
quarter_df$Avg_fit <- predict(model_avg, newdata = quarter_df)

# Plot
plot_avg <- ggplot(quarter_df, aes(x = Quarter_seq)) +
  geom_point(aes(y = avg_syr_per_person), color = "black", na.rm = TRUE) +
  geom_line(aes(y = Avg_fit), color = "black", linewidth = 1) +
  geom_vline(xintercept = min(quarter_df$Quarter_seq[quarter_df$interruption == 1]), 
             color = "black", linetype = "dotdash", linewidth = 1) +
  geom_vline(xintercept = max(quarter_df$Quarter_seq[quarter_df$interruption == 1]), 
             color = "black", linetype = "dotdash", linewidth = 1) +
  scale_x_continuous(breaks = seq(1, nrow(quarter_df), by = 4), 
                     labels = quarter_df$Quarter[seq(1, nrow(quarter_df), by = 4)]) +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA)) +
  labs(title = "Average syringes per person per quarter", 
       x = "Quarter", 
       y = "Average syringes per person") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"))

# Save
ggsave("interrupted_time_series_avg_syr_per_person.png", plot_avg, width = 12, height = 6)





# ==============================
# MODEL AND PLOT FOR avg_syr_per_person
# ==============================
quarter_df <- quarter_df %>%
  mutate(avg_syr_per_person = case_when(
    unique_ids_syringes > 0 ~ total_syringes / unique_ids_syringes,
    Quarter %in% c("2020 Q2", "2020 Q3", "2020 Q4") ~ 0,
    TRUE ~ NA_real_
  ))

model_visits <- glm(avg_syr_per_person ~ Quarter_seq + interruption + Time_since_interrupt + post:Quarter_after,
                    family = poisson(link = "log"), data = quarter_df)

# Robust standard errors
model_visits_robust <- coeftest(model_visits, vcov = vcovHC(model_visits, type = "HC1"))
print(model_visits_robust)

# Predicted values
quarter_df$Total_fit_visits <- predict(model_visits, type = "response")

# Plot
plot_visits <- ggplot(quarter_df, aes(x = Quarter_seq)) +
  geom_point(aes(y = avg_syr_per_person), color = "black") +
  geom_line(aes(y = Total_fit_visits), color = "black", linewidth = 1) +
  geom_vline(xintercept = min(quarter_df$Quarter_seq[quarter_df$interruption == 1]), 
             color = "black", linetype = "dotdash", linewidth = 1) +
  geom_vline(xintercept = max(quarter_df$Quarter_seq[quarter_df$interruption == 1]), 
             color = "black", linetype = "dotdash", linewidth = 1) +
  scale_x_continuous(breaks = seq(1, nrow(quarter_df), by = 4), 
                     labels = quarter_df$Quarter[seq(1, nrow(quarter_df), by = 4)]) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Total visits to service for syringes", 
       x = "Quarter", 
       y = "Average syringes per person") +
  theme_bw() +
  theme(panel.grid = element_blank())

ggsave("interrupted_time_series_avg_syr_per_person.png", plot_visits)