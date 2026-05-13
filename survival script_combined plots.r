
## load packages
pacman::p_load(tidyr, writexl, readxl, gplot2, dplyr)

## set wd
setwd("C:/Users/vl22683/OneDrive - University of Bristol/Documents/Publications/Romania PWID/data")

## hcv and hiv combined incidence figure

# Load both datasets
results_hiv <- read.csv("results_df_two_yearly_rubin_hiv.csv", stringsAsFactors = FALSE)
results_hcv <- read.csv("results_df_two_yearly_rubin_hcv.csv", stringsAsFactors = FALSE)

# Add disease identifier and combine
results_hiv$Disease <- "HIV"
results_hcv$Disease <- "HCV"

# Rename infection columns to same name
names(results_hiv)[names(results_hiv) == "Mean_hiv_infections"] <- "Mean_infections"
names(results_hcv)[names(results_hcv) == "Mean_HCV_infections"] <- "Mean_infections"

# Now combine
combined_df <- rbind(results_hiv, results_hcv)

# Remove the Overall row
combined_df <- combined_df %>% filter(Interval != "Overall")

# remove previous plot from environment
rm(combined_incidence_plot_rubin)

# Combined plot
combined_incidence_plot_rubin <- ggplot(
  combined_df,
  aes(x = Interval,
      y = Incidence_rate,
      group = Disease)
) +
  geom_line(aes(linetype = Disease),
            color = "black",
            linewidth = 0.8) +
  geom_point(shape = 18,
             size = 3,
             color = "black") +
  geom_ribbon(aes(ymin = Lower_bound,
                  ymax = Upper_bound,
                  fill = Disease),
                  alpha = .2, color = NA) +
  scale_x_discrete(expand = expansion(mult = c(0, 0))) +                  
  scale_linetype_manual(values = c("HIV" = "solid",
                                   "HCV" = "dashed")) +
  theme_minimal(base_size = 14) +
  labs(
    x = "Two-yearly Interval",
    y = "Mean Incidence Rate per 100 Person-Years",
    linetype = "Disease"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_line(color = "black"),    
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "bottom"
  )

ggsave("plots/combined_hcv_hiv_incidence_plot_rubin.png", plot = combined_incidence_plot_rubin, width = 10, height = 6, dpi = 300)

## hiv and hcv combined prevalence figure

# hiv data 
hiv_data <- read.csv("hiv_summary_table.csv", stringsAsFactors = FALSE)

# keep hiv data only
hiv_data <- hiv_data %>%
    filter(Variable == "test_year") %>%
    select(Level, hiv_Negative, hiv_Positive, Total, Proportion_Positive)

# lower and upper bounds of prevalence
hiv_data <- hiv_data %>%
  rowwise() %>%
  mutate(
    ci = list(prop.test(hiv_Positive, Total)$conf.int),
    Lower_bound = ci[1]*100,
    Upper_bound = ci[2]*100
  ) %>%
  ungroup()

# hcv data
hcv_data <- read.csv("hcv_summary_table.csv", stringsAsFactors = FALSE)

# keep hcv data only
hcv_data <- hcv_data %>%
    filter(Variable == "test_year") %>%
    select(Level, hcv_Negative, hcv_Positive, Total, Proportion_Positive)

# lower and upper bounds of prevalence
hcv_data <- hcv_data %>%
  rowwise() %>%
  mutate(
    ci = list(prop.test(hcv_Positive, Total)$conf.int),
    Lower_bound = ci[1]*100,
    Upper_bound = ci[2]*100
  ) %>%
  ungroup()

# add disease identifier and combine
hiv_data$Disease <- "HIV"
hcv_data$Disease <- "HCV"

# Rename infection columns to same name
names(hiv_data)[names(hiv_data) == "hiv_Negative"] <- "neg_infections"
names(hiv_data)[names(hiv_data) == "hiv_Positive"] <- "pos_infections"

names(hcv_data)[names(hcv_data) == "hcv_Negative"] <- "neg_infections"
names(hcv_data)[names(hcv_data) == "hcv_Positive"] <- "pos_infections"

# Now combine
combined_df <- rbind(hiv_data, hcv_data)

# remove previous plot from environment
rm(combined_prevalence_plot)

# Combined plot
combined_prevalence_plot <- ggplot(
  combined_df,
  aes(x = Level,
      y = Proportion_Positive,
      group = Disease)
) +
  geom_line(aes(linetype = Disease),
            color = "black",
            linewidth = 0.8) +
  geom_point(shape = 18,
             size = 3,
             color = "black") +
  geom_ribbon(aes(ymin = Lower_bound,
                  ymax = Upper_bound,
                  fill = Disease),
                  alpha = .2, color = NA) +
  scale_linetype_manual(values = c("HIV" = "solid",
                                   "HCV" = "dashed")) +
  theme_minimal(base_size = 14) +
  labs(
    x = "Test year",
    y = "Prevalence",
    linetype = "Disease"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_line(color = "black"),    
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "bottom"
  )

ggsave("plots/combined_hcv_hiv_prevalence_plot.png", plot = combined_prevalence_plot, width = 10, height = 6, dpi = 300)

View(combined_df)

## hcv and hiv combined incidence figure - sensitivity analysis 1

# load both datasets
results_hiv_s1 <- read.csv("two_yearly_results_long_midpoint_hiv.csv", stringsAsFactors = FALSE)
results_hcv_s1 <- read.csv("two_yearly_results_long_midpoint_hcv.csv", stringsAsFactors = FALSE)

# rename columns
names(results_hiv_s1)[names(results_hiv_s1) == "total_hiv_infections"] <- "infections"
names(results_hcv_s1)[names(results_hcv_s1) == "total_hiv_infections"] <- "infections"
names(results_hcv_s1)[names(results_hcv_s1) == "total_hcv_infections"] <- "infections"

# add disease identifier and combine
results_hiv_s1$Disease <- "HIV"
results_hcv_s1$Disease <- "HCV"

# combine
combined_df_s1 <- rbind(results_hiv_s1, results_hcv_s1)

# Combined plot
combined_incidence_plot_s1 <- ggplot(
  combined_df_s1,
  aes(x = two_year_interval,
      y = incidence_rate,
      group = Disease)
) +
  geom_line(aes(linetype = Disease),
            color = "black",
            linewidth = 0.8) +
  geom_point(shape = 18,
             size = 3,
             color = "black") +
  geom_ribbon(aes(ymin = lower_bound,
                  ymax = upper_bound,
                  fill = Disease),
                  alpha = .2, color = NA) +
  scale_x_discrete(expand = expansion(mult = c(0, 0))) +                  
  scale_linetype_manual(values = c("HIV" = "solid",
                                   "HCV" = "dashed")) +
  theme_minimal(base_size = 14) +
  labs(
    x = "Two-yearly Interval",
    y = "Incidence Rate per 100 Person-Years",
    linetype = "Disease"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_line(color = "black"),    
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "bottom"
  )

ggsave("plots/combined_hcv_hiv_incidence_plot_s1.png", plot = combined_incidence_plot_s1, width = 10, height = 6, dpi = 300)

# sensitivity analysis two

results_hcv_s2 <- read.csv("two_yearly_results_first_dataframe_hcv.csv", stringsAsFactors = FALSE)
results_hiv_s2 <- read.csv("two_yearly_results_first_dataframe_hiv.csv", stringsAsFactors = FALSE)

# rename columns
names(results_hiv_s2)[names(results_hiv_s2) == "total_hiv_infections"] <- "infections"
names(results_hcv_s2)[names(results_hcv_s2) == "total_hcv_infections"] <- "infections"

# add disease identifier and combine
results_hiv_s2$Disease <- "HIV"
results_hcv_s2$Disease <- "HCV"

# combine
combined_df_s2 <- rbind(results_hiv_s2, results_hcv_s2)

# Combined plot
combined_incidence_plot_s2 <- ggplot(
  combined_df_s2,
  aes(x = two_year_interval,
      y = incidence_rate,
      group = Disease)
) +
  geom_line(aes(linetype = Disease),
            color = "black",
            linewidth = 0.8) +
  geom_point(shape = 18,
             size = 3,
             color = "black") +
  geom_ribbon(aes(ymin = lower_bound,
                  ymax = upper_bound,
                  fill = Disease),
                  alpha = .2, color = NA) +
  scale_x_discrete(expand = expansion(mult = c(0, 0))) +                  
  scale_linetype_manual(values = c("HIV" = "solid",
                                   "HCV" = "dashed")) +
  theme_minimal(base_size = 14) +
  labs(
    x = "Two-yearly Interval",
    y = "Incidence Rate per 100 Person-Years",
    linetype = "Disease"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_line(color = "black"),    
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "bottom"
  )

ggsave("plots/combined_hcv_hiv_incidence_plot_s2.png", plot = combined_incidence_plot_s2, width = 10, height = 6, dpi = 300)
