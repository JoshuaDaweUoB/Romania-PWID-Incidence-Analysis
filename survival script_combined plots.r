
## load packages
pacman::p_load(tidyr, writexl, readxl, ggplot2, dplyr, patchwork)

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
names(results_hcv)[names(results_hcv) == "Mean_hcv_infections"] <- "Mean_infections"

# Now combine
combined_df <- rbind(results_hiv, results_hcv)

# remove the overall row, relabel intervals as 2013–14 etc. and set legend labels
combined_df <- combined_df %>%
  filter(Interval != "Overall") %>%
  mutate(
    Interval = factor(sub("^(\\d{4})-\\d{2}(\\d{2})$", "\\1\u2013\\2", Interval)),
    Disease = factor(Disease, levels = c("HCV", "HIV")),
    observed = paste0(Disease, " (observed)"),
    interpolation = paste0(Disease, " (linear interpolation)")
  )

# colours, markers and line types
disease_cols <- c("HCV" = "#E8796E", "HIV" = "#24786B")
observed_cols <- setNames(disease_cols, paste0(names(disease_cols), " (observed)"))
interp_cols <- setNames(disease_cols, paste0(names(disease_cols), " (linear interpolation)"))
observed_shapes <- setNames(c(18, 16), names(observed_cols))
observed_sizes <- c(5, 4)
interp_lines <- setNames(c("dashed", "dotted"), names(interp_cols))

# shared x scale so the plot and table line up
x_scale <- scale_x_discrete(expand = expansion(add = 0.5))

# incidence plot
combined_incidence_plot_rubin <- ggplot(combined_df, aes(x = Interval, group = Disease)) +
  geom_ribbon(aes(ymin = Lower_bound, ymax = Upper_bound, fill = interpolation), key_glyph = "rect") +
  geom_line(data = filter(combined_df, Disease == "HCV"), aes(y = Incidence_rate, linetype = interpolation),
            colour = disease_cols[["HCV"]], linewidth = 0.8) +
  geom_line(data = filter(combined_df, Disease == "HIV"), aes(y = Incidence_rate, linetype = interpolation),
            colour = disease_cols[["HIV"]], linewidth = 0.8) +
  geom_errorbar(aes(ymin = Lower_bound, ymax = Upper_bound, colour = observed), width = 0.06, linewidth = 0.6) +
  geom_point(aes(y = Incidence_rate, colour = observed, shape = observed, size = observed)) +
  x_scale +
  scale_y_continuous(limits = c(0, 30), breaks = seq(0, 30, 5), expand = expansion(mult = c(0, 0.03))) +
  scale_colour_manual(values = observed_cols, name = NULL) +
  scale_shape_manual(values = observed_shapes, name = NULL) +
  scale_size_manual(values = setNames(observed_sizes, names(observed_cols)), name = NULL) +
  scale_fill_manual(values = setNames(scales::alpha(interp_cols, 0.18), names(interp_cols)), name = NULL) +
  scale_linetype_manual(values = interp_lines, name = NULL) +
  guides(
    colour = guide_legend(order = 1, ncol = 1),
    shape = guide_legend(order = 1, ncol = 1),
    size = guide_legend(order = 1, ncol = 1),
    fill = guide_legend(order = 2, ncol = 1),
    linetype = guide_legend(order = 2, ncol = 1, override.aes = list(colour = unname(disease_cols)))
  ) +
  labs(x = "Time period", y = "Incidence rate per 100 person-years") +
  theme_minimal(base_size = 14) +
  theme(
    axis.line = element_line(colour = "black"),
    axis.ticks = element_line(colour = "black"),
    axis.text = element_text(colour = "black"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(colour = "grey88"),
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background = element_rect(fill = "white", colour = NA),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.key.width = unit(1.5, "cm"),
    legend.text = element_text(size = 12)
  )

# cases/person-years table under the plot
table_df <- combined_df %>%
  mutate(
    row = factor(paste0(Disease, ", cases/PY"), levels = c("HCV, cases/PY", "HIV, cases/PY")),
    label = sprintf("%.1f/%.1f", Mean_infections, Mean_person_years)
  )

cases_py_table <- ggplot(table_df, aes(x = Interval, y = row)) +
  geom_tile(fill = NA, colour = "grey85", width = 1, height = 1) +
  geom_text(aes(label = label), size = 4) +
  x_scale +
  scale_y_discrete(expand = expansion(add = 0.5)) +
  theme_void(base_size = 14) +
  theme(
    axis.text.y = element_text(face = "bold", hjust = 1, size = 11, margin = margin(r = 8)),
    plot.background = element_rect(fill = "white", colour = NA)
  )

# combine plot and table
combined_incidence_figure_rubin <- combined_incidence_plot_rubin / cases_py_table +
  plot_layout(heights = c(5, 1))

ggsave("plots/combined_hcv_hiv_incidence_plot_rubin.png", plot = combined_incidence_figure_rubin, width = 10, height = 7.5, dpi = 300)

## hiv and hcv combined prevalence figure

# hiv data 
hiv_data <- read.csv("table1_hiv_summary_table.csv", stringsAsFactors = FALSE)

# keep hiv data only
hiv_data <- hiv_data %>%
    filter(Variable == "appointment_year") %>%
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
hcv_data <- read.csv("table2_hcv_summary_table.csv", stringsAsFactors = FALSE)

# keep hcv data only
hcv_data <- hcv_data %>%
    filter(Variable == "appointment_year") %>%
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

# combined plot
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

# sensitivity analysis three

results_hcv_s3 <- read.csv("incidence_trends_hcv_s3.csv", stringsAsFactors = FALSE)
results_hiv_s3 <- read.csv("incidence_trends_hiv_s3.csv", stringsAsFactors = FALSE)

# rename columns
names(results_hiv_s3)[names(results_hiv_s3) == "median_total_hiv_infections"] <- "infections"
names(results_hcv_s3)[names(results_hcv_s3) == "median_total_hcv_infections"] <- "infections"

# add disease identifier and combine
results_hiv_s3$Disease <- "HIV"
results_hcv_s3$Disease <- "HCV"

# combine
combined_df_s3 <- rbind(results_hiv_s3, results_hcv_s3)

# combined plot
combined_incidence_plot_s3 <- ggplot(
  combined_df_s3,
  aes(x = two_year_interval,
      y = median_incidence_rate,
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

ggsave("plots/combined_hcv_hiv_incidence_plot_s3.png", plot = combined_incidence_plot_s3, width = 10, height = 6, dpi = 300)
