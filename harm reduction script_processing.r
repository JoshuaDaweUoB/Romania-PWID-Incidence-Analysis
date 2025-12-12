## load packages
pacman::p_load(dplyr, tidyr, withr, lubridate, MASS, writexl, readxl, arsenal, survival, broom, ggplot2, purrr, tableone, zoo, scales)

## set wd
setwd("C:/Users/vl22683/OneDrive - University of Bristol/Documents/Publications/Romania PWID/data")

## load data
romania_pwid_raw <- readxl::read_excel("ARAS DATA IDU 2013-2022.xlsx")

# keep relevant variables
romania_pwid_ts <- romania_pwid_raw %>%
  dplyr::select(
    id,
    appointment_dte,
    syringe_secondary,
    drug_type,
    condoms_distributed,
    syringes_distributed_1ml,
    syringes_distributed_2ml,
    syringes_recovered,
    hcv_test_rslt,
    hiv_test_rslt
  )

## recode hiv/hcv test results to 0/1
romania_pwid_ts <- romania_pwid_ts %>%
  dplyr::mutate(
    hiv_test_rslt = dplyr::case_when(hiv_test_rslt == 1 ~ 0L, hiv_test_rslt == 2 ~ 1L, TRUE ~ as.integer(hiv_test_rslt)),
    hcv_test_rslt = dplyr::case_when(hcv_test_rslt == 1 ~ 0L, hcv_test_rslt == 2 ~ 1L, TRUE ~ as.integer(hcv_test_rslt))
  )

# flag rows with any harm-reduction use
romania_pwid_ts <- romania_pwid_ts %>%
  dplyr::mutate(
    hr_use_flag = dplyr::if_else(
      rowSums(cbind(
        !is.na(condoms_distributed),
        !is.na(syringes_distributed_1ml),
        !is.na(syringes_distributed_2ml),
        !is.na(syringes_recovered),
        !is.na(hcv_test_rslt),
        !is.na(hiv_test_rslt)
      )) > 0,
      1L, 0L
    )
  ) %>%
  dplyr::filter(hr_use_flag == 1L)

## generate time periods (Year + Half + Quarter)
romania_pwid_ts <- romania_pwid_ts %>%
  dplyr::mutate(
    appointment_dte_chr = as.character(appointment_dte) |> trimws() |> sub(" UTC$", "", .),
    appointment_dte = dplyr::case_when(
      grepl("^\\d{2}\\.\\d{2}\\.\\d{4}$", appointment_dte_chr) ~ lubridate::dmy(appointment_dte_chr),
      grepl("^\\d{4}-\\d{2}-\\d{2}$", appointment_dte_chr) ~ lubridate::ymd(appointment_dte_chr),
      is.numeric(appointment_dte) ~ as.Date(appointment_dte, origin = "1899-12-30"),
      inherits(appointment_dte, "POSIXt") ~ as.Date(appointment_dte),
      inherits(appointment_dte, "Date") ~ appointment_dte,
      TRUE ~ as.Date(NA)
    ),
    Year = lubridate::year(appointment_dte),
    Half_num = ifelse(lubridate::month(appointment_dte) <= 6, 1L, 2L),
    Half = paste0(lubridate::year(appointment_dte), " H", Half_num),
    Quarter_num = as.integer(((lubridate::month(appointment_dte) - 1) %/% 3) + 1L),
    Quarter = paste0(lubridate::year(appointment_dte), " Q", Quarter_num)
  ) %>%
  dplyr::select(-appointment_dte_chr) %>%
  dplyr::arrange(appointment_dte)

# Filter to 2014 onwards
romania_pwid_ts <- romania_pwid_ts %>% dplyr::filter(Year >= 2014L)

# Ensure all halves and quarters exist from 2014 to max, in order
years_full <- seq(min(romania_pwid_ts$Year, na.rm = TRUE), max(romania_pwid_ts$Year, na.rm = TRUE))
half_levels <- as.vector(outer(years_full, c(" H1", " H2"), paste0))
romania_pwid_ts <- romania_pwid_ts %>% dplyr::mutate(Half = factor(Half, levels = half_levels))
quarter_levels <- as.vector(outer(years_full, c(" Q1", " Q2", " Q3", " Q4"), paste0))
romania_pwid_ts <- romania_pwid_ts %>% dplyr::mutate(Quarter = factor(Quarter, levels = quarter_levels))

# Helper: summarise by period
summarize_by_period <- function(df, period_col) {
  has_hiv <- "hiv_test_rslt" %in% names(df)
  has_hcv <- "hcv_test_rslt" %in% names(df)

  df %>%
    dplyr::group_by({{ period_col }}) %>%
    dplyr::summarise(
      total_visits = dplyr::n(),
      unique_ids = dplyr::n_distinct(id),
      # Per-reason visit counts
      total_visits_condom   = sum(!is.na(condoms_distributed)      & condoms_distributed        > 0, na.rm = TRUE),
      total_visits_syringe  = sum((!is.na(syringes_distributed_1ml) & syringes_distributed_1ml  > 0) |
                                   (!is.na(syringes_distributed_2ml) & syringes_distributed_2ml > 0), na.rm = TRUE),
      total_visits_hcv      = if (has_hcv) sum(!is.na(hcv_test_rslt)) else NA_integer_,
      total_visits_hiv      = if (has_hiv) sum(!is.na(hiv_test_rslt)) else NA_integer_,
      # Per-reason unique IDs
      unique_ids_condom   = dplyr::n_distinct(dplyr::if_else(!is.na(condoms_distributed)      & condoms_distributed        > 0, id, NA_character_)),
      unique_ids_syringes = dplyr::n_distinct(dplyr::if_else((!is.na(syringes_distributed_1ml) & syringes_distributed_1ml  > 0) |
                                                              (!is.na(syringes_distributed_2ml) & syringes_distributed_2ml > 0), id, NA_character_)),
      unique_ids_hcv      = if (has_hcv) dplyr::n_distinct(dplyr::if_else(!is.na(hcv_test_rslt), id, NA_character_)) else NA_integer_,
      unique_ids_hiv      = if (has_hiv) dplyr::n_distinct(dplyr::if_else(!is.na(hiv_test_rslt), id, NA_character_)) else NA_integer_,
      unique_condoms_events   = sum(!is.na(condoms_distributed)      & condoms_distributed        > 0, na.rm = TRUE),
      unique_1ml_events       = sum(!is.na(syringes_distributed_1ml)  & syringes_distributed_1ml   > 0, na.rm = TRUE),
      unique_2ml_events       = sum(!is.na(syringes_distributed_2ml)  & syringes_distributed_2ml   > 0, na.rm = TRUE),
      unique_recovered_events = sum(!is.na(syringes_recovered)        & syringes_recovered         > 0, na.rm = TRUE),
      total_condoms   = sum(condoms_distributed,      na.rm = TRUE),
      total_1ml       = sum(syringes_distributed_1ml, na.rm = TRUE),
      total_2ml       = sum(syringes_distributed_2ml, na.rm = TRUE),
      total_syringes  = sum(syringes_distributed_1ml, na.rm = TRUE) + sum(syringes_distributed_2ml, na.rm = TRUE),
      total_recovered = sum(syringes_recovered,       na.rm = TRUE),
      hiv_tests      = if (has_hiv) sum(!is.na(hiv_test_rslt)) else NA_integer_,
      hiv_positives  = if (has_hiv) sum(hiv_test_rslt == 1, na.rm = TRUE) else NA_integer_,
      hiv_positivity = if (has_hiv) { denom <- sum(!is.na(hiv_test_rslt)); ifelse(denom > 0, sum(hiv_test_rslt == 1, na.rm = TRUE)/denom, NA_real_) } else NA_real_,
      hcv_tests      = if (has_hcv) sum(!is.na(hcv_test_rslt)) else NA_integer_,
      hcv_positives  = if (has_hcv) sum(hcv_test_rslt == 1, na.rm = TRUE) else NA_integer_,
      hcv_positivity = if (has_hcv) { denom <- sum(!is.na(hcv_test_rslt)); ifelse(denom > 0, sum(hcv_test_rslt == 1, na.rm = TRUE)/denom, NA_real_) } else NA_real_,
      .groups = "drop"
    )
}

# Yearly summary (2014+ complete)
year_range <- seq(2015L, max(romania_pwid_ts$Year, na.rm = TRUE))
yearly_df <- summarize_by_period(romania_pwid_ts, period_col = Year) %>%
  tidyr::complete(Year = year_range) %>%
  dplyr::mutate(
    dplyr::across(
      c(unique_ids, unique_condoms_events, unique_1ml_events, unique_2ml_events, unique_recovered_events,
        total_condoms, total_1ml, total_2ml, total_syringes, total_recovered,
        hiv_tests, hiv_positives, hcv_tests, hcv_positives, total_visits,
        total_visits_condom, total_visits_syringe, total_visits_hcv, total_visits_hiv,
        unique_ids_condom, unique_ids_syringes, unique_ids_hcv, unique_ids_hiv),
      ~ tidyr::replace_na(., 0)
    ),
    hiv_positivity = dplyr::if_else(hiv_tests > 0, hiv_positives / hiv_tests, NA_real_),
    hcv_positivity = dplyr::if_else(hcv_tests > 0, hcv_positives / hcv_tests, NA_real_)
  ) %>%
  dplyr::arrange(Year)

# Half-year summary (2014+ complete) and half start date
half_df <- summarize_by_period(romania_pwid_ts, period_col = Half) %>%
  tidyr::complete(Half = factor(half_levels, levels = half_levels)) %>%
  dplyr::mutate(
    dplyr::across(
      c(unique_ids, unique_condoms_events, unique_1ml_events, unique_2ml_events, unique_recovered_events,
        total_condoms, total_1ml, total_2ml, total_syringes, total_recovered,
        hiv_tests, hiv_positives, hcv_tests, hcv_positives, total_visits,
        total_visits_condom, total_visits_syringe, total_visits_hcv, total_visits_hiv,
        unique_ids_condom, unique_ids_syringes, unique_ids_hcv, unique_ids_hiv),
      ~ tidyr::replace_na(., 0)
    ),
    hiv_positivity = dplyr::if_else(hiv_tests > 0, hiv_positives / hiv_tests, NA_real_),
    hcv_positivity = dplyr::if_else(hcv_tests > 0, hcv_positives / hcv_tests, NA_real_),
    Half_start = as.Date(paste0(sub(" H[12]$", "", Half), ifelse(grepl("H1$", Half), "-01-01", "-07-01")))
  ) %>%
  dplyr::arrange(Half_start)

# Quarter summary (2014+ complete) and quarter start date
quarter_df <- summarize_by_period(romania_pwid_ts, period_col = Quarter) %>%
  tidyr::complete(Quarter = factor(quarter_levels, levels = quarter_levels)) %>%
  dplyr::mutate(
    dplyr::across(
      c(unique_ids, unique_condoms_events, unique_1ml_events, unique_2ml_events, unique_recovered_events,
        total_condoms, total_1ml, total_2ml, total_syringes, total_recovered,
        hiv_tests, hiv_positives, hcv_tests, hcv_positives, total_visits,
        total_visits_condom, total_visits_syringe, total_visits_hcv, total_visits_hiv,
        unique_ids_condom, unique_ids_syringes, unique_ids_hcv, unique_ids_hiv),
      ~ tidyr::replace_na(., 0)
    ),
    hiv_positivity = dplyr::if_else(hiv_tests > 0, hiv_positives / hiv_tests, NA_real_),
    hcv_positivity = dplyr::if_else(hcv_tests > 0, hcv_positives / hcv_tests, NA_real_),
    Quarter_start = as.Date(paste0(sub(" Q[1-4]$", "", Quarter),
                                   dplyr::case_when(
                                     grepl("Q1$", Quarter) ~ "-01-01",
                                     grepl("Q2$", Quarter) ~ "-04-01",
                                     grepl("Q3$", Quarter) ~ "-07-01",
                                     TRUE ~ "-10-01"
                                   )))
  ) %>%
  dplyr::arrange(Quarter_start)

## Service usage: split figures
# Yearly unique individuals (overall, condoms, syringes)
yearly_unique_df <- yearly_df %>%
  dplyr::filter(Year >= 2015L) %>%
  dplyr::select(Year, unique_ids, unique_ids_condom, unique_ids_syringes) %>%
  tidyr::pivot_longer(c(unique_ids, unique_ids_condom, unique_ids_syringes),
                      names_to = "metric", values_to = "count") %>%
  dplyr::mutate(metric = factor(metric,
    levels = c("unique_ids", "unique_ids_condom", "unique_ids_syringes"),
    labels = c("Unique individuals (all)", "Unique individuals (condoms)", "Unique individuals (syringes)")
  ))

p_yearly_unique <- ggplot2::ggplot(yearly_unique_df,
  ggplot2::aes(x = Year, y = count, color = metric, linetype = metric, group = metric)) +
  ggplot2::geom_line(linewidth = 1) +
  ggplot2::geom_point(size = 2) +
  ggplot2::scale_x_continuous(breaks = year_range) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::labs(title = "Yearly service usage: unique individuals",
                x = "Year", y = "Unique individuals", color = "Series", linetype = "Series") +
  ggplot2::theme_classic(base_size = 12) +
  ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white", color = NA),
                 plot.background  = ggplot2::element_rect(fill = "white", color = NA),
                 legend.background = ggplot2::element_rect(fill = "white", color = NA),
                 legend.key = ggplot2::element_rect(fill = "white", color = NA))

# Yearly total visits (overall, condoms, syringes)
yearly_visits_only_df <- yearly_df %>%
  dplyr::filter(Year >= 2015L) %>%
  dplyr::select(Year, total_visits, total_visits_condom, total_visits_syringe) %>%
  tidyr::pivot_longer(c(total_visits, total_visits_condom, total_visits_syringe),
                      names_to = "metric", values_to = "count") %>%
  dplyr::mutate(metric = factor(metric,
    levels = c("total_visits", "total_visits_condom", "total_visits_syringe"),
    labels = c("Total visits (all)", "Total visits (condoms)", "Total visits (syringes)")
  ))

p_yearly_visits_total <- ggplot2::ggplot(yearly_visits_only_df,
  ggplot2::aes(x = Year, y = count, color = metric, linetype = metric, group = metric)) +
  ggplot2::geom_line(linewidth = 1) +
  ggplot2::geom_point(size = 2) +
  ggplot2::scale_x_continuous(breaks = year_range) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::labs(title = "Yearly service usage: total visits",
                x = "Year", y = "Total visits", color = "Series", linetype = "Series") +
  ggplot2::theme_classic(base_size = 12) +
  ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white", color = NA),
                 plot.background  = ggplot2::element_rect(fill = "white", color = NA),
                 legend.background = ggplot2::element_rect(fill = "white", color = NA),
                 legend.key = ggplot2::element_rect(fill = "white", color = NA))

# Precompute explicit half-year breaks from 2015-01-01
half_breaks <- seq(as.Date("2015-01-01"), max(half_df$Half_start, na.rm = TRUE), by = "6 months")
half_labels <- function(d) paste0(format(d, "%Y"), " H", ifelse(format(d, "%m") == "01", "1", "2"))

# Precompute explicit quarter breaks from 2015-01-01
quarter_breaks <- seq(as.Date("2015-01-01"), max(quarter_df$Quarter_start, na.rm = TRUE), by = "3 months")
quarter_labels <- function(d) paste0(format(d, "%Y"), " Q",
                                     dplyr::case_when(
                                       format(d, "%m") == "01" ~ "1",
                                       format(d, "%m") == "04" ~ "2",
                                       format(d, "%m") == "07" ~ "3",
                                       TRUE ~ "4"
                                     ))

# Half-year unique individuals (overall, condoms, syringes)
half_unique_df <- half_df %>%
  dplyr::filter(Half_start >= as.Date("2015-01-01")) %>%
  dplyr::select(Half_start, unique_ids, unique_ids_condom, unique_ids_syringes) %>%
  tidyr::pivot_longer(c(unique_ids, unique_ids_condom, unique_ids_syringes),
                      names_to = "metric", values_to = "count") %>%
  dplyr::mutate(metric = factor(metric,
    levels = c("unique_ids", "unique_ids_condom", "unique_ids_syringes"),
    labels = c("Unique individuals (all)", "Unique individuals (condoms)", "Unique individuals (syringes)")
  ))

p_half_unique <- ggplot2::ggplot(half_unique_df,
  ggplot2::aes(x = Half_start, y = count, color = metric, linetype = metric, group = metric)) +
  ggplot2::geom_line(linewidth = 1, na.rm = TRUE) +
  ggplot2::geom_point(size = 2, na.rm = TRUE) +
  ggplot2::scale_x_date(breaks = half_breaks, labels = half_labels, expand = ggplot2::expansion(mult = c(0.01, 0.01))) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::labs(title = "Six-month service usage: unique individuals",
                x = "Half-year", y = "Unique individuals", color = "Series", linetype = "Series") +
  ggplot2::theme_classic(base_size = 12) +
  ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white", color = NA),
                 plot.background  = ggplot2::element_rect(fill = "white", color = NA),
                 axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                 legend.background = ggplot2::element_rect(fill = "white", color = NA),
                 legend.key = ggplot2::element_rect(fill = "white", color = NA))

# Quarter unique individuals (overall, condoms, syringes)
quarter_unique_df <- quarter_df %>%
  dplyr::select(Quarter_start, unique_ids, unique_ids_condom, unique_ids_syringes) %>%
  tidyr::pivot_longer(c(unique_ids, unique_ids_condom, unique_ids_syringes),
                      names_to = "metric", values_to = "count") %>%
  dplyr::mutate(metric = factor(metric,
    levels = c("unique_ids", "unique_ids_condom", "unique_ids_syringes"),
    labels = c("Unique individuals (all)", "Unique individuals (condoms)", "Unique individuals (syringes)")
  ))

p_quarter_unique <- ggplot2::ggplot(quarter_unique_df,
  ggplot2::aes(x = Quarter_start, y = count, color = metric, linetype = metric, group = metric)) +
  ggplot2::geom_line(linewidth = 1, na.rm = TRUE) +
  ggplot2::geom_point(size = 2, na.rm = TRUE) +
  ggplot2::scale_x_date(breaks = quarter_breaks, labels = quarter_labels, expand = ggplot2::expansion(mult = c(0.01, 0.01))) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::labs(title = "Quarterly service usage: unique individuals",
                x = "Quarter", y = "Unique individuals", color = "Series", linetype = "Series") +
  ggplot2::theme_classic(base_size = 12) +
  ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white", color = NA),
                 plot.background  = ggplot2::element_rect(fill = "white", color = NA),
                 axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                 legend.background = ggplot2::element_rect(fill = "white", color = NA),
                 legend.key = ggplot2::element_rect(fill = "white", color = NA))

# Half-year total visits (overall, condoms, syringes)
half_visits_only_df <- half_df %>%
  dplyr::filter(Half_start >= as.Date("2015-01-01")) %>%
  dplyr::select(Half_start, total_visits, total_visits_condom, total_visits_syringe) %>%
  tidyr::pivot_longer(c(total_visits, total_visits_condom, total_visits_syringe),
                      names_to = "metric", values_to = "count") %>%
  dplyr::mutate(metric = factor(metric,
    levels = c("total_visits", "total_visits_condom", "total_visits_syringe"),
    labels = c("Total visits (all)", "Total visits (condoms)", "Total visits (syringes)")
  ))

p_half_visits_total <- ggplot2::ggplot(half_visits_only_df,
  ggplot2::aes(x = Half_start, y = count, color = metric, linetype = metric, group = metric)) +
  ggplot2::geom_line(linewidth = 1, na.rm = TRUE) +
  ggplot2::geom_point(size = 2, na.rm = TRUE) +
  ggplot2::scale_x_date(breaks = half_breaks, labels = half_labels, expand = ggplot2::expansion(mult = c(0.01, 0.01))) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::labs(title = "Six-month service usage: total visits",
                x = "Half-year", y = "Total visits", color = "Series", linetype = "Series") +
  ggplot2::theme_classic(base_size = 12) +
  ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white", color = NA),
                 plot.background  = ggplot2::element_rect(fill = "white", color = NA),
                 axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                 legend.background = ggplot2::element_rect(fill = "white", color = NA),
                 legend.key = ggplot2::element_rect(fill = "white", color = NA))

# Quarter total visits (overall, condoms, syringes)
quarter_visits_only_df <- quarter_df %>%
  dplyr::select(Quarter_start, total_visits, total_visits_condom, total_visits_syringe) %>%
  tidyr::pivot_longer(c(total_visits, total_visits_condom, total_visits_syringe),
                      names_to = "metric", values_to = "count") %>%
  dplyr::mutate(metric = factor(metric,
    levels = c("total_visits", "total_visits_condom", "total_visits_syringe"),
    labels = c("Total visits (all)", "Total visits (condoms)", "Total visits (syringes)")
  ))

p_quarter_visits_total <- ggplot2::ggplot(quarter_visits_only_df,
  ggplot2::aes(x = Quarter_start, y = count, color = metric, linetype = metric, group = metric)) +
  ggplot2::geom_line(linewidth = 1, na.rm = TRUE) +
  ggplot2::geom_point(size = 2, na.rm = TRUE) +
  ggplot2::scale_x_date(breaks = quarter_breaks, labels = quarter_labels, expand = ggplot2::expansion(mult = c(0.01, 0.01))) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::labs(title = "Quarterly service usage: total visits",
                x = "Quarter", y = "Total visits", color = "Series", linetype = "Series") +
  ggplot2::theme_classic(base_size = 12) +
  ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white", color = NA),
                 plot.background  = ggplot2::element_rect(fill = "white", color = NA),
                 axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                 legend.background = ggplot2::element_rect(fill = "white", color = NA),
                 legend.key = ggplot2::element_rect(fill = "white", color = NA))

## Syringe-only usage figures (unique individuals and visits)
# Yearly syringe-only usage
yearly_syr_usage_df <- yearly_df %>%
  dplyr::filter(Year >= 2015L) %>%
  dplyr::select(Year, unique_ids_syringes, total_visits_syringe) %>%
  tidyr::pivot_longer(c(unique_ids_syringes, total_visits_syringe),
                      names_to = "metric", values_to = "count") %>%
  dplyr::mutate(metric = factor(metric,
    levels = c("Individuals accessing syringes", "total_visits_syringe"),
    labels = c("Visits to service to access syringes", "Syringe visits")
  ))

p_yearly_syringe_usage <- ggplot2::ggplot(yearly_syr_usage_df,
  ggplot2::aes(x = Year, y = count, color = metric, linetype = metric, group = metric)) +
  ggplot2::geom_line(linewidth = 1) +
  ggplot2::geom_point(size = 2) +
  ggplot2::scale_x_continuous(breaks = year_range) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::labs(title = "Yearly syringe usage: unique individuals and visits",
                x = "Year", y = "Count", color = "Series", linetype = "Series") +
  ggplot2::theme_classic(base_size = 12) +
  ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white", color = NA),
                 plot.background  = ggplot2::element_rect(fill = "white", color = NA),
                 legend.background = ggplot2::element_rect(fill = "white", color = NA),
                 legend.key = ggplot2::element_rect(fill = "white", color = NA))

# Half-year syringe-only usage
half_syr_usage_df <- half_df %>%
  dplyr::filter(Half_start >= as.Date("2015-01-01")) %>%
  dplyr::select(Half_start, unique_ids_syringes, total_visits_syringe) %>%
  tidyr::pivot_longer(c(unique_ids_syringes, total_visits_syringe),
                      names_to = "metric", values_to = "count") %>%
  dplyr::mutate(metric = factor(metric,
    levels = c("unique_ids_syringes", "total_visits_syringe"),
    labels = c("Individuals accessing syringes", "Total visits to service for syringes")
  ))

p_half_syringe_usage <- ggplot2::ggplot(half_syr_usage_df,
  ggplot2::aes(x = Half_start, y = count, color = metric, linetype = metric, group = metric)) +
  ggplot2::geom_line(linewidth = 1, na.rm = TRUE) +
  ggplot2::geom_point(size = 2, na.rm = TRUE) +
  ggplot2::scale_x_date(breaks = half_breaks, labels = half_labels, expand = ggplot2::expansion(mult = c(0.01, 0.01))) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::labs(title = "Six-month syringe usage: unique individuals and visits to service",
                x = "Half-year", y = "Count", color = "Series", linetype = "Series") +
  ggplot2::theme_classic(base_size = 12) +
  ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white", color = NA),
                 plot.background  = ggplot2::element_rect(fill = "white", color = NA),
                 axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                 legend.background = ggplot2::element_rect(fill = "white", color = NA),
                 legend.key = ggplot2::element_rect(fill = "white", color = NA))

# Quarterly syringe-only usage
quarter_syr_usage_df <- quarter_df %>%
  dplyr::filter(Quarter_start >= as.Date("2015-01-01")) %>%
  dplyr::select(Quarter_start, unique_ids_syringes, total_visits_syringe) %>%
  tidyr::pivot_longer(c(unique_ids_syringes, total_visits_syringe),
                      names_to = "metric", values_to = "count") %>%
  dplyr::mutate(metric = factor(metric,
    levels = c("unique_ids_syringes", "total_visits_syringe"),
    labels = c("Individuals accessing syringes", "Total visits to service for syringes")
  ))

p_quarter_syringe_usage <- ggplot2::ggplot(quarter_syr_usage_df,
  ggplot2::aes(x = Quarter_start, y = count, color = metric, linetype = metric, group = metric)) +
  ggplot2::geom_line(linewidth = 1, na.rm = TRUE) +
  ggplot2::geom_point(size = 2, na.rm = TRUE) +
  ggplot2::scale_x_date(breaks = quarter_breaks, labels = quarter_labels, expand = ggplot2::expansion(mult = c(0.01, 0.01))) +
  ggplot2::scale_y_continuous(labels = scales::comma, breaks = scales::breaks_width(500)) +
  ggplot2::labs(title = "Quarterly syringe usage: unique individuals and visits",
                x = "Quarter", y = "Count", color = "Series", linetype = "Series") +
  ggplot2::theme_classic(base_size = 12) +
  ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white", color = NA),
                 plot.background  = ggplot2::element_rect(fill = "white", color = NA),
                 axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                 legend.background = ggplot2::element_rect(fill = "white", color = NA),
                 legend.key = ggplot2::element_rect(fill = "white", color = NA))

# Yearly totals plot
yearly_three_df <- yearly_df %>%
  dplyr::filter(Year >= 2015L) %>%
  dplyr::select(Year, total_syringes, total_recovered) %>%
  tidyr::pivot_longer(c(total_syringes, total_recovered), names_to = "metric", values_to = "total") %>%
  dplyr::mutate(metric = factor(metric,
    levels = c("total_syringes", "total_recovered"),
    labels = c("All syringes distributed", "Syringes recovered")
  ))

p_yearly3 <- ggplot2::ggplot(yearly_three_df,
  ggplot2::aes(x = Year, y = total, color = metric, linetype = metric, group = metric)) +
  ggplot2::geom_line(linewidth = 1) +
  ggplot2::geom_point(size = 2) +
  ggplot2::scale_x_continuous(breaks = year_range) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::labs(title = "Yearly totals: syringes distributed vs recovered",
                x = "Year", y = "Total count", color = "Metric", linetype = "Metric") +
  ggplot2::theme_classic(base_size = 12) +
  ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white", color = NA),
                 plot.background  = ggplot2::element_rect(fill = "white", color = NA),
                 legend.background = ggplot2::element_rect(fill = "white", color = NA),
                 legend.key = ggplot2::element_rect(fill = "white", color = NA))

# Half-year totals plot (true 6-month ticks from 2014)
half_three_df <- half_df %>%
  dplyr::filter(Half_start >= as.Date("2015-01-01")) %>%
  dplyr::select(Half, Half_start, total_syringes, total_recovered) %>%
  tidyr::pivot_longer(c(total_syringes, total_recovered), names_to = "metric", values_to = "total") %>%
  dplyr::mutate(metric = factor(metric,
    levels = c("total_syringes", "total_recovered"),
    labels = c("All syringes distributed", "Syringes recovered")
  ))

p_half3 <- ggplot2::ggplot(half_three_df,
  ggplot2::aes(x = Half_start, y = total, color = metric, linetype = metric, group = metric)) +
  ggplot2::geom_line(linewidth = 1) +
  ggplot2::geom_point(size = 2) +
  ggplot2::scale_x_date(breaks = half_breaks, labels = half_labels, expand = ggplot2::expansion(mult = c(0.01, 0.01))) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::labs(title = "Six-month totals: syringes distributed vs recovered",
                x = "Half-year", y = "Total count", color = "Metric", linetype = "Metric") +
  ggplot2::theme_classic(base_size = 12) +
  ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white", color = NA),
                 plot.background  = ggplot2::element_rect(fill = "white", color = NA),
                 legend.background = ggplot2::element_rect(fill = "white", color = NA),
                 legend.key = ggplot2::element_rect(fill = "white", color = NA),
                 axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

# Quarterly totals plot (true 3-month ticks from 2014)
quarter_three_df <- quarter_df %>%
  dplyr::filter(Quarter_start >= as.Date("2015-01-01")) %>%
  dplyr::select(Quarter, Quarter_start, total_syringes, total_recovered) %>%
  tidyr::pivot_longer(c(total_syringes, total_recovered), names_to = "metric", values_to = "total") %>%
  dplyr::mutate(metric = factor(metric,
    levels = c("total_syringes", "total_recovered"),
    labels = c("All syringes distributed", "Syringes recovered")
  ))

p_quarter3 <- ggplot2::ggplot(quarter_three_df,
  ggplot2::aes(x = Quarter_start, y = total, color = metric, linetype = metric, group = metric)) +
  ggplot2::geom_line(linewidth = 1) +
  ggplot2::geom_point(size = 2) +
  ggplot2::scale_x_date(breaks = quarter_breaks, labels = quarter_labels, expand = ggplot2::expansion(mult = c(0.01, 0.01))) +
  ggplot2::scale_y_continuous(labels = scales::comma, breaks = scales::breaks_width(50000)) +
  ggplot2::labs(title = "Quarterly totals: syringes distributed vs recovered",
                x = "Quarter", y = "Total count", color = "Metric", linetype = "Metric") +
  ggplot2::theme_classic(base_size = 12) +
  ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white", color = NA),
                 plot.background  = ggplot2::element_rect(fill = "white", color = NA),
                 legend.background = ggplot2::element_rect(fill = "white", color = NA),
                 legend.key = ggplot2::element_rect(fill = "white", color = NA),
                 axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

# Yearly BBV positivity
yearly_pos_df <- yearly_df %>%
  dplyr::filter(Year >= 2015L) %>%
  dplyr::select(Year, hiv_positivity, hcv_positivity) %>%
  tidyr::pivot_longer(c(hiv_positivity, hcv_positivity), names_to = "virus", values_to = "positivity") %>%
  dplyr::mutate(virus = dplyr::recode(virus, hiv_positivity = "HIV", hcv_positivity = "HCV")) %>%
  tidyr::drop_na(positivity)

bbv_positivity_yearly <- ggplot2::ggplot(yearly_pos_df,
  ggplot2::aes(x = Year, y = positivity, color = virus, linetype = virus, group = virus)) +
  ggplot2::geom_line(linewidth = 1) +
  ggplot2::geom_point(size = 2) +
  ggplot2::scale_x_continuous(breaks = year_range) +
  ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  ggplot2::labs(title = "Yearly BBV test positivity", x = "Year", y = "Positivity", color = "Virus", linetype = "Virus") +
  ggplot2::theme_classic(base_size = 12) +
  ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white", color = NA),
                 plot.background  = ggplot2::element_rect(fill = "white", color = NA),
                 legend.background = ggplot2::element_rect(fill = "white", color = NA),
                 legend.key = ggplot2::element_rect(fill = "white", color = NA))

# Half-year BBV positivity (date axis, 6-month ticks from 2014)
half_pos_df <- half_df %>%
  dplyr::filter(Half_start >= as.Date("2015-01-01")) %>%
  dplyr::select(Half, Half_start, hiv_positivity, hcv_positivity) %>%
  tidyr::pivot_longer(c(hiv_positivity, hcv_positivity), names_to = "virus", values_to = "positivity") %>%
  dplyr::mutate(virus = dplyr::recode(virus, hiv_positivity = "HIV", hcv_positivity = "HCV"))

bbv_positivity_halfyear <- ggplot2::ggplot(half_pos_df,
  ggplot2::aes(x = Half_start, y = positivity, color = virus, linetype = virus, group = virus)) +
  ggplot2::geom_line(linewidth = 1, na.rm = TRUE) +
  ggplot2::geom_point(size = 2, na.rm = TRUE) +
  ggplot2::scale_x_date(breaks = half_breaks, labels = half_labels, expand = ggplot2::expansion(mult = c(0.01, 0.01))) +
  ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  ggplot2::labs(title = "Six-month BBV test positivity", x = "Half-year", y = "Positivity", color = "Virus", linetype = "Virus") +
  ggplot2::theme_classic(base_size = 12) +
  ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white", color = NA),
                 plot.background  = ggplot2::element_rect(fill = "white", color = NA),
                 legend.background = ggplot2::element_rect(fill = "white", color = NA),
                 legend.key = ggplot2::element_rect(fill = "white", color = NA),
                 axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

# Quarterly BBV positivity (date axis, 3-month ticks from 2014)
quarter_pos_df <- quarter_df %>%
  dplyr::filter(Quarter_start >= as.Date("2015-01-01")) %>%
  dplyr::select(Quarter, Quarter_start, hiv_positivity, hcv_positivity) %>%
  tidyr::pivot_longer(c(hiv_positivity, hcv_positivity), names_to = "virus", values_to = "positivity") %>%
  dplyr::mutate(virus = dplyr::recode(virus, hiv_positivity = "HIV", hcv_positivity = "HCV"))

bbv_positivity_quarter <- ggplot2::ggplot(quarter_pos_df,
  ggplot2::aes(x = Quarter_start, y = positivity, color = virus, linetype = virus, group = virus)) +
  ggplot2::geom_line(linewidth = 1, na.rm = TRUE) +
  ggplot2::geom_point(size = 2, na.rm = TRUE) +
  ggplot2::scale_x_date(breaks = quarter_breaks, labels = quarter_labels, expand = ggplot2::expansion(mult = c(0.01, 0.01))) +
  ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  ggplot2::labs(title = "Quarterly BBV test positivity", x = "Quarter", y = "Positivity", color = "Virus", linetype = "Virus") +
  ggplot2::theme_classic(base_size = 12) +
  ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white", color = NA),
                 plot.background  = ggplot2::element_rect(fill = "white", color = NA),
                 legend.background = ggplot2::element_rect(fill = "white", color = NA),
                 legend.key = ggplot2::element_rect(fill = "white", color = NA),
                 axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

# Yearly average syringes per person (target 300)
yearly_avg_df <- yearly_df %>%
  dplyr::filter(Year >= 2015L) %>%
  dplyr::mutate(avg_syr_per_person = dplyr::if_else(!is.na(unique_ids_syringes) & unique_ids_syringes > 0, total_syringes / unique_ids_syringes, NA_real_)) %>%
  dplyr::select(Year, avg_syr_per_person)

avg_syringes_per_person_yearly <- ggplot2::ggplot(yearly_avg_df,
  ggplot2::aes(x = Year, y = avg_syr_per_person, group = 1)) +
  ggplot2::geom_line(color = "#1b6ca8", linewidth = 1) +
  ggplot2::geom_point(color = "#1b6ca8", size = 2) +
  ggplot2::scale_x_continuous(breaks = year_range) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::labs(title = "Average syringes per person per year", x = "Year", y = "Average syringes per person") +
  ggplot2::theme_classic(base_size = 12) +
  ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white", color = NA),
                 plot.background  = ggplot2::element_rect(fill = "white", color = NA)) +
  ggplot2::geom_hline(yintercept = 300, color = "#d55e00", linetype = "dashed", linewidth = 0.9) +
  ggplot2::annotate("text", x = max(yearly_avg_df$Year, na.rm = TRUE), y = 300, label = "WHO target: 300 syringes",
                    hjust = 1.05, vjust = -0.6, color = "#d55e00", size = 3.5)

# Half-year average syringes per person (date axis, 6-month ticks)
half_avg_df <- half_df %>%
  dplyr::filter(Half_start >= as.Date("2015-01-01")) %>%
  dplyr::mutate(
    avg_syr_per_person = dplyr::case_when(
      !is.na(unique_ids_syringes) & unique_ids_syringes > 0 ~ total_syringes / unique_ids_syringes,
      (is.na(unique_ids_syringes) | unique_ids_syringes == 0) & dplyr::coalesce(total_syringes, 0) == 0 ~ 0,
      TRUE ~ NA_real_
    )
  ) %>%
  dplyr::select(Half, Half_start, avg_syr_per_person)

avg_syringes_per_person_halfyear <- ggplot2::ggplot(half_avg_df,
  ggplot2::aes(x = Half_start, y = avg_syr_per_person, group = 1)) +
  ggplot2::geom_line(color = "#1b6ca8", linewidth = 1, na.rm = TRUE) +
  ggplot2::geom_point(color = "#1b6ca8", size = 2, na.rm = TRUE) +
  ggplot2::scale_x_date(breaks = half_breaks, labels = half_labels, expand = ggplot2::expansion(mult = c(0.01, 0.01))) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::labs(title = "Average syringes per person per half-year", x = "Half-year", y = "Average syringes per person") +
  ggplot2::theme_classic(base_size = 12) +
  ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white", color = NA),
                 plot.background  = ggplot2::element_rect(fill = "white", color = NA),
                 axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
  ggplot2::geom_hline(yintercept = 150, color = "#d55e00", linetype = "dashed", linewidth = 0.9) +
  ggplot2::annotate("text",
                    x = max(half_avg_df$Half_start, na.rm = TRUE), y = 150,
                    label = "WHO target: 150 syringes",
                    hjust = 1.05, vjust = -0.6, color = "#d55e00", size = 3.5)

# Quarterly average syringes per person (date axis, 3-month ticks)
quarter_avg_df <- quarter_df %>%
  dplyr::filter(Quarter_start >= as.Date("2015-01-01")) %>%
  dplyr::mutate(
    avg_syr_per_person = dplyr::case_when(
      !is.na(unique_ids_syringes) & unique_ids_syringes > 0 ~ total_syringes / unique_ids_syringes,
      (is.na(unique_ids_syringes) | unique_ids_syringes == 0) & dplyr::coalesce(total_syringes, 0) == 0 ~ 0,
      TRUE ~ NA_real_
    )
  ) %>%
  dplyr::select(Quarter, Quarter_start, avg_syr_per_person)

avg_syringes_per_person_quarter <- ggplot2::ggplot(quarter_avg_df,
  ggplot2::aes(x = Quarter_start, y = avg_syr_per_person, group = 1)) +
  ggplot2::geom_line(color = "#1b6ca8", linewidth = 1, na.rm = TRUE) +
  ggplot2::geom_point(color = "#1b6ca8", size = 2, na.rm = TRUE) +
  ggplot2::scale_x_date(breaks = quarter_breaks, labels = quarter_labels, expand = ggplot2::expansion(mult = c(0.01, 0.01))) +
  ggplot2::scale_y_continuous(labels = scales::comma, breaks = scales::breaks_width(25)) +
  ggplot2::labs(title = "Average syringes per person per quarter", x = "Quarter", y = "Average syringes per person") +
  ggplot2::theme_classic(base_size = 12) +
  ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white", color = NA),
                 plot.background  = ggplot2::element_rect(fill = "white", color = NA),
                 axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
  ggplot2::geom_hline(yintercept = 150, color = "#d55e00", linetype = "dashed", linewidth = 0.9) +
  ggplot2::annotate("text",
                    x = max(quarter_avg_df$Quarter_start, na.rm = TRUE), y = 75,
                    label = "WHO target: 75 syringes",
                    hjust = 1.05, vjust = -0.6, color = "#d55e00", size = 3.5)

# Show in viewer
p_yearly3
p_half3
p_quarter3
bbv_positivity_yearly
bbv_positivity_halfyear
bbv_positivity_quarter
avg_syringes_per_person_yearly
avg_syringes_per_person_halfyear
avg_syringes_per_person_quarter

# New service usage figures (split)
p_yearly_unique
p_yearly_visits_total
p_half_unique
p_half_visits_total
p_quarter_unique
p_quarter_visits_total
p_yearly_syringe_usage
p_half_syringe_usage
p_quarter_syringe_usage

# Save all figures
if (!dir.exists("figures")) dir.create("figures")
ggplot2::ggsave("figures/yearly_syringes_recovered.png", plot = p_yearly3, width = 14, height = 5.5, units = "in", dpi = 300, bg = "white")
ggplot2::ggsave("figures/yearly_syringes_recovered.pdf", plot = p_yearly3, width = 14, height = 5.5, units = "in", bg = "white")
ggplot2::ggsave("figures/halfyear_syringes_recovered.png", plot = p_half3, width = 14, height = 5.5, units = "in", dpi = 300, bg = "white")
ggplot2::ggsave("figures/halfyear_syringes_recovered.pdf", plot = p_half3, width = 14, height = 5.5, units = "in", bg = "white")
ggplot2::ggsave("figures/quarterly_syringes_recovered.png", plot = p_quarter3, width = 14, height = 5.5, units = "in", dpi = 300, bg = "white")
ggplot2::ggsave("figures/quarterly_syringes_recovered.pdf", plot = p_quarter3, width = 14, height = 5.5, units = "in", bg = "white")
ggplot2::ggsave("figures/bbv_positivity_yearly.png", plot = bbv_positivity_yearly, width = 14, height = 5.5, units = "in", dpi = 300, bg = "white")
ggplot2::ggsave("figures/bbv_positivity_yearly.pdf", plot = bbv_positivity_yearly, width = 14, height = 5.5, units = "in", bg = "white")
ggplot2::ggsave("figures/bbv_positivity_halfyear.png", plot = bbv_positivity_halfyear, width = 14, height = 5.5, units = "in", dpi = 300, bg = "white")
ggplot2::ggsave("figures/bbv_positivity_halfyear.pdf", plot = bbv_positivity_halfyear, width = 14, height = 5.5, units = "in", bg = "white")
ggplot2::ggsave("figures/bbv_positivity_quarterly.png", plot = bbv_positivity_quarter, width = 14, height = 5.5, units = "in", dpi = 300, bg = "white")
ggplot2::ggsave("figures/bbv_positivity_quarterly.pdf", plot = bbv_positivity_quarter, width = 14, height = 5.5, units = "in", bg = "white")
ggplot2::ggsave("figures/avg_syringes_per_person_yearly.png", plot = avg_syringes_per_person_yearly, width = 14, height = 5.5, units = "in", dpi = 300, bg = "white")
ggplot2::ggsave("figures/avg_syringes_per_person_yearly.pdf", plot = avg_syringes_per_person_yearly, width = 14, height = 5.5, units = "in", bg = "white")
ggplot2::ggsave("figures/avg_syringes_per_person_halfyear.png", plot = avg_syringes_per_person_halfyear, width = 14, height = 5.5, units = "in", dpi = 300, bg = "white")
ggplot2::ggsave("figures/avg_syringes_per_person_halfyear.pdf", plot = avg_syringes_per_person_halfyear, width = 14, height = 5.5, units = "in", bg = "white")
ggplot2::ggsave("figures/avg_syringes_per_person_quarterly.png", plot = avg_syringes_per_person_quarter, width = 14, height = 5.5, units = "in", dpi = 300, bg = "white")
ggplot2::ggsave("figures/avg_syringes_per_person_quarterly.pdf", plot = avg_syringes_per_person_quarter, width = 14, height = 5.5, units = "in", bg = "white")
ggplot2::ggsave("figures/service_usage_yearly_unique.png", plot = p_yearly_unique, width = 14, height = 5.5, units = "in", dpi = 300, bg = "white")
ggplot2::ggsave("figures/service_usage_yearly_unique.pdf", plot = p_yearly_unique, width = 14, height = 5.5, units = "in", bg = "white")
ggplot2::ggsave("figures/service_usage_yearly_visits.png", plot = p_yearly_visits_total, width = 14, height = 5.5, units = "in", dpi = 300, bg = "white")
ggplot2::ggsave("figures/service_usage_yearly_visits.pdf", plot = p_yearly_visits_total, width = 14, height = 5.5, units = "in", bg = "white")
ggplot2::ggsave("figures/service_usage_halfyear_unique.png", plot = p_half_unique, width = 14, height = 5.5, units = "in", dpi = 300, bg = "white")
ggplot2::ggsave("figures/service_usage_halfyear_unique.pdf", plot = p_half_unique, width = 14, height = 5.5, units = "in", bg = "white")
ggplot2::ggsave("figures/service_usage_halfyear_visits.png", plot = p_half_visits_total, width = 14, height = 5.5, units = "in", dpi = 300, bg = "white")
ggplot2::ggsave("figures/service_usage_halfyear_visits.pdf", plot = p_half_visits_total, width = 14, height = 5.5, units = "in", bg = "white")
ggplot2::ggsave("figures/service_usage_quarter_unique.png", plot = p_quarter_unique, width = 14, height = 5.5, units = "in", dpi = 300, bg = "white")
ggplot2::ggsave("figures/service_usage_quarter_unique.pdf", plot = p_quarter_unique, width = 14, height = 5.5, units = "in", bg = "white")
ggplot2::ggsave("figures/service_usage_quarter_visits.png", plot = p_quarter_visits_total, width = 14, height = 5.5, units = "in", dpi = 300, bg = "white")
ggplot2::ggsave("figures/service_usage_quarter_visits.pdf", plot = p_quarter_visits_total, width = 14, height = 5.5, units = "in", bg = "white")
ggplot2::ggsave("figures/service_usage_syringes_yearly.png", plot = p_yearly_syringe_usage, width = 14, height = 5.5, units = "in", dpi = 300, bg = "white")
ggplot2::ggsave("figures/service_usage_syringes_yearly.pdf", plot = p_yearly_syringe_usage, width = 14, height = 5.5, units = "in", bg = "white")
ggplot2::ggsave("figures/service_usage_syringes_halfyear.png", plot = p_half_syringe_usage, width = 14, height = 5.5, units = "in", dpi = 300, bg = "white")
ggplot2::ggsave("figures/service_usage_syringes_halfyear.pdf", plot = p_half_syringe_usage, width = 14, height = 5.5, units = "in", bg = "white")
ggplot2::ggsave("figures/service_usage_syringes_quarterly.png", plot = p_quarter_syringe_usage, width = 14, height = 5.5, units = "in", dpi = 300, bg = "white")
ggplot2::ggsave("figures/service_usage_syringes_quarterly.pdf", plot = p_quarter_syringe_usage, width = 14, height = 5.5, units = "in", bg = "white")


