## load packages
pacman::p_load(MASS, dplyr, tidyr, withr, lubridate, writexl, readxl, arsenal, survival, broom, ggplot2, purrr, tableone, zoo, scales)

## set wd
setwd("C:/Users/vl22683/OneDrive - University of Bristol/Documents/Publications/Romania PWID/data")

## load data
romania_pwid_raw <- readxl::read_excel("ARAS DATA IDU 2013-2022.xlsx")

# keep relevant variables
romania_pwid_ts <- romania_pwid_raw %>%
  select(
    id,
    appointment_dte,
    condoms_distributed,
    syringes_distributed_1ml,
    syringes_distributed_2ml,
    hcv_test_rslt,
    hiv_test_rslt
  )

## recode hiv/hcv test results to 0/1
romania_pwid_ts <- romania_pwid_ts %>%
  mutate(
    hiv_test_rslt = case_when(hiv_test_rslt == 1 ~ 0L, hiv_test_rslt == 2 ~ 1L, TRUE ~ as.integer(hiv_test_rslt)),
    hcv_test_rslt = case_when(hcv_test_rslt == 1 ~ 0L, hcv_test_rslt == 2 ~ 1L, TRUE ~ as.integer(hcv_test_rslt))
  )

# flag rows with any harm-reduction use
romania_pwid_ts <- romania_pwid_ts %>%
  mutate(
    hr_use_flag = if_else(
      rowSums(cbind(
        !is.na(syringes_distributed_1ml),
        !is.na(syringes_distributed_2ml),
        !is.na(hcv_test_rslt),
        !is.na(hiv_test_rslt)
      )) > 0,
      1L, 0L
    )
  ) %>%
  filter(hr_use_flag == 1L)

## generate time periods (Year + Half + Quarter)
romania_pwid_ts <- romania_pwid_ts %>%
  mutate(
    appointment_dte_chr = sub(" UTC$", "", trimws(as.character(appointment_dte))),
    appointment_dte = case_when(
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
  select(-appointment_dte_chr) %>%
  arrange(appointment_dte)

# drop rows with no usable date
romania_pwid_ts <- romania_pwid_ts %>% filter(!is.na(Year))

# ensure all halves and quarters exist from first to last year, in order
years_full <- seq(min(romania_pwid_ts$Year, na.rm = TRUE), max(romania_pwid_ts$Year, na.rm = TRUE))
half_levels <- as.vector(outer(years_full, c(" H1", " H2"), paste0))
romania_pwid_ts <- romania_pwid_ts %>% mutate(Half = factor(Half, levels = half_levels))
quarter_levels <- as.vector(outer(years_full, c(" Q1", " Q2", " Q3", " Q4"), paste0))
romania_pwid_ts <- romania_pwid_ts %>% mutate(Quarter = factor(Quarter, levels = quarter_levels))

## service usage table (all years)
service_df <- romania_pwid_ts %>% filter(Year <= 2022L)

summarise_service <- function(df) {
  df %>%
    summarise(
      individuals = n_distinct(id),
      visits = n(),
      syringes = sum(syringes_distributed_1ml, syringes_distributed_2ml, na.rm = TRUE)
    ) %>%
    mutate(syringes_per_person = syringes / individuals)
}

# overall counts each individual once across all years; yearly counts once per year
service_table <- bind_rows(
  service_df %>% summarise_service() %>% mutate(Year = "Overall"),
  service_df %>% group_by(Year = as.character(Year)) %>% summarise_service()
) %>%
  mutate(
    individuals = scales::comma(individuals),
    visits = scales::comma(visits),
    syringes = scales::comma(syringes),
    syringes_per_person = scales::comma(syringes_per_person, accuracy = 0.1)
  ) %>%
  tidyr::pivot_longer(-Year, names_to = "Measure") %>%
  tidyr::pivot_wider(names_from = Year, values_from = value) %>%
  mutate(Measure = recode(Measure,
    individuals = "Individuals accessing service",
    visits = "Total visits to service",
    syringes = "Syringes distributed by service",
    syringes_per_person = "Number of syringes per person accessing service"
  ))

service_table

# save table to data folder (working directory)
writexl::write_xlsx(service_table, "table3_service_trends.xlsx")
