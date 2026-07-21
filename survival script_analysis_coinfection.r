## load packages
pacman::p_load(dplyr, tidyr, withr, lubridate, MASS, writexl, readxl, arsenal, survival, broom, ggplot2, purrr, tableone, stringr)

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

print(names(romania_pwid_treatment))

# delete redunant column and add oat column
romania_pwid_treatment <- romania_pwid_treatment %>%
  dplyr::select(appointment_dte, id) %>%
  mutate(oat = 1)

# tab out number of oat rows
table(romania_pwid_treatment$oat, useNA="ifany") ## 1376 rows
n_distinct(romania_pwid_treatment$id) ## 1066 distinct

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
romania_pwid_coinfection_combined <- romania_pwid_raw[!is.na(romania_pwid_raw$hiv_test_rslt) | romania_pwid_raw$hcv_test_rslt | !is.na(romania_pwid_raw$oat), ]
write.csv(romania_pwid_coinfection_combined, "romania_pwid_coinfection_combined.csv")

# recode gender 
romania_pwid_coinfection <- romania_pwid_coinfection_combined %>%
  mutate(
    gender = case_when(
      gender == 2 ~ 0,
      TRUE ~ gender
    ),
    gender = factor(gender, levels = c(0, 1), labels = c("Female", "Male"))
  )
  
# age four categories
romania_pwid_coinfection <- romania_pwid_coinfection %>%
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

# one year prior to test
romania_pwid_coinfection <- romania_pwid_coinfection %>%
  mutate(
    hcv_test_dte = if_else(!is.na(hcv_test_rslt), as.Date(appointment_dte), as.Date(NA)),
    hcv_test_dte_12m_prev = hcv_test_dte - years(1),
    hiv_test_dte = if_else(!is.na(hiv_test_rslt), as.Date(appointment_dte), as.Date(NA)),
    hiv_test_dte_12m_prev = hiv_test_dte - years(1)
  )

# main drug injected
table(trimws(as.character(romania_pwid_coinfection$drug_type)), useNA = "ifany")

romania_pwid_coinfection <- romania_pwid_coinfection %>%
  mutate(
    drug_type_main = case_when(
      drug_type == "0" | is.na(drug_type) ~ "Undeclared",
      str_detect(drug_type, "\\+") | str_length(drug_type) > 1 & str_detect(drug_type, "[HLM]") ~ "Polyconsumer",
      drug_type == "1" | drug_type == "H" ~ "Heroin",
      drug_type == "2" | drug_type == "L" ~ "Legal",
      drug_type == "3" ~ "Polyconsumer",
      drug_type == "4" ~ "Other drugs",
      drug_type == "5" | drug_type == "M" ~ "Methadone",
      TRUE ~ "Other"
    )
  )

table(romania_pwid_coinfection$drug_type_main, romania_pwid_coinfection$drug_type, useNA = "ifany")

# sequence negative tests

# create row ids
romania_pwid_coinfection <- romania_pwid_coinfection %>%
  arrange(id, appointment_dte) %>%
  mutate(row_id = row_number())


# sequence of negative HCV tests
hcv_neg_test_seq <- romania_pwid_coinfection %>%
  filter(hcv_test_rslt == 1) %>%
  arrange(id, appointment_dte) %>%
  group_by(id) %>%
  mutate(neg_hcv_seq = row_number()) %>%
  ungroup() %>%
  dplyr::select(row_id, neg_hcv_seq)

# sequence of negative HIV tests
hiv_neg_test_seq <- romania_pwid_coinfection %>%
  filter(hiv_test_rslt == 1) %>%
  arrange(id, appointment_dte) %>%
  group_by(id) %>%
  mutate(neg_hiv_seq = row_number()) %>%
  ungroup() %>%
  dplyr::select(row_id, neg_hiv_seq)

# merge back using row_id
romania_pwid_coinfection <- romania_pwid_coinfection %>%
  left_join(hcv_neg_test_seq, by = "row_id") %>%
  left_join(hiv_neg_test_seq, by = "row_id")

# create last_hcv_test_dte / last_hiv_test_dte
romania_pwid_coinfection <- romania_pwid_coinfection %>%
  group_by(id) %>%
  mutate(
    last_hcv_test_dte = suppressWarnings(max(appointment_dte[!is.na(hcv_test_rslt)], na.rm = TRUE)),
    last_hcv_test_dte = replace(last_hcv_test_dte, is.infinite(last_hcv_test_dte), NA),
    last_hcv_test_dte = as.Date(last_hcv_test_dte, origin = "1970-01-01"),
    last_hiv_test_dte = suppressWarnings(max(appointment_dte[!is.na(hiv_test_rslt)], na.rm = TRUE)),
    last_hiv_test_dte = replace(last_hiv_test_dte, is.infinite(last_hiv_test_dte), NA),
    last_hiv_test_dte = as.Date(last_hiv_test_dte, origin = "1970-01-01")
  ) %>%
  ungroup()

# create a single variable for any recorded test
romania_pwid_coinfection <- romania_pwid_coinfection %>%
  mutate(
    appointment_dte = as.Date(substr(appointment_dte, 1, 10)),
    any_test_dte = if_else(
      !is.na(hcv_test_rslt) | !is.na(hiv_test_rslt),
      appointment_dte,
      as.Date(NA)
    )
  )

# first ever recorded test
first_test <- romania_pwid_coinfection %>%
  summarise(first_any_test_dte = min(any_test_dte, na.rm = TRUE))

print(first_test)

# last ever recorded test
last_test <- romania_pwid_coinfection %>%
  summarise(last_any_test_dte = max(any_test_dte, na.rm = TRUE))

print(last_test)

# lifetime exposure variables

# recode roma ethnicity
romania_pwid_coinfection <- romania_pwid_coinfection %>%
  group_by(id) %>%
  mutate(ethnic_roma_ever = ifelse(any(ethnic_roma == 1, na.rm = TRUE), 1, 0)) %>%
  ungroup() %>%
  mutate(ethnic_roma_ever = factor(ethnic_roma_ever, levels = c(0, 1)))

# recode hcv ever
romania_pwid_coinfection <- romania_pwid_coinfection %>%
  group_by(id) %>%
  mutate(hcv_ever = ifelse(any(hcv_test_rslt == 2, na.rm = TRUE), 1, 0)) %>%
  ungroup() %>%
  mutate(hcv_ever = factor(hcv_ever, levels = c(0, 1)))

# recode hiv ever
romania_pwid_coinfection <- romania_pwid_coinfection %>%
  group_by(id) %>%
  mutate(hiv_ever = ifelse(any(hiv_test_rslt == 2, na.rm = TRUE), 1, 0)) %>%
  ungroup() %>%
  mutate(hiv_ever = factor(hiv_ever, levels = c(0, 1)))

# recode oat
romania_pwid_coinfection <- romania_pwid_coinfection %>%
  group_by(id) %>%
  mutate(oat_ever = ifelse(any(oat == 1, na.rm = TRUE), 1, 0)) %>%
  ungroup() %>%
  mutate(oat_ever = factor(oat_ever, levels = c(0, 1)))

# recode sex work
romania_pwid_coinfection <- romania_pwid_coinfection %>%
  group_by(id) %>%
  mutate(sex_work_ever = ifelse(any(sex_work_current), 1, 0)) %>%
  ungroup() %>%
  mutate(sex_work_ever = factor(sex_work_ever, levels = c(0, 1)))

# recode homelessness
romania_pwid_coinfection <- romania_pwid_coinfection %>%
  group_by(id) %>%
  mutate(homeless_ever = ifelse(any(homeless_current), 1, 0)) %>%
  ungroup() %>%
  mutate(homeless_ever = factor(homeless_ever, levels = c(0, 1)))

# make vars factors
romania_pwid_coinfection <- romania_pwid_coinfection %>%
  mutate(
    oat_current = factor(oat_ever, levels = c(0, 1)),
    sex_work_current = factor(sex_work_current, levels = c(0, 1)),
    homeless_current = factor(homeless_current, levels = c(0, 1))
  )

# tab out changing lifetime vars
current_vars <- c("oat", "oat_ever", "sex_work_current", "sex_work_ever", "homeless_current", "homeless_ever")
table_current <- CreateTableOne(vars = current_vars, data = romania_pwid_coinfection)
print(table_current, showAllLevels = TRUE)

# tab out unchanging lifetime vars
vars <- c("ethnic_roma_ever", "hcv_ever", "hiv_ever")
table_roma_hcv_hiv <- CreateTableOne(vars = vars, data = romania_pwid_coinfection)
print(table_roma_hcv_hiv, showAllLevels = TRUE)

# date exposures occured
romania_pwid_coinfection <- romania_pwid_coinfection %>%
  mutate(
    oat_dte = dplyr::if_else(oat == 1, appointment_dte, as.Date(NA)),
    sex_work_current_dte = dplyr::if_else(sex_work_current == 1, appointment_dte, as.Date(NA)),
    homeless_current_dte = dplyr::if_else(homeless_current == 1, appointment_dte, as.Date(NA))
  )

# recode other values of _ever to 1 for ids with any current exposure
romania_pwid_coinfection <- romania_pwid_coinfection %>%
  group_by(id) %>%
  mutate(
    oat_ever = as.integer(any(oat == 1, na.rm = TRUE)),
    sex_work_ever = as.integer(any(sex_work_current == 1, na.rm = TRUE)),
    homeless_ever = as.integer(any(homeless_current == 1, na.rm = TRUE))
  ) %>%
  ungroup()

# force onto all rows
romania_pwid_coinfection <- romania_pwid_coinfection %>%
  group_by(id) %>%
  mutate(
    oat_ever = max(oat_ever, na.rm = TRUE),
    homeless_ever = max(homeless_ever, na.rm = TRUE),
    sex_work_ever = max(sex_work_ever, na.rm = TRUE)
  ) %>%
  ungroup()

# QA check
romania_pwid_coinfection %>%
  group_by(id) %>%
  summarise(
    oat_check = n_distinct(oat_ever),
    homeless_check = n_distinct(homeless_ever),
    hcv_check = n_distinct(hcv_ever),
    hiv_check = n_distinct(hiv_ever)
  ) %>%
  summarise(across(everything(), max))

# make vars factors for table
romania_pwid_coinfection <- romania_pwid_coinfection %>%
  mutate(
    oat_ever = factor(oat_ever, levels = c(0, 1)),
    sex_work_ever = factor(sex_work_ever, levels = c(0, 1)),
    homeless_ever = factor(homeless_ever, levels = c(0, 1)),
    ethnic_roma_ever = factor(ethnic_roma_ever, levels = c(0, 1)),
  )

current_vars <- c("oat_ever", "sex_work_ever", "homeless_ever")
table_current <- CreateTableOne(vars = current_vars, data = romania_pwid_coinfection)
print(table_current, showAllLevels = TRUE)

# distinct rows of oat
n_distinct(romania_pwid_coinfection$id[romania_pwid_coinfection$oat_ever == 1])

# find first exposure date
romania_pwid_coinfection <- romania_pwid_coinfection %>%
  group_by(id) %>%
  mutate(
    oat_first_exposure_dte = min(oat_dte, na.rm = TRUE),
    sex_work_current_first_exposure_dte = min(sex_work_current_dte, na.rm = TRUE),
    homeless_current_first_exposure_dte = min(homeless_current_dte, na.rm = TRUE)
  ) %>%
  ungroup()

# set _ever variables to 0 if not exposed
ever_vars <- c("ethnic_roma_ever", "oat_ever", "sex_work_ever", "homeless_ever")
romania_pwid_coinfection <- romania_pwid_coinfection %>%
  mutate(
    across(all_of(ever_vars),
      ~ factor(ifelse(. == 1, 1, 0),
        levels = c(0, 1)
        )))

# select and order vars
romania_pwid_coinfection <- romania_pwid_coinfection %>%
  dplyr::select(
    id, gender, age_4cat, oat_ever, sex_work_ever, homeless_ever, ethnic_roma_ever, drug_type_main, everything())

ever_vars <- c("gender", "age_4cat", "ethnic_roma_ever", "oat_ever", "sex_work_ever", "homeless_ever", "drug_type_main")
table_ever <- CreateTableOne(vars = ever_vars, data = romania_pwid_coinfection)
print(table_ever, showAllLevels = TRUE)

# distinct rows of oat
n_distinct(romania_pwid_coinfection$id[romania_pwid_coinfection$oat_ever == 1])

# find first negative HCV test date for each id
first_neg_hcv_dates <- romania_pwid_coinfection %>%
  filter(neg_hcv_seq == 1) %>%
  group_by(id) %>%
  summarise(first_hcv_neg_test_dte = min(appointment_dte, na.rm = TRUE), .groups = "drop")

# find first negative HIV test date for each id
first_neg_hiv_dates <- romania_pwid_coinfection %>%
  filter(neg_hiv_seq == 1) %>%
  group_by(id) %>%
  summarise(first_hiv_neg_test_dte = min(appointment_dte, na.rm = TRUE), .groups = "drop")

# relevel sex work variable
romania_pwid_coinfection <- romania_pwid_coinfection %>%
  mutate(
    sex_work_ever_4cat = factor(
      case_when(
      sex_work_ever == 0 & gender == "Female" ~ 0,
      sex_work_ever == 1 & gender == "Female" ~ 1, 
      sex_work_ever == 0 & gender == "Male" ~ 2,
      sex_work_ever == 1 & gender == "Male" ~ 3
    ),
    levels = c(0, 1, 2, 3), 
    labels = c("No sex work - female", "Sex work - female", "No sex work - male", "Sex work - male")
  ))

# save overall cohort

# id sequence
overall_data <- romania_pwid_coinfection %>%
  group_by(id) %>%
  arrange(id, appointment_dte) %>%
  mutate(id_seq = row_number()) %>%
  ungroup()

# restrict to one row per id
overall_data <- overall_data %>%
  filter(id_seq == 1)

# table of exposures
ever_vars <- c("gender", "age_4cat", "ethnic_roma_ever", "oat_ever", "sex_work_ever_4cat", "homeless_ever", "drug_type_main")
current_table <- CreateTableOne(
  vars = ever_vars,
  data = overall_data
)
print(current_table, showAllLevels = TRUE)

# distinct rows of oat
n_distinct(romania_pwid_coinfection$id[romania_pwid_coinfection$oat_ever == 1])

# restrict to columns of interest
overall_data <- overall_data %>%
  dplyr::select(id, gender, age_4cat, ethnic_roma_ever, sex_work_ever, sex_work_ever_4cat, homeless_ever, oat_ever, drug_type_main)

# generate a table
table_vars <- c("gender", "age_4cat", "ethnic_roma_ever", "oat_ever", "sex_work_ever_4cat", "homeless_ever", "drug_type_main")

overall_table <- CreateTableOne(
  vars = table_vars,
  data = overall_data
)
print(overall_table, showAllLevels = TRUE)

# save overall data
saveRDS(overall_data, file = "overall_data_coinfection.rds")

## baseline coinfection cohort

# remove rows where either hcv or hiv test result is missing
romania_pwid_coinfection <- romania_pwid_coinfection[
  !is.na(romania_pwid_coinfection$hcv_test_rslt) & !is.na(romania_pwid_coinfection$hiv_test_rslt),
]

# distinct rows of oat
n_distinct(romania_pwid_coinfection$id[romania_pwid_coinfection$oat_ever == 1])

# remove rows where hcv or hiv test result is indeterminate
romania_pwid_coinfection <- romania_pwid_coinfection %>%
  filter(!hcv_test_rslt == 3, !hiv_test_rslt == 3)

# sequence by id 
romania_pwid_coinfection <- romania_pwid_coinfection %>%
  group_by(id) %>%
  arrange(id, appointment_dte) %>%
  mutate(id_seq = row_number()) %>%
  ungroup(id)

# keep rows where id_seq equals 1
romania_pwid_coinfection_bl <- romania_pwid_coinfection %>%
  filter(id_seq == 1)

# generate a table
table_vars <- c("gender", "age_4cat", "ethnic_roma_ever", "sex_work_ever_4cat", "homeless_ever", "oat_ever", "drug_type_main")

overall_table <- CreateTableOne(
  vars = table_vars,
  data = romania_pwid_coinfection_bl
)
print(overall_table, showAllLevels = TRUE)

# distinct rows of oat
n_distinct(romania_pwid_coinfection_bl$id[romania_pwid_coinfection_bl$oat_ever == 1])

# hcv and hiv test results
table(romania_pwid_coinfection_bl$hcv_test_rslt)
table(romania_pwid_coinfection_bl$hiv_test_rslt)

# save df of all participants tested for both hcv and hiv
write.csv(romania_pwid_coinfection_bl, "romania_pwid_coinfection_bl.csv", row.names = FALSE)

## Table 3: HIV and HCV coinfection among PWID

# set reference levels for each exposure
baseline_coinfection <- romania_pwid_coinfection_bl %>%
  mutate(
    coinfection_status = case_when(
      hcv_test_rslt == 2 & hiv_test_rslt == 2 ~ "Coinfection",
      hcv_test_rslt == 2 & hiv_test_rslt == 1 ~ "HCV-only",
      hcv_test_rslt == 1 & hiv_test_rslt == 2 ~ "HIV-only",
      hcv_test_rslt == 1 & hiv_test_rslt == 1 ~ "Neither"
    ),
    gender = relevel(as.factor(gender), ref = "Female"),
    age_4cat = relevel(as.factor(age_4cat), ref = "<30"),
    ethnic_roma_ever = relevel(as.factor(ethnic_roma_ever), ref = "0"),
    sex_work_ever_4cat = relevel(as.factor(sex_work_ever_4cat), ref = "No sex work - female"),
    homeless_ever = relevel(as.factor(homeless_ever), ref = "0"),
    oat_ever = relevel(as.factor(oat_ever), ref = "0"),
    drug_type_main = relevel(as.factor(drug_type_main), ref = "Heroin")
  )

exposures <- c("gender", "age_4cat", "ethnic_roma_ever", "sex_work_ever_4cat", "homeless_ever", "oat_ever", "drug_type_main")

characteristic_labels <- c(
  gender = "Sex",
  age_4cat = "Age group",
  ethnic_roma_ever = "Roma ethnicity",
  sex_work_ever_4cat = "Reported sex work",
  homeless_ever = "Reported homelessness",
  oat_ever = "Received OAMT",
  drug_type_main = "Main drug used"
)

level_labels <- list(
  gender = c(Female = "Female", Male = "Male"),
  age_4cat = c(`<30` = "<30 years", `30-39` = "30\u201339 years", `40-49` = "40\u201349 years", `50+` = "50+ years"),
  ethnic_roma_ever = c(`0` = "No", `1` = "Yes"),
  sex_work_ever_4cat = c(
    `No sex work - female` = "Women, no sex work",
    `Sex work - female` = "Women, sex work",
    `No sex work - male` = "Men, no sex work",
    `Sex work - male` = "Men, sex work"
  ),
  homeless_ever = c(`0` = "No", `1` = "Yes"),
  oat_ever = c(`0` = "No", `1` = "Yes"),
  drug_type_main = c(
    Heroin = "Heroin",
    Legal = "Legal drugs",
    Methadone = "Methadone",
    `Other drugs` = "Other drugs",
    Polyconsumer = "Polydrug use",
    Undeclared = "Undeclared"
  )
)

group_levels <- c("Neither", "HIV-only", "HCV-only", "Coinfection")

# N (%): column percentage within the given infection group
n_pct <- function(data, var, level, group) {
  grp_data <- data %>% filter(coinfection_status == group)
  n_total <- nrow(grp_data)
  n_level <- sum(as.character(grp_data[[var]]) == level, na.rm = TRUE)
  pct <- if (n_total > 0) 100 * n_level / n_total else NA
  sprintf("%d (%.1f)", n_level, pct)
}

# unadjusted PR for a level, comparing infection group vs "Neither"
pr_level <- function(data, var, level, ref_level, group) {
  if (level == ref_level) return("ref.")
  model_data <- data %>% filter(coinfection_status %in% c("Neither", group))
  model_data$outcome_bin <- ifelse(model_data$coinfection_status == group, 1, 0)
  model <- tryCatch(
    glm(
      as.formula(paste0("outcome_bin ~ ", var)),
      data = model_data,
      family = poisson(link = "log"),
      control = glm.control(maxit = 100)
    ),
    error = function(e) NULL
  )
  if (is.null(model)) return("NE")
  tidy_model <- broom::tidy(model, exponentiate = TRUE, conf.int = TRUE)
  row <- tidy_model %>% filter(term == paste0(var, level))
  if (nrow(row) == 0) return(NA_character_)
  sprintf("%.2f (%.2f-%.2f)", row$estimate, row$conf.low, row$conf.high)
}

table_rows <- list()

# total row
total_row <- c(Characteristic = "Total")
for (group in group_levels) {
  n_group <- sum(baseline_coinfection$coinfection_status == group)
  pct_group <- 100 * n_group / nrow(baseline_coinfection)
  total_row[[paste0(group, "_n_pct")]] <- sprintf("%d (%.1f)", n_group, pct_group)
  total_row[[paste0(group, "_pr")]] <- ""
}
table_rows[["Total"]] <- total_row

# characteristic + level rows
for (var in exposures) {

  header_row <- c(Characteristic = characteristic_labels[[var]])
  for (group in group_levels) {
    header_row[[paste0(group, "_n_pct")]] <- ""
    header_row[[paste0(group, "_pr")]] <- ""
  }
  table_rows[[paste0(var, "_header")]] <- header_row

  levels_this_var <- names(level_labels[[var]])
  ref_level <- levels_this_var[1]

  for (level in levels_this_var) {
    level_label <- level_labels[[var]][[level]]
    row <- c(Characteristic = paste0("  ", level_label))
    for (group in group_levels) {
      row[[paste0(group, "_n_pct")]] <- n_pct(baseline_coinfection, var, level, group)
      row[[paste0(group, "_pr")]] <- if (group == "Neither") "" else pr_level(baseline_coinfection, var, level, ref_level, group)
    }
    table_rows[[paste0(var, "_", level)]] <- row
  }
}

table3 <- bind_rows(table_rows)

colnames(table3) <- c(
  "Characteristic",
  "No infection N (%)", "No infection PR (95% CI)",
  "HIV mono-infected N (%)", "HIV mono-infected PR (95% CI)",
  "HCV mono-infected N (%)", "HCV mono-infected PR (95% CI)",
  "HIV and HCV co-infected N (%)", "HIV and HCV co-infected PR (95% CI)"
)

# drop the empty PR column for the reference (no infection) group
table3 <- table3 %>% dplyr::select(-`No infection PR (95% CI)`)

write.csv(table3, "table3_coinfection.csv", row.names = FALSE)
