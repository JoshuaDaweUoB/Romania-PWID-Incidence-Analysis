## load packages
pacman::p_load(dplyr, tidyr, withr, lubridate, MASS, writexl, readxl, arsenal, survival, broom, ggplot2, outbreaks, incidence)

## set wd
setwd("C:/Users/vl22683/OneDrive - University of Bristol/Documents/Publications/Romania PWID/code/incidence exercise")

## load data
dat <- ebola_sim$linelist$date_of_onset
class(dat)
View(dat)
head(dat)
i <- incidence(dat)
i
plot(i)

# weekly, starting on Monday (ISO week, default)
i.7 <- incidence(dat, interval = "1 week")
plot(i.7)

# semi-weekly, starting on Saturday
i.14 <- incidence(dat, interval = "2 saturday weeks")
plot(i.14, border = "white")

i.7.hosp <- with(ebola_sim_clean$linelist, 
     incidence(date_of_onset, interval = "week", groups = hospital))
i.7.hosp

View(ebola_sim_clean)

head(get_counts(i.7.hosp))
