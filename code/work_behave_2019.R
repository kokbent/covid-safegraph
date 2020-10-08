library(dplyr)
library(data.table)
library(ggplot2)
library(sf)
library(lubridate)
library(tidyr)
library(stringr)
library(googledrive)

#### Functions ----
fill_NA_w_neighbour <- function (v) {
  ind_na <- which(is.na(v))
  ind_obs <- which(!is.na(v))
  ind <- sapply(ind_na, function (x) which.min(abs(ind_obs-x)))
  ind <- ind_obs[ind]
  v[ind_na] <- v[ind]
  return(v)
}

#### CBG Data ----
cbg <- st_read("shp/cenacs_2018.shp")
cbg1 <- get_agegrp(cbg)

#### Safegraph SDM data ----
drive_auth(email = "kokbent@ufl.edu", use_oob = T)
drive_download("sdm-consolidated-2019.csv", "data/sdm-consolidated-2019.csv",
               overwrite = T)

#### Data Wrangling ----
dat <- fread("data/sdm-consolidated-2019.csv")
dat$origin_census_block_group <- as.character(dat$origin_census_block_group)
dat$date <- as.Date(dat$date)
dat <- as.data.table(dat)

#### Distance travelled analysis ----
work_prop <- cbind(dat[,.(date, origin_census_block_group)], prop = dat[,work_prop])
df <- work_prop
df1 <- df %>%
  rename(work_prop = prop)
df2 <- df1 %>%
  left_join(cbg1, by = c("origin_census_block_group" = "GEOID10")) %>%
  group_by(date, age_grp) %>%
  summarise(work_prop = sum(n * work_prop) / sum(n)) %>%
  mutate(week = epiweek(date)) %>%
  group_by(week, age_grp) %>%
  summarise(work_prop = mean(work_prop))

df2$week_date <- ymd("2019-01-01")
week(df2$week_date) <- df2$week
wday(df2$week_date) <- 6

p1.3 <- df2 %>%
  filter(week %in% 2:39) %>%
  ggplot() +
  geom_line(aes(x=week_date, y=work_prop, colour=age_grp), lwd = 1.1, show.legend = F) +
  scale_colour_manual(values = c("#018571", "#80cdc1", "#dfc27d", "#a6611a"),
                      name = "Age group") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  theme_minimal() +
  labs(title = "2019 Fraction work behaviour of Florida",
       x = "", y = "") +
  theme(legend.position = c(0.85, 0.85))

df3 <- df2 %>%
  group_by(week) %>%
  mutate(rel_activity = work_prop - work_prop[age_grp == "65 and above"])

p1.4 <- df3 %>%
  filter(week %in% 2:39) %>%
  ggplot() +
  geom_line(aes(x=week_date, y=rel_activity, colour=age_grp), lwd = 1.1, show.legend = F) +
  scale_colour_manual(values = c("#018571", "#80cdc1", "#dfc27d", "#a6611a"),
                      name = "Age group") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  theme_minimal() +
  labs(title = "2019 Relative activity among age groups of Florida",
       x = "", y = "") +
  theme(legend.position = c(0.85, 0.85))

#### Dade + Broward + PB
df4 <- df1 %>%
  filter(str_starts(origin_census_block_group, "12086") |
           str_starts(origin_census_block_group, "12011") |
           str_starts(origin_census_block_group, "12099")) %>%
  left_join(cbg1, by = c("origin_census_block_group" = "GEOID10")) %>%
  group_by(date, age_grp) %>%
  summarise(work_prop = sum(n * work_prop) / sum(n)) %>%
  mutate(week = epiweek(date)) %>%
  group_by(week, age_grp) %>%
  summarise(work_prop = mean(work_prop))

df4$week_date <- ymd("2020-01-01")
week(df4$week_date) <- df4$week
wday(df4$week_date) <- 6

df4 %>%
  filter(week %in% 2:39) %>%
  ggplot() +
  geom_line(aes(x=week_date, y=work_prop, colour=age_grp), lwd=1.1) +
  scale_colour_manual(values = c("#018571", "#80cdc1", "#dfc27d", "#a6611a"),
                      name = "Age group") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  theme_minimal() +
  labs(title = "Fraction out-of-state travel of Dade, Broward and Palm Beach",
       x = "", y = "") +
  theme(legend.position = c(0.85, 0.85))

df5 <- df4 %>%
  group_by(week, week_date) %>%
  mutate(rel_activity = work_prop - work_prop[age_grp == "65 and above"])

df5 %>%
  filter(week %in% 2:39) %>%
  ggplot() +
  geom_line(aes(x=week_date, y=rel_activity, colour=age_grp), lwd = 1.1) +
  scale_colour_manual(values = c("#018571", "#80cdc1", "#dfc27d", "#a6611a"),
                      name = "Age group") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  theme_minimal() +
  labs(title = "Relative activity among age groups of Dade, Broward and Palm Beach",
       x = "", y = "") +
  theme(legend.position = c(0.85, 0.85))
