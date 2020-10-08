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
drive_download("sdm-consolidated-2020-10-02.csv", "data/sdm-consolidated.csv",
               overwrite = T)

#### Data Wrangling ----
dat <- fread("data/sdm-consolidated.csv")
dat <- dat %>%
  filter(date >= ymd("2020-01-01"))
dat$origin_census_block_group <- as.character(dat$origin_census_block_group)
dat$date <- as.Date(dat$date)
dat <- as.data.table(dat)

#### Distance travelled analysis ----
home_time <- cbind(dat[,.(date, origin_census_block_group)], dat[,`0-25`:`>100`])
df <- home_time %>%
  pivot_longer(cols = `0-25`:`>100`,
               names_to = "home_time", values_to = "freq")

df <- df %>%
  mutate(freq = ifelse(is.na(freq), 0, freq)) %>%
  group_by(date, origin_census_block_group) %>%
  mutate(prop = freq/sum(freq))

df1 <- df %>%
  filter(home_time %in% c("76-100", ">100")) %>%
  group_by(date, origin_census_block_group) %>%
  summarise(long_stay = sum(prop))

df2 <- df1 %>%
  left_join(cbg1, by = c("origin_census_block_group" = "GEOID10")) %>%
  group_by(date, age_grp) %>%
  summarise(long_stay = sum(n * long_stay) / sum(n)) %>%
  mutate(week = epiweek(date)) %>%
  group_by(week, age_grp) %>%
  summarise(long_stay = mean(long_stay))

df2$week_date <- ymd("2020-01-01")
week(df2$week_date) <- df2$week
wday(df2$week_date) <- 6

p2.1 <- df2 %>%
  filter(week %in% 2:39) %>%
  ggplot() +
  geom_line(aes(x=week_date, y=1-long_stay, colour=age_grp), lwd = 1.1) +
  scale_colour_manual(values = c("#018571", "#80cdc1", "#dfc27d", "#a6611a"),
                      name = "Age group") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  theme_minimal() +
  labs(title = "2020 Mobility trend of Florida (fraction staying at home <= 75%)",
       x = "", y = "") +
  theme(legend.position = c(0.75, 0.85),
        legend.background = element_rect(fill="white",
                                         size=0.5, linetype="solid", 
                                         colour ="black")) +
  guides(colour=guide_legend(nrow=2))

df3 <- df2 %>%
  group_by(week) %>%
  mutate(rel_activity = (1 - long_stay) - (1 - long_stay[age_grp == "65 and above"]))

p2.2 <- df3 %>%
  filter(week %in% 2:39) %>%
  ggplot() +
  geom_line(aes(x=week_date, y=rel_activity, colour=age_grp), lwd = 1.1, show.legend = F) +
  scale_colour_manual(values = c("#018571", "#80cdc1", "#dfc27d", "#a6611a"),
                      name = "Age group") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  theme_minimal() +
  labs(title = "2020 Relative activity among age groups of Florida",
       x = "", y = "") +
  theme(legend.position = c(0.85, 0.85))

#### Dade + Broward + PB
df4 <- df1 %>%
  filter(str_starts(origin_census_block_group, "12086") |
           str_starts(origin_census_block_group, "12011") |
           str_starts(origin_census_block_group, "12099")) %>%
  left_join(cbg1, by = c("origin_census_block_group" = "GEOID10")) %>%
  group_by(date, age_grp) %>%
  summarise(long_stay = sum(n * long_stay) / sum(n)) %>%
  mutate(week = epiweek(date)) %>%
  group_by(week, age_grp) %>%
  summarise(long_stay = mean(long_stay))

df4$week_date <- ymd("2020-01-01")
week(df4$week_date) <- df4$week
wday(df4$week_date) <- 6

df4 %>%
  filter(week %in% 2:39) %>%
  ggplot() +
  geom_line(aes(x=week_date, y=1-long_stay, colour=age_grp), lwd=1.1) +
  scale_colour_manual(values = c("#018571", "#80cdc1", "#dfc27d", "#a6611a"),
                      name = "Age group") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  theme_minimal() +
  labs(title = "Mobility trend of Dade, Broward and Palm Beach (fraction staying at home <= 75%)",
       x = "", y = "") +
  theme(legend.position = c(0.85, 0.85))

df5 <- df4 %>%
  group_by(week, week_date) %>%
  mutate(rel_activity = (1 - long_stay) - (1 - long_stay[age_grp == "65 and above"]))

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
