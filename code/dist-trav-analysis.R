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
cbg1 <- cbg %>%
  select(GEOID10, COUNTYFP10, TOTALPOP, MED_AGE, DEN_POP) %>%
  st_set_geometry(NULL)

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
dat <- dat %>%
  left_join(cbg1, by = c("origin_census_block_group" = "GEOID10")) %>%
  as.data.table()

#### Distance travelled analysis ----
dist_trav <- cbind(dat[,.(date, origin_census_block_group, MED_AGE, TOTALPOP)], dat[,`16001-50000`:`8001-16000`])
df <- dist_trav %>%
  pivot_longer(cols = `16001-50000`:`8001-16000`,
               names_to = "dist_trav", values_to = "freq")

df <- df %>%
  mutate(freq = ifelse(is.na(freq), 0, freq)) %>%
  group_by(date, origin_census_block_group) %>%
  mutate(prop = freq/sum(freq))

df1 <- df %>%
  filter(dist_trav %in% c("0", "1-1000", "1001-2000", "2001-8000")) %>%
  group_by(date, origin_census_block_group, MED_AGE, TOTALPOP) %>%
  summarise(short_trav = sum(prop))

df2 <- df1 %>%
  ungroup() %>%
  mutate(age_grp = case_when(
    MED_AGE <= 35.7 ~ 1,
    MED_AGE <= 42.7 ~ 2,
    MED_AGE <= 51.6 ~ 3,
    T               ~ 4
  )) %>%
  group_by(date, age_grp) %>%
  summarise(short_trav = sum(TOTALPOP * short_trav)/sum(TOTALPOP)) %>%
  ungroup() %>%
  mutate(week = epiweek(date)) %>%
  group_by(week, age_grp) %>%
  summarise(short_trav = mean(short_trav))

df2 %>%
  filter(week %in% 2:39) %>%
  ggplot() +
  geom_line(aes(x=week, y=short_trav, colour=as.factor(age_grp)))
