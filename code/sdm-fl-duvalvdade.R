library(dplyr)
library(data.table)
library(ggplot2)
library(sf)
library(lubridate)
library(tidyr)
library(stringr)
library(googledrive)

cbg <- st_read("cenacs_2018.shp")
cbg1 <- cbg %>%
  select(GEOID10, MED_AGE, TOTALPOP, DEN_POP)
cbg1 <- st_set_geometry(cbg1, NULL)

# drive_auth(use_oob = T)
# drive_download("sdm-consolidated-2020-08-09-reduced.csv", "data/sdm-consolidated.csv",
#                overwrite = T)

dat <- fread("data/sdm-consolidated.csv")
dat$origin_census_block_group <- as.character(dat$origin_census_block_group)
dat$date <- as.Date(dat$date)
dat <- dat %>%
  left_join(cbg1, by = c("origin_census_block_group" = "GEOID10"))
dat1 <- dat %>%
  filter(str_starts(origin_census_block_group, "12086") | str_starts(origin_census_block_group, "12031"))

dat1 <- dat1 %>%
  mutate(county = ifelse(str_starts(origin_census_block_group, "12086"),
                         "Dade", "Duval"))

dat2 <- dat1 %>%
  # filter(origin_census_block_group %in% tmp1$origin_census_block_group) %>%
  group_by(date, county) %>%
  summarise(perc_at_home = sum(TOTALPOP * median_percentage_time_home)/sum(TOTALPOP),
            perc_non_home = 100 - perc_at_home,
            dist_trav = sum(TOTALPOP * distance_traveled_from_home / 1000) / sum(TOTALPOP),
            home_time = sum(TOTALPOP * median_home_dwell_time) / sum(TOTALPOP),
            non_home_time = sum(TOTALPOP * median_non_home_dwell_time) / sum(TOTALPOP),
            dev_count = sum(device_count))

dat2 <- dat2 %>%
  group_by(county) %>%
  mutate(perc_non_home_ma7 = stats::filter(perc_non_home, rep(1/7, 7)))

ggplot(dat2) +
  geom_line(aes(date, perc_non_home, colour = county)) +
  theme_bw() +
  scale_x_date(date_breaks = "1 month", date_labels = "%b")

ggplot(dat2) +
  geom_line(aes(date, perc_non_home_ma7, colour = county)) +
  theme_bw() +
  scale_x_date(date_breaks = "1 month", date_labels = "%b")

fill_NA_w_neighbour <- function (v) {
  ind_na <- which(is.na(v))
  ind_obs <- which(!is.na(v))
  ind <- sapply(ind_na, function (x) which.min(abs(ind_obs-x)))
  ind <- ind_obs[ind]
  v[ind_na] <- v[ind]
  return(v)
}

df <- dat2 %>%
  select(date, county, prop_non_home = perc_non_home, 
         prop_non_home_ma7 = perc_non_home_ma7) %>%
  mutate(prop_non_home = prop_non_home / 100,
         prop_non_home_ma7 = fill_NA_w_neighbour(prop_non_home_ma7 / 100))
data.table::fwrite(df, "Dade_vs_Duval_mobility_full.csv")

df %>%
  filter(county == "Dade") %>%
  data.table::fwrite("Dade_mobility_full.csv")

drive_upload("Dade_mobility_full.csv", overwrite = T)

df %>%
  filter(county == "Duval") %>%
  data.table::fwrite("Duval_mobility_full.csv")

drive_upload("Duval_mobility_full.csv", overwrite = T)
