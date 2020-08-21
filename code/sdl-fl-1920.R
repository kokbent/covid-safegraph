library(dplyr)
library(data.table)
library(ggplot2)
library(sf)
library(lubridate)
library(tidyr)
library(googledrive)

cbg <- st_read("cenacs_2018.shp")
cbg1 <- cbg %>%
  select(GEOID10, MED_AGE, TOTALPOP, DEN_POP)
cbg1 <- st_set_geometry(cbg1, NULL)

drive_auth(use_oob = T)
drive_download("sdm-consolidated-2020-08-02-reduced.csv", "data/sdm-consolidated.csv",
               overwrite = T)

dat <- fread("data/sdm-consolidated.csv")
dat$origin_census_block_group <- as.character(dat$origin_census_block_group)
dat$date <- as.Date(dat$date)

dt1 <- seq(ymd("2020-01-01"), ymd("2020-12-31"), by = 1)
dt2 <- seq(ymd("2019-01-01"), ymd("2019-12-31"), by = 1)

adj_dt1 <- dt1[epiweek(dt1) %in% 2:30]
adj_dt2 <- dt2[epiweek(dt2) %in% 2:30]

tmp <- data.frame(date = c(adj_dt1, adj_dt2), day = rep(1:length(adj_dt1), 2))
dat <- dat %>%
  left_join(tmp)
dat <- dat %>%
  left_join(cbg1, by = c("origin_census_block_group" = "GEOID10"))
dat <- dat %>%
  filter(!is.na(day)) %>%
  filter(DEN_POP <= 7)

dat$year <- year(dat$date)
dat$date_aligned <- adj_dt1[dat$day]

dat1 <- dat %>%
  group_by(year, date_aligned) %>%
  summarise(perc_at_home = sum(TOTALPOP * median_percentage_time_home)/sum(TOTALPOP),
            perc_non_home = 100 - perc_at_home)

#### Non-home time PLOT
ggplot(dat1) +
  geom_line(aes(date_aligned, perc_non_home, colour = as.factor(year))) +
  scale_x_date(date_breaks = "1 month",
               date_labels = "%b") +
  labs(title = "% time staying at home")

####
dat2 <- dat1 %>%
  select(year, date_aligned, perc_non_home) %>%
  pivot_wider(everything(),
              names_from = year,
              values_from = perc_non_home)
dat2 <- dat2 %>%
  mutate(ma7_2019 = stats::filter(`2019`, rep(1/7, 7), sides = 2),
         ma7_2020 = stats::filter(`2020`, rep(1/7, 7), sides = 2))
dat2$ratio <- dat2$`2020` / dat2$`2019`
dat2$ma7_ratio <- dat2$ma7_2020 / dat2$ma7_2019

ggplot(dat2) +
  geom_line(aes(date_aligned, ma7_ratio)) +
  geom_line(aes(date_aligned, ratio), lty = 2)

#### Establish baseline (using Jan-Feb)
baseline <- dat2 %>%
  filter(date_aligned <= ymd("2020-02-29")) %>%
  summarise(baseline = mean(ma7_ratio, na.rm = T)) %>%
  .$baseline

dat2$std_ma7_ratio <- dat2$ma7_ratio / baseline
ggplot(dat2) +
  geom_line(aes(date_aligned, std_ma7_ratio)) +
  geom_line(aes(date_aligned, ma7_ratio), lty = 2) +
  geom_hline(yintercept = 1.0, lty = 2) +
  scale_x_date(date_breaks = "1 month",
               date_labels = "%b")
