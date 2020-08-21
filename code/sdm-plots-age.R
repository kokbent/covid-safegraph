library(dplyr)
library(data.table)
library(ggplot2)
library(sf)
library(lubridate)
library(tidyr)

cbg <- st_read("cenacs_2018.shp")
cbg1 <- cbg %>%
  select(GEOID10, MED_AGE)
cbg1 <- st_set_geometry(cbg1, NULL)

dat <- fread("sdm-consolidated-2020-07-26.csv")
dat$origin_census_block_group <- as.character(dat$origin_census_block_group)
dat$date <- as.Date(dat$date)

dt1 <- seq(ymd("2020-01-01"), ymd("2020-07-20"), by = 1)
dt2 <- seq(ymd("2019-01-01"), ymd("2019-07-20"), by = 1)

adj_dt1 <- dt1[epiweek(dt1) %in% 2:29]
adj_dt2 <- dt2[epiweek(dt2) %in% 2:29]

tmp <- data.frame(date = c(adj_dt1, adj_dt2), day = rep(1:length(adj_dt1), 2))
dat <- dat %>%
  left_join(tmp)
dat <- dat %>%
  left_join(cbg1, by = c("origin_census_block_group" = "GEOID10"))
dat <- dat %>%
  filter(!is.na(day))

dat$year <- year(dat$date)
dat$Age_grp <- case_when(
  dat$MED_AGE < 25 ~ "< 25",
  dat$MED_AGE <= 44 ~ "25 - 44",
  dat$MED_AGE <= 64 ~ "45 - 64",
  T ~ "> 65",
)

dat$date_aligned <- adj_dt1[dat$day]

dat1 <- dat %>%
  group_by(year, date_aligned, Age_grp) %>%
  summarise(perc_at_home = mean(median_percentage_time_home))

#### SAH PLOT
ggplot(dat1) +
  geom_line(aes(date_aligned, perc_at_home, colour = Age_grp)) +
  facet_wrap(~ year, nrow = 2) +
  scale_x_date(date_breaks = "1 month",
               date_labels = "%b") +
  labs(title = "% time staying at home")


####
dat_dist <- dat %>%
  select(year, date_aligned, origin_census_block_group, Age_grp, `16001-50000`:`8001-16000`) %>%
  pivot_longer(-c(year:Age_grp),
               names_to = "dist_bin",
               values_to = "freq")

dat_dist <- dat_dist %>%
  group_by(year, date_aligned, Age_grp, dist_bin) %>%
  summarise(freq = sum(freq, na.rm = T)) %>%
  group_by(year, date_aligned, Age_grp) %>%
  mutate(prop = freq/sum(freq))

dat_longdist <- dat_dist %>%
  filter(dist_bin == '>50000')
dat_shortdist <- dat_dist %>%
  filter(dist_bin %in% c('0', '1-1000', '1001-2000')) %>%
  group_by(year, date_aligned, Age_grp) %>%
  summarise(prop = sum(prop))


#### Long dist PLOT
ggplot(dat_longdist) +
  geom_line(aes(date_aligned, prop, colour = Age_grp)) +
  facet_wrap(~ year, nrow = 2) +
  scale_x_date(date_breaks = "1 month",
               date_labels = "%b") +
  labs(title = "% > 50km")

#### Short dist PLOT
ggplot(dat_shortdist) +
  geom_line(aes(date_aligned, prop, colour = Age_grp)) +
  facet_wrap(~ year, nrow = 2) +
  scale_x_date(date_breaks = "1 month",
               date_labels = "%b") +
  labs(title = "% < 2km")


#### Out state PLOT
dat2 <- dat %>%
  group_by(year, date_aligned, Age_grp) %>%
  summarise(out_of_state = mean(destination_cbgs))

ggplot(dat2) +
  geom_line(aes(date_aligned, out_of_state, colour = Age_grp)) +
  facet_wrap(~ year, nrow = 2) +
  scale_x_date(date_breaks = "1 month",
               date_labels = "%b") +
  labs(title = "% out of state travel")
