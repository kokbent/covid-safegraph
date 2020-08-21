library(dplyr)
library(data.table)
library(ggplot2)
library(sf)
library(lubridate)
library(tidyr)
library(googledrive)

drive_auth(use_oob = T, email = "kokbent@ufl.edu")
drive_download("sdm-az-consolidated-2020-08-06-reduced.csv", "data/sdm-az-consolidated.csv",
               overwrite = T)
drive_download("az-cbg-pop.csv", "data/az-cbg-pop.csv",
               overwrite = T)

dat <- fread("data/sdm-ga-consolidated.csv")
dat$origin_census_block_group <- as.character(dat$origin_census_block_group)
dat$date <- as.Date(dat$date)

cbg1 <- fread("data/ga-cbg-pop.csv")
cbg1$census_block_group <- as.character(cbg1$census_block_group)

dat <- dat %>%
  left_join(cbg1, by = c("origin_census_block_group" = "census_block_group"))
dat <- dat %>% rename(TOTALPOP = B01001e1)

dat2 <- dat %>%
  # filter(origin_census_block_group %in% tmp1$origin_census_block_group) %>%
  group_by(date) %>%
  summarise(perc_at_home = sum(TOTALPOP * median_percentage_time_home)/sum(TOTALPOP),
            perc_non_home = 100 - perc_at_home,
            dist_trav = sum(TOTALPOP * distance_traveled_from_home / 1000) / sum(TOTALPOP),
            home_time = sum(TOTALPOP * median_home_dwell_time) / sum(TOTALPOP),
            non_home_time = sum(TOTALPOP * median_non_home_dwell_time) / sum(TOTALPOP),
            dev_count = sum(device_count))
dat2$perc_non_home_ma7 <- stats::filter(dat2$perc_non_home, rep(1/7, 7))
dat2$home_time_ma7 <- stats::filter(dat2$home_time, rep(1/7, 7))
dat2$non_home_time_ma7 <- stats::filter(dat2$non_home_time, rep(1/7, 7))

ggplot(dat2) +
  geom_line(aes(date, perc_non_home)) +
  theme_bw()

ggplot(dat2) +
  geom_line(aes(date, dist_trav)) +
  theme_bw()

p1 <- ggplot(dat2) +
  geom_line(aes(date, home_time)) +
  theme_bw() +
  labs(title = "Time at home")

p2 <- ggplot(dat2) +
  geom_line(aes(date, non_home_time)) +
  theme_bw() +
  labs(title = "Time not at home")

ggplot(dat2) +
  geom_line(aes(date, home_time_ma7)) +
  theme_bw() +
  labs(title = "Time at home")

ggplot(dat2) +
  geom_line(aes(date, non_home_time_ma7)) +
  theme_bw() +
  labs(title = "Time not at home")

#### Check Median non-home time
dat1 <- dat %>%
  select(date, origin_census_block_group, median_non_home_dwell_time, `21-45`:`>1440`)
dat1 <- dat1 %>%
  pivot_longer(-c(date:median_non_home_dwell_time),
               names_to = "bucket",
               values_to = "freq")
dat1$freq <- ifelse(is.na(dat1$freq), 0, dat1$freq)
dat1 <- dat1 %>%
  filter(bucket != ">1440")
buck <- unique(dat1$bucket) %>% sort
tmp <- unlist(stringr::str_split(buck, "[<>-]")) %>% matrix(ncol = 2, byrow = T)
df <- cbind(as.data.frame(buck, stringsAsFactors = F), as.data.frame(tmp, stringsAsFactors = F))
colnames(df) <- c("buck", "lo", "hi")
df$lo[1] <- "0"
df <- df %>% 
  mutate(lo = as.character(lo) %>% as.numeric, 
         hi = as.character(hi) %>% as.numeric) %>% 
  arrange(lo)
df$mid <- rowMeans(df[,c("lo", "hi")])

find_median <- function(v) {
  rep(df$mid, v) %>% median()
}

dat1$bucket <- factor(dat1$bucket, levels = df$buck)
dat1 <- dat1 %>%
  arrange(date, bucket)
dat1 <- dat1 %>%
  group_by(date, origin_census_block_group, median_non_home_dwell_time) %>%
  summarise(med_bucket = find_median(freq))
dat1 <- dat1 %>%
  left_join(cbg1, by = c("origin_census_block_group" = "census_block_group")) %>%
  rename(TOTALPOP = B01001e1)

dat2 <- dat1 %>%
  mutate(diff = median_non_home_dwell_time - med_bucket) %>%
  group_by(date) %>%
  summarise(diff = mean(diff))

ggplot(dat2) +
  geom_line(aes(date, diff)) +
  theme_bw()
