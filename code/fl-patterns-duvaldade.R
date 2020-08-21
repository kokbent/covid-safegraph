library(googledrive)
library(dplyr)
library(data.table)
library(ggplot2)
library(sf)
library(lubridate)
library(tidyr)
library(jsonlite)
library(stringr)
library(furrr)

drive_auth(
  use_oob = T
)
# drive_find(n = 30)
months <- c(paste0("2019-", str_pad(1:12, 2, pad = "0")), 
            paste0("2020-", str_pad(1:7, 2, pad = "0")))
for (m in months) {
  infile <- paste0("safegraph-patterns/output/patterns-vbd-", m, ".csv")
  outfile <- paste0("data/patterns-vbd-", m, ".csv")
  drive_download(infile, outfile, overwrite = T)
  
  infile <- paste0("safegraph-patterns/output/hp-", m, ".csv")
  outfile <- paste0("data/hp-", m, ".csv")
  drive_download(infile, outfile, overwrite = T)
}

raw_visits_df <- data.frame()
for (m in months) {
  infile <- paste0("data/patterns-vbd-", m, ".csv")
  dat <- fread(infile)
  
  df <- dat %>%
    filter(str_starts(poi_cbg, "12086") | str_starts(poi_cbg, "12031")) %>%
    mutate(county = ifelse(str_starts(poi_cbg, "12031"), "Duval", "Dade")) %>%
    group_by(county) %>%
    summarise(visit = sum(raw_visit_counts),
              n_poi = n()) %>%
    mutate(month = m)
  
  infile <- paste0("data/hp-", m, ".csv")
  hp <- fread(infile)
  hp1 <- hp %>%
    filter(str_starts(census_block_group, "12086") | str_starts(census_block_group, "12031")) %>%
    mutate(county = ifelse(str_starts(census_block_group, "12031"), "Duval", "Dade")) %>%
    group_by(county) %>%
    summarise(ndevice = sum(number_devices_residing))
  df <- df %>%
    left_join(hp1)
  
  raw_visits_df <- rbind(raw_visits_df, df)
}

raw_visits_df1 <- raw_visits_df %>%
  mutate(month = ymd(paste0(month, "-01"))) %>%
  group_by(county) %>%
  mutate(mean_ndev = mean(ndevice)) %>%
  mutate(adj_visit = visit * mean_ndev / ndevice,
         adj_visit_per_poi = adj_visit / n_poi)

ggplot(raw_visits_df1) +
  geom_line(aes(x = month, y = adj_visit_per_poi, colour = county)) +
  scale_x_date(date_breaks = "2 month", date_labels = "%b %y")

ggplot(raw_visits_df1) +
  geom_line(aes(x = month, y = visit/n_poi, colour = county)) +
  scale_x_date(date_breaks = "2 month", date_labels = "%b %y")

ggplot(raw_visits_df1) +
  geom_line(aes(x = month, y = visit/ndevice, colour = county)) +
  scale_x_date(date_breaks = "2 month", date_labels = "%b %y")
