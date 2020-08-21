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
months <- paste0("2020-0", 1:6)
for (m in months) {
  infile <- paste0("safegraph-patterns/output/patterns-vbd-", m, ".csv")
  outfile <- paste0("data/patterns-vbd-", m, ".csv")
  drive_download(infile, outfile, overwrite = T)
  
  infile <- paste0("safegraph-patterns/output/hp-", m, ".csv")
  outfile <- paste0("data/hp-", m, ".csv")
  drive_download(infile, outfile, overwrite = T)
}

## Function
jsonchr_to_longdf <- function (jsonchr) {
  tbl <- jsonchr %>%
    fromJSON() %>%
    as_tibble
  if (nrow(tbl) == 0) return(data.frame())
  pivot_longer(tbl, everything(), names_to = 'cbg', values_to = 'freq')
}

## CBG shape
cbg <- st_read("cenacs_2018.shp") %>%
  mutate(GEOID10 = as.character(GEOID10))
cbg <- st_set_geometry(cbg, NULL)

## Run
plan(multisession)

for (m in months) {
  print(m)
  infile <- paste0("data/patterns-", m, ".csv")
  dat <- fread(infile)
  dat <- dat %>%
    mutate(visitor_home_cbgs = str_replace_all(visitor_home_cbgs, '""', '"'))
  
  home_cbg_freq <- dat$visitor_home_cbgs %>%
    future_map_dfr(~ jsonchr_to_longdf(.x), .id = "id", .progress = T)
  home_cbg_freq <- home_cbg_freq %>%
    filter(str_starts(cbg, "12"))
  home_cbg_freq$id <- as.numeric(home_cbg_freq$id)
  home_cbg_freq$safegraph_place_id <- dat$safegraph_place_id[home_cbg_freq$id]
  
  ##
  
  infile <- paste0("data/patterns-vbd-", m, ".csv")
  dat <- fread(infile)
  infile <- paste0("data/hp-", m, ".csv")
  hp <- fread(infile)
  hp$census_block_group <- as.character(hp$census_block_group)
  
  vbd <- str_remove_all(dat$visits_by_day, '[\\[\\]]') %>% str_split(",") %>% purrr::map(as.numeric)
  vbd <- do.call("rbind", vbd)
  vbd <- apply(vbd, 1, function (x) x / sum(x)) %>% t
  
  home_cbg_freq <- home_cbg_freq %>%
    left_join(dat %>% select(safegraph_place_id, raw_visit_counts, raw_visitor_counts))
  home_cbg_freq <- home_cbg_freq %>%
    left_join(hp %>% select(cbg = census_block_group, number_devices_residing)) %>%
    left_join(cbg %>% select(GEOID10, TOTALPOP),
              by = c("cbg" = "GEOID10"))
  home_cbg_freq1 <- home_cbg_freq %>%
    filter(!is.na(TOTALPOP))
  
  home_cbg_freq1$visit_ratio <- with(home_cbg_freq1, raw_visit_counts / raw_visitor_counts)
  home_cbg_freq1$scale_factor <- with(home_cbg_freq1, freq / raw_visitor_counts)
  
  home_cbg_freq1 <- home_cbg_freq1 %>%
    mutate(actual_visitor = TOTALPOP * scale_factor,
           actual_visit = actual_visitor * visit_ratio)
  poi_visits <- home_cbg_freq1 %>%
    group_by(safegraph_place_id) %>%
    summarise(actual_visit = sum(actual_visit))
  poi_visits <- dat %>%
    select(safegraph_place_id) %>%
    left_join(poi_visits)
  daily_visits <- colSums(poi_visits$actual_visit * vbd, na.rm = T)
  date <- paste0(m, "-", str_pad(1:length(daily_visits), width = 2, side = "left", pad = "0"))
  out_df <- data.frame(date = date, total_visits = daily_visits)

  outfile <- paste0("output/fl-visits-", m, ".csv")
  fwrite(out_df, outfile)
}

for (m in months) {
  infile <- paste0("safegraph-patterns/", m, "/normalization_stats.csv")
  outfile <- paste0("data/norm-", m, ".csv")
  drive_download(infile, outfile, overwrite = T)
}

df <- data.frame()
for (m in months) {
  infile <- paste0("output/fl-visits-", m, ".csv")
  df1 <- fread(infile)
  df1$date <- as.Date(df1$date)
  
  infile <- paste0("data/norm-", m, ".csv")
  norm <- fread(infile) %>%
    mutate(date = paste(year, month, day, sep="-") %>% ymd()) %>%
    arrange(date)
  
  df1$total_devices_seen <- norm$total_devices_seen
  
  df <- rbind(df, df1)
}

mod <- lm(df$total_visits ~ df$total_devices_seen)
summary(mod)
plot(df$total_devices_seen, df$total_visits)
df$resid <- resid(mod)

df$ma7 <- stats::filter(df$resid, rep(1/7, 7), sides = 1)
plot(df$date, df$resid, type = "l", axes=F)
axis(1, at = ymd(c("2020-01-01", "2020-02-01", "2020-03-01", "2020-04-01", "2020-05-01", "2020-06-01", "2020-07-01")),
     labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul"))

plot(df$date, df$total_devices_seen, type = "l", axes=F)
qplot(df$date, df$total_visits, geom = "smooth")
qplot(df$date, df$total_visits, geom = "line")
