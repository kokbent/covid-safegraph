rm(list=ls())
gc()

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
months <- paste0("2020-0", 7)
for (m in months) {
  infile <- paste0("safegraph-patterns/output/patterns-", m, ".csv")
  outfile <- paste0("data/patterns-", m, ".csv")
  drive_download(infile, outfile, overwrite = T)
  
  infile <- paste0("safegraph-patterns/output/core-", m, ".csv")
  outfile <- paste0("data/core-", m, ".csv")
  drive_download(infile, outfile, overwrite = T)
  
  infile <- paste0("safegraph-patterns/output/hp-", m, ".csv")
  outfile <- paste0("data/hp-", m, ".csv")
  drive_download(infile, outfile, overwrite = T)
}

patterns <- fread("data/patterns-2020-07.csv")
patterns

core <- fread("data/core-2020-07.csv")
core

hp <- fread("data/hp-2020-07.csv", 
            colClasses = c(census_block_group="character"))

#### Functions
jsonchr_to_longdf <- function (sg_id, jsonchr) {
  tbl <- jsonchr %>%
    fromJSON() %>%
    as_tibble
  if (nrow(tbl) == 0) return(data.frame())
  tbl <- pivot_longer(tbl, everything(), names_to = 'cbg', values_to = 'freq')
  tbl$safegraph_place_id <- sg_id
  return(tbl)
}

#### CBG shape
cbg <- st_read("../fl-covid19/tmp/cenacs/cenacs_2018.shp") %>%
  mutate(GEOID10 = as.character(GEOID10))
cbg <- st_set_geometry(cbg, NULL)
cbg

#### Run
plan(multisession(workers = 4))

patterns <- patterns %>%
  mutate(visitor_home_cbgs = str_replace_all(visitor_home_cbgs, '""', '"'))

home_cbg_freq <- future_map2_dfr(patterns$safegraph_place_id,
                                 patterns$visitor_home_cbgs,
                                 ~ jsonchr_to_longdf(.x, .y), .progress = T)
home_cbg_freq <- home_cbg_freq %>%
  filter(str_starts(cbg, "12"))

plan(sequential)

#### Merge hp to home_cbg_freq first
home_cbg_freq <- home_cbg_freq %>%
  left_join(hp %>% select(census_block_group, number_devices_residing),
            by = c("cbg" = "census_block_group"))

home_cbg_freq <- home_cbg_freq %>%
  mutate(fpd = freq/number_devices_residing)


#### For now merge top cat into home_cbg_freq
home_cbg_freq <- home_cbg_freq %>%
  left_join(core %>% select(safegraph_place_id, top_category, sub_category))
home_cbg_freq <- home_cbg_freq %>% filter(!is.na(top_category))
fwrite(home_cbg_freq, "data/home_cbg_freq.csv")

table(home_cbg_freq$top_category)○

home_cbg_freq %>%
  filter(top_category == "Restaurants and Other Eating Places") %>%
  count(sub_category)
