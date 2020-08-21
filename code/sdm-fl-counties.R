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
drive_download("sdm-consolidated-2020-08-21-reduced.csv", "data/sdm-consolidated.csv",
               overwrite = T)

#### Data Wrangling ----
dat <- fread("data/sdm-consolidated.csv")
dat <- dat %>%
  filter(date >= ymd("2020-01-01"))
dat$origin_census_block_group <- as.character(dat$origin_census_block_group)
dat$date <- as.Date(dat$date)
dat <- dat %>%
  left_join(cbg1, by = c("origin_census_block_group" = "GEOID10"))

#### Counties list ----
counties <- list()
counties$Alachua <- 1
counties$Escambia <- 33
counties$Dade <- 86
counties$Duval <- 31
counties$Leonplus <- c(13, 37, 39, 45, 63, 65, 73, 77, 129)
counties$Broward <- 11
counties$Sumterplus <- c(69, 119)
counties$Florida <- unique(cbg1$COUNTYFP10)

#### Loop through Counties list ----
comb_df <- data.frame()
for (i in 1:length(counties)) {
  nombre <- names(counties)[i]
  cnts <- str_pad(counties[[i]], width = 3,
                  side = "left", pad = "0")
  dat1 <- dat %>%
    filter(COUNTYFP10 %in% cnts) %>%
    mutate(county = nombre)
  
  dat2 <- dat1 %>%
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
  
  df <- dat2 %>%
    select(date, county, prop_non_home = perc_non_home, 
           prop_non_home_ma7 = perc_non_home_ma7) %>%
    mutate(prop_non_home = prop_non_home / 100,
           prop_non_home_ma7 = fill_NA_w_neighbour(prop_non_home_ma7 / 100))
  
  outfile <- paste0("output/mobility-", tolower(nombre), ".csv")
  data.table::fwrite(df, outfile)
  
  comb_df <- bind_rows(comb_df, df)
}

ggplot(comb_df) +
  geom_line(aes(date, prop_non_home, colour = county)) +
  theme_bw() +
  scale_x_date(date_breaks = "1 month", date_labels = "%b")

ggplot(comb_df) +
  geom_line(aes(date, prop_non_home_ma7, colour = county)) +
  theme_bw() +
  scale_x_date(date_breaks = "1 month", date_labels = "%b")