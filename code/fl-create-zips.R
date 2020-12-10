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
library(magrittr)

#### CBG shape
cbg <- st_read("../fl-covid19/tmp/cenacs/cenacs_2018.shp") %>%
  mutate(GEOID10 = as.character(GEOID10))
cbg <- st_set_geometry(cbg, NULL)
cbg

####
home_cbg_freq <- fread("data/home_cbg_freq.csv",
                       colClasses = c(cbg = "character"))

home_cbg_freq$freq[home_cbg_freq$freq == 4] <- sample(2:4, sum(home_cbg_freq$freq == 4), replace = T)


####
zip_tract <- readxl::read_xlsx("data/ZIP_TRACT_092020.xlsx") %>%
  select(ZIP, TRACT) %>%
  filter(str_starts(TRACT, "12"))

####
cbg_topcat <- home_cbg_freq %>%
  group_by(cbg, top_category) %>%
  summarise(freq = sum(freq),
            number_devices_residing = unique(number_devices_residing))
cbg_topcat %<>%
  left_join(cbg %>% select(GEOID10, TOTALPOP), by = c("cbg" = "GEOID10"))
cbg_topcat %<>%
  group_by(cbg) %>%
  complete(cbg = cbg, top_category = unique(cbg_topcat$top_category),
           number_devices_residing = number_devices_residing,
           TOTALPOP = TOTALPOP, fill = list(freq = 0))

cbg_topcat %<>%
  filter(TOTALPOP != 0)

tract_topcat <- cbg_topcat %>%
  mutate(tract = str_sub(cbg, 1, 11),
         fpd = freq/number_devices_residing) %>%
  group_by(tract, top_category) %>%
  summarise(
    fpd = sum(fpd * TOTALPOP) / sum(TOTALPOP),
    freq = sum(freq),
    number_devices_residing = sum(number_devices_residing),
    TOTALPOP = sum(TOTALPOP))

zip_topcat <- tract_topcat %>%
  left_join(zip_tract,
            by = c("tract" = "TRACT")) %>%
  group_by(ZIP, top_category) %>%
  summarise(fpd = sum(fpd * TOTALPOP) / sum(TOTALPOP),
            freq = sum(freq),
            number_devices_residing = sum(number_devices_residing),
            TOTALPOP = sum(TOTALPOP)) %>%
  filter(top_category != "")

# tmp <- zip_topcat %>%
#   filter(top_category == "Restaurants and Other Eating Places") %>%
#   mutate(fpd2 = freq/number_devices_residing)
# plot(tmp$fpd, tmp$fpd2)

selcat <- zip_topcat %>%
  group_by(top_category) %>%
  summarise(n = sum(fpd != 0)) %>%
  filter(n/length(unique(zip_tract$ZIP)) >= 0.5)

zip_topcat <- zip_topcat %>%
  filter(top_category %in% selcat$top_category)
  
top_cat_code <- zip_topcat$top_category %>%
  unique %>%
  sort
names(top_cat_code) <- str_sub(top_cat_code, 1, 3) %>%
  paste0(1:length(top_cat_code))

zip_topcat$top_category <- factor(zip_topcat$top_category,
                                  levels = top_cat_code,
                                  labels = names(top_cat_code))
# zip_topcat %<>%
#   mutate(fpd = freq/number_devices_residing)

zip_tcwide <- zip_topcat %>%
  pivot_wider(c(ZIP, top_category, fpd), names_from = "top_category",
              values_from = "fpd", values_fill = list(fpd=0))
zip_tcmat <- as.matrix(zip_tcwide[,-1])
row.names(zip_tcmat) <- zip_tcwide$ZIP

colMeans(zip_tcmat == 0)
prc <- princomp(zip_tcmat)
load <- as.data.frame(t(prc$loadings[,1:10]))
load %>%
  mutate(comp = row_number()) %>%
  pivot_longer(-comp) %>%
  ggplot() +
  geom_raster(aes(x=name, y=comp, fill=value)) +
  colorspace::scale_fill_continuous_diverging(mid=0) +
  theme(axis.text.x = element_text(angle = 90))

readr::write_rds(zip_tcwide, "data/zip_tcwide.rds")

#### CBG shape
cbg <- st_read("../fl-covid19/tmp/cenacs/cenacs_2018.shp") %>%
  mutate(GEOID10 = as.character(GEOID10))
cbg <- st_set_geometry(cbg, NULL)
cbg

tract_dem <- cbg %>%
  mutate(tract = str_sub(GEOID10, 1, 11)) %>%
  group_by(tract) %>%
  summarise(MED_AGE = sum(MED_AGE * TOTALPOP)/sum(TOTALPOP),
            TOTALPOP = sum(TOTALPOP),
            ACRES = sum(ACRES))

zip_dem <- tract_dem %>%
  left_join(zip_tract,
            by = c("tract" = "TRACT")) %>%
  group_by(ZIP) %>%
  summarise(MED_AGE = sum(MED_AGE * TOTALPOP)/sum(TOTALPOP),
            TOTALPOP = sum(TOTALPOP),
            ACRES = sum(ACRES))

zip_covid1 <- fread("../fl-covid19-extra-analysis/data/zipcase_20200801.csv")
zip_covid2 <- fread("../fl-covid19-extra-analysis/data/zipcase_20200701.csv")

zip_covid1 %<>%
  filter(!Cases_1 %in% c("0", "<5")) %>%
  select(ZIP, COUNTYNAME, Case = Cases_1)
zip_covid2 %<>%
  filter(!Cases_1 %in% c("0", "<5")) %>%
  select(ZIP, COUNTYNAME, Case = Cases_1)
zip_jul <- zip_covid1 %>%
  inner_join(zip_covid2, by = c("ZIP", "COUNTYNAME")) %>%
  distinct(ZIP, .keep_all = T) %>%
  mutate(Case = as.numeric(Case.x) - as.numeric(Case.y),
         ZIP = as.character(ZIP))

zip_jul %<>%
  left_join(zip_dem) %<>%
  mutate(Case_p10k = Case / TOTALPOP * 10000)

readr::write_rds(list(zip_jul = zip_jul, top_cat_code = top_cat_code), 
                 "data/zip_jul.rds")
