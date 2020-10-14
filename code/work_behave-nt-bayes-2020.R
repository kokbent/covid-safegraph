rm(list=ls())
gc()

library(dplyr)
library(data.table)
library(ggplot2)
library(sf)
library(lubridate)
library(tidyr)
library(stringr)
library(googledrive)
library(nimble)
source("code/get_agegrp.R")

#### CBG Data ----
cbg <- st_read("shp/cenacs_2018.shp")
cbg1 <- get_agegrp_nt(cbg) %>%
  select(-n)
cbg <- st_set_geometry(cbg, NULL)

#### Data ----
dat <- fread("data/sdm-consolidated.csv")
dat <- dat %>%
  filter(date >= ymd("2020-01-01"))
dat$origin_census_block_group <- as.character(dat$origin_census_block_group)
dat$date <- as.Date(dat$date)
dat <- as.data.table(dat)

#### Distance travelled analysis ----
wb <- dat[,.(date, origin_census_block_group, 
             part_time_work_behavior_devices, full_time_work_behavior_devices, device_count)]
df <- wb %>%
  filter(date >= ymd("2020-01-05"), date <= ymd("2020-09-26")) %>%
  mutate(freq = part_time_work_behavior_devices + full_time_work_behavior_devices,
         n = device_count)

df1 <- df %>%
  mutate(week = epiweek(date)) %>%
  group_by(week, origin_census_block_group) %>%
  summarise(freq = sum(freq),
            n = sum(n))

mask <- df1 %>%
  ungroup() %>%
  left_join(cbg %>% select(GEOID10, TOTALPOP), 
            by = c("origin_census_block_group" = "GEOID10")) %>%
  filter(n / TOTALPOP >= 7 * 0.02 & n / TOTALPOP <= 7 * 0.5) %>%
  count(origin_census_block_group) %>%
  filter(n == 38)

df2 <- df1 %>%
  ungroup() %>%
  filter(origin_census_block_group %in% mask$origin_census_block_group) %>%
  left_join(cbg1, by = c("origin_census_block_group" = "GEOID10")) %>%
  pivot_wider(names_from = age_grp, values_from = prop)

# r <- as.matrix(df2 %>% filter(week == min(week)) %>%
#                  .[,c("00 - 17", "18 - 39", "40 - 64", "65 and above")])
r <- as.matrix(df2 %>% filter(week == min(week)) %>%
                 .[,c("18 - 29", "30 - 49", "50 - 64", "65 and above")])
df3 <- df2 %>%
  select(week, origin_census_block_group, freq, n)
y <- df3 %>%
  select(-n) %>%
  pivot_wider(names_from = week, values_from = freq, values_fill = list(freq = 0)) %>%
  select(-origin_census_block_group) %>%
  as.matrix()
y <- round(y/7)
n <- df3 %>%
  select(-freq) %>%
  pivot_wider(names_from = week, values_from = n, values_fill = list(n = 0)) %>%
  select(-origin_census_block_group) %>%
  as.matrix()
n <- round(n/7)

# r <- r[-10424,]
# y <- y[-10424,]
# n <- n[-10424,]

#### Fit with NIMBLE ----
constants <- list(Nb = nrow(y),
                  Nt = ncol(y),
                  Ng = 4)

data <- list(y = y,
             n = n,
             r = r)
inits <- list(mu = matrix(0.5, nrow = constants$Nt, ncol = 4))

source("code/mob_age_JAGS.R")
system.time(model <- nimbleModel(code, constants = constants, data = data, inits = inits,
                                 calculate = F))

conf <- configureMCMC(model, 
                      monitors = c("theta"))
mcmc <- buildMCMC(conf)
modelc <- compileNimble(model)
mcmcc <- compileNimble(mcmc)
# Uncomment for quick sanity check
samp <- runMCMC(mcmcc, niter = 5000, nburnin = 2500, thin = 1, nchains = 3,
                progressBar = T, samples = T, samplesAsCodaMCMC = T)
# samp <- runMCMC(mcmcc, niter = 100000, nburnin = 50000, thin = 10, nchains = 1,
#                 setSeed = 4342024, progressBar = T,
#                 samples = T, samplesAsCodaMCMC = T)
save(samp, file = paste0("output/mob-age-v2-wb-noteen.rda"))

# summ <- MCMCvis::MCMCsummary(samp)
# summ
# 
# out <- data.frame(week = rep(2:39, 4), theta = summ$mean, 
#                   age_grp = rep(c("00 - 17", "18 - 39", "40 - 64", "65 and above"), each = 38))
# out$week_date <- ymd("2020-01-01")
# week(out$week_date) <- out$week
# wday(out$week_date) <- 6
# 
# ggplot(out) +
#   geom_line(aes(x=week_date, y=theta, colour=age_grp), lwd = 1.1) +
#   labs(x="Week", y="", title="Fraction at home >= 75% of time") +
#   scale_x_date(date_breaks = "1 month", date_labels = "%b")
