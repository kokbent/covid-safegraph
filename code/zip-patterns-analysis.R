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
library(glmmTMB)
library(lme4)

zip_tcwide <- readr::read_rds("data/zip_tcwide.rds")
tmp <- readr::read_rds("data/zip_jul.rds")
zip_jul <- tmp$zip_jul
top_cat_code <- tmp$top_cat_code

zip_tcmat <- as.matrix(zip_tcwide[,-1])
row.names(zip_tcmat) <- zip_tcwide$ZIP

zip_tcmat <- zip_tcmat[,c("Res76", "Les46", "Gen35", "Gro37", "Gas33", "Oth61", "Bui14", 
                          "Hea39", "Spo82", "Mus52", "Dep22", "Oth65", "Off58", "Tra87", 
                          "Aut7", "Use88", "Aut5", "Clo16", "Off55", "Ele28")]

colMeans(zip_tcmat == 0)
prc <- princomp(zip_tcmat, cor=T)
load <- as.data.frame(t(prc$loadings[,1:10]))
load %>%
  mutate(comp = row_number()) %>%
  pivot_longer(-comp) %>%
  ggplot() +
  geom_raster(aes(x=name, y=comp, fill=value)) +
  colorspace::scale_fill_continuous_diverging(mid=0) +
  theme(axis.text.x = element_text(angle = 90))

zip_tcwide <- bind_cols(zip_tcwide[,"ZIP"], as.data.frame(prc$scores[,1:10]))

zip_dat <- zip_jul %>%
  left_join(zip_tcwide)

zip_dat %<>%
  filter(Case_p10k > 0, TOTALPOP > 10000)

f <- paste0("Comp.", 1:10) %>%
  paste(collapse = "+")
f <- paste0("Case_p10k ~ MED_AGE + scale(TOTALPOP) + I(TOTALPOP/ACRES) + (1 | COUNTYNAME) +
            scale(MEDHHINC) + ", f)
# f <- "Case ~ MED_AGE + scale(TOTALPOP) + I(TOTALPOP/ACRES) + (1 | COUNTYNAME) + scale(as.numeric(Case.y))"
mod <- lmerTest::lmer(formula(f),
        data = zip_dat)
summary(mod)

MuMIn::r.squaredGLMM(mod)

with(zip_dat %>% filter(TOTALPOP >= 10000),
     plot(Comp.1, Case_p10k))

zip_dat %>% filter(TOTALPOP >= 10000) %>%
  ggplot(aes(x=Comp.1, Case_p10k)) +
  geom_point() +
  geom_smooth()

####
zip_tcwide <- readr::read_rds("data/zip_tcwide.rds")
tmp <- readr::read_rds("data/zip_jul.rds")
zip_jul <- tmp$zip_jul
top_cat_code <- tmp$top_cat_code

zip_tcwide[,-1] <- zip_tcwide[,-1] / rowSums(zip_tcwide[,-1])
selcat <- colMeans(zip_tcwide[,-1]) %>% sort(decreasing = T) %>% {.[1:15]} %>% names

zip_dat <- zip_jul %>%
  left_join(zip_tcwide)

zip_dat %<>%
  filter(Case_p10k > 0)

f <- paste0(selcat) %>%
  paste(collapse = "+")
f <- paste0("Case ~ MED_AGE + scale(TOTALPOP) + I(TOTALPOP/ACRES) + (1 | COUNTYNAME) + scale(MEDHHINC) +", f)
# f <- "Case ~ MED_AGE + scale(TOTALPOP) + I(TOTALPOP/ACRES) + (1 | COUNTYNAME) + scale(as.numeric(Case.y))"
mod <- lmerTest::lmer(formula(f),
                      data = zip_dat)
summary(mod)

MuMIn::r.squaredGLMM(mod)

with(zip_dat %>% filter(TOTALPOP >= 10000),
     plot(Res76, Case_p10k))
plot(zip_dat$Res76, zip_dat$Case)
