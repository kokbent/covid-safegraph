library(dplyr)
library(data.table)
library(ggplot2)
library(lubridate)

ht <- c("<= 25%", "26 - 50%", "51 - 75%", ">= 76%")
df <- data.frame()
for (i in 1:4) {
  in_file <- paste0("output/mob-age-v2-ht", i, ".rda")
  load(in_file)
  summ <- MCMCvis::MCMCsummary(samp)
  
  out <- data.frame(week = rep(2:39, 5), theta = summ$mean, 
                    age_grp = rep(c("00 - 17", "18 - 29", "30 - 49", "50 - 64", "65 and above"), each = 38))
  out$week_date <- ymd("2020-01-01")
  week(out$week_date) <- out$week
  wday(out$week_date) <- 6
  out$ht <- ht[i]
  df <- bind_rows(df, out)
}

df$ht <- factor(df$ht, levels = ht)

ggplot(df) +
  geom_line(aes(x=week_date, y=theta, colour=age_grp), lwd = 1.1) +
  labs(x="Week", y="", title="Fraction of persons staying at home for X% of time") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_colour_viridis_d(name = "Age group", option = "C", end = 0.9, direction = -1) +
  facet_wrap(~ ht, nrow = 4) +
  ggpubr::theme_pubclean()
