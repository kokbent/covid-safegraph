# get_agegrp <- function (cbg) {
#   tmp <- cbg %>%
#     select(GEOID10, starts_with("AGE_")) %>%
#     select(-AGE_65_UP)
#   tmp <- st_set_geometry(tmp, NULL)
#   colnames(tmp)[colnames(tmp) == "AGE_UNDER5"] <- "AGE_0_5"
#   colnames(tmp)[ncol(tmp)] <- "AGE_85_120"
#   
#   tmp1 <- tmp %>%
#     pivot_longer(cols = -GEOID10,
#                  names_to = c("age_lo", "age_hi"),
#                  names_pattern = "AGE_(.*)_(.*)",
#                  values_to = "freq") %>%
#     group_by(GEOID10) %>%
#     mutate(prop = freq / sum(freq))
#   
#   cbg_agegrp <- tmp1 %>%
#     mutate(age_grp = case_when(
#       age_hi <= 19 ~ "00 - 19",
#       age_hi <= 49 ~ "20 - 49",
#       age_hi <= 69 ~ "50 - 69",
#       TRUE         ~ "70 and above"
#     )) %>%
#     group_by(GEOID10, age_grp) %>%
#     summarise(n = sum(freq), prop = sum(prop))
#   
#   return(cbg_agegrp)
# }

#### Alt version
get_agegrp <- function (cbg) {
  tmp <- cbg %>%
    select(GEOID10, starts_with("AGE_")) %>%
    select(-AGE_65_UP)
  tmp <- st_set_geometry(tmp, NULL)
  colnames(tmp)[colnames(tmp) == "AGE_UNDER5"] <- "AGE_0_5"
  colnames(tmp)[ncol(tmp)] <- "AGE_85_120"
  
  tmp1 <- tmp %>%
    pivot_longer(cols = -GEOID10,
                 names_to = c("age_lo", "age_hi"),
                 names_pattern = "AGE_(.*)_(.*)",
                 values_to = "freq") %>%
    group_by(GEOID10) %>%
    mutate(prop = freq / sum(freq))
  
  cbg_agegrp <- tmp1 %>%
    mutate(age_grp = case_when(
      age_hi <= 17 ~ "00 - 17",
      age_hi <= 39 ~ "18 - 39",
      age_hi <= 64 ~ "40 - 64",
      TRUE         ~ "65 and above"
    )) %>%
    group_by(GEOID10, age_grp) %>%
    summarise(n = sum(freq), prop = sum(prop))
  
  return(cbg_agegrp)
}

#### No kids
get_agegrp1 <- function (cbg) {
  tmp <- cbg %>%
    select(GEOID10, starts_with("AGE_")) %>%
    select(-AGE_65_UP)
  tmp <- st_set_geometry(tmp, NULL)
  colnames(tmp)[colnames(tmp) == "AGE_UNDER5"] <- "AGE_0_5"
  colnames(tmp)[ncol(tmp)] <- "AGE_85_120"
  
  tmp1 <- tmp %>%
    pivot_longer(cols = -GEOID10,
                 names_to = c("age_lo", "age_hi"),
                 names_pattern = "AGE_(.*)_(.*)",
                 values_to = "freq") %>%
    group_by(GEOID10) %>%
    mutate(prop = freq / sum(freq))
  
  cbg_agegrp <- tmp1 %>%
    mutate(age_grp = case_when(
      age_hi <= 17 ~ "00 - 17",
      age_hi <= 29 ~ "18 - 29",
      age_hi <= 49 ~ "30 - 49",
      age_hi <= 64 ~ "50 - 64",
      TRUE         ~ "65 and above"
    )) %>%
    filter(age_grp != "00 - 17") %>%
    group_by(GEOID10, age_grp) %>%
    summarise(n = sum(freq)) %>%
    group_by(GEOID10) %>%
    mutate(prop = n/sum(n))
  
  return(cbg_agegrp)
}