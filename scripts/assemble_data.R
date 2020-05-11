#===============================================================================#
# ASSEMBLE ALL DATA
#
# Cecile Murray
#===============================================================================#

libs <- c("here",
          "tidyverse",
          "magrittr",
          "purrr",
          "knitr", 
          "kableExtra",
          "janitor",
          "tidycensus",
          "sf")
lapply(libs, library, character.only = TRUE)

#===============================================================================#
# LOAD AND DO FINAL PREP
#===============================================================================#

# load data
load("data/IL_ZCTA_area.Rdata")
load("data/IL_collapsed_ACS.Rdata")

# reshape collapsed to wide
il_collapsed_wide <- il_collapsed %>% 
  select(
    GEOID,
    var_flag,
    share
  ) %>% 
  pivot_wider(
    names_from = "var_flag",
    values_from = "share"
  ) %>% 
  dplyr::rename(
    "over60_share" = "age",
    "ptcommute_share" = "commute",
    "uninsured_share" = "health"
  )
         
# get pop totals on their own
il_pop <- il_collapsed %>% 
  filter(var_flag == "age") %>% 
  select(
    GEOID,
    total
  )

# load and prep covid data
covid <- read_csv("data/il-covid-counts-by-zipcode-5_7_2020.csv") %>% 
  clean_names() %>% 
  mutate(
    zip = as.character(zip)
  ) %>% 
  select(
    zip,
    confirmed_cases,
    total_tested
  )

#===============================================================================#
# JOIN  
#===============================================================================#

master <- covid %>% 
  left_join(
    il_zcta_area,
    by = c("zip" = "ZCTA5CE10")
  ) %>% 
  left_join(
    il_collapsed_wide,
    by = c("zip" = "GEOID")
  ) %>% 
  left_join(
    il_pop,
    by = c("zip" = "GEOID")
  ) %>% 
  mutate(
    pop_density = ALAND10 / total
  )

#===============================================================================#
# EXPORT
#===============================================================================#

write_csv(master, "data/MASTER_DATASET.csv")

#===============================================================================#
# WHAT'S AN LM DO
#===============================================================================#

testreg <- lm(confirmed_cases ~ over60_share + ptcommute_share + 
                uninsured_share + race_Black + race_Hisp + pop_density,
              data = master)
