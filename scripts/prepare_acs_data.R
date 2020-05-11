#===============================================================================#
# SOCI 40217 PROJECT: ASSEMBLE DATA
#
# Cecile Murray
#===============================================================================#

libs <- c("here",
          "tidyverse",
          "purrr",
          "knitr", 
          "kableExtra",
          "janitor",
          "tidycensus")
lapply(libs, library, character.only = TRUE)

CENSUS_KEY <- Sys.getenv("CENSUS_API_KEY")

#===============================================================================#
# REQUEST ACS DATA FOR ZCTAS
#===============================================================================#

# create list of variables needed for % ages
age_vars <- map_chr(
  c(1,
    seq(18, 25), # men aged 60+
    seq(42, 49)), # women aged 60+
  ~ paste0("B01001_", 
           str_pad(., width = 3, side = "left", pad = "0"))
)


# create list of variables needed for % uninsured
health_vars <- map_chr(
  c(1,
    seq(5, 29, 3),
    seq(33, 57, 3)),
  ~ paste0("B27001_",
           str_pad(., width = 3, side = "left", pad = "0"))
) 

# create list of variables of % Black, White, Hispanic
race_vars <- map_chr(
  c("", "B", "H", "I"),
  ~ paste0("B01001", ., "_001")
)

# create variable list for % using public transportation
commute_vars <- map_chr(
  c(1, 8),
  ~ paste0("B08006_",
           str_pad(., width = 3, side = "left", pad = "0"))
)

# create final list of variables
acs_vars <- c(age_vars, race_vars, health_vars, commute_vars)

il_data <- get_acs("zcta",
                  variables = acs_vars,
                  year = 2018,
                  key = CENSUS_KEY) 

# save(il_data,
#      file = "data/IL_ACS_data.Rdata")

# load("data/IL_ACS_data.Rdata")

#===============================================================================#
# COLLAPSE ACS DATA
#===============================================================================#

il_collapsed <- il_data %>% 
  arrange(GEOID) %>% 
  mutate(
    var_num = as.numeric(str_sub(variable, -3)),
    var_flag = case_when(
      str_detect(variable, "B01001B_001") ~ "race_Black",
      str_detect(variable, "B01001H_001") ~ "race_NHW",
      str_detect(variable, "B01001I_001") ~ "race_Hisp",
      str_starts(variable, "B27001")  ~ "health",
      str_starts(variable, "B01001")  ~ "age",
      str_starts(variable, "B08006") ~ "commute"
    ),
    universe = case_when(
      var_flag == "age" | str_detect(var_flag, "race") ~ "age_race",
      var_flag == "health" ~ "health",
      var_flag == "commute" ~"commute"
    ), 
    total = if_else(var_num == 1 &
                            !str_detect(var_flag, "race"),
                         estimate, 
                         NA_real_)
  ) %>% 
  group_by(GEOID, universe) %>%
  fill(total) %>% 
  filter(estimate != total | is.na(total)) %>% 
  ungroup() %>% 
  group_by(GEOID, var_flag) %>% 
  summarize(
    est = sum(estimate, na.rm = TRUE),
    total = first(total),
    share = est / total
  ) 

# save(il_collapsed, 
#      file = "data/IL_collapsed_ACS.Rdata")
