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
covid <- read_csv("data/il-covid-counts-by-zipcode-5_19_2020.csv") %>% 
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
  right_join(
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
  ) %>% 
  filter(zip != "53142", # drop problematic WI ZCTA
         !is.na(over60_share), # dropping two ZIPs here, not sure why
         !is.na(ptcommute_share)) # dropping one here bc var is missing


#===============================================================================#
# EXPORT
#===============================================================================#

# save(master, file = "data/MASTER_2020-06-03.Rdata")

write_csv(master, "data/MASTER_DATASET.csv")

covid_zcta <- il_zcta %>% 
  distinct(ZCTA5CE10, .keep_all = TRUE) %>% 
  filter(ZCTA5CE10 %in% master$zip,
         ZCTA5CE10 != "53142") 

covid_zcta %>% 
  st_write(dsn = "data/covid_zcta.shp",
           layer = "data/covid_zcta.shp",
           driver = "ESRI Shapefile")

#===============================================================================#
# MASTER ONLY FOR COOK COUNTY
#===============================================================================#

cook_master <- master %>% 
  filter(zip %in% cook_zcta$ZCTA5CE10) 

write_csv(cook_master, "data/COOK_MASTER.csv")

cook_zcta_nomissing <- il_zcta %>% 
  distinct(ZCTA5CE10, .keep_all = TRUE) %>% 
  filter(ZCTA5CE10 %in% cook_master$zip,
         ZCTA5CE10 != "53142") 

cook_zcta_nomissing %>% 
  st_write(dsn = "data/Cook_ZCTA_nomissing.shp",
           layer = "data/Cook_ZCTA_nomissing.shp",
           driver = "ESRI Shapefile")

#===============================================================================#
# VERSION FOR CHICAGO METRO
#===============================================================================#

chi_master <- master %>% 
  semi_join(
    chi_zcta,
    by = c("zip" = "ZCTA5CE10")
  ) %>% 
  mutate_at(
    vars(one_of(c("ptcommute_share", "race_Black"))),
    ~ if_else(. < 0.001, 0.001, .)
  ) %>% 
  filter(zip != "60927",
         zip != "61068")

write_csv(chi_master, "data/CHI_METRO_MASTER.csv")

# now create non-missing shapefile
chi_zcta_nomissing <- il_zcta %>% 
  distinct(ZCTA5CE10, .keep_all = TRUE) %>%
  filter(ZCTA5CE10 %in% chi_master$zip,
         ZCTA5CE10 != "53142",
         ZCTA5CE10 != "61068") 

chi_zcta_nomissing %>% 
  st_write(dsn = "processed_shp/ChiMetro_ZCTA_nomissing.shp",
           layer = "processed_shp/ChiMetro_ZCTA_nomissing.shp",
           driver = "ESRI Shapefile")




#===============================================================================#
# PLOT COVID CASE COUNTS
#===============================================================================#

cook_zcta %>% 
  left_join(
    master,
    by = c("ZCTA5CE10" = "zip")
  ) %>% 
  ggplot(
    aes(fill = confirmed_cases)
  ) +
  geom_sf() +
  labs(fill = "Confirmed cases",
       caption = "Source: Illinois Department of Public Health") +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = c(0.1, 0.2))

# ggsave("plots/Cook_confirmed_case_map_2020-05-07.png")
