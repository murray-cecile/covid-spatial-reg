#===============================================================================#
# EXPLORE COVID DATA
#
# Cecile Murray
#===============================================================================#

libs <- c("here",
          "tidyverse",
          "purrr",
          "knitr", 
          "kableExtra",
          "janitor",
          "tidycensus",
          "tigris",
          "sf")
lapply(libs, library, character.only = TRUE)

# bring in ILPDH data
covid <- read_csv("data/ILDPH_2020-04-27.csv") %>% 
  clean_names() %>% 
  mutate(zip = as.character(zip))

# set caching for shapefiles to avoid continually re-downloading
options(tigris_use_cache = TRUE)
options(tigris_cache_dir(paste0(here(), "/data")))
readRenviron('~/.Renviron')

# get IL state shapefile
il_st_shp <- states() %>% 
  st_as_sf() %>% 
  filter(GEOID == "17")

# get IL ZCTA shapefile
zctas_shp <- zctas() %>% 
  st_as_sf() %>% 
  st_simplify()

#===============================================================================#
# EXPLORATORY VIZ
#===============================================================================#

covid %>% 
  ggplot(aes(x = positive_cases)) +
  geom_histogram()

covid_zctas <- zctas_shp %>% 
  filter(GEOID10 %in% covid$zip) %>% 
  st_transform(54032) %>% 
  st_simplify() %>% 
  st_transform(4326)

#===============================================================================#
# REQUEST ACS DATA FOR ZCTAS
#===============================================================================#

il_pop <- get_acs("zcta",
                  variables = "B01001_001",
                  year = 2018) %>% 
  filter(GEOID %in% covid$zip)



