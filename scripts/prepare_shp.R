#===============================================================================#
# PREPARE SHAPEFILES
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


#===============================================================================#
# GET SHAPEFILES
#===============================================================================#

options(tigris_use_cache = TRUE)

# get IL state shapefile
il_st_shp <- states(cb = TRUE) %>%
  st_as_sf() %>%
  filter(GEOID == "17") 

# get ZCTA shapefile
zctas_shp <- zctas(cb = TRUE) %>%
  st_as_sf() 

# check state file
ggplot(il_st_shp) +
  geom_sf()

#===============================================================================#
# SPATIAL JOIN
#===============================================================================#

# spatial join: polygon inside polygon
il_zcta <- st_intersection(zctas_shp, il_st_shp) %>% 
  st_collection_extract()  

# check result
ggplot(il_zcta) +
  geom_sf()


# save... hopefully
il_zcta %>% 
  st_write( 
         dsn = "data/IL_ZCTA_simple.shp",
         layer = "data/IL_ZCTA_simple.shp",
         driver = "ESRI Shapefile")


#===============================================================================#
# EXTRACT AREA
#===============================================================================#

# extract area and drop geometry
il_zcta_area <- il_zcta %>% 
  select(ZCTA5CE10,
         ALAND10) %>% 
  st_drop_geometry() %>% 
  mutate(
    ALAND10 = as.numeric(ALAND10)
  )

# get population density

save(il_zcta_area, 
     file = "data/IL_ZCTA_area.Rdata")
  
il_zcta %>% 
  left_join(covid,
            by = c("ZCTA5CE10" = "zip")) %>% 
  ggplot(aes(fill = confirmed_cases)) +
  geom_sf()

ggsave("plots/confirmed_map.png")
