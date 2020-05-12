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

# get IL county shapefile
il_cty_shp <- counties(cb = TRUE, 
                       state = "17") %>% 
  st_as_sf() 

# Cook county
cook_cty_sf <- il_cty_shp %>% 
  filter(COUNTYFP == "031")

# Chicago metro counties in IL
chi_metro_county_list <- c("Cook",
                           "DeKalb",
                           "DuPage",
                           "Grundy",
                           "Kankakee",
                           "Kane",
                           "Kendall",
                           "McHenry",
                           "Will",
                           "Lake")

chi_sf <- il_cty_shp %>% 
  filter(NAME %in% chi_metro_county_list) %>% 
  st_union()


# get ZCTA shapefile
zctas_shp <- zctas(cb = TRUE) %>%
  st_as_sf() 

# check state file
ggplot(il_st_shp) +
  geom_sf()

# check Cook county file
ggplot(cook_cty_sf) +
  geom_sf()

# check Chicago metro shp
ggplot(chi_sf) +
  geom_sf()

#===============================================================================#
# SPATIAL JOIN
#===============================================================================#

# spatial join: polygon inside polygon
il_zcta <- st_intersection(zctas_shp, il_st_shp) %>% 
  st_collection_extract()  

# spatial join with cook county border
cook_zcta <- st_intersection(cook_cty_sf, zctas_shp)

# spatial join with Chicago metro
chi_zcta <- st_intersection(chi_sf, zctas_shp) 

# check result
ggplot(chi_zcta) +
  geom_sf()


# save... hopefully
il_zcta %>% 
  st_write( 
         dsn = "data/IL_ZCTA_simple.shp",
         layer = "data/IL_ZCTA_simple.shp",
         driver = "ESRI Shapefile")

# # save... hopefully
# cook_zcta %>% 
#   st_write( 
#     dsn = "data/Cook_ZCTA_simple.shp",
#     layer = "data/Cook_ZCTA_simple.shp",
#     driver = "ESRI Shapefile")

# # save... hopefully
# chi_zcta %>% 
#   st_write( 
#     dsn = "processed_shp/ChiMetro_ZCTA_simple.shp",
#     layer = "processed_shp/ChiMetro_ZCTA_simple.shp",
#     driver = "ESRI Shapefile")


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
