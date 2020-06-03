#===============================================================================#
# ALLOCATE LEHD BLOCK DATA TO ZIP CODES
#
# Cecile Murray
#===============================================================================#

libs <- c("here",
          "tidyverse",
          "purrr",
          "knitr", 
          "kableExtra",
          "janitor",
          "tigris",
          "sf")
lapply(libs, library, character.only = TRUE)

#===============================================================================#
# PREP LODES
#===============================================================================#

# read in LODES files: keep only first three cols for memory
col_spec = paste0("ccd", glue::glue_collapse(rep("_", 10)))
il_lodes_main <- read_csv("data/il_od_main_JT00_2017.csv", 
                          col_types = col_spec)
il_lodes_aux <- read_csv("data/il_od_aux_JT00_2017.csv",
                         col_types = col_spec)


# bind rows; change column names/type; add state flags
il_lodes <- bind_rows(
  il_lodes_main,
  il_lodes_aux
) %>% 
  dplyr::rename(
    "total_jobs" = "S000",
  ) %>% 
  mutate(
    w_geocode = as.character(w_geocode),
    h_stfips = str_sub(h_geocode, 1, 2),
    w_stfips = str_sub(w_geocode, 1, 2)
  ) 


# diagnostics
length(unique(il_lodes$h_stfips)) # 51 total states represented in home blocks
length(unique(il_lodes$w_stfips)) # 1 total states represented in word blocks

# non-IL share for home tracts: about 5%
il_lodes %>% 
  mutate(
    h_in_IL = if_else(h_stfips == "17", TRUE, FALSE)
  ) %>% 
  tabyl(h_in_IL) %>% 
  adorn_percentages(denominator = "col")

# remove original files 
rm(il_lodes_aux, il_lodes_main)


#===============================================================================#
# GET IL CENSUS BLOCK CENTROIDS
#===============================================================================# 
 
# get polygon dataframe of Census blocks in IL - VERY BIG, USE CAUTION
il_blocks <- blocks(state = "17")

# extract centroids
il_block_centroids <- il_blocks %>% 
  st_as_sf() %>% 
  st_centroid()

# remove giant block file
rm(il_blocks)

#===============================================================================#
# LOAD ZCTA POLYGONS AND SPATIAL JOIN
#===============================================================================#

il_zcta <- st_read("data/IL_ZCTA_simple.shp")

# polygon-point join
block_in_zcta <- sf::st_join(
  il_zcta,
  il_block_centroids,
  join = st_contains,
  suffix = c("_zip", "_blk")
  )

glimpse(block_in_zcta)

# DIAGNOSTICS  =============================#

# check whether all work blocks in LODES have an assignment: 20 unique missing
il_lodes %>% 
  filter(!w_geocode %in% block_in_zcta$GEOID10_blk) %>% 
  distinct(w_geocode) %>% 
  nrow() 

# see which these are
il_lodes[which(!unique(il_lodes$w_geocode) %in% block_in_zcta$GEOID10_blk), "w_geocode"]

# 388 total jobs
il_lodes %>% 
  filter(!w_geocode %in% block_in_zcta$GEOID10_blk) %>% 
  summarize(sum(total_jobs))

# then repeat for home blocks in IL: 46 such blocks
il_lodes %>% 
  filter(h_stfips == "17", !h_geocode %in% block_in_zcta$GEOID10_blk) %>% 
  distinct(h_geocode) %>% 
  nrow()

# 1243 jobs
il_lodes %>% 
  filter(h_stfips == "17", !h_geocode %in% block_in_zcta$GEOID10_blk) %>% 
  summarize(sum(total_jobs))

# see which these are
il_lodes[which(!unique(il_lodes$h_geocode) %in% block_in_zcta$GEOID10_blk), "h_geocode"]

# lastly, are these blocks in the overall blocks file?
il_lodes %>% 
  filter(h_stfips == "17", # ignore blocks that are out of state and don't match
         !h_geocode %in% block_in_zcta$GEOID10_blk |
           !w_geocode %in% block_in_zcta$GEOID10_blk, # look for non-matches
         !h_geocode %in% il_block_centroids$GEOID10 |
           !w_geocode %in% il_block_centroids$GEOID10 # check if in all blocks
  ) %>% 
  nrow() # 0 obs, so this strategy should work

# ASSIGN NON-MATCHING TO NEAREST ZCTA  =============================#

missing_blocks <- il_lodes %>% 
  select(contains("geocode")) %>% 
  gather() %>% 
  distinct(value) %>% 
  filter(!value %in% block_in_zcta$GEOID10_blk,
         str_sub(value, 1, 2) == "17") %>% 
  dplyr::rename(
    "GEOID10" = "value"
  ) %>% 
  left_join(
    il_block_centroids,
    by = "GEOID10"
  ) %>% 
  st_as_sf() %>% 
  st_join(
    il_zcta,
    join = st_nearest_feature,
    suffix = c("_blk", "_zip")
  ) 

# FINALIZE BLOCK TO ZCTA ALLOCATION  =============================#

# combine poly and nearest blocks; drop geometry
block2zcta <- rbind(
  block_in_zcta %>% select(ZCTA5CE10, GEOID10_blk),
  missing_blocks %>% select(ZCTA5CE10, GEOID10_blk)
) %>% 
  st_drop_geometry() 

# check that we have ZIP assignments for all block pairs
il_lodes %>% 
  filter(h_stfips == "17",
         !w_geocode %in% block2zcta$GEOID10_blk |
           !h_geocode %in% block2zcta$GEOID10_blk) %>% 
  nrow() # hurray, 0

# save this file
save(block2zcta, file = "data/block2zcta.Rdata")

#===============================================================================#
# ALLOCATE LEHD
#===============================================================================#

# join IL lodes with block-ZCTA crosswalk and collapse to ZIP level
il_lodes_zip <- il_lodes %>% 
  left_join(
    block2zcta,
    by = c("w_geocode" = "GEOID10_blk")
  ) %>% 
  left_join(
    block2zcta,
    by = c("h_geocode" = "GEOID10_blk"),
    suffix = c("", "_h")
  ) %>% 
  dplyr::rename(
    "w_zip" = "ZCTA5CE10",
    "h_zip" = "ZCTA5CE10_h"
  ) %>% 
  mutate(
    h_zip = if_else(h_stfips != "17", "99999", as.character(h_zip)),
  ) %>% 
  group_by(w_zip, h_zip) %>% 
  summarize(
    total_jobs = sum(total_jobs) 
  )

# diagnostics
glimpse(il_lodes_zip)

# note warnings about implicit NA?
il_lodes_zip %>% filter(is.na(h_zip)) %>% glimpse() # 0 rows
il_lodes_zip %>% filter(is.na(w_zip)) %>% glimpse() # 0 rows

# save this intermediate file
save(il_lodes_zip, file = "data/IL_lodes_zip.csv")

