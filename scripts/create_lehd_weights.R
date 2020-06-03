#===============================================================================#
# CREATE LEHD-BASED SPATIAL WEIGHTS
#
# Cecile Murray
#===============================================================================#

libs <- c("here",
          "tidyverse",
          "purrr",
          "janitor",
          "sp",
          "sf")
lapply(libs, library, character.only = TRUE)

load("data/IL_lodes_zip.csv")

# chi_zips <- read_csv("data/CHI_METRO_MASTER.csv") %>% select(zip)

#===============================================================================#
# CREATE PROPORTIONAL WEIGHTS AND RESHAPE
#===============================================================================#

# steps for origin-based and destination-based weights:
# - compute sum of jobs originating (ending) in each ZIP
# - compute job_share for each ZIP pair, with above sum as denominator
# - recode any share that is less than 0.01 as 0
# - recode so that no ZIP is its own neighbor
# check whether the rows and colums of the two matrices match
# finally, sum the resulting origin- and destination-based matrices


# create row-normalized orgin-based matrix
origin_weights <- il_lodes_zip %>% 
  ungroup() %>% 
  filter(h_zip %in% sp_master$ZCTA5CE10 | w_zip %in% sp_master$ZCTA5CE10) %>% 
  dplyr::rename(
    "jobs" = "total_jobs"
  ) %>% 
  group_by(h_zip) %>% 
  mutate(
    h_zip_jobs = sum(jobs),
    w_zip_share = jobs / h_zip_jobs
  ) %>% 
  mutate(
    w_zip_share = if_else(w_zip_share < 0.01, 0, w_zip_share), # drop small shares
    w_zip_share = if_else(w_zip == h_zip, 0, w_zip_share) # can't be own neighbor
  ) %>% 
  ungroup() %>% 
  select(
    h_zip,
    w_zip,
    w_zip_share
  ) %>% 
  pivot_wider(names_from = "w_zip", values_from = "w_zip_share") 
  
    
# create row_normalized destination-based matrix
destination_weights <- il_lodes_zip %>% 
  filter(h_zip %in% sp_master$ZCTA5CE10 | w_zip %in% sp_master$ZCTA5CE10) %>% 
  ungroup() %>% 
  dplyr::rename(
    "jobs" = "total_jobs"
  ) %>% 
  group_by(w_zip) %>% 
  mutate(
    w_zip_jobs = sum(jobs),
    h_zip_share = jobs / w_zip_jobs
  ) %>% 
  mutate(
    h_zip_share = if_else(h_zip_share < 0.01, 0, h_zip_share), # drop small shares
    h_zip_share = if_else(w_zip == h_zip, 0, h_zip_share) # can't be own neighbor
  ) %>% 
  ungroup() %>% 
  select(
    h_zip,
    w_zip,
    h_zip_share
  ) %>% 
  pivot_wider(names_from = "h_zip", values_from = "h_zip_share")

# CHECK RESULTS  =============================#

# dimension check
nrow(destination_weights) == ncol(origin_weights) - 1
nrow(origin_weights) == ncol(destination_weights) - 1

# make sure rows and cols are sorted in same (ascending) order
origin_weights$h_zip == select(destination_weights, -w_zip) %>% names
destination_weights$w_zip == select(origin_weights, -h_zip) %>% names

# COMBINE  =============================#

# now combine them
flow_matrix <- select(origin_weights, -h_zip) +
  t(select(destination_weights, -w_zip))

#===============================================================================#
# TURN FLOW MATRIX INTO SPATIAL POINTS OBJECT
#===============================================================================#

# test out strategy from this post
# https://stackoverflow.com/questions/43436466/create-grid-in-r-for-kriging-in-gstat
test_ext <- raster::extent(0, 5, 0, 3)
test_grid <- raster::raster(test_ext)
test_poly <- raster::rasterToPolygons(test_grid)
plot(test_poly)

# define extent, then make into spatial polygons
ext <- raster::extent(0, ncol(flow_matrix), 0, nrow(flow_matrix))
grid <- raster::raster(ext,
                       nrows = nrow(flow_matrix),
                       ncols = ncol(flow_matrix))
flow_poly <- raster::rasterToPolygons(grid)
plot(flow_poly)

# now make rook contiguity weights and set weight value?
flow_nb <- poly2nb(flow_poly, queen = FALSE)
flow_W <- nb2listw(flow_nb)
                 