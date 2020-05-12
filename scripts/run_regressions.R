#===============================================================================#
# RUN REGRESSIONS
#
# Cecile Murray
#===============================================================================#

libs <- c("here",
          "tidyverse",
          "purrr",
          "knitr", 
          "kableExtra",
          "janitor",
          "sf",
          "spdep")
lapply(libs, library, character.only = TRUE)

# read in data
il_zcta <- st_read("data/IL_ZCTA_simple.shp") 
covid_zcta_shp <- st_read("data/covid_zcta.shp")

#===============================================================================#
# WHAT'S AN LM DO
#===============================================================================#

testreg <- lm(confirmed_cases ~ over60_share + ptcommute_share + 
                uninsured_share + race_Black + race_Hisp + pop_density,
              data = master)

chi_testreg <- lm(confirmed_cases ~ over60_share + ptcommute_share + 
                    uninsured_share + race_Black + race_Hisp + pop_density,
                  data = chi_master)


# make weights
coords <- st_centroid(covid_zcta_shp)
W_dist <- knn2nb(knearneigh(coords$geometry, 4))


# list.knn3 = knearneigh(il_zcta, k = 4, longlat = TRUE)
# W<-nb2listw(list.queen, style="W", zero.policy=TRUE)


moran.lm <- lm.morantest(testreg, W_dist, alternative="two.sided")
