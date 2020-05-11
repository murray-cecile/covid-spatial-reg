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


# make weights
list.queen = poly2nb(il_zcta, queen = TRUE)
W<-nb2listw(list.queen, style="W", zero.policy=TRUE)


moran.lm<-lm.morantest(testreg, W, alternative="two.sided")
