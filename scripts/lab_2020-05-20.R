#===============================================================================#
# IN CLASS LAB
#
# Cecile Murray
#===============================================================================#

libs <- c("here",
          "tidyverse",
          "purrr",
          "knitr", 
          "kableExtra",
          "janitor",
          "spdep",
          "spatialreg",
          "sphet")
lapply(libs, library, character.only = TRUE)

#===============================================================================#
# LOAD AND PREPARE DATA
#===============================================================================#

load("data/MASTER.Rdata")

il_zcta <- st_read("data/IL_ZCTA_simple.shp")
# sf::st_crs(il_zcta)
# plot(st_geometry(il_zcta))

# join with master
sp_master <- master %>% 
  left_join(il_zcta,
            by = c("zip" = "ZCTA5CE10")) %>% 
  mutate(
    ln_confirmed_cases = log(confirmed_cases)
  )

#===============================================================================#
# MAKE WEIGHTS MATRICES
#===============================================================================#

# make 1st order queen contiguity weights
queen1 <- poly2nb(il_zcta)
summary(queen1)

# get centroid coords
coords <- sp_master %>%
  st_as_sf %>%
  st_centroid() %>%
  st_geometry()
# plot(st_geometry(il_zcta), border="grey")
# plot(queen1, coords, add=TRUE, col = "green")

# example of reading GAL file
# queen1b <- read.gal("FILENAME")

# make KNN distance weights matrix with k = 5
knn5 <- knn2nb(knearneigh(coords))
plot(st_geometry(il_zcta), border="grey")
plot(knn5, coords, add=TRUE, col ="blue")

#===============================================================================#
# REGRESSIONS
#===============================================================================#

# row standardized
W <- nb2listw(knn5)

ols <- lm(ln_confirmed_cases ~ over60_share + 
            ptcommute_share + 
            uninsured_share +
            race_Black +
            race_Hisp +
            pop_density +
            total,
          data = filter(sp_master, ln_confirmed_cases > 2))
summary(ols)

shapiro.test(sp_master$confirmed_cases)
lmtest::bptest(ols)
car::vif(ols)

lm.morantest(ols, W)
lm.LMtests(ols, W, test="all")

lagmodel <- lagsarlm(confirmed_cases ~ over60_share + 
                       ptcommute_share + 
                       uninsured_share +
                       race_Black +
                       race_Hisp +
                       pop_density +
                       total,
                     data = sp_master,
                     listw = W)
summary(lagmodel)
# spatial multiplier is 1/(1-rho): 

lagmodel2 <- stsls(confirmed_cases ~ over60_share + 
                     ptcommute_share + 
                     uninsured_share +
                     race_Black +
                     race_Hisp +
                     pop_density +
                     total,
                   data = sp_master,
                   listw = W, 
                   robust = TRUE)
summary(lagmodel2)

# now do error with max likelihood
errormodel <- errorsarlm(
  confirmed_cases ~ over60_share + 
    ptcommute_share + 
    uninsured_share +
    race_Black +
    race_Hisp +
    pop_density +
    total,
  data = sp_master,
  listw = W
)
summary(errormodel)

errormodel2 <- GMerrorsar(
  confirmed_cases ~ over60_share + 
    ptcommute_share + 
    uninsured_share +
    race_Black +
    race_Hisp +
    pop_density +
    total,
  data = sp_master,
  listw = W
)
summary(errormodel2)
