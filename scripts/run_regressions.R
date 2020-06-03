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
          "spdep",
          "spatialreg",
          "sphet")
lapply(libs, library, character.only = TRUE)

#===============================================================================#
# LOAD AND PREPARE DATA
#===============================================================================#

master <- load("data/MASTER_2020-06-03.Rdata")

il_zcta <- st_read("data/IL_ZCTA_simple.shp")
# sf::st_crs(il_zcta)
# plot(st_geometry(il_zcta))


# join with master
sp_master <- il_zcta %>% 
  left_join(master, by = c("ZCTA5CE10" = "zip")) %>% 
  mutate(
    confirmed_cases = replace_na(confirmed_cases, 0),
    ln_confirmed_cases = log(confirmed_cases / total),
    confirmed_cases_per_capita = confirmed_cases / total
  ) %>% 
  filter(!is.na(total),
         ZCTA5CE10 != "63736")

map(sp_master, ~sum(is.na(.)))

#===============================================================================#
# PLOTS
#===============================================================================#

# plot distribution of dependent variable
sp_master %>% 
  ggplot(aes(x = confirmed_cases_per_capita)) +
  geom_histogram()

sp_master %>% 
  ggplot(aes(x = ln_confirmed_cases)) +
  geom_histogram()

 sp_master %>% 
  st_as_sf() %>% 
  ggplot(aes(fill = confirmed_cases_per_capita)) +
  geom_sf(lwd = 0) +
  labs(fill = "Confirmed cases per capita",
       caption = "Source: ILDPH. Data as of May 19, 2020")
# ggsave("plots/IL_cases_per_capita.png")
 
 
#===============================================================================#
# MAKE KNN WEIGHTS
#===============================================================================#

# get centroid coords
coords <- sp_master %>%
  st_as_sf %>%
  st_centroid() %>%
  st_geometry()

# make KNN distance weights matrix with k = 4
knn4 <- knn2nb(knearneigh(coords, k = 4))
plot(st_geometry(st_as_sf((il_zcta))), border="gray")
plot(knn4, coords, add=TRUE, col ="blue")

# make 1st order queen contiguity weights
queen1 <- poly2nb(st_as_sf(sp_master))
summary(queen1)

plot(st_geometry(st_as_sf((sp_master))), border="gray")
plot(queen1, coords, add=TRUE, col ="blue", )

# # 9 problematic border ZIP
# missing <- il_zcta[c(126, 337, 1047, 1048, 1118, 1119, 1120, 1160, 1162), ]
# il_zcta <- il_zcta %>% 
#   filter(!ZCTA5CE10 %in% missing$ZCTA5CE10)
# 
# # make 1st order queen contiguity weights - everyone has a neighbor
# queen1 <- poly2nb(il_zcta)
# summary(queen1)


#===============================================================================#
# OLS SPECIFICATION
#===============================================================================#

chi_ols <- lm(confirmed_cases_per_capita ~ over60_share + 
            ptcommute_share + 
            uninsured_share +
            race_Black +
            race_Hisp +
            pop_density,
          data = sp_master)

summary(chi_ols)

# SPATIAL DIAGNOSTICS  =============================#

W_queen1 <- nb2listw(queen1)

# we definitely reject normality here
shapiro.test(sp_master$confirmed_cases_per_capita)

# heteroskedasticity and multicollinearity diagnostics
lmtest::bptest(chi_ols) 
car::vif(chi_ols)

moran <- lm.morantest(chi_ols, W_queen1, alternative = "two.sided")
moran$statistic

lm.LMtests(chi_ols, W_queen1, test="all")

queen1_wts <- map(W_queen1$weights, length) %>% unlist() %>% data.frame()
names(queen1_wts) <- "neighbor_count"

queen1_wts %>% 
  ggplot(aes(x = neighbor_count)) +
  geom_histogram(binwidth = 1, color = "gray10", lwd = 0.2) +
  labs(x = "Number of neighbors", 
       y = "Count")

# ggsave("plots/Queen_num_neighbors.png")
  
#===============================================================================#
# SPATIAL LAG MODEL
#===============================================================================#

lagmodel2 <- stsls(confirmed_cases_per_capita ~ over60_share + 
                     ptcommute_share + 
                     uninsured_share +
                     race_Black +
                     race_Hisp +
                     pop_density,
                   data = sp_master,
                   listw = W_queen1, 
                   robust = TRUE)
summary(lagmodel2)

# get impacts
sparse_W <- as(W_queen1, "CsparseMatrix")
trMC <- trW(sparse_W, type="MC")
impacts(lagmodel2, tr=trMC, R=100)
impacts(lagmodel2, listw = W_queen1) 

#===============================================================================#
# LEHD WEIGHTS
#===============================================================================#

# SPATIAL DIAGNOSTICS  =============================#


moran <- lm.morantest(chi_ols, flow_W, alternative = "two.sided")
moran$statistic

lm.LMtests(chi_ols, W_queen1, test="all")