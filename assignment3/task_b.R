##  EXERCISE B

# PACKAGES ---------------------------------------------------------------------

library(pacman)
p_load(
  haven,
  sf,
  sp,
  data.table,
  tidyverse,
  dplyr,
  stargazer,
  SDPDmod,
  spatialreg,
  spdep,
  units
)

# LOADING DATA -----------------------------------------------------------------

intersect_coord <- read_dta("./data/harari/intersect_coord.dta")
geoconflict_main <- read_dta("./data/harari/geoconflict_main.dta", encoding = "UTF-8")
raster_Africa <- st_read("./data/harari/raster_Africa.shp")

## Preparing for merging
raster <- raster_Africa %>%
  rename( "_ID" = CELLID,
          lon = longitude_,
          lat = latitude_m)

df <- merge(geoconflict_main, raster, by = c("_ID", "lat", "lon")) %>% 
  filter(!(country_largest_share %in% c("Western Sahara", "Gambia"))) %>% #these countries are not listed in the online appendix
  select(-c("ctry_18", "ctry_46", "W_ctry_18", "W_ctry_46")) %>%  #corresponding dummy columns - not sure is needed
  rename(year_ = year,
         country_ = country_largest_share) %>% 
  mutate(year_ = as.factor(year_),
         country_ = gsub(" ", "_", country_),
         country_ = as.factor(country_),
         cell = as.factor(cell))

## Number of observations is still not correct! They have 35042
colSums(subset(geoconflict_main, select = 73:120) == 1)

# REPLICATION TABLE 2 ----------------------------------------------------------
## Model 1
model_1 <- lm(ANY_EVENT_ACLED ~ SPEI4pg + L1_SPEI4pg + L2_SPEI4pg +
                 GSmain_ext_SPEI4pg + L1_GSmain_ext_SPEI4pg + L2_GSmain_ext_SPEI4pg + 
                 elevation_cell + rough_cell + area_cell + use_primary + 
                 dis_river_cell + shared + border + any_mineral + ELF +
                 year_ + country_, data = df)
summary(model_1)

## Model 2
# From equation 2: include all W*controls and W*country FE!
df_alternative <- df %>% 
  select(c(2, 7:15, 20, 46:51, 58:117, 119:163))
model_2 <- lm(ANY_EVENT_ACLED ~. , data = df_alternative)
summary(model_2)

## Model 3
contiguity_matrix <- mOrdNbr(raster, m =1)

# queen_nb <- poly2nb(raster, row.names=raster$"_ID", queen=TRUE)
# B.list.queen <- nb2listw(queen_nb, style = "B", zero.policy=TRUE)
# B.queen <- listw2mat(B.list.queen)

## Setting a 180 km cutoff
dist_matrix <- st_distance(raster)
dist_threshold <- set_units(180000, "m")
dist_threshold_matrix <- ifelse(dist_matrix > dist_threshold, 0, 1)

# Remove diagonal elements
diag(dist_threshold_matrix) <- 0

binary_matrix <- contiguity_matrix * dist_threshold_matrix

#distw <- dnearneigh(st_coordinates(st_centroid(raster)), 0, 180000, row.names=raster$"_ID")

# Dynamic spatial Durbin model with spatial and time fixed effects (with Lee-Yu transformation)
model_3 <- SDPDm(ANY_EVENT_ACLED ~ SPEI4pg + L1_SPEI4pg + L2_SPEI4pg +
                  GSmain_ext_SPEI4pg + L1_GSmain_ext_SPEI4pg + L2_GSmain_ext_SPEI4pg + 
                  elevation_cell + rough_cell + area_cell + use_primary + 
                  dis_river_cell + shared + border + any_mineral + ELF,
                W = binary_matrix,
                data = df,
                dynamic = TRUE,
                index = c("country_", "year_"),
                model = "sdm")
summary(model_3)
