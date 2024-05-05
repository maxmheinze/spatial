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
  units,
  splm
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
  #filter(!(country_largest_share %in% c("Western Sahara", "Gambia"))) %>% #these countries are not listed in the online appendix
  filter(!(year == 1997)) %>%
  rename(year_ = year,
         country_ = country_largest_share) %>% 
  mutate(year_ = as.factor(year_),
         country_ = gsub(" ", "_", country_),
         country_ = as.factor(country_),
         cell = as.factor(cell))

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
  select(c(4,5, 7:15, 20, 46:51, 58:119, 121:167))
model_2 <- lm(ANY_EVENT_ACLED ~ . - cell , data = df_alternative)
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
W <- mat2listw(binary_matrix, style = "W")
#distw <- dnearneigh(st_coordinates(st_centroid(raster)), 0, 180000, row.names=raster$"_ID")

model_3 <- spml(ANY_EVENT_ACLED ~ . - cell,
             data = df_alternative,
             index=c("cell","year_"),
             listw = W,
             model="pooling",
             lag=TRUE)
summary(model_3)

## Model 4
model_4 <- spml(ANY_EVENT_ACLED ~ . + country_:year_ - year_ - country_ - cell,
                data = df_alternative,
                listw = W,
                model="pooling",
                lag=TRUE)
summary(model_4)

# VERTICAL HORIZONTAL CONTIGUITY -----------------------------------------------
plot(st_geometry(raster))
coords <- st_coordinates(st_centroid(raster))

queen_nb <- poly2nb(raster, row.names=raster$"_ID", queen=TRUE)
plot(queen_nb, coords, add=TRUE, col="red", cex=0.5)
B.list.queen <- nb2listw(queen_nb, style = "B", zero.policy=TRUE)
B.queen <- listw2mat(B.list.queen)

rook_nb <- poly2nb(raster, row.names=raster$'_ID', queen=FALSE)
plot(rook_nb, coords, add=TRUE, col="green", cex=0.5)
