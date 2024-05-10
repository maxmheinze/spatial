##  EXERCISE B

# PACKAGES ---------------------------------------------------------------------

pacman::p_load(spatialreg, bsreg, patchwork,
               gridExtra, fixest, splm, stringi,
               stringr, stringdist, haven, sf, dplyr, fuzzyjoin, 
               comparator, digest, zoomerjoin, tidyr,
               fixest, conleyreg, plm, stargazer,
               tidyverse, tmap, spdep, SDPDmod, generics, knitr, 
               kableExtra, formatR, readxl,
               haven, flextable, broom, units)

# LOADING DATA -----------------------------------------------------------------

intersect_coord <- read_dta("./assignment3/data/harari/intersect_coord.dta")
geoconflict_main <- read_dta("./assignment3/data/harari/geoconflict_main.dta", encoding = "UTF-8")
raster_Africa <- st_read("./assignment3/data/harari/raster_Africa.shp")

## Preparing for merging
raster <- raster_Africa %>%
  rename( "_ID" = CELLID,
          lon = longitude_,
          lat = latitude_m)

df_1 <- merge(geoconflict_main, raster, by = c("_ID","lat", "lon")) %>% 
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
model_1 <- plm(ANY_EVENT_ACLED ~ SPEI4pg + L1_SPEI4pg + L2_SPEI4pg +
                 GSmain_ext_SPEI4pg + L1_GSmain_ext_SPEI4pg + L2_GSmain_ext_SPEI4pg + 
                 elevation_cell + rough_cell + area_cell + as.factor(use_primary) + 
                 dis_river_cell + as.factor(shared) + as.factor(border) + as.factor(any_mineral) + ELF +
                 as.factor(year_) + i(country_, as.numeric(year_)),
               data = df_1,
               index = c("cell", "year_"),
               model = "pooling")
summary(model_1)

## Model 2
## From equation 2: include all W*controls and W*country FE!
model_2 <- plm(ANY_EVENT_ACLED ~ SPEI4pg + L1_SPEI4pg + L2_SPEI4pg +
                 GSmain_ext_SPEI4pg + L1_GSmain_ext_SPEI4pg + L2_GSmain_ext_SPEI4pg +
                 W_SPEI4pg + W_L1_SPEI4pg + W_L2_SPEI4pg +
                 W_GSmain_ext_SPEI4pg + W_L1_GSmain_ext_SPEI4pg + W_L2_GSmain_ext_SPEI4pg +
                 elevation_cell + rough_cell + area_cell + as.factor(use_primary) + 
                 dis_river_cell + as.factor(shared) + as.factor(border) + as.factor(any_mineral) + ELF +
                 W_elevation_cell + W_rough_cell + W_area_cell + W_use_primary + 
                 W_dis_river_cell + as.factor(W_shared)  + as.factor(W_border) + as.factor(W_any_mineral) + W_ELF +
                 as.factor(year_) + i(country_, as.numeric(year_)),
               data = df_1,
               index = c("cell", "year_"),
               model = "pooling")
summary(model_2)

## Model 3
coor <- df_1 %>%
  filter(year_ == 2000) %>%
  select(cell, geometry) %>%
  st_as_sf(crs = "WGS84")

distw <- dnearneigh(st_centroid(coor), 0, 180, row.names = coor$cell)
W <- nb2listw(distw, style = "B", zero.policy = TRUE) 
distmat <- listw2mat(W)



model_3 <- spml(ANY_EVENT_ACLED ~ lag(ANY_EVENT_ACLED) + SPEI4pg + L1_SPEI4pg + L2_SPEI4pg +
                  GSmain_ext_SPEI4pg + L1_GSmain_ext_SPEI4pg + L2_GSmain_ext_SPEI4pg +
                  W_SPEI4pg + W_L1_SPEI4pg + W_L2_SPEI4pg +
                  W_GSmain_ext_SPEI4pg + W_L1_GSmain_ext_SPEI4pg + W_L2_GSmain_ext_SPEI4pg +
                  elevation_cell + rough_cell + area_cell + as.factor(use_primary) + 
                  dis_river_cell + as.factor(shared) + as.factor(border) + as.factor(any_mineral) + ELF +
                  W_elevation_cell + W_rough_cell + W_area_cell + as.factor(W_border) +
                  as.factor(W_use_primary) + W_dis_river_cell + as.factor(W_shared) + as.factor(W_any_mineral) + W_ELF +
                  as.factor(country_):as.numeric(year_),
                data = df_1,
                index= c("cell","year_"),
                listw = W,
                model="pooling",
                effect = "time",
                spatial.error = "none", 
                zero.policy = TRUE, 
                lag=TRUE,
                dynamic = TRUE,
                local=list( parallel = T))
summary(model_3)

df_4 <- df_1 %>%
  mutate(countryyear = paste0(country_, "_", year_))


## Model 4
model_4 <- spml(ANY_EVENT_ACLED ~ lag(ANY_EVENT_ACLED) + SPEI4pg + L1_SPEI4pg + L2_SPEI4pg + 
                  GSmain_ext_SPEI4pg + L1_GSmain_ext_SPEI4pg + L2_GSmain_ext_SPEI4pg +
                  W_SPEI4pg + W_L1_SPEI4pg + W_L2_SPEI4pg +
                  W_GSmain_ext_SPEI4pg + W_L1_GSmain_ext_SPEI4pg + W_L2_GSmain_ext_SPEI4pg +
                  elevation_cell + rough_cell + area_cell + as.factor(use_primary) + dis_river_cell + 
                  as.factor(shared) + as.factor(border) + as.factor(any_mineral) + ELF + 
                  W_elevation_cell + W_rough_cell + W_area_cell + as.factor(W_use_primary) + W_dis_river_cell + 
                  as.factor(W_shared)  + as.factor(W_border) + as.factor(W_any_mineral) + W_ELF +
                  as.factor(countryyear),
                data = df_4,
                index= c("cell", "year_"),
                listw = W,
                model="pooling",
                effect = "individual",
                spatial.error="none",
                zero.policy = TRUE,
                dynamic = TRUE,
                lag = TRUE, 
                Hess = TRUE,
                local=list( parallel = T))
summary(model_4)

mod4 <- spml(ANY_EVENT_ACLED ~ lag(ANY_EVENT_ACLED) + SPEI4pg + L1_SPEI4pg + L2_SPEI4pg + GSmain_ext_SPEI4pg + L1_GSmain_ext_SPEI4pg + L2_GSmain_ext_SPEI4pg + W_GSmain_ext_SPEI4pg + 
               W_L1_GSmain_ext_SPEI4pg + W_L2_GSmain_ext_SPEI4pg + W_SPEI4pg + W_L1_SPEI4pg + W_L2_SPEI4pg + elevation_cell + rough_cell + area_cell + as.factor(use_primary) + 
               dis_river_cell + as.factor(shared) +  as.factor(border) + as.factor(any_mineral) +
               ELF + W_elevation_cell + W_rough_cell + W_area_cell + W_ELF + as.factor(W_any_mineral) + as.factor(W_shared)  + 
               W_dis_river_cell + as.factor(W_use_primary) + as.factor(countryyear), # country x year FE
             data = df_4, 
             index= c("cell","year_"),
             listw = W,
             model = "pooling",  # no fixed effects
             effect = "individual", 
             spatial.error="none",
             zero.policy = TRUE, 
             dynamic = TRUE,
             lag = TRUE, 
             Hess = TRUE,
             local=list( parallel = T)) # makes it faster

# VERTICAL HORIZONTAL CONTIGUITY -----------------------------------------------
'The horizontal (vertical) contiguity matrix means that only cells which share
the same latitude (longitude) are considered to be adjacent.'

plot(st_geometry(coor))
coords <- st_coordinates(st_centroid(coor))
rook_nb <- poly2nb(coor, row.names=coor$cell, queen=FALSE)
plot(rook_nb, coords, add=TRUE, col="green", cex=0.5)

neighbors_horizontal <- function(centroids) {
  
  # Extract latitudes
  latitudes <- st_coordinates(centroids)[,2]
  distances <- st_distance(centroids)
  
  # Initialize a matrix to store horizontal neighbors
  num_points <- nrow(centroids)
  horizontal_neighbors <- matrix(0, nrow = num_points, ncol = num_points)
  
  for (i in seq_len(num_points)) {
    # Find points within the same latitude band
    neighbor_ids <- which(round(latitudes,2) == round(latitudes[i],2) & distances[i,] < set_units(180000, "m"))
    
    # Store neighbor ids in the matrix
    horizontal_neighbors[i, neighbor_ids] <- 1
    
    }
  
  # Convert the matrix to a listw object
  listw <- mat2listw(horizontal_neighbors, style="B", zero.policy = TRUE)
  
  return(listw)
}

neighbors_vertical <- function(centroids) {
  
  longitudes <- st_coordinates(centroids)[,1]
  distances <- st_distance(centroids)
  
  # Initialize a matrix to store vertical neighbors
  num_points <- nrow(centroids)
  vertical_neighbors <- matrix(0, nrow = num_points, ncol = num_points)
  
  for (i in seq_len(num_points)) {
    
    neighbor_ids <- which(round(longitudes,2) == round(longitudes[i],2) & distances[i,] < set_units(180000, "m"))
    
    # Store neighbor ids in the matrix
    vertical_neighbors[i, neighbor_ids] <- 1
  }
  
  # Convert the matrix to a listw object
  listw <- mat2listw(vertical_neighbors, style="B", zero.policy = TRUE)
  
  return(listw)
}

# Apply the functions   
horizontal <- neighbors_horizontal(st_centroid(coor))
vertical <- neighbors_vertical(st_centroid(coor))

# Try visualize network to see if correct
plot(st_geometry(coor))
coords <- st_coordinates(st_centroid(coor))
plot(horizontal, coords, add=TRUE, col="blue", cex=0.5)
plot(vertical, coords, add=TRUE, col="red", cex=0.5)
