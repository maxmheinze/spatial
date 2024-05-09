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
raster_Africa_alt <- read_sf("./data/harari/raster_Africa.shp")

## Preparing for merging
raster <- raster_Africa %>%
  rename( "_ID" = CELLID,
          lon = longitude_,
          lat = latitude_m)

df_1 <- merge(geoconflict_main, raster, by = c("lat", "lon")) %>% 
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
                 year_ + country_, data = df_1)
summary(model_1)

## Model 2
## From equation 2: include all W*controls and W*country FE!
df_2 <- df_1 %>% 
  select(c(5, 7:15, 20, 46:51, 58:119, 121:167)) #I leave out one country (Zimbawe)

model_2 <- lm(ANY_EVENT_ACLED ~ . , data = df_2)
summary(model_2)

## Model 3
coor <- df_1 %>%
  filter(year_ == 2000) %>%
  select(cell, geometry) %>%
  st_as_sf()

distw <- dnearneigh(st_centroid(coor), 0, 180, row.names = coor$cell)
W <- nb2listw(distw, style = "B", zero.policy = TRUE) 

model_3 <- spml(ANY_EVENT_ACLED ~ lag(ANY_EVENT_ACLED) + SPEI4pg + L1_SPEI4pg + L2_SPEI4pg + 
                  GSmain_ext_SPEI4pg + L1_GSmain_ext_SPEI4pg + L2_GSmain_ext_SPEI4pg +
                  W_SPEI4pg + W_L1_SPEI4pg + W_L2_SPEI4pg +
                  W_GSmain_ext_SPEI4pg + W_L1_GSmain_ext_SPEI4pg + W_L2_GSmain_ext_SPEI4pg +
                  elevation_cell + rough_cell + area_cell + use_primary + dis_river_cell + shared +  border + any_mineral + ELF + 
                  W_elevation_cell + W_rough_cell + W_area_cell + W_use_primary + W_dis_river_cell + W_shared  + W_border + W_any_mineral + W_ELF +
                  country_ + year_,
             data = df_1[,1:73],
             index= c("cell","year_"),
             listw = W,
             model="pooling",
             effect = "time",
             spatial.error="none",
             lag = TRUE, 
             Hess = TRUE,
             local=list( parallel = T))
summary(model_3)


## Model 4
model_4 <- spml(ANY_EVENT_ACLED ~ SPEI4pg + L1_SPEI4pg + L2_SPEI4pg + 
                  GSmain_ext_SPEI4pg + L1_GSmain_ext_SPEI4pg + L2_GSmain_ext_SPEI4pg +
                  W_SPEI4pg + W_L1_SPEI4pg + W_L2_SPEI4pg +
                  W_GSmain_ext_SPEI4pg + W_L1_GSmain_ext_SPEI4pg + W_L2_GSmain_ext_SPEI4pg +
                  elevation_cell + rough_cell + area_cell + use_primary + dis_river_cell + shared +  border + any_mineral + ELF + 
                  W_elevation_cell + W_rough_cell + W_area_cell + W_use_primary + W_dis_river_cell + W_shared  + W_border + W_any_mineral + W_ELF +
                  country_:year_,
                data = df_1[,1:73],
                index= c("cell", "year_"),
                listw = W,
                model="pooling",
                effect = "time",
                spatial.error="none",
                lag = TRUE, 
                Hess = TRUE,
                local=list( parallel = T))
summary(model_4)

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
  
  # Initialize a matrix to store horizontal neighbors
  num_points <- nrow(centroids)
  horizontal_neighbors <- matrix(0, nrow = num_points, ncol = num_points)
  
  for (i in seq_len(num_points)) {
    # Find points within the same latitude band
    neighbor_ids <- which(latitudes == latitudes[i])
    
    # Store neighbor ids in the matrix
    horizontal_neighbors[i, neighbor_ids] <- 1
  }
  
  # Convert the matrix to a listw object
  listw <- mat2listw(horizontal_neighbors, style="B", zero.policy = TRUE)
  
  return(listw)
}

neighbors_vertical <- function(centroids) {
  
  # Extract longitudes
  longitudes <- st_coordinates(centroids)[,1]
  
  # Initialize a matrix to store vertical neighbors
  num_points <- nrow(centroids)
  vertical_neighbors <- matrix(0, nrow = num_points, ncol = num_points)
  
  for (i in seq_len(num_points)) {
    # Find points within the same longitude band
    neighbor_ids <- which(longitudes == longitudes[i])
    
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
plot(horizontal, coords, add=TRUE, col="blue", cex=0.5)
plot(vertical, coords, add=TRUE, col="red", cex=0.5)

## Vertical and Horizontal Version Model 3/4
model_3_ver <- spml(ANY_EVENT_ACLED ~ lag(ANY_EVENT_ACLED) + SPEI4pg + L1_SPEI4pg + L2_SPEI4pg + 
                  GSmain_ext_SPEI4pg + L1_GSmain_ext_SPEI4pg + L2_GSmain_ext_SPEI4pg +
                  W_SPEI4pg + W_L1_SPEI4pg + W_L2_SPEI4pg +
                  W_GSmain_ext_SPEI4pg + W_L1_GSmain_ext_SPEI4pg + W_L2_GSmain_ext_SPEI4pg +
                  elevation_cell + rough_cell + area_cell + use_primary + dis_river_cell + shared +  border + any_mineral + ELF + 
                  W_elevation_cell + W_rough_cell + W_area_cell + W_use_primary + W_dis_river_cell + W_shared  + W_border + W_any_mineral + W_ELF +
                  country_ + year_,
                data = df_1[,1:73],
                index= c("cell","year_"),
                listw = vertical,
                model="pooling",
                effect = "time",
                spatial.error="none",
                lag = TRUE, 
                Hess = TRUE,
                local=list( parallel = T))
summary(model_3_ver)

model_3_hor <- spml(ANY_EVENT_ACLED ~ lag(ANY_EVENT_ACLED) + SPEI4pg + L1_SPEI4pg + L2_SPEI4pg + 
                  GSmain_ext_SPEI4pg + L1_GSmain_ext_SPEI4pg + L2_GSmain_ext_SPEI4pg +
                  W_SPEI4pg + W_L1_SPEI4pg + W_L2_SPEI4pg +
                  W_GSmain_ext_SPEI4pg + W_L1_GSmain_ext_SPEI4pg + W_L2_GSmain_ext_SPEI4pg +
                  elevation_cell + rough_cell + area_cell + use_primary + dis_river_cell + shared +  border + any_mineral + ELF + 
                  W_elevation_cell + W_rough_cell + W_area_cell + W_use_primary + W_dis_river_cell + W_shared  + W_border + W_any_mineral + W_ELF +
                  country_ + year_,
                data = df_1[,1:73],
                index= c("cell","year_"),
                listw = horizontal,
                model="pooling",
                effect = "time",
                spatial.error="none",
                lag = TRUE, 
                Hess = TRUE,
                local=list( parallel = T))
summary(model_3_hor)


model_4_ver <- spml(ANY_EVENT_ACLED ~ SPEI4pg + L1_SPEI4pg + L2_SPEI4pg + 
                  GSmain_ext_SPEI4pg + L1_GSmain_ext_SPEI4pg + L2_GSmain_ext_SPEI4pg +
                  W_SPEI4pg + W_L1_SPEI4pg + W_L2_SPEI4pg +
                  W_GSmain_ext_SPEI4pg + W_L1_GSmain_ext_SPEI4pg + W_L2_GSmain_ext_SPEI4pg +
                  elevation_cell + rough_cell + area_cell + use_primary + dis_river_cell + shared +  border + any_mineral + ELF + 
                  W_elevation_cell + W_rough_cell + W_area_cell + W_use_primary + W_dis_river_cell + W_shared  + W_border + W_any_mineral + W_ELF +
                  country_:year_,
                data = df_1[,1:73],
                index= c("cell", "year_"),
                listw = vertical,
                model="pooling",
                effect = "time",
                spatial.error="none",
                lag = TRUE, 
                Hess = TRUE,
                local=list( parallel = T))
summary(model_4_ver)

model_4_hor <- spml(ANY_EVENT_ACLED ~ SPEI4pg + L1_SPEI4pg + L2_SPEI4pg + 
                  GSmain_ext_SPEI4pg + L1_GSmain_ext_SPEI4pg + L2_GSmain_ext_SPEI4pg +
                  W_SPEI4pg + W_L1_SPEI4pg + W_L2_SPEI4pg +
                  W_GSmain_ext_SPEI4pg + W_L1_GSmain_ext_SPEI4pg + W_L2_GSmain_ext_SPEI4pg +
                  elevation_cell + rough_cell + area_cell + use_primary + dis_river_cell + shared +  border + any_mineral + ELF + 
                  W_elevation_cell + W_rough_cell + W_area_cell + W_use_primary + W_dis_river_cell + W_shared  + W_border + W_any_mineral + W_ELF +
                  country_:year_,
                data = df_1[,1:73],
                index= c("cell", "year_"),
                listw = horizontal,
                model="pooling",
                effect = "time",
                spatial.error="none",
                lag = TRUE, 
                Hess = TRUE,
                local=list( parallel = T))
summary(model_4_hor)