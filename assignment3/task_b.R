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

df_1 <- merge(geoconflict_main, raster, by = c("_ID", "lat", "lon")) %>% 
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
  st_as_sf(coords = c("lat", "lon"), crs = "WGS84") %>% 
  filter(year_ == 2000)
distw <- dnearneigh(st_centroid(coor), 0, 180)
W <- nb2listw(distw, style = "B", zero.policy = TRUE) 

model_3 <- spml(ANY_EVENT_ACLED ~ SPEI4pg + L1_SPEI4pg + L2_SPEI4pg + 
                  GSmain_ext_SPEI4pg + L1_GSmain_ext_SPEI4pg + L2_GSmain_ext_SPEI4pg +
                  W_SPEI4pg + W_L1_SPEI4pg + W_L2_SPEI4pg +
                  W_GSmain_ext_SPEI4pg + W_L1_GSmain_ext_SPEI4pg + W_L2_GSmain_ext_SPEI4pg +
                  elevation_cell + rough_cell + area_cell + use_primary + dis_river_cell + shared +  border + any_mineral + ELF + 
                  W_elevation_cell + W_rough_cell + W_area_cell + W_use_primary + W_dis_river_cell + W_shared  + W_border + W_any_mineral + W_ELF +
                  country_ + year_,
             data = df_1,
             index="cell",
             listw = W,
             model="pooling",
             effect = "time",
             spatial.error="none",
             lag=TRUE)
summary(model_3)


## Model 4
model_4 <- spml(ANY_EVENT_ACLED ~ SPEI4pg + L1_SPEI4pg + L2_SPEI4pg + 
                  GSmain_ext_SPEI4pg + L1_GSmain_ext_SPEI4pg + L2_GSmain_ext_SPEI4pg +
                  W_SPEI4pg + W_L1_SPEI4pg + W_L2_SPEI4pg +
                  W_GSmain_ext_SPEI4pg + W_L1_GSmain_ext_SPEI4pg + W_L2_GSmain_ext_SPEI4pg +
                  elevation_cell + rough_cell + area_cell + use_primary + dis_river_cell + shared +  border + any_mineral + ELF + 
                  W_elevation_cell + W_rough_cell + W_area_cell + W_use_primary + W_dis_river_cell + W_shared  + W_border + W_any_mineral + W_ELF +
                  country_:year_,
                data = df_1,
                index="cell",
                listw = W,
                model="pooling",
                effect = "time",
                spatial.error="none",
                lag=TRUE)
summary(model_4)

# VERTICAL HORIZONTAL CONTIGUITY -----------------------------------------------
'The horizontal (vertical) contiguity matrix means that only cells which share
the same latitude (longitude) are considered to be adjacent.'

plot(st_geometry(raster))
coords <- st_coordinates(st_centroid(raster))
rook_nb <- poly2nb(raster, row.names=raster$'_ID', queen=FALSE)
plot(rook_nb, coords, add=TRUE, col="green", cex=0.5)

neighbors_horizontal <- function(centroids) {

  latitudes <- st_coordinates(centroids)[,2]
  distances <- st_distance(centroids)
  
  # Initialize neighbors list
  neighbors_list <- vector("list", nrow(centroids))
  
  for (i in seq_len(nrow(centroids))) {
    # Find points within the same latitude band
    neighbor_ids <- latitudes[i] == latitudes[i] & distances[i,] < set_units(180000, "meters")
    
    # Exclude the point itself from its list of neighbors
    neighbor_ids <- neighbor_ids[neighbor_ids != i]
    
    # Store neighbor ids
    neighbors_list[[i]] <- as.integer(neighbor_ids)
  }
  
  return(neighbors_list)
}

neighbors_vertical <- function(centroids) {
  
  longitudes <- st_coordinates(centroids)[,1]
  distances <- st_distance(centroids)
  
  # Initialize neighbors list
  neighbors_list <- vector("list", nrow(centroids))
  
  for (i in seq_len(nrow(centroids))) {
    # Find points within the same longitude band
    neighbor_ids <- longitudes[i] == longitudes[i] & distances[i,] < set_units(180000, "meters")
    
    # Exclude the point itself from its list of neighbors
    neighbor_ids <- neighbor_ids[neighbor_ids != i]
    
    # Store neighbor ids
    neighbors_list[[i]] <- as.integer(neighbor_ids)
  }
  
  return(neighbors_list)
}

# Apply the functions   
horizontal <- neighbors_horizontal(raster)
vertical <- neighbors_vertical(raster)

# Try visualize network to see if correct
# plot(st_geometry(raster))
# plot(vertical_listw, coords, add=TRUE, col="green", cex=0.5)
