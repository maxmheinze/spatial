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
                 year_ + country_, data = df)
summary(model_1)

## Model 2
## From equation 2: include all W*controls and W*country FE!
df_2 <- df_1 %>% 
  select(c(4,5, 7:15, 20, 46:51, 58:119, 121:167)) #I leave out one country (Zimbawe)

model_2 <- lm(ANY_EVENT_ACLED ~ . - cell , data = df_2)
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

#distw <- dnearneigh(st_coordinates(st_centroid(raster)), 0, 180000, row.names=raster$"_ID")

## Remove diagonal elements
diag(dist_threshold_matrix) <- 0

binary_matrix <- contiguity_matrix * dist_threshold_matrix
W <- mat2listw(binary_matrix, style = "B", zero.policy = TRUE) 

model_3 <- spml(ANY_EVENT_ACLED ~ . - cell,
             data = df_2,
             index= "cell",
             listw = W,
             model="pooling",
             lag=TRUE)
summary(model_3)

model_3 <- SDPDm(ANY_EVENT_ACLED ~ . - cell,
                data = df_2,
                index= c("cell", "year_"),
                W = binary_matrix,
                model = "sdm",
                effect = "twoways",
                dynamic = TRUE)
summary(model_3)

## Model 4
model_4 <- spml(ANY_EVENT_ACLED ~ . + country_:year_ - year_ - country_ - cell,
                data = df_2,
                index= "cell",
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

neighbors_horizontal <- function(centroids) {

  # Extract latitudes
  latitudes <- st_coordinates(centroids)[,2]
  
  # Initialize neighbors list
  neighbors_list <- vector("list", nrow(centroids))
  
  for (i in seq_len(nrow(centroids))) {
    # Find points within the same latitude band and within distance
    neighbor_ids <- latitudes == latitudes[i]
    
    # Exclude the point itself from its list of neighbors
    neighbor_ids <- neighbor_ids[neighbor_ids != i]
    
    # Store neighbor ids
    neighbors_list[[i]] <- as.integer(neighbor_ids)
  }
  
  return(neighbors_list)
}

neighbors_vertical <- function(centroids) {
  
  # Extract longitudes
  longitudes <- st_coordinates(centroids)[,1]
  
  # Initialize neighbors list
  neighbors_list <- vector("list", nrow(centroids))
  
  for (i in seq_len(nrow(centroids))) {
    # Find points within the same longitude band and within distance
    neighbor_ids <- which(longitudes - longitudes[i] == 0)
    
    # Exclude the point itself from its list of neighbors
    neighbor_ids <- neighbor_ids[neighbor_ids != i]
    
    # Store neighbor ids
    neighbors_list[[i]] <- as.integer(neighbor_ids)
  }
  
  return(neighbors_list)
}

# Apply the functions   
vertical <- neighbors_vertical(raster)
horizontal <- neighbors_horizontal(raster)

# Number of elements
n <- length(vertical)

# Initialize an adjacency matrix with 0s
adj_matrix_vertical <- matrix(0, nrow = n, ncol = n)
adj_matrix_horizontal <- matrix(0, nrow = n, ncol = n)

# Populate the adjacency matrix
for (i in seq_len(n)) {
  # Ensure indices are within the valid range
  valid_indices_vertical <- vertical[[i]][vertical[[i]] <= n]
  valid_indices_horizontal <- vertical[[i]][vertical[[i]] <= n]
  
  # Set matrix elements to 1
  adj_matrix_vertical[i, valid_indices_vertical] <- 1
  adj_matrix_horizontal[i, valid_indices_horizontal] <- 1
}

## Turn the matrices into listw objects
vertical_listw <- mat2listw(adj_matrix_vertical, style = "B", zero.policy=TRUE)
horizontal_listw <-mat2listw(adj_matrix_horizontal, style = "B", zero.policy=TRUE)

'The horizontal contiguity matrix means that only cells which share the same latitude 
are considered to be adjacent. Vertical contiguity means that only cells which share 
the same longitude are considered adjacent.'

plot(st_geometry(raster))
plot(horizontal_listw, coords, add=TRUE, col="green", cex=0.5)
