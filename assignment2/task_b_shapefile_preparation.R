
# Header ------------------------------------------------------------------

pacman::p_load(
  tidyverse,
  sf,
  magrittr
)


# Load --------------------------------------------------------------------

# For reproduction, exchange the file paths below with your local file paths
# Original shapefiles are downloaded from https://gadm.org/download_country.html

# arg2 <- read_sf("/Users/heinzemax/Downloads/gadm41_ARG_shp/gadm41_ARG_2.shp")
# bra2 <- read_sf("/Users/heinzemax/Downloads/gadm41_BRA_shp/gadm41_BRA_2.shp")
# pry2 <- read_sf("/Users/heinzemax/Downloads/gadm41_PRY_shp/gadm41_PRY_2.shp")


# Subset ------------------------------------------------------------------

# Argentina: Misiones, Corrientes
# Brazil: Rio Grande do Sul
# Paraguay: Misiones, Itapúa

arg2 %<>%
  filter(NAME_1 %in% c("Misiones", "Corrientes"))

bra2 %<>%
  filter(NAME_1 == "Rio Grande do Sul")

pry2 %<>%
  filter(NAME_1 %in% c("Misiones", "Itapúa"))


# Save --------------------------------------------------------------------

task_b_shapefile <- rbind(arg2, bra2, pry2)

st_write(task_b_shapefile, "./assignment2/data/task_b_shapefile_2.shp")



# Load Adm 0 and Adm 1 ----------------------------------------------------

# For reproduction, exchange the file paths below with your local file paths
# Original shapefiles are downloaded from https://gadm.org/download_country.html

# arg0 <- read_sf("/Users/heinzemax/Downloads/gadm41_ARG_shp/gadm41_ARG_0.shp")
# bra0 <- read_sf("/Users/heinzemax/Downloads/gadm41_BRA_shp/gadm41_BRA_0.shp")
# pry0 <- read_sf("/Users/heinzemax/Downloads/gadm41_PRY_shp/gadm41_PRY_0.shp")
# 
# arg1 <- read_sf("/Users/heinzemax/Downloads/gadm41_ARG_shp/gadm41_ARG_1.shp")
# bra1 <- read_sf("/Users/heinzemax/Downloads/gadm41_BRA_shp/gadm41_BRA_1.shp")
# pry1 <- read_sf("/Users/heinzemax/Downloads/gadm41_PRY_shp/gadm41_PRY_1.shp")



# Save --------------------------------------------------------------------

task_b_shapefile <- rbind(arg0, bra0, pry0)

st_write(task_b_shapefile, "./assignment2/data/task_b_shapefile_0.shp")

task_b_shapefile <- rbind(arg1, bra1, pry1)

st_write(task_b_shapefile, "./assignment2/data/task_b_shapefile_1.shp")
