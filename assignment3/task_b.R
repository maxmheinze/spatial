# REPLICATION

getwd()
library(pacman)
p_load(
  haven,
  dplyr,
  sf,
  tmap,
  data.table,
  viridis,
  tidyverse,
  stargazer
)

intersect_coord <- read_dta("./data/harari/intersect_coord.dta")
geoconflict_main <- read_dta("./data/harari/geoconflict_main.dta")
raster_Africa <- st_read("./data/harari/raster_Africa.shp")

raster <- raster_Africa %>% 
  rename( "_ID" = CELLID,
          lon = longitude_,
          lat = latitude_m)

df <- merge(geoconflict_main, raster, by = c("_ID", "lat", "lon"), y.all = TRUE)

model <- lm(ANY_EVENT_ACLED ~ SPEI4pg+ L1_SPEI4pg+ L2_SPEI4pg+
               GSmain_ext_SPEI4pg+ L1_GSmain_ext_SPEI4pg+ L2_GSmain_ext_SPEI4pg+
               W_SPEI4pg+ W_L1_SPEI4pg+ W_L2_SPEI4pg+ 
               W_GSmain_ext_SPEI4pg+ W_L1_GSmain_ext_SPEI4pg+ W_L2_GSmain_ext_SPEI4pg+
               elevation_cell+ rough_cell+ area_cell+ use_primary+ 
               dis_river_cell+ shared+ border+ any_mineral+ ELF+
               as.factor(year)+ as.factor(country_largest_share), data = df)
summary(model)
