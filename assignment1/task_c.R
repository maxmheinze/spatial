library(dplyr)
library(ggplot2)
library(sf)
library(data.table)
library(readr)
library(eurostat)
#-------Download NUTS shapefile and data of interest------------------
temp <- tempfile(fileext = ".zip")

download.file("http://ec.europa.eu/eurostat/cache/GISCO/distribution/v2/nuts/download/ref-nuts-2021-03m.shp.zip", temp)

outDir <- "./data"
unzip(temp, exdir=outDir)
unzip("./data/NUTS_RG_03M_2021_4326_LEVL_2.shp.zip", exdir=outDir)


#Data of interest at NUTS 2 level (Regional fertility rate)
EurostatTOC <- get_eurostat_toc()
df1 <- get_eurostat("tgs00100", time_format = "raw")
head(df1)


#------Read the shapefile and map the data to a new CRS-----------------
#Read shapefile
shp1 <- st_read(dsn = "./data", layer ="NUTS_RG_03M_2021_4326_LEVL_2") 

#Find out CRS of the shapefile
CRS <- st_crs(shp1)
print(CRS)

#Change CRS to EPSG 3752
shp1 <- st_transform(shp1, 
                     st_crs(3752))
#Remove overseas regions
overseas <- c("FRY1", "FRY2", "FRY3", "FRY4", "FRY5", "FRZZ", 
              "PT20", "PT30", "PTZZ", 
              "ES70", "ESZZ", 
              "NO0B", "NOZZ")
shp1 <- shp1[! shp1$NUTS_ID %in% overseas, ]


#------Merge shapefile and dataset---------------

df1 <- df1[df1$TIME_PERIOD == 2019, ]

df1 <- df1[!df1$geo %in% overseas,]

merged_shp1 <- left_join(shp1, df1, by=c("NUTS_ID"="geo"))


#Visualization 1 - Continious scale
ggplot() +
  geom_sf(data = merged_shp1, aes(fill = values)) +
  scale_fill_continuous(name = "Fertility Rate") +
  labs(title = "Fertility Rate by Region")
  theme_minimal()

#Visualization 2
