
pacman::p_load(
  dplyr,
  ggplot2,
  sf
)

#Set up----------------------------------------------

#Load dataset and shapefile
load("./data/data1.rda")
shp <- st_read(dsn = "./data/EU27.shp")

#Select variables of interest
vars <- c("IDb", "pr80b", "pr103b", "lninv1b", "lndens.empb")
data <- data1[vars]

#Create productivity growth variable for countries of interest
merged_shp <- left_join(shp, data, by=c("Id"="IDb"))
countries <- c("AT", "DE","ES","FR","IT","PT")
df <- merged_shp[grep(paste(countries, collapse = "|"),merged_shp$Id),]
df <- na.omit(df)
df$prgrowth <- ((df$pr103b-df$pr80b)/df$pr80b)*100

#Visualization--------------------------------------------

# Cut the productivity growth values into five segments to plot them
breaks <- c(-20,-10, 0, 15, 30, 50)
labels <- c("(-20,-10)","(-10,0)", "(0,15)","(15,30)","(30,50)")
df$prgrowth_bins <- cut(df$prgrowth, breaks = breaks, labels = labels)

#Visualization of productivity growth rate
bin_colors <- c("lightyellow", "yellow", "orange", "red","brown")
ggplot() +
  geom_sf(data = df, aes(fill = prgrowth_bins)) +
  scale_fill_manual(name = "%", 
                    breaks = labels, 
                    values = bin_colors) +
  labs(title = "Productivity Growth Rate (1980-2003)") +
  theme_minimal()



