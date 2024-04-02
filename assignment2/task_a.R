
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



#Create weight matrices---------------
#Based on distance threshold

#Create centroids of regions
centr <- st_centroid(df)
coord <- st_coordinates(centr)
dist <- dnearneigh(coord, 0, 10000, row.names=df$Id)
summary(dist)

#It appears that setting a maximum value of 10000 in the distance threshold yields too many links per region.
#Therefore, we will compute the minimum value of the max. distance threshold so that no region is isolated.

#Minimum distance threshold so there are no disconnected regions
k1 <- knearneigh(coord, k=1)
k1 <- knn2nb(k1)
link.max <- max(unlist(nbdists(k1, coords=coord)))
link.max

#We use the obtained maximum value of the threshold
dist <- dnearneigh(coord, 0, link.max, row.names=df$Id)
summary(dist)
#With this new distance threshold, each region has an average of 8.3 links.

#Now, we create a weight matrix, where each region has a positive value if its kth neighbor is below
#the maximum distance threshold (2.65) and 0 if it is above.
w1 <- nb2mat(dist, style= "W")


#Smooth distance-decay-----------------------------------------------------------------------
#First, we create a smooth distance-decay function so that the value decreases as distance between regions increases

function0 <- function(x){
  exp(-0.4*x)
}

#Then, we create a matrix by transforming the distance matrix that contains the distances between centroids with the smooth distance-decay function 
distance <- st_distance(centr)
g2 <- matrix(NA, nrow=nrow(distance), ncol=ncol(distance))

for(i in seq_along(distance)){
  g2[i]<-function0(distance[i])
}

#Finally, we row-normalize the matrix and set its diagonal to 0 to create a weight matrix.
w2 <- g2/rowSums(g2)
diag(w2) <- 0

#Contiguity-based matrix-------------------------------------------------------------------------------------------
#We construct a matrix such that in the row of each region each element will be positive for contiguous neighbors
# and 0 otherwise. 
contiguity <- poly2nb(df, row.names=df$Id, queen=TRUE)

w3 <- nb2mat(contiguity, style = "W", zero.policy = TRUE)