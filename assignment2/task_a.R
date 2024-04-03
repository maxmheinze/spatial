
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



#Comparing the matrices--------------------------------
#First, we can compute the sparsity of each matrix (proportion of zero elements).
sp1 <- sum(w1 == 0)/length(w1)
sp2<- sum(w2 == 0)/length(w2)
sp3 <- sum(w3 == 0)/length(w3)
sparsity <- c(sp1, sp2, sp3)
print(sparsity)
#We observe that the elements of the matrices based on a distance threshold and on contiguity are mostly zero.
# The matrix based on a smooth distance-decay function has a much smaller proportion of zero elements (only 103, the diagonal elements)
# On the one hand, given the small distance threshold imposed and the large number of regions it's not surprising that w1 and w3 have a lot of zeros, representing no link with most neighbors.
#On the other hand, since we use a decaying exponential function to construct w2, even the regions that are furthest away from each other have a positive value. 
# Therefore, the only zero elements come from the diagonal.

#Then we compute their eigenvalues.
eigenw1 <- eigen(w1)$value
eigenw2 <- eigen(w2)$value
eigenw3 <- eigen(w3)$value

#Since each matrix has 103 eigenvectors, we focus on the largest 5.
eigentop1 <- eigenw1[1:5]
eigentop2 <- eigenw2[1:5]
eigentop3 <- eigenw3[1:5]

eigenvalues <- cbind(eigentop1, eigentop2, eigentop3)
print(eigenvalues)

#We observe that the top 5 eigenvalues of w1 and w3 are larger than those of w2. This shows there is a stronger spatial autocorrelation level in w1 and w2.
#On the other hand, the eigenvalues of w1 and w3 appear to decrease gradually, whereas those of w2 decrease more rapidly. This indicates a faster decay in spatial autocorrelation with distance.

#Now, in order to analyze the matrices from a graph theory perspective, we convert the weight matrices into adjacency (binary) matrices.
d1 <- ifelse(w1 > 0, 1, 0)
d2 <- ifelse(w2 > 0, 1, 0)
d3 <- ifelse(w3 > 0, 1, 0)

#Once we have adjacency matrices, we can compute their degree distribution.Note that since w2 has no zero off-diagonal elements, every region is connected to each other,
#so there is no point on computing the degree distribution of d2.

node_degrees1 <- rowSums(d1)
node_degrees3 <- rowSums(d3)

hist(node_degrees1, main = "Degree Distribution D1", xlab = "Node Degree", ylab = "Frequency", xlim = c(0,25), ylim = c(0,25))
hist(node_degrees3, main = "Degree Distribution D3", xlab = "Node Degree", ylab = "Frequency", xlim = c(0,15), ylim = c(0,25))


#Plotting the matrices---------------------------
v1 <- raster(w1)
v2 <- raster(w2)
v3 <- raster(w3)
plot(v1)
plot(v2)
plot(v3)

#We observe that w1 and w3 represent a network there are less connections between regions, but the existing links are stronger than those of w2.
#w2, on the other hand, represents a much more interconnected network, where each region is connected to each other but the links are weaker on average.
#w2 can help us visualize the clusters in the network. To do so, we must take into account how the countries are ordered in the matrix.
#The order (from first rows(columns) to last) is Austria, Germany, Spain, France, Italy and Portugal. (This is because the region codes are ordered alphabetically
# AT, DE, ES, FR, IT, PT). Looking at w2 we observe the biggest cluster is located among the Austrian and German regions (top-left corner). Then, the smaller cluster
#in the middle represents Spain. As we can see, Spanish regions have a almost non-existing link with Austria and Germany, and their stronger link are related to Portugal (middle of the bottom (or right) part).
#Then, in the diagonal below Spain we see France. We can see how it is the country with more connections to the rest of countries.
#Below in the diagonal we observe the cluster formed by the Italian regions. We see that their strongest foreign links are with Austrian regions.
#Finally, in the very bottom-right corner we see Portugal. It is clear how Portuguese regions are isolated from all countries except Spain.


#Computing a measure of spatial autocorrelation for productivity growth------------------------------------------

#To measure spatial autocorrelation, we will compute a Global Moran's I statistic for each matrix.

#(1) Distance threshold matrix
w1listw <- mat2listw(w1, style = "W") #We tranform w1 into a listw so that moran.test() works
i1 <- moran.test(df$prgrowth, w1listw)
i1 <- i1$estimate["Moran I statistic"]
#(2) Smooth distance-decay  matrix
w2listw <- mat2listw(w2, style = "W")
i2 <- moran.test(df$prgrowth, w2listw)
i2 <- i2$estimate["Moran I statistic"]
#(3) Contiguity matrix
w3listw <- mat2listw(w3, style = "W", zero.policy = TRUE)
i3 <- moran.test(df$prgrowth, w3listw)
i3 <- i3$estimate["Moran I statistic"]

#I's statistics comparison
I_score_comparison <- c("",i1, i2, i3)
names(I_score_comparison) <- c("I's score","w1", "w2", "w3")
print(I_score_comparison)

#The Global Moran's I score measures spatial autocorrelation, indicating the degree of similarity between neighboring regions in 
#terms of  productivity growth rate. We observe that w1 and w3 have a similar value around 0.55, indicating a moderate positive spatial autocorrelation between the regions.
#The I statistic of matrix w2 is somewhat smaller, implying a lower spatial autocorrelation, though it still shows signs of clustering.

