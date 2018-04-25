# Cluster Analysis 
# April 1 2018 
# Luke Taylor Daniels 
View(state.na)
data("swiss") #Load your data set 
df <- swiss 
df <- na.omit(swiss)  #Remove any missing values that are present in the data set
View(df)
df.scaled <- scale(df)
df.scaled
#We standardize the data using the function scale()
head(df, n=3)

df.scaled
##### Required R Packages

library(ggplot2)
library(factoextra)
library(cluster)
library(latexpdf)
library(purrr)

# There are many R functions for computing distances between pairs of observations 
# 1.) dist() - Base R - Accepts only numeric data! 
# 2.) get_dist() - - factoextra package - Accepts only numeric data, but supports correlation based distance measures!
# 3.) daisy() - clusterpackage - Able to handle any variable type! 


disteucl <- get_dist( x = df, method = "euclidean", stand = TRUE) #Stand = TRUE indicates that the variables will be scaled! 
euclmatrix <- as.matrix(disteucl)
View(euclmatrix)


#### But what if we Have variables that are binary or categorical? - Use the Daisy Function in Cluster

distgower <- daisy(df.scaled, metric = "gower", stand= FALSE)
distgower                                       

euclmatrix <- as.matrix(disteucl)
View(euclmatrix)


##### Visualizing the Distance Matrix 

DistanceMap <- fviz_dist(disteucl, order = TRUE, show_labels = TRUE, lab_size = 4, gradient = list(low = "red", mid = "white", high = "blue")) + labs(title = "Distance Similarities")
DistanceMap
# dist.obj = distance matrix, order should always equal TRUE to visualize, This is a ggplot extension so we can manipulate it as if we are in ggplot! 




######################################################## Step 2 

### Using Base R for calculating cluster means

set.seed(123) # Since this algorithm starts with k randomly selected centroids, its recomended to use set.seed for Rs random number generator
km.res <- kmeans(df.scaled, centers = 4, iter.max = 250, nstart = 25)
print(km.res) 

# Its possible to compute the mean of each variable by clusters using the original data:
aggregate(swiss, by=list(cluster=km.res$cluster), mean)
dd <- cbind(swiss, cluster = km.res$cluster) #We can add the clusters assignments to the original frame 
head(dd)

# We can also access the various pieces of information in kmeans 

km.res$size #Items in each cluster 
km.res$centers # The Cluster Means 

# Visuzlizing K- Means 
swiss
fviz_cluster(km.res, data = df.scaled, choose.vars = c("Fertility", "Catholic"), stand = TRUE,
             geom = c("point", "text"), repel = TRUE,
             show.clust.cent = TRUE, ellipse = TRUE, ellipse.type = "confidence",
             ellipse.level = 0.95, ellipse.alpha = 0.2,
            labelsize = 8, main = "Swiss Cluster plot", xlab = NULL,
             ylab = NULL, outlier.color = "black", outlier.shape = 19,
             ggtheme = theme_classic())

# However We manually assigned the cluster value! How do we know the correct number?

##### Method 1: The Elbow Method in Base R

set.seed(123)

# We want the within sqaures for clusters 2 - 15. 
k.max <- 15 
wss <- sapply(1:k.max, function(k){kmeans(df,k, nstart = 50)$tot.withinss})
wss
plot(1:k.max, wss, type = "b", pch= 19, frame = FALSE, # type = b indicated we want points joined by lines
     xlab = "Number of Clusters K",      # pch = the shape of the point we can, 19 indicates a circle 
     ylab = "Total within-custers sum of squares")

#### The Elbow Method Using FactoExtra ###
fviz_nbclust(df.scaled, FUNcluster = kmeans, method = "wss") #FunCluster is the type of clustering we want


fviz_nbclust(df.scaled, FUNcluster = kmeans, method = "wss") +
  geom_vline(xintercept = 6, linetype = 2)





### The Silhouette Method Base R ###

avg_sil <- function(k){ 
  km.res <- kmeans(df.scaled, centers = k, nstart = 25)
 ss <- silhouette(km.res$cluster, dist(df))
  mean(ss[, 3])
}

# Compute and plot wss for k = 2 to k =15
k.values <- 2:15

#extracting silhouettes
avg_sil_values <- map_dbl(k.values, avg_sil)

plot(k.values, avg_sil_values,
type = "b", pch = 19, frame = FALSE, 
xlab = "Number of Cluster K",
ylab = "Average Silhouettes")


#### Computing means with k = 4 clusters 

set.seed(100)
km.res <- kmeans(df, 4, nstart = 50)
km.res



##### Silhouette Method Using FactoExtra ######

fviz_nbclust(df, kmeans, method = "silhouette") + theme_classic()


##### Now we can go back to the start to redefine our cluster analysis 

km.res <- kmeans(df.scaled, centers = 2, iter.max = 3, nstart = 25)
print(km.res) 

fviz_cluster(km.res, data = df.scaled, choose.vars = NULL, stand = TRUE,
             geom = c("point", "text"), repel = TRUE,
             show.clust.cent = TRUE, ellipse = TRUE, ellipse.type = "confidence",
             ellipse.level = 0.95, ellipse.alpha = 0.2,
             labelsize = 8, main = "Swiss Cluster plot", xlab = NULL,
             ylab = NULL, outlier.color = "black", outlier.shape = 19,
             ggtheme = theme_classic())


##### THE GAP STATISTIC FOR NUMBER OF CLUSTERS ####

fviz_nbclust(df, kmeans, nstart = 25, method = "gap_stat", nboot = 500) + 
  labs(subtitle = "Gap Statistic Method")



library("NbClust")
nb <- NbClust(df, distance = "euclidean", min.nc = 2,   
                                                      #NbClust package provides 30 indices for determining the number of clusters and proposes to user the best clustering scheme from the different results obtained by varying all combinations of number of clusters, distance measures, and clustering methods.
              max.nc = 10, method = "kmeans")

fviz_nbclust(nb)



######### VALIDATING OUR CLUSTERS #########
library(fpc)
hc.res <- eclust(df, "hclust", k= 6 , hc_metric = "euclidean",
                 hc_method = "ward.D2", graph = FALSE)
fviz_dend(hc.res, show_labels = TRUE, palette = "jco", repel = TRUE, cex = .5,as.ggplot =TRUE) + 
  theme(axis.text.x=element_text(size = 10 , angle=90))

km.res <- eclust(df, "kmeans", k = 6, nstart = 25, graph = FALSE)
fviz_cluster(km.res, geom = "point", ellipse.type = "norm", ggtheme = theme_minimal())


fviz_silhouette(km.res, palette = "jco", ggtheme = theme_classic())

# Extracting silhouette values 

silinfo <- km.res$silinfo
silinfo

silinfo$clus.avg.widths

# Determining the bad observations 
km.res$silinfo$widths # Look at the width of each observation 
sil <- km.res$silinfo$widths
sil
neg_sil_index <- which(sil[, 'sil_width'] < 0)
sil[neg_sil_index, , drop = FALSE]



############################# Partitioning Using PAM 
dev.off()
DataNew <- iris[,-5]
View(DataNew)
DataNew <- na.omit(DataNew)
DataNew.scaled <- scale(DataNew)

distgower <- daisy(DataNew, metric = "gower", stand= TRUE)

fviz_nbclust(x = DataNew, FUNcluster = cluster::pam, method = "gap_stat", nboot = 500) + 
  labs(subtitle = "Gap Statistic Method")

pam.res <- pam(DataNew,3)
print(pam.res)

nb <- NbClust(DataNew, distance = "euclidean", min.nc = 2,   
              #NbClust package provides 30 indices for determining the number of clusters and proposes to user the best clustering scheme from the different results obtained by varying all combinations of number of clusters, distance measures, and clustering methods.
              max.nc = 10, method = "kmeans")

fviz_nbclust(nb)

pam.res <- pam(DataNew,3)
print(pam.res)

fviz_cluster(pam.res, data = DataNew, choose.vars = NULL, stand = TRUE,
             geom = c("point", "text"), repel = TRUE,
             show.clust.cent = TRUE, ellipse = TRUE, ellipse.type = "confidence",
             ellipse.level = 0.95, ellipse.alpha = 0.2,
             labelsize = 8, main = "Swiss Cluster plot", xlab = NULL,
             ylab = NULL, outlier.color = "black", outlier.shape = 19,
             ggtheme = theme_classic())


#########################################################
# Hierarchical Clustering #

#### IN Base R ###

res.dist <- dist(swiss, method= "euclidean") 

# The linkage function takes the distance information and groups 
# pairs of objects into clusters based on their similarity. 

res.hc <- hclust(d = res.dist, method = "ward.D2") #ward minimizes the total within-cluster variance 

#Print Dendogram 

fviz_dend(res.hc, cex = 0.5)

# One way to measure how well the cluster tree generated reflects your data is to compute the cophenetic distances and the original distance data
# If the clustering is valid, the linking of objects in the cluster tree should have a strong corelation 

res.coph <- cophenetic(res.hc) 
cor(res.dist, res.coph) #Valus above 0.75 are considered good 

# One problem with heirarchical clustering is that is does not tell how many clusters there are.
# The base R function cuttree() can be used 

#Cut tree into 4 groups 

grp <- cutree(res.hc, k = 4)
head(grp, n = 4)

# Number of elements in each cluster 

table(grp)
# The result of the cuts can be visualized using dviz_dend()

fviz_dend(res.hc, k = 4, cex = 0.5, k_colors = c("blue", "red", "green", "purple"),
          color_labels_by_k = TRUE, rect = TRUE) + theme_void() # add rectangle around groups 



#### The Approach Using Cluster ####
# These functions perform all the necessary steps for you.  You don't need to execute 
# the scale(), dist() and hclust() separately. 

res.agnes <- agnes(x = df, #data frame
                   stand = TRUE, #standardize the data
                   metric = "euclidiean", #metric for distance matrix 
                   method = "ward") #Linkage Method


fviz_dend(res.agnes, cex = 0.6, k =4, rect = TRUE) #Now we don't need the long code to cut the tree in order to visualize
fviz_dend(res.agnes, cex = 0.6, k =4, rect = TRUE, horiz =TRUE, lower_rect = -5) #Now we don't need the long code to cut the tree in order to visualize
fviz_dend(res.agnes, cex = 0.5, k =4, type = "circular", rect = TRUE)

##################################

######### Visualizing Dendograms ########

fviz_dend(res.agnes, cex = 0.6, k =4, rect = TRUE, color_labels_by_k = TRUE, 
          rect_border = c("red", "green", "blue", "purple"), rect_fill = TRUE, lower_rect = -5)

# Different Way 

require("igraph")
fviz_dend(res.agnes, k =4, k_colors = "jco", type = "phylogenic", repel = TRUE, phylo_layout = "layout.gem")
fviz_dend(res.agnes, k = 4, k_colors = "jco", type = "phylogenic", relep = TRUE , phylo_layout = "layout_as_tree")

#### We can zoom in on the dendogram if we have a really large data set 

# let get the original back up 

fviz_dend(res.agnes, cex = 0.6, k =4, rect = TRUE, color_labels_by_k = TRUE, 
          rect_border = c("red", "green", "blue", "purple"), rect_fill = TRUE, lower_rect = -5)

fviz_dend(res.agnes, xlim = c(1,20), ylim = c(-2, 3))


#### Saving the Dendrogram into a large PDF ####

pdf("dendrogram.pdf", width = 10, height = 15)
p <- fviz_dend(res.agnes, cex = 0.6, k =4, rect = TRUE, color_labels_by_k = TRUE, 
               rect_border = c("red", "green", "blue", "purple"), rect_fill = TRUE, lower_rect = -5)
print(p)
dev.off()

############################ VALIDATION ######

# A big issue in cluster analysis is that clustering methods will return clusters even if the data does not contain
# anly clusters. Thus, you blinkly apply a cluster method when it is not appropriate

DataFrame2 <- iris[,-5]
# random data generated from the iris data set 
random_df <- apply(DataFrame, 2, function(x){runif(length(x), 
                                            min(x), (max(x)))})
random_df <- as.data.frame(random_df)

# Standardize the data sets 
DataFrame2 <- iris.scaled <- scale(DataFrame2)
random_df <- scale(random_df)

# We can view the two data sets using factoextra and a Princimple component Analysis 
# the fviz_pca is a principal component analysis that reduces the dimensionality of the multivariate data

fviz_pca_ind(prcomp(DataFrame2), title = "Normal Data", habillage = iris$Species, #habillage is an obtional factor variable for coloring the observations by group 
             palette = "jco", geom = "point")

fviz_pca_ind(prcomp(random_df), title = "random Data", geom = "point")

# From this we know there are three meaningful clusters

# The Hopkins tests, tests the spatial randomness of the data 
# If the Hopkins Stat is < 0.5 then it is unlikely that the data has significant clusters 
# We us get_clust_tendency in the factoextra package
set.seed(123)
res <- get_clust_tendency(DataFrame2, n = nrow(DataFrame2)-1, graph = TRUE)
res$hopkins_stat
res

res <- get_clust_tendency(random_df, n = nrow(random_df)-1, graph = TRUE)
res$hopkins_stat

#################### Calculating the P-Values

library(pvclust)
data("lung")
head(lung)
View(lung)

set.seed(123)
ss <- sample(1:73, 30) #extract 20 samples out of
newdata <- lung[, ss]
res.pv <- parPvclust(cl = NULL,newdata, method.hclust = "average", method.dist = "correlation", nboot =1000, iseed = NULL)

plot(res.pv, hang = -1, cex = 0.5)



###################### DENSITY CLUSTERING 


#### Computing the PAM clustering ####

pam.res <- pam(df, 2)
pam.res

pam.res$medoids

fviz_cluster( pam.res, palette = c("blue", "red"), 
              ellipse.type = "t", 
              repel = TRUE,  # Avoids overplotting labels
              ggtheme = theme_classic() )
              
              Kassambara, Alboukadel. Practical Guide to Cluster Analysis in R: Unsupervised Machine Learning (Multivariate Analysis Book 1) (p. 46). Kindle Edition. 
