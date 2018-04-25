# Cluster Analysis 

# Preliminaries 

library(cluster)
library(factoextra)
library(ggplot2)
library(NbClust)


# Prepare Data 

swiss.scaled <- scale(swiss)  # Talk About Euclidean Distance How To 

# Step 1: Do Clusters Even Exist? 

fviz_pca_ind(prcomp(swiss.scaled,   # Discuss Principle Component Analysis 
                    title = "Normal Data", 
                    palette = "jco", geom = "point"))

Hopkins <- get_clust_tendency(swiss.scaled, # A numeric data frame or matrix
                              n=nrow(swiss.scaled)-1, #The number of points selected from sample
                              seed = 123) #Specify seed for reproducible results 

Hopkins$hopkins_stat

# Step 2: Calculate Multivariate Distances

distgower <- daisy(swiss.scaled, # Matrix with either dissimilarity matrix or normal data
                   metric = "gower", # Use gower if there are categorical variables 
                   stand = TRUE) # If TRUE then the measurements are standardized beforehand


##### Visualize the Matrix 

DistanceMap <- fviz_dist(dist.obj = distgower, #The Distance Object
                         show_labels = TRUE, 
                         lab_size =4) + 
  labs(title = "Distance Matrix")

DistanceMap


# Step 3: Clustering Using K-Means 

# Discuss how the Algorithm Works 

set.seed(123) # Since this algorithm starts with k randomly selected centroid it is recomended to set the seed
km.res <- kmeans(df.scaled, centers = 6, 
                 iter.max = 250,  #The maximum number of iterations allowed 
                 nstart = 25) # How many random centers should be choosen 
head(km.res)

# Its possible to add the cluster assignments to the original data set 
aggregate(swiss, by = list(cluster = km.res$cluster), mean)
swiss2 <- cbind(swiss, cluster = km.res$cluster)
head(swiss2)

# Visualizing K- Means
fviz_cluster(km.res, # Object created by a clusting function such as kmeans
             data = df, # The data used for clustering 
             choose.vars = c("Fertility", "Catholic"), #We can cluster based on select variables 
             stand = TRUE, #If TRUE data has been standardizes
             geom = "point") 


## What is the Right Number of Clusters? 

#### Method 1: Elbow Method Using FactoExtra 

fviz_nbclust(swiss.scaled, # Numeric matrix or data frame
             FUNcluster = kmeans, # Partitioning method (kmeans, pam, diana)
             method = "wss") #Used for estimating possible clusters


# wss indicates that it will create clusters such that the total intra-cluster variation is minimized.  So basically 
#it will make the clusters as compact as possible

#### Method 2 : Silhouette Method Using FactoExtra 
fviz_nbclust(swiss.scaled, 
             kmeans, 
             method = "silhouette") + 
  theme_classic()
# if we leave the diss command = to NULL then it will calculate distances for us

# The silhouette method measures the quality of clustering. The maximum silhouete value is the optimal clusters


#### Method 3 : GAP Statistic 
fviz_nbclust(swiss.scaled, 
             kmeans, 
             nstart = 25, 
             method = "gap_stat", 
             nboot = 500) +  # The number of Monte Carlo "bootstrap" samples
  labs(subtitle = "Gap Statistic Method")

#### Method 4: Test Them All At Once 

nb <- NbClust(swiss.scaled, 
              distance = "euclidean",
              min.nc = 2, # Minimum Number of Clusters 
              max.nc = 10, # Maximum Number of Clusters 
              method = "kmeans")
fviz_nbclust(nb)


################### Validating Our Results ####################
# eclust is a faster way to perform clustering, the FUNcluster allows us to put practically any method in

km.res <- eclust(swiss.scaled, # A numeric data frame
                 stand = FALSE,
                 FUNcluster = "kmeans",  #The type of clustering you want to do 
                 hc_metric = "manhattan",
                 k = 4, #If NULL the Gap Stat is used to find the number of clusters 
                 nstart = 25, 
                 graph = TRUE)

### I AM PURPOSEFULLY PUTTING 6 in TO Show How we can Validate

fviz_cluster(km.res, geom = "point", ellipse.type = "norm")

fviz_silhouette(km.res, palette = "jco", ggtheme = theme_classic())

# Extracting The Observations That Are in The Wrong Cluster!

silinfo <- km.res$silinfo
silinfo$clus.avg.widths

# Determining The Bad Observations 

negSil <- which(sil[, 'sil_width'] < 0)
sil[negSil, , drop = FALSE]


# Look What Silhouette we get when clusters are 2 

km.res <- eclust(swiss, 
                 "kmeans", 
                 k = 2,
                 stand = TRUE, #If TRUE then Eclust will standardize data
                 nstart = 25, 
                 graph = FALSE)
fviz_cluster(km.res, geom = "point", ellipse.type = "norm")

fviz_silhouette(sil.obj = km.res, #An object produced by a clustering function such as eclust of pam
                palette = "jco", 
                ggtheme = theme_classic())
# The Silhouette method is a cluster validation approach that measures how well an observation is clustered

################## APPROACH 2: Hierarchical Clustering ##################

res.agnes <- agnes( x = distgower,# dissimilarity matrix
                    diss = TRUE, # If TRUE the x is assumed to be dissimilarity matrix 
                    stand = TRUE, #If TRUE then the measurements in x are standardized before calculating dissimilarities
                    metric = "euclidiean", #Metric for Distance 
                    method = "ward") #Linkage Method

fviz_dend(res.agnes, # Cluster output
          cex = 0.6) # size of labels 


## How do we determing the number of clusters in Hierarchical? 

nb <- NbClust(swiss.scaled, #Data Must Be Scaled 
              distance = "euclidean", 
              min.nc = 2, 
              max.nc = 10, 
              method = "ward.D2")

fviz_dend(res.agnes,
          cex = 0.6, # size of labels 
          k = 2, # number of clusters in dendrogram 
          rect = TRUE) # Put a rectangle around the clusters



########################## Visualizing Dendrograms #######################

fviz_dend(res.agnes, cex = 0.6, k = 2, 
          rect = TRUE, 
          rect_border = c("red", "blue"), 
          rect_fill = TRUE, 
          lower_rect = -5) 

fviz_dend(res.agnes, k =2, k_colors = "jco", type = "phylogenic", 
          repel = TRUE, phylo_layout = "layout.gem")


fviz_dend(res.agnes, k = 2, k_colors = "jco", type = "phylogenic", 
          repel = TRUE , phylo_layout = "layout_as_tree")

fviz_dend(res.agnes, cex = 0.6, k = 2, type = "circular", rect = TRUE)


########################## Calculate P -Values #####################

library(pvclust)
data("lung")
head(lung)


set.seed(123)
ss <- sample(1:73, 30) #extract 20 samples out of
newdata <- lung[, ss]
res.pv <- parPvclust(cl = NULL,newdata, method.hclust = "average", method.dist = "correlation", nboot =300, iseed = NULL)

plot(res.pv, hang = -1, cex = 0.5)


