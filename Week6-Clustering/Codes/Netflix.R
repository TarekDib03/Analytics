# Introduction to Clustering

# Download and loading data
# Copy Url and download data into the movieLens file
fileUrl <- "http://files.grouplens.org/datasets/movielens/ml-100k/u.item"
download.file(fileUrl, destfile = "movieLens.txt")
movies <- read.table("movieLens.txt", header=FALSE, sep="|",quote="\"")

# Add column names
colnames(movies) = c("ID", "Title", "ReleaseDate", "VideoReleaseDate", "IMDB", "Unknown", "Action", 
                     "Adventure", "Animation", "Childrens", "Comedy", "Crime", "Documentary", "Drama", 
                     "Fantasy", "FilmNoir", "Horror", "Musical", "Mystery", "Romance", "SciFi", 
                     "Thriller", "War", "Western")
str(movies)

# Process data
# Remove unnecessary variables
movies$ID = NULL
movies$ReleaseDate = NULL
movies$VideoReleaseDate = NULL
movies$IMDB = NULL

# Remove duplicates
movies = unique(movies)
# Take a look at our data again:
str(movies)

# number of Comedy movies
sum(movies$Comedy == 1)
# Western movies
sum(movies$Western == 1)
# Romance and drama
sum(movies$Romance == 1 & movies$Drama == 1)


# Cluster Analysis

# Compute distances. Cluster on genres. Columns 2 through 20
distances = dist(movies[2:20], method = "euclidean")

# Hierarchical clustering. ward cares about the distance between clusters using the centroid distance
# and also the variance in each of the cluster
clusterMovies = hclust(distances, method = "ward") 

# Plot the dendrogram
plot(clusterMovies)

# Assign points to clusters
clusterGroups = cutree(clusterMovies, k = 10)

# Now let's figure out what the clusters are like.
# Let's use the tapply function to compute the percentage of movies in each genre and cluster

tapply(movies$Action, clusterGroups, mean)
tapply(movies$Romance, clusterGroups, mean)

# We can repeat this for each genre. If you do, you get the results in ClusterMeans.ods

# AN ADVANCED APPROACH TO FINDING CLUSTER CENTROIDS
spl = split(movies[2:20], clusterGroups)
#spl[[1]] is the same as subset(movies[2:20], clusterGroups == 1)
lapply(spl, colMeans)

# Find which cluster Men in Black is in.
subset(movies, Title=="Men in Black (1997)")
clusterGroups[257]

# Create a new data set with just the movies from cluster 2
cluster2 = subset(movies, clusterGroups==2)

# Look at the first 10 titles in this cluster:
cluster2$Title[1:10]


'''Run the cutree function again to create the cluster groups, but this time pick k = 2 clusters. 
It turns out that the algorithm groups all of the movies that only belong to one specific genre in one 
cluster (cluster 2), and puts all of the other movies in the other cluster (cluster 1). What is the 
genre that all of the movies in cluster 2 belong to?'''
clusterGroups2 = cutree(clusterMovies, k = 2)
spl = split(movies[2:20], clusterGroups2)
lapply(spl, colMeans)
