# Packages
library("ggplot2")

# Parameters
set.seed(2073)
n_clusters <- 3
n_per_cluster <- 30
max_iter <- 10

# Setup
cluster_centers_x <- runif(min=-5, max=5, n=n_clusters)
cluster_centers_y <- runif(min=-5, max=5, n=n_clusters)
data <- data.frame(matrix(NA, nrow=0, ncol=3))
names(data) <- c("x","y","cluster")
for (i in 1:n_clusters) {
  data_temp <- data.frame(matrix(NA, nrow=n_per_cluster, ncol=2))
  names(data_temp) <- c("x","y")
  data_temp$cluster <- i
  data_temp$x <- cluster_centers_x[i] + rnorm(n_per_cluster)
  data_temp$y <- cluster_centers_y[i] + rnorm(n_per_cluster)
  data <- rbind(data, data_temp)
}

# Plot the "truth"
plot(data$x, data$y, col=data$cluster)

# Initialize k-means
centroids <- data.frame(cluster=1:n_clusters, x=NA, y=NA)
centroids$x <- runif(n=n_clusters, min=min(data$x), max=max(data$x))
centroids$y <- runif(n=n_clusters, min=min(data$y), max=max(data$y))
points(centroids$x, centroids$y, col="orange", pch=15)

# Update...
converged <- FALSE
iter <- 0
data2 <- data
while(converged == FALSE & iter < max_iter) {
  
  iter <- iter + 1
  
  # Compute distances and assign clusters
  distances <- as.data.frame(sapply(1:n_clusters, function(i) {
    sqrt((data$x - centroids$x[i])^2 + (data$y - centroids$y[i])^2)
  }))
  data$cluster <- apply(distances, 1, which.min)
  
  # Update centroids
  new_centroids <- data %>%
    group_by(cluster) %>%
    summarise(x = mean(x), y = mean(y), .groups = "drop")
  
  # Check for convergence (if centroids do not change)
  if (all(abs(new_centroids$x - centroids$x) < 1e-6) &&
      all(abs(new_centroids$y - centroids$y) < 1e-6)) {
    converged <- TRUE
  }
  
  # Update centroid positions
  centroids <- new_centroids
  
  # Plot progress
  plot(data$x, data$y, col=data$cluster, main=paste("Iteration", iter))
  points(centroids$x, centroids$y, col="orange", pch=15, cex=2)
  Sys.sleep(0.5)  # Pause to visualize iterations
}