# Packages
library("ggplot2")
library("plotly")

# Parameters
set.seed(2073)
n_clusters <- 3
n_per_cluster <- 30
max_iter <- 10

# Setup
cluster_centers_x <- runif(min=-5, max=5, n=n_clusters)
cluster_centers_y <- runif(min=-5, max=5, n=n_clusters)
cluster_centers_z <- runif(min=-5, max=5, n=n_clusters)
data <- data.frame(matrix(NA, nrow=0, ncol=4))
names(data) <- c("x","y","z", "cluster")
for (i in 1:n_clusters) {
  data_temp <- data.frame(matrix(NA, nrow=n_per_cluster, ncol=3))
  names(data_temp) <- c("x","y","z")
  data_temp$cluster <- i
  data_temp$x <- cluster_centers_x[i] + rnorm(n_per_cluster)
  data_temp$y <- cluster_centers_y[i] + rnorm(n_per_cluster)
  data_temp$z <- cluster_centers_y[i] + rnorm(n_per_cluster)
  data <- rbind(data, data_temp)
}

# Plot the "truth"
p <- plot_ly(data, x = ~x, y = ~y, z = ~z, color = ~factor(cluster), colors = "Set1",
             type = "scatter3d", mode = "markers")
p

# Initialize k-means
centroids <- data.frame(cluster=1:n_clusters, x=NA, y=NA)
centroids$x <- runif(n=n_clusters, min=min(data$x), max=max(data$x))
centroids$y <- runif(n=n_clusters, min=min(data$y), max=max(data$y))
centroids$z <- runif(n=n_clusters, min=min(data$z), max=max(data$z))
p %>%
  add_trace(data = centroids, x = ~x, y = ~y, z = ~z, type = "scatter3d",
            mode = "markers", marker = list(size = 10, color = "orange", symbol = "x"))

# Update...
converged <- FALSE
iter <- 0
data2 <- data
while(converged == FALSE & iter < max_iter) {

  iter <- iter + 1
  
  # Compute distances and assign clusters
  distances <- as.data.frame(sapply(1:n_clusters, function(i) {
    sqrt((data$x - centroids$x[i])^2 + (data$y - centroids$y[i])^2 + (data$z - centroids$z[i])^2)
  }))
  
  data$cluster <- apply(distances, 1, which.min)
  
  # Update centroids
  new_centroids <- data %>%
    group_by(cluster) %>%
    summarise(x = mean(x), y = mean(y), z = mean(z), .groups = "drop")
  
  # Fix missing clusters: Reinitialize intelligently
  if (nrow(new_centroids) < n_clusters) {
    missing_clusters <- setdiff(1:n_clusters, new_centroids$cluster)
    for (mc in missing_clusters) {
      # Find farthest existing point instead of random choice
      farthest_point <- data[which.max(rowSums(distances)), 1:3]
      new_centroids <- bind_rows(new_centroids, tibble(cluster = mc, x = farthest_point$x, y = farthest_point$y, z = farthest_point$z))
    }
    new_centroids <- arrange(new_centroids, cluster)
  }
  
  # Fix convergence check (corrected z comparison)
  if (all(abs(new_centroids$x - centroids$x) < 1e-6) &&
      all(abs(new_centroids$y - centroids$y) < 1e-6) &&
      all(abs(new_centroids$z - centroids$z) < 1e-6)) {
    converged <- TRUE
  }
  
  # Update centroid positions
  centroids <- new_centroids
  
  # Plot progress
  p <- plot_ly(data, x = ~x, y = ~y, z = ~z, color = ~factor(cluster), colors = "Set1",
               type = "scatter3d", mode = "markers") %>% 
    add_trace(data = centroids, x = ~x, y = ~y, z = ~z, type = "scatter3d",
              mode = "markers", marker = list(size = 10, color = "orange", symbol = "x")) %>% 
    layout(title = paste("Iteration", iter))
  
  p
  
}