# Packages
library("SNPRelate")
library("qrcode")
library("magick")
library("tidyverse")
library("plotly")
library("GGally")
library("GWASTools")
library("GWASdata")

##### Setup
### Genetics data
genofile <- snpgdsOpen(snpgdsExampleFileName())
geno_data <- data.frame(read.gdsn(index.gdsn(genofile, "genotype")))
names(geno_data) <- read.gdsn(index.gdsn(genofile, "snp.rs.id"))
geno_data <- cbind(data.frame(sample=read.gdsn(index.gdsn(genofile, "sample.id")),
                              population=read.gdsn(index.gdsn(genofile, "sample.annot/pop.group"))),
                   geno_data)
write.csv(x=geno_data,
          file="/Users/jonathanchernus/Documents/Teaching/hugen2073-textbook/exercises/11_machine_learning/genotypes.csv",
          quote=FALSE,
          row.names=FALSE)


data(illuminaSnpADF)
data(illuminaScanADF)
f <- system.file("extdata", "illumina_qxy.gds", package = "GWASdata")
gds <- GdsIntensityReader(f)
inten <- IntensityData(gds, snpAnnot = illuminaSnpADF, scanAnnot = illuminaScanADF)
xintensity <- getX(inten)
yintensity <- getY(inten)
save(xintensity,
     yintensity,
     file="/Users/jonathanchernus/Documents/Teaching/hugen2073-textbook/exercises/11_machine_learning/intensities.RData"
)
# 4 and 5 are good... 26 has to bad clusters? ... 33 has two clusters bleeding into each other
# 36: noise
# 43 ... bad
#i <- i+1
#plot(x[i,], y[i,], main=i)

### Rickroll data
# Rickroll-PCA silhouette demo (ONE block).
# Now with embedded text at the top that will also appear in PCA.
# Requires: magick

img_path <- "/Users/jonathanchernus/Documents/Teaching/hugen2073-textbook/exercises/11_machine_learning/rick_astley.jpg"
side <- 230
thr  <- 0.55
set.seed(1)

# ---- 1) Read image ----
img <- image_read(img_path)
# Resize first (so text placement matches final grid)
img <- image_resize(img, paste0(side, "x", side, "!"))
# ---- ADD TEXT BANNER ----
banner_height <- round(side * 0.15)
# Create white banner
banner <- image_blank(width = side, height = banner_height, color = "white")
# Add text to banner
banner <- image_annotate(
  banner,
  text = "NEVER GONNA GIVE YOU UP",
  size = round(side / 20),
  gravity = "center",
  color = "black",
  weight = 700
)

# Stack banner on top of image
img <- image_append(c(banner, img), stack = TRUE)

# Resize again to square (since we added height)
img <- image_resize(img, paste0(side, "x", side, "!"))

# ---- Convert to grayscale ----
img_g <- img |>
  image_convert(colorspace = "gray") |>
  image_contrast(sharpen = 1)

px <- as.numeric(image_data(img_g, channels = "gray")[1,,]) / 255
px <- matrix(px, nrow = side, ncol = side, byrow = TRUE)

# ---- 2) Keep dark pixels (silhouette) ----
grid_full <- expand.grid(r = 1:side, c = 1:side)
intensity_full <- px[cbind(grid_full$r, grid_full$c)]

keep <- intensity_full < thr
grid <- grid_full[keep, , drop = FALSE]
n <- nrow(grid)

x_true <- scale(grid$c, center = TRUE, scale = FALSE)[,1]
y_true <- scale(-grid$r, center = TRUE, scale = FALSE)[,1]

# ---- 3) High-dimensional embedding ----
p_x     <- 80
p_y     <- 80
p_mix   <- 60
p_warp  <- 60
p_noise <- 250

sd_sig  <- 0.08
sd_mix  <- 0.12
sd_warp <- 0.12
sd_n    <- 1.00

Xx <- replicate(p_x, x_true + rnorm(n, sd = sd_sig))
Xy <- replicate(p_y, y_true + rnorm(n, sd = sd_sig))

A <- matrix(runif(p_mix*2, -2, 2), nrow = p_mix, ncol = 2)
Xm <- sapply(1:p_mix, function(j)
  A[j,1]*x_true + A[j,2]*y_true + rnorm(n, sd = sd_mix)
)
Xm <- as.matrix(Xm)

Xw_base <- cbind(
  sin(x_true/3) + rnorm(n, sd = sd_warp),
  cos(x_true/4) + rnorm(n, sd = sd_warp),
  sin(y_true/3) + rnorm(n, sd = sd_warp),
  cos(y_true/4) + rnorm(n, sd = sd_warp),
  (x_true*y_true)/50 + rnorm(n, sd = sd_warp),
  (x_true^2)/200 + rnorm(n, sd = sd_warp),
  (y_true^2)/200 + rnorm(n, sd = sd_warp)
)

Xw <- do.call(cbind,
              replicate(ceiling(p_warp/ncol(Xw_base)), Xw_base, simplify = FALSE)
)
Xw <- Xw[, 1:p_warp, drop = FALSE]

Xn <- matrix(rnorm(n * p_noise, sd = sd_n), nrow = n)

X <- scale(cbind(Xx, Xy, Xm, Xw, Xn))
save(X, file="/Users/jonathanchernus/Documents/Teaching/hugen2073-textbook/exercises/11_machine_learning/pcadata.RData")

# ---- PCA ----
pca <- prcomp(X, center = FALSE, scale. = FALSE)
pc1 <- pca$x[,1]
pc2 <- pca$x[,2]

# ---- Plots ----
op <- par(mfrow = c(1,2), mar = c(3,3,2,1))

plot(x_true, y_true,
     pch = 16, cex = 0.25, asp = 1,
     xlab = "x_true", ylab = "y_true",
     main = "Input silhouette (with text)")

plot(pc1, pc2,
     pch = 16, cex = 0.25, asp = 1,
     xlab = "PC1", ylab = "PC2",
     main = "PCA shape-only reveal")

par(op)




### Exercise 1
# geno_data contains some genotyping array data
# Check that you can understand what the variables (probably) mean
# What is n? What is p?
# Make some plots. What can you learn?
# What kind of structure do you think *could* exist in this dataset?
# Do any of the plots you made demonstrate that structure?

### Exercise 2
# Try the same thing for non_geno_data

### Exercise 3
# 3a Distance concentration
# Draw 200 points from a normal distribution in d dimensions
# How close are the closest pair? How far are the furthest pair?
# Get the ratio of those distances (biggest to smallest)
set.seed(1)
distance_spread <- function(d, n=200) {
  X <- matrix(rnorm(n*d), nrow=n)
  D <- as.matrix(dist(X))
  D <- D[upper.tri(D)]
  c(min=min(D), max=max(D), mean=mean(D))
}
# Do this in a bunch of dimensions from 2 to 10,000
dims <- c(2, 5, 10, 50, 100, 500, 1000, 10000)
results <- sapply(dims, distance_spread)
ratio <- results["min",] / results["max",]
plot(dims, ratio, type="b",
     xlab="Dimension",
     ylab="Min/max distance ratio",
     main="Distance concentration")

# 3b Volume explosion
# It's pretty obvious that measure increases exponentially with dimension
# But also see that most of the space is near the "boundary"
# (The unit ball is *inside* the unit cube in each space)
vol_ratio <- function(d) {
  (pi^(d/2) / gamma(d/2 + 1)) / (2^d)
}
dims <- 1:20
ratios <- sapply(dims, vol_ratio)
plot(dims, ratios, type="b",
     xlab="Dimension",
     ylab="Volume of unit ball / unit cube")

# 3c Sparsity
# The average distance between uniform points gets bigger with dimension
dims <- 1:50
mean_dist <- sapply(dims, function(d) {
  X <- matrix(runif(1000*d), ncol=d)
  mean(dist(X))
})
plot(dims, mean_dist, type="b",
     xlab="Dimension",
     ylab="Mean distance (uniform cube)")

# 3d "Hallucinating" structure
# Simulate random data
set.seed(1)
n <- 400
d <- 100
X <- matrix(rnorm(n*d), nrow=n)
# Apply fancy ML stuff!
# PCA for visualization
# K-means clustering in original high-D space
pca <- prcomp(X, scale.=TRUE)
Z <- pca$x[,1:2]
# Plot the clusters
# Wow, we found patterns in the data! (But the data is random and meaningless)
km <- kmeans(X, centers=4, nstart=50)
plot(Z, col=km$cluster, pch=16,
     xlab="PC1", ylab="PC2",
     main="Pure noise + k-means clustering")
# Draw convex hulls to exaggerate cluster appearance
for (k in 1:4) {
  pts <- Z[km$cluster == k, ]
  hull <- chull(pts)
  polygon(pts[hull, ], border=k, lwd=2)
}

### Exercise 4
# 4a Apply PCA to the data from Exercises 1 and 2
# Use the prcomp function on the data, X
# Try a 2D plot of the PCs (use a small plotting symbol, like pch=".")
pca <- prcomp(X)
pc1 <- pca$x[,1]
pc2 <- pca$x[,2]
plot(pc1,pc2, pch=".")

# 4b Apply PCA to the genotype data
# Use procomp again. Are there any variables you should leave out?
# Make a 2D plot - how do you interpret it?
# Do you wonder anything about PC3? Plot it, too.
pca <- prcomp(geno_data[,c(-1,-2)])
pc1 <- pca$x[,1]
pc2 <- pca$x[,2]
plot(pc1,pc2, col=factor(geno_data$population))

pc3 <- pca$x[,3]
pcdata <- data.frame(pc1=pc1, pc2=pc2, pc3=pc3, group=geno_data$population)
plot_ly(pcdata,
        x = ~pc1,
        y = ~pc2,
        z = ~pc3,
        color = ~group,
        type = "scatter3d",
        mode = "markers")



### Example 5
# Make a scree plot for the PCA above - how many PCs look "interesting"?
# Variance explained
var_explained <- pca$sdev^2
prop_var <- var_explained / sum(var_explained)
# Scree plot
plot(prop_var,
     xlab = "Principal Component",
     ylab = "Proportion of Variance Explained")
plot(prop_var,
     xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     xlim=c(1,20))

### Exercise 7
# Make a parallel coordinate plot, colored by population
# How many PCs should you use for the plot?
# Interpret the plot
ggparcoord(
  cbind(
    pca$x[,1:20],
    data.frame(geno_data$population)
    ),
  columns = 1:20,
  groupColumn = 21
) 

### Exercise 8
# Repeat exercises 6-7 for only the HCB+JPT subset of the data
pca <- prcomp(geno_data[geno_data$population %in% c("HCB","JPT"),c(-1,-2)])
pc1 <- pca$x[,1]
pc2 <- pca$x[,2]
plot(pc1,pc2, col=factor(geno_data$population[geno_data$population %in% c("HCB","JPT")]))
var_explained <- pca$sdev^2
prop_var <- var_explained / sum(var_explained)
# Scree plot
plot(prop_var,
     xlab = "Principal Component",
     ylab = "Proportion of Variance Explained")
plot(prop_var,
     xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     xlim=c(1,20))
# Parallel coordinate plot
ggparcoord(
  cbind(
    pca$x[,1:20],
    data.frame(geno_data$population[geno_data$population %in% c("HCB","JPT")])
  ),
  columns = 1:20,
  groupColumn = 21
) 

### Exercise 9
# Look at these SNP intensity plots from a genotyping array
# Imagine we want to call the genotypes by clustering the points
# First, how many clusters should we expect/use?
# Second, pick a SNP you think is well-genotped and a SNP you think is poorly genoyped
# We will try clustering them with k-means
par(mfrow=c(7,7))
for (i in 1:49) {
  plot(xintensity[i,], yintensity[i,],
       main=i, cex=0.4, xlab="", ylab="", xaxt = "n", yaxt = "n")
}
par(mfrow=c(1,1))

# Pick k
# Try with a few snps
k <- 3
kmeans_click <- function(x, y, k = 3, iters = 20, seed = 1) {
  set.seed(seed)
  pts <- cbind(as.numeric(x), as.numeric(y))
  pts <- pts[complete.cases(pts), , drop = FALSE]
  n <- nrow(pts)
  
  cols <- c("red","blue","darkgreen","orange","purple","brown","cyan","magenta")[1:k]
  centers <- pts[sample.int(n, k), , drop = FALSE]
  
  # show initial random seeds
  plot(pts[,1], pts[,2], pch=16, col="grey70", xlab="X", ylab="Y",
       main="init seeds (click)")
  points(centers[,1], centers[,2], pch=4, cex=3, lwd=3, col=cols)
  locator(1)
  
  cl_old <- integer(n)
  
  for (t in 1:iters) {
    # assign
    d2 <- sapply(1:k, function(j)
      (pts[,1]-centers[j,1])^2 + (pts[,2]-centers[j,2])^2
    )
    cl <- max.col(-d2)
    
    # update (with empty-cluster reseed)
    for (j in 1:k) {
      idx <- which(cl == j)
      if (length(idx) > 0) centers[j,] <- colMeans(pts[idx, , drop=FALSE])
      else centers[j,] <- pts[sample.int(n, 1), ]
    }
    
    plot(pts[,1], pts[,2], pch=16, col=cols[cl], xlab="X", ylab="Y",
         main=paste("iter", t, "(click)"))
    points(centers[,1], centers[,2], pch=4, cex=3, lwd=3, col=cols)
    locator(1)
    
    if (identical(cl, cl_old)) break
    cl_old <- cl
  }
  
  invisible(list(cluster=cl, centers=centers))
}

# 4, 33
i <- 33
x <-  xintensity[i,]
y <-  yintensity[i,]
kmeans_click(x, y, k = 3, seed = 4)


### Exercise 10
# Last, apply k-means to the population data







genofile <- snpgdsOpen(snpgdsExampleFileName())
geno <- read.gdsn(index.gdsn(genofile, "genotype"))
pop <- read.gdsn(index.gdsn(genofile, "sample.annot/pop.group"))
# Run PCA
pca <- snpgdsPCA(genofile, autosome.only=FALSE)

# Extract scores
pc.percent <- pca$varprop*100
tab <- data.frame(
  sample.id = pca$sample.id,
  PC1 = pca$eigenvect[,1],
  PC2 = pca$eigenvect[,2]
)

plot(tab$PC1, tab$PC2,
     xlab=paste0("PC1 (", round(pc.percent[1],1), "%)"),
     ylab=paste0("PC2 (", round(pc.percent[2],1), "%)"),
     pch=19, col=factor(pop))