# Load packages
# Not using ggplot2 here, we just want the 'alpha' function
library("tidyverse")

# Read the data in
# You'll need to download the data and change the path
data <- read.csv("/Users/jonathanchernus/Documents/Teaching/2024s/HUGEN2073/lectures/lecture3_4/20220120-rlm-2073_Data.csv")

# Look at the data
dim(data)
head(data)
tail(data)
summary(data)
table(data$sex)

# Look at age and height
# Experiment with bin widths
hist(data$age)
hist(data$height)
hist(data$height, breaks = 10)
hist(data$height, breaks = 100)
hist(data$height, breaks = 1000)

# Scatterplot
plot(x = data$age, y = data$height)

# Change point settings
#   alpha is a transparency parameter (25% opaque)
plot(data$age, data$height,
     pch = 16,
     col = alpha("black", 0.25))

# How can we stratify by sex?
# Plotting separately in two windows
#   Females: dark green triangles
# y-axes don't match!
par(mfrow=c(1,2))
plot(data$age[data$sex == "F"],
     data$height[data$sex == "F"],
     pch = 17, col = alpha("darkgreen", 0.25),
     xlab="Age", ylab="Height")
#   Males: purple circles
plot(data$age[data$sex == "M"],
     data$height[data$sex == "M"],
     pch = 16, col = alpha("purple", 0.25),
     xlab="Age", ylab="Height")
par(mfrow=c(1,1))

# What are min and max ages?
summary(data$height)

# Plot again, this time with the same y-axis
par(mfrow=c(1,2))
plot(data$age[data$sex == "F"],
     data$height[data$sex == "F"],
     pch = 17, col = alpha("darkgreen", 0.25),
     xlab="Age", ylab="Height",
     ylim = c(143, 204))
#   Males: purple circles
plot(data$age[data$sex == "M"],
     data$height[data$sex == "M"],
     pch = 16, col = alpha("purple", 0.25),
     xlab="Age", ylab="Height",
     ylim = c(143, 204))
par(mfrow=c(1,1))

# Instead of two separate plots, use plot() and then points()
#   xlim and ylim are determined by plot(), based on only the females
#   so some male points are outside the plotting window
plot(data$age[data$sex == "F"],
     data$height[data$sex == "F"],
     pch = 17, col = alpha("darkgreen", 0.25),
     xlab="Age", ylab="Height")
#   Males: purple circles
points(data$age[data$sex == "M"],
     data$height[data$sex == "M"],
     pch = 16, col = alpha("purple", 0.25))

# Plot again, this time setting the y-axis manually
plot(data$age[data$sex == "F"],
     data$height[data$sex == "F"],
     pch = 17, col = alpha("darkgreen", 0.25),
     xlab="Age", ylab="Height",
     ylim = c(143, 204))
#   Males: purple circles
points(data$age[data$sex == "M"],
       data$height[data$sex == "M"],
       pch = 16, col = alpha("purple", 0.25))

# Another approach: use a *single* plot command
# Need to create vectors containing pch and col values
data$pch[data$sex == "F"] <- 17
data$pch[data$sex == "M"] <- 16
data$col[data$sex == "F"] <- "darkgreen"
data$col[data$sex == "M"] <- "purple"

# Check this this looks right
head(data)
table(data$sex, data$pch)
table(data$sex, data$col)

# Plot
plot(data$age, data$height,
     pch = data$pch,
     col = alpha(data$col, 0.25),
     xlab="Age", ylab="Height")