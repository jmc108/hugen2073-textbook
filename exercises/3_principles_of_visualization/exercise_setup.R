library(magick)
library(tidyverse)

# Read image
img <- image_read("./rick-astley.jpg") |>
  image_convert(type = "Grayscale") |>
  image_resize("300x300!") |>
  image_quantize(max = 2)

# arr is [channel, y, x] with hex values like "dd"
v_hex <- as.vector(arr[1,,])
v_int <- strtoi(v_hex, base = 16L)

m <- matrix(
  v_int,
  nrow = dim(arr)[2],
  ncol = dim(arr)[3],
  byrow = FALSE
)

dim(m)     # should be 300 300
range(m)   # should be ~0..255

rick_pts <- expand.grid(
  x = seq_len(ncol(m)),
  y = seq_len(nrow(m))
) |>
  mutate(pixel = as.vector(m)) |>
  filter(pixel == min(m)) |>
  mutate(
    a = rnorm(n()),
    b = runif(n()),
    group = sample(letters[1:5], n(), replace = TRUE)
  )

str(rick_pts)



ggplot(rick_pts, aes(x, y)) +
  geom_point(size = 0.1) +
  theme_void()

library(dplyr)

set.seed(123)  # reproducible decoys + sampling

student_df <- rick_pts |>
  transmute(
    v1 = x,
    v2 = -y,
    v3 = a,
    v4 = b,
    v5 = as.integer(factor(group))
  )

# Write out both formats (pick one or keep both)
write.csv(student_df, "./data1.csv", row.names = FALSE)

data1 <- read.csv("./data1.csv")
ggplot(data1, aes(v1, v2)) +
  geom_point(size = 0.1) +
  theme_void()