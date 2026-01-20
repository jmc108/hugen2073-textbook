# Install packages (just once)
install.packages("VennDiagram")
install.packages("UpSetR")
if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("GenomicRanges")
BiocManager::install("ComplexHeatmap")
install.packages("remotes")
remotes::install_github("jokergoo/ComplexHeatmap")

# Load packages
library("VennDiagram")
library("UpSetR")
library("RColorBrewer")
library("GenomicRanges")
library("ComplexHeatmap")



##### Making a 5-way Venn diagram

# Generate 3 sets of 200 words
set1 <- paste(rep("gene_" , 200) , sample(c(1:1000) , 200 , replace=F) , sep="")
set2 <- paste(rep("gene_" , 200) , sample(c(1:1000) , 200 , replace=F) , sep="")
set3 <- paste(rep("gene_" , 200) , sample(c(1:1000) , 200 , replace=F) , sep="")
set4 <- paste(rep("gene_" , 200) , sample(c(1:1000) , 200 , replace=F) , sep="")
set5 <- paste(rep("gene_" , 200) , sample(c(1:1000) , 200 , replace=F) , sep="")

# Chart once
venn.diagram(
  x = list(set1, set2, set3, set4, set5),
  category.names = paste("Set", 1:5),
  filename = '~/Desktop/venn_diagram_plain.png',
  output=TRUE
)

# Chart fancier
myCol <- brewer.pal(5, "Spectral")

venn.diagram(
  x = list(set1, set2, set3, set4, set5),
  category.names = c("Set 1" , "Set 2 " , "Set 3", "Set 4", "Set 5"),
  filename = '~/Desktop/venn_diagram_fancier.png',
  output=TRUE,
  fill = myCol,
)

##### Making upset charts
# Read files, convert to GRanges objects
# Data here: https://jokergoo.github.io/ComplexHeatmap-reference/book/upset-plot.html#example-with-the-genomic-regions
file_list <- c(
  "ESC" = "/Users/jonathanchernus/Downloads/E016-H3K4me3.narrowPeak",
  "ES-deriv1" = "/Users/jonathanchernus/Downloads/E004-H3K4me3.narrowPeak",
  "ES-deriv2" = "/Users/jonathanchernus/Downloads/E006-H3K4me3.narrowPeak",
  "Brain" = "/Users/jonathanchernus/Downloads/E071-H3K4me3.narrowPeak",
  "Muscle" = "/Users/jonathanchernus/Downloads/E100-H3K4me3.narrowPeak",
  "Heart" = "/Users/jonathanchernus/Downloads/E104-H3K4me3.narrowPeak"
)
peak_list <- lapply(file_list, function(f) {
  df = read.table(f)
  GRanges(seqnames = df[, 1], ranges = IRanges(df[, 2], df [, 3]))
})


# Make combination matrix
# This summarizes overlaps between the regions
# The "set sizes" are now sums of widths of genomic regions
m <- make_comb_mat(peak_list)
m <- m[comb_size(m) > 500000]
UpSet(m)

# Slightly better formatting
UpSet(m, 
      top_annotation = upset_top_annotation(
        m,
        axis_param = list(at = c(0, 1e7, 2e7),
                          labels = c("0Mb", "10Mb", "20Mb")),
        height = unit(4, "cm")
      ),
      right_annotation = upset_right_annotation(
        m,
        axis_param = list(at = c(0, 2e7, 4e7, 6e7),
                          labels = c("0Mb", "20Mb", "40Mb", "60Mb"),
                          labels_rot = 0),
        width = unit(4, "cm")
      ))


# Make it even more complicated...
# Add mean methylation for each genomic region (randomly generated here)
# Add distance to nearest TSS (randomly generated here)
subgroup = c("ESC" = "group1",
             "ES-deriv1" = "group1",
             "ES-deriv2" = "group1",
             "Brain" = "group2",
             "Muscle" = "group2",
             "Heart" = "group2"
)
comb_sets = lapply(comb_name(m), function(nm) extract_comb(m, nm))
comb_sets = lapply(comb_sets, function(gr) {
  # we just randomly generate dist_to_tss and mean_meth
  gr$dist_to_tss = abs(rnorm(length(gr), mean = runif(1, min = 500, max = 2000), sd = 1000))
  gr$mean_meth = abs(rnorm(length(gr), mean = 0.1, sd = 0.1))
  gr
})
UpSet(m, 
      top_annotation = upset_top_annotation(
        m,
        axis_param = list(at = c(0, 1e7, 2e7),
                          labels = c("0Mb", "10Mb", "20Mb")),
        height = unit(4, "cm")
      ),
      right_annotation = upset_right_annotation(
        m,
        axis_param = list(at = c(0, 2e7, 4e7, 6e7),
                          labels = c("0Mb", "20Mb", "40Mb", "60Mb"),
                          labels_rot = 0),
        width = unit(4, "cm")
      ),
      left_annotation = rowAnnotation(group = subgroup[set_name(m)], show_annotation_name = FALSE),
      bottom_annotation = HeatmapAnnotation(
        dist_to_tss = anno_boxplot(lapply(comb_sets, function(gr) gr$dist_to_tss), outline = FALSE),
        mean_meth = sapply(comb_sets, function(gr) mean(gr$mean_meth)),
        annotation_name_side = "left"
      )
)


# Another example - a movie dataset
# There are a lot of possibly genres - just just the top 6
movies = read.csv(system.file("extdata", "movies.csv", package = "UpSetR"), 
                  header = TRUE, sep = ";")
head(movies)
m <- make_comb_mat(movies, top_n_sets = 6)
m

m <- m[comb_degree(m) > 0]
UpSet(m)


# Customize the plot more
ss <- set_size(m)
cs <- comb_size(m)
ht <- UpSet(m, 
           set_order = order(ss),
           comb_order = order(comb_degree(m), -cs),
           top_annotation = HeatmapAnnotation(
             "Genre Intersections" = anno_barplot(cs, 
                                                  ylim = c(0, max(cs)*1.1),
                                                  border = FALSE, 
                                                  gp = gpar(fill = "black"), 
                                                  height = unit(4, "cm")
             ), 
             annotation_name_side = "left", 
             annotation_name_rot = 90),
           left_annotation = rowAnnotation(
             "Movies Per Genre" = anno_barplot(-ss, 
                                               baseline = 0,
                                               axis_param = list(
                                                 at = c(0, -500, -1000, -1500),
                                                 labels = c(0, 500, 1000, 1500),
                                                 labels_rot = 0),
                                               border = FALSE, 
                                               gp = gpar(fill = "black"), 
                                               width = unit(4, "cm")
             ),
             set_name = anno_text(set_name(m), 
                                  location = 0.5, 
                                  just = "center",
                                  width = max_text_width(set_name(m)) + unit(4, "mm"))
           ), 
           right_annotation = NULL,
           show_row_names = FALSE)
ht = draw(ht)
od = column_order(ht)
decorate_annotation("Genre Intersections", {
  grid.text(cs[od], x = seq_along(cs), y = unit(cs[od], "native") + unit(2, "pt"), 
            default.units = "native", just = c("left", "bottom"), 
            gp = gpar(fontsize = 6, col = "#404040"), rot = 45)
})
